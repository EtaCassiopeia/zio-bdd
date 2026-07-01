package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * Unit gate for the embedded (FFI) adapter's optional-capability wiring (#185).
 *
 * The FFI surface is exactly create-imposter / replace-stubs / read-recorded,
 * so every stub-based capability (faults, scripting, proxy/record, templating)
 * is realised as a whole-imposter `rift_replace_stubs` with the capability stub
 * registered ahead of the tracked rules (first-match). This spec drives the
 * adapter against a recording [[EmbeddedEngine]] double — no native library —
 * and asserts the emitted stub JSON, so it runs in a plain `sbt test`.
 *
 * StatefulScenarios + StateInspection stay unsupported: they need the
 * scenario-state endpoints (pin/read/reset initial state) the C-ABI does not
 * expose, so the embedded capability set is deliberately those four.
 */
object EmbeddedRiftCapabilitiesSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A recording engine that echoes the requested port (parsed from the config, like the real
  // engine) and keeps the latest replace-stubs JSON per port so the test can inspect the wire.
  private final class RecordingEngine(replaced: Ref[Map[Int, String]]) extends EmbeddedEngine:
    def createImposter(configJson: String): IO[MockError, Int] =
      ZIO.succeed(portOf(configJson))
    def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit] =
      replaced.update(_.updated(port, stubsJson))
    def recorded(port: Int): IO[MockError, String] = ZIO.succeed("[]")

    private def portOf(cfg: String): Int =
      cfg
        .fromJson[Json]
        .toOption
        .collect { case o: Json.Obj => o }
        .flatMap(_.fields.collectFirst { case ("port", Json.Num(n)) => n.intValue })
        .getOrElse(0)

  private def portFromUri(baseUri: String): Int = baseUri.substring(baseUri.lastIndexOf(':') + 1).toInt

  private def withControl(
    use: (MockControl, Ref[Map[Int, String]]) => IO[Any, TestResult]
  ): ZIO[Provisioning, Any, TestResult] =
    for
      prov     <- ZIO.service[Provisioning]
      replaced <- Ref.make(Map.empty[Int, String])
      control  <- EmbeddedRiftMockControl.make(RecordingEngine(replaced), prov)
      result   <- use(control, replaced)
    yield result

  def spec = suite("EmbeddedRiftCapabilities")(
    test("advertises exactly the four stub-based capabilities and every accessor succeeds") {
      withControl { (control, _) =>
        for
          faultsE   <- control.faults.either
          scriptE   <- control.scripting.either
          proxyE    <- control.proxyRecord.either
          templateE <- control.templating.either
        yield assertTrue(
          control.capabilities == Set(
            Capability.Faults,
            Capability.Scripting,
            Capability.ProxyRecord,
            Capability.Templating
          ),
          faultsE.isRight,
          scriptE.isRight,
          proxyE.isRight,
          templateE.isRight
        )
      }
    },
    test("faults.inject rebuilds the imposter with a first-match _rift.fault stub ahead of the rules") {
      withControl { (control, replaced) =>
        for
          space  <- control.provision(pingSource).map(_.head)
          port    = portFromUri(space.baseUri)
          faults <- control.faults
          _      <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          json   <- replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          json.contains("CONNECTION_RESET_BY_PEER"),
          json.contains("/boom"),
          json.contains("_rift"),
          json.indexOf("/boom") < json.indexOf("/ping") // fault wins first-match over the normal rule
        )
      }
    },
    test("scripting/proxyRecord/templating each rebuild with a first-match capability stub ahead of the rules") {
      withControl { (control, replaced) =>
        for
          space    <- control.provision(pingSource).map(_.head)
          port      = portFromUri(space.baseUri)
          script   <- control.scripting
          proxy    <- control.proxyRecord
          template <- control.templating
          _        <- script.inject(space, RequestMatch(path = PathMatch.Exact("/s")), Script(ScriptEngine.Rhai, "code"))
          _        <- proxy.proxy(space, RequestMatch(path = PathMatch.Exact("/p")), "http://up")
          _ <- template.inject(
                 space,
                 RequestMatch(path = PathMatch.Exact("/t")),
                 ResponseTemplate(body = "x${V}", captures = List(TemplateCapture("${V}", TemplateSource.Body, ".*")))
               )
          json <- replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          json.contains("rhai") && json.contains("/s"),
          json.contains("proxyOnce") && json.contains("http://up") && json.contains("/p"),
          json.contains("copy") && json.contains("${V}") && json.contains("/t"),
          // each capability stub wins first-match — the `contains` guards above ensure these index
          // comparisons aren't satisfied vacuously by a `-1` (absent) match.
          json.indexOf("/s") < json.indexOf("/ping"),
          json.indexOf("/p") < json.indexOf("/ping"),
          json.indexOf("/t") < json.indexOf("/ping")
        )
      }
    },
    test("an injected capability rule is tracked and removable; the rebuild drops the stub, unknown ids fail") {
      withControl { (control, replaced) =>
        for
          space  <- control.provision(pingSource).map(_.head)
          port    = portFromUri(space.baseUri)
          faults <- control.faults
          ruleId <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          rmE    <- control.removeRule(space, ruleId).either
          json   <- replaced.get.map(_.getOrElse(port, ""))
          ghostE <- control.removeRule(space, RuleId("ghost")).either
        yield assertTrue(
          rmE.isRight,
          !json.contains("CONNECTION_RESET_BY_PEER"), // the fault stub is gone after removal
          !json.contains("/boom"),
          json.contains("/ping"), // the normal rule survives
          ghostE == Left(MockError.RuleNotFound(space.id, RuleId("ghost")))
        )
      }
    },
    test("an injected extra survives a later addRule/replaceRules and stays first-match ahead of the new rules") {
      withControl { (control, replaced) =>
        val addedRule = MockRule(RequestMatch(path = PathMatch.Exact("/added")), ResponseDef(status = 200))
        val replRule  = MockRule(RequestMatch(path = PathMatch.Exact("/replaced")), ResponseDef(status = 200))
        for
          space     <- control.provision(pingSource).map(_.head)
          port       = portFromUri(space.baseUri)
          faults    <- control.faults
          _         <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          _         <- control.addRule(space, addedRule, Priority.Base)
          afterAdd  <- replaced.get.map(_.getOrElse(port, ""))
          _         <- control.replaceRules(space, List(replRule))
          afterRepl <- replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          // addRule keeps the fault stub, still ahead of the newly-added rule
          afterAdd.contains("CONNECTION_RESET_BY_PEER") && afterAdd.contains("/added"),
          afterAdd.indexOf("/boom") < afterAdd.indexOf("/added"),
          // replaceRules swaps the ruleset but preserves the injected fault, still first-match
          afterRepl.contains("CONNECTION_RESET_BY_PEER") && afterRepl.contains("/replaced"),
          !afterRepl.contains("/added"),
          afterRepl.indexOf("/boom") < afterRepl.indexOf("/replaced")
        )
      }
    },
    test("scenarios + stateInspection stay unsupported and are not advertised") {
      withControl { (control, _) =>
        for
          scenE <- control.scenarios.either
          siE   <- control.stateInspection.either
        yield assertTrue(
          scenE == Left(Unsupported(Capability.StatefulScenarios, "embedded")),
          siE == Left(Unsupported(Capability.StateInspection, "embedded")),
          !control.capabilities.contains(Capability.StatefulScenarios),
          !control.capabilities.contains(Capability.StateInspection)
        )
      }
    }
  ).provide(Provisioning.live) @@ TestAspect.withLiveClock
