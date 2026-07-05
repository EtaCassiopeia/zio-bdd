package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.FakeRift
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * Unit gate for the embedded (FFI) adapter's v2 wiring (#193) — no native
 * library. The data plane is a recording [[EmbeddedEngine]] double (echoes the
 * requested port, captures the latest replace-stubs JSON per port, records
 * `deleteImposter` calls), and the in-process admin plane is stood in for by
 * [[FakeRift]] (the same fake the container adapter's unit spec uses), so the
 * scenario/state control-plane calls the v2 adapter routes over the admin URL
 * can be asserted with no Docker and no engine.
 *
 * With the v2 admin plane up the adapter is capability-complete: all six
 * capabilities are advertised; the stub-based four are realised as
 * whole-imposter `rift_replace_stubs`, the stateful two route to the admin
 * client, and `destroy` frees the port via `rift_delete_imposter`.
 */
object EmbeddedRiftCapabilitiesSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A recording engine that echoes the requested port (parsed from the config, like the real
  // engine), keeps the latest replace-stubs JSON per port, and records deleteImposter calls. The v2
  // admin-plane methods aren't exercised here (the adapter is built directly with a FakeRift-backed
  // Admin), so they return canned values.
  private final class RecordingEngine(replaced: Ref[Map[Int, String]], deleted: Ref[Chunk[Int]]) extends EmbeddedEngine:
    def createImposter(configJson: String): IO[MockError, Int] =
      ZIO.succeed(portOf(configJson))
    def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit] =
      replaced.update(_.updated(port, stubsJson))
    def recorded(port: Int): IO[MockError, String] = ZIO.succeed("[]")
    def serveAdmin(optionsJson: String): IO[MockError, EmbeddedEngine.AdminInfo] =
      ZIO.succeed(EmbeddedEngine.AdminInfo("http://unused", 0))
    def deleteImposter(port: Int): IO[MockError, Unit]     = deleted.update(_ :+ port)
    def buildInfo: IO[MockError, EmbeddedEngine.BuildInfo] = ZIO.succeed(EmbeddedEngine.BuildInfo("test"))

    private def portOf(cfg: String): Int =
      cfg
        .fromJson[Json]
        .toOption
        .collect { case o: Json.Obj => o }
        .flatMap(_.fields.collectFirst { case ("port", Json.Num(n)) => n.intValue })
        .getOrElse(0)

  private def portFromUri(baseUri: String): Int = baseUri.substring(baseUri.lastIndexOf(':') + 1).toInt

  private def withControl(
    use: (MockControl, Ref[Map[Int, String]], Ref[Chunk[Int]], FakeRift) => IO[Any, TestResult]
  ): ZIO[Client & Provisioning, Any, TestResult] =
    ZIO.scoped {
      for
        prov            <- ZIO.service[Provisioning]
        client          <- ZIO.service[Client]
        replaced        <- Ref.make(Map.empty[Int, String])
        deleted         <- Ref.make(Chunk.empty[Int])
        adminAndFake    <- FakeRift.started
        (adminUrl, fake) = adminAndFake
        control <- EmbeddedRiftMockControl.make(
                     RecordingEngine(replaced, deleted),
                     prov,
                     EmbeddedRiftMockControl.Admin(adminUrl, client)
                   )
        result <- use(control, replaced, deleted, fake)
      yield result
    }

  def spec = suite("EmbeddedRiftCapabilities")(
    test("#211: an authored port is honoured — the imposter binds on it and baseUri reflects it") {
      withControl { (control, _, _, _) =>
        for
          space <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(9998)))).map(_.head)
        yield assertTrue(portFromUri(space.baseUri) == 9998, space.baseUri == "http://localhost:9998")
      }
    },
    test("#211: omitting the port keeps the auto-assigned default (a nonzero free port, not 9998)") {
      withControl { (control, _, _, _) =>
        for space <- control.provision(pingSource).map(_.head)
        yield assertTrue(portFromUri(space.baseUri) > 0, portFromUri(space.baseUri) != 9998)
      }
    },
    test("advertises all six capabilities and every accessor succeeds") {
      withControl { (control, _, _, _) =>
        for
          faultsE   <- control.faults.either
          scriptE   <- control.scripting.either
          proxyE    <- control.proxyRecord.either
          templateE <- control.templating.either
          scenE     <- control.scenarios.either
          siE       <- control.stateInspection.either
        yield assertTrue(
          control.capabilities == Set(
            Capability.Faults,
            Capability.Scripting,
            Capability.ProxyRecord,
            Capability.Templating,
            Capability.StatefulScenarios,
            Capability.StateInspection
          ),
          faultsE.isRight,
          scriptE.isRight,
          proxyE.isRight,
          templateE.isRight,
          scenE.isRight,
          siE.isRight
        )
      }
    },
    test("faults.inject rebuilds the imposter with a first-match _rift.fault stub ahead of the rules") {
      withControl { (control, replaced, _, _) =>
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
      withControl { (control, replaced, _, _) =>
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
          json.indexOf("/s") < json.indexOf("/ping"),
          json.indexOf("/p") < json.indexOf("/ping"),
          json.indexOf("/t") < json.indexOf("/ping")
        )
      }
    },
    test("an injected capability rule is tracked and removable; the rebuild drops the stub, unknown ids fail") {
      withControl { (control, replaced, _, _) =>
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
          !json.contains("CONNECTION_RESET_BY_PEER"),
          !json.contains("/boom"),
          json.contains("/ping"),
          ghostE == Left(MockError.RuleNotFound(space.id, RuleId("ghost")))
        )
      }
    },
    test("an injected extra survives a later addRule/replaceRules and stays first-match ahead of the new rules") {
      withControl { (control, replaced, _, _) =>
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
          afterAdd.contains("CONNECTION_RESET_BY_PEER") && afterAdd.contains("/added"),
          afterAdd.indexOf("/boom") < afterAdd.indexOf("/added"),
          afterRepl.contains("CONNECTION_RESET_BY_PEER") && afterRepl.contains("/replaced"),
          !afterRepl.contains("/added"),
          afterRepl.indexOf("/boom") < afterRepl.indexOf("/replaced")
        )
      }
    },
    test("scenarios: define registers the stateful stubs (FFI) and pins the initial state (admin plane)") {
      withControl { (control, replaced, _, fake) =>
        val invoice = ScenarioDef(
          "invoice",
          List(
            StatefulRule(
              ScenarioState("Unpaid"),
              RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/pay")),
              ResponseDef(status = 200),
              thenState = Some(ScenarioState("Paid"))
            )
          ),
          initial = ScenarioState("Unpaid")
        )
        for
          space <- control.provision(pingSource).map(_.head)
          port   = portFromUri(space.baseUri)
          ss    <- control.scenarios
          _     <- ss.define(space, invoice)
          stubs <- replaced.get.map(_.getOrElse(port, ""))
          puts  <- fake.scenarioPuts.get
        yield assertTrue(
          // the stateful stub is registered via rift_replace_stubs, ahead of the /ping rule
          stubs.contains("\"scenarioName\":\"invoice\""),
          stubs.contains("\"requiredScenarioState\":\"Unpaid\""),
          stubs.contains("\"newScenarioState\":\"Paid\""),
          stubs.indexOf("/pay") < stubs.indexOf("/ping"),
          // define pins the initial state via PUT …/scenarios/invoice/state {state, flowId=port}
          puts.exists(p =>
            p.startsWith(s"$port/invoice") && p.contains("\"state\":\"Unpaid\"") && p.contains(s"\"flowId\":\"$port\"")
          )
        )
      }
    },
    test("stateInspection: currentState reads the admin view; setState/reset pin; unknown scenarios fail") {
      withControl { (control, _, _, fake) =>
        val sc = ScenarioDef("s", List.empty, initial = ScenarioState("A"))
        for
          space  <- control.provision(pingSource).map(_.head)
          port    = portFromUri(space.baseUri)
          ss     <- control.scenarios
          si     <- control.stateInspection
          _      <- ss.define(space, sc)
          _      <- fake.setScenarios(s"""{"flowId":"$port","scenarios":[{"name":"s","state":"B"}]}""")
          cur    <- si.currentState(space, "s")
          gets   <- fake.scenarioGets.get
          _      <- si.setState(space, "s", ScenarioState("C"))
          _      <- ss.reset(space, "s")
          puts   <- fake.scenarioPuts.get
          ghostS <- si.currentState(space, "ghost").either
          ghostR <- ss.reset(space, "ghost").either
          // setState guards on the locally-tracked scenarios (a distinct path from currentState's
          // admin-view check and reset's map lookup) — an unknown name must fail before any PUT.
          ghostSet  <- si.setState(space, "ghost", ScenarioState("Z")).either
          putsAfter <- fake.scenarioPuts.get
        yield assertTrue(
          cur == ScenarioState("B"),
          gets.exists(_ == s"$port?$port"),                                            // GET …/scenarios?flowId=port
          puts.exists(p => p.startsWith(s"$port/s") && p.contains("\"state\":\"C\"")), // setState
          puts.exists(p => p.startsWith(s"$port/s") && p.contains("\"state\":\"A\"")), // reset → initial
          ghostS == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          ghostR == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          ghostSet == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          !putsAfter.exists(_.startsWith(s"$port/ghost")) // the guard rejects before issuing any PUT
        )
      }
    },
    test("a defined scenario's stubs survive a later addRule/replaceRules, staying ahead of the new rules") {
      withControl { (control, replaced, _, _) =>
        val invoice = ScenarioDef(
          "invoice",
          List(
            StatefulRule(
              ScenarioState("Unpaid"),
              RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/pay")),
              ResponseDef(status = 200),
              thenState = Some(ScenarioState("Paid"))
            )
          ),
          initial = ScenarioState("Unpaid")
        )
        val addedRule = MockRule(RequestMatch(path = PathMatch.Exact("/added")), ResponseDef(status = 200))
        val replRule  = MockRule(RequestMatch(path = PathMatch.Exact("/replaced")), ResponseDef(status = 200))
        for
          space     <- control.provision(pingSource).map(_.head)
          port       = portFromUri(space.baseUri)
          ss        <- control.scenarios
          _         <- ss.define(space, invoice)
          _         <- control.addRule(space, addedRule, Priority.Base)
          afterAdd  <- replaced.get.map(_.getOrElse(port, ""))
          _         <- control.replaceRules(space, List(replRule))
          afterRepl <- replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          // the whole-imposter rebuild threads scenarioStubs through addRule, still ahead of the new rule
          afterAdd.contains("\"scenarioName\":\"invoice\"") && afterAdd.contains("/added"),
          afterAdd.indexOf("/pay") < afterAdd.indexOf("/added"),
          // replaceRules swaps the ruleset but preserves the scenario stub, still first-match
          afterRepl.contains("\"scenarioName\":\"invoice\"") && afterRepl.contains("/replaced"),
          !afterRepl.contains("/added"),
          afterRepl.indexOf("/pay") < afterRepl.indexOf("/replaced")
        )
      }
    },
    test("destroy frees the port via rift_delete_imposter; later ops on the space fail SpaceNotFound") {
      withControl { (control, _, deleted, _) =>
        for
          space   <- control.provision(pingSource).map(_.head)
          port     = portFromUri(space.baseUri)
          _       <- control.destroy(space)
          deletes <- deleted.get
          goneE   <- control.received(space).either
        yield assertTrue(
          deletes == Chunk(port),
          goneE == Left(MockError.SpaceNotFound(space.id))
        )
      }
    }
  ).provide(Provisioning.live, Client.default) @@ TestAspect.withLiveClock
