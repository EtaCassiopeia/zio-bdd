package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.RiftMode
import zio.json.*
import zio.test.*

/**
 * Unit gate for the embedded (FFI) adapter — no native library, and no loopback
 * HTTP admin plane (#244). The engine is a recording [[EmbeddedEngine]] double
 * that captures every downcall: the data plane (echoes the requested port,
 * keeps the latest replace-stubs JSON per port, records `deleteImposter`), the
 * scenario/flow-state plane (`rift_flow_state_get`/`put`), and the correlated
 * space plane (`rift_space_add_stub`/`delete`/`recorded`). So the whole SPI —
 * including the admin long tail that used to route over the in-process admin
 * URL — is asserted with no Docker, no engine, and no HTTP client.
 */
object EmbeddedRiftCapabilitiesSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A recording engine that echoes the requested port (parsed from the config, like the real engine),
  // keeps the latest replace-stubs JSON per port, records deleteImposter calls, and — for #244 —
  // captures the scenario/flow-state map and the per-space stub lists the adapter drives over FFI.
  private final class RecordingEngine(
    val replaced: Ref[Map[Int, String]],
    val deleted: Ref[Chunk[Int]],
    val flowState: Ref[Map[(Int, String, String), String]],
    val spaceStubs: Ref[Map[(Int, String), Vector[String]]]
  ) extends EmbeddedEngine:
    def createImposter(configJson: String): IO[MockError, Int] = ZIO.succeed(portOf(configJson))
    def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit] =
      replaced.update(_.updated(port, stubsJson))
    def recorded(port: Int): IO[MockError, String] = ZIO.succeed("[]")
    def serveAdmin(optionsJson: String): IO[MockError, EmbeddedEngine.AdminInfo] =
      ZIO.succeed(EmbeddedEngine.AdminInfo("http://unused", 0))
    def deleteImposter(port: Int): IO[MockError, Unit]     = deleted.update(_ :+ port)
    def buildInfo: IO[MockError, EmbeddedEngine.BuildInfo] = ZIO.succeed(EmbeddedEngine.BuildInfo("test"))

    // Admin long tail over the recorded FFI (#244): flow-state is keyed (port, flowId, key) -> the
    // stored valueJson; `get` decodes it back to the string form the adapter reads (scenario state is
    // a JSON string). The space plane appends/clears the per-(port, flowId) stub list.
    def flowStateGet(port: Int, flowId: String, key: String): IO[MockError, Option[String]] =
      flowState.get.map(_.get((port, flowId, key)).flatMap(_.fromJson[String].toOption))
    def flowStatePut(port: Int, flowId: String, key: String, valueJson: String): IO[MockError, Unit] =
      flowState.update(_.updated((port, flowId, key), valueJson))
    def spaceAddStub(port: Int, flowId: String, stubJson: String): IO[MockError, Unit] =
      spaceStubs.update(m => m.updated((port, flowId), m.getOrElse((port, flowId), Vector.empty) :+ stubJson))
    def spaceDelete(port: Int, flowId: String): IO[MockError, Unit] =
      spaceStubs.update(_ - ((port, flowId)))
    def spaceRecorded(port: Int, flowId: String): IO[MockError, String] = ZIO.succeed("[]")

    private def portOf(cfg: String): Int =
      cfg
        .fromJson[zio.json.ast.Json]
        .toOption
        .collect { case o: zio.json.ast.Json.Obj => o }
        .flatMap(_.fields.collectFirst { case ("port", zio.json.ast.Json.Num(n)) => n.intValue })
        .getOrElse(0)

  private def portFromUri(baseUri: String): Int = baseUri.substring(baseUri.lastIndexOf(':') + 1).toInt

  private def withControl(mode: RiftMode = RiftMode.PerInstance)(
    use: (MockControl, RecordingEngine) => IO[Any, TestResult]
  ): ZIO[Provisioning, Any, TestResult] =
    ZIO.scoped {
      for
        prov       <- ZIO.service[Provisioning]
        replaced   <- Ref.make(Map.empty[Int, String])
        deleted    <- Ref.make(Chunk.empty[Int])
        flowState  <- Ref.make(Map.empty[(Int, String, String), String])
        spaceStubs <- Ref.make(Map.empty[(Int, String), Vector[String]])
        engine      = RecordingEngine(replaced, deleted, flowState, spaceStubs)
        control    <- EmbeddedRiftMockControl.make(engine, prov, mode)
        result     <- use(control, engine)
      yield result
    }

  def spec = suite("EmbeddedRiftCapabilities")(
    test("#211: an authored port is honoured — the imposter binds on it and baseUri reflects it") {
      withControl() { (control, _) =>
        for space <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(9998)))).map(_.head)
        yield assertTrue(portFromUri(space.baseUri) == 9998, space.baseUri == "http://localhost:9998")
      }
    },
    test("#211: omitting the port keeps the auto-assigned default (a nonzero free port, not 9998)") {
      withControl() { (control, _) =>
        for space <- control.provision(pingSource).map(_.head)
        yield assertTrue(portFromUri(space.baseUri) > 0, portFromUri(space.baseUri) != 9998)
      }
    },
    test("advertises all six capabilities and every accessor succeeds") {
      withControl() { (control, _) =>
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
      withControl() { (control, engine) =>
        for
          space  <- control.provision(pingSource).map(_.head)
          port    = portFromUri(space.baseUri)
          faults <- control.faults
          _      <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          json   <- engine.replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          json.contains("CONNECTION_RESET_BY_PEER"),
          json.contains("/boom"),
          json.contains("_rift"),
          json.indexOf("/boom") < json.indexOf("/ping")
        )
      }
    },
    test("scripting/proxyRecord/templating each rebuild with a first-match capability stub ahead of the rules") {
      withControl() { (control, engine) =>
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
          json <- engine.replaced.get.map(_.getOrElse(port, ""))
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
      withControl() { (control, engine) =>
        for
          space  <- control.provision(pingSource).map(_.head)
          port    = portFromUri(space.baseUri)
          faults <- control.faults
          ruleId <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          rmE    <- control.removeRule(space, ruleId).either
          json   <- engine.replaced.get.map(_.getOrElse(port, ""))
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
      withControl() { (control, engine) =>
        val addedRule = MockRule(RequestMatch(path = PathMatch.Exact("/added")), ResponseDef(status = 200))
        val replRule  = MockRule(RequestMatch(path = PathMatch.Exact("/replaced")), ResponseDef(status = 200))
        for
          space     <- control.provision(pingSource).map(_.head)
          port       = portFromUri(space.baseUri)
          faults    <- control.faults
          _         <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          _         <- control.addRule(space, addedRule, Priority.Base)
          afterAdd  <- engine.replaced.get.map(_.getOrElse(port, ""))
          _         <- control.replaceRules(space, List(replRule))
          afterRepl <- engine.replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          afterAdd.contains("CONNECTION_RESET_BY_PEER") && afterAdd.contains("/added"),
          afterAdd.indexOf("/boom") < afterAdd.indexOf("/added"),
          afterRepl.contains("CONNECTION_RESET_BY_PEER") && afterRepl.contains("/replaced"),
          !afterRepl.contains("/added"),
          afterRepl.indexOf("/boom") < afterRepl.indexOf("/replaced")
        )
      }
    },
    test("scenarios: define registers the stateful stubs (replace_stubs) and pins the initial state (flow_state_put)") {
      withControl() { (control, engine) =>
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
          stubs <- engine.replaced.get.map(_.getOrElse(port, ""))
          flow  <- engine.flowState.get
        yield assertTrue(
          stubs.contains("\"scenarioName\":\"invoice\""),
          stubs.contains("\"requiredScenarioState\":\"Unpaid\""),
          stubs.contains("\"newScenarioState\":\"Paid\""),
          stubs.indexOf("/pay") < stubs.indexOf("/ping"),
          // define pins the initial state via rift_flow_state_put(port, flowId=port, "invoice", "\"Unpaid\"")
          flow.get((port, port.toString, "invoice")).contains("\"Unpaid\"")
        )
      }
    },
    test("stateInspection: currentState reads flow state (FFI); setState/reset pin; unknown scenarios fail") {
      withControl() { (control, engine) =>
        val sc = ScenarioDef("s", List.empty, initial = ScenarioState("A"))
        for
          space      <- control.provision(pingSource).map(_.head)
          port        = portFromUri(space.baseUri)
          ss         <- control.scenarios
          si         <- control.stateInspection
          _          <- ss.define(space, sc)                                                    // pins "A"
          _          <- engine.flowState.update(_.updated((port, port.toString, "s"), "\"B\"")) // simulate a transition
          cur        <- si.currentState(space, "s")
          _          <- si.setState(space, "s", ScenarioState("C"))
          afterSet   <- engine.flowState.get.map(_.get((port, port.toString, "s")))
          _          <- ss.reset(space, "s")
          afterReset <- engine.flowState.get.map(_.get((port, port.toString, "s")))
          ghostS     <- si.currentState(space, "ghost").either
          ghostR     <- ss.reset(space, "ghost").either
          // setState guards on the locally-tracked scenarios — an unknown name must fail before any put.
          ghostSet  <- si.setState(space, "ghost", ScenarioState("Z")).either
          flowAfter <- engine.flowState.get
        yield assertTrue(
          cur == ScenarioState("B"),
          afterSet.contains("\"C\""),
          afterReset.contains("\"A\""), // reset → initial
          ghostS == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          ghostR == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          ghostSet == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          !flowAfter.contains((port, port.toString, "ghost")) // the guard rejects before issuing any put
        )
      }
    },
    test("a defined scenario's stubs survive a later addRule/replaceRules, staying ahead of the new rules") {
      withControl() { (control, engine) =>
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
          afterAdd  <- engine.replaced.get.map(_.getOrElse(port, ""))
          _         <- control.replaceRules(space, List(replRule))
          afterRepl <- engine.replaced.get.map(_.getOrElse(port, ""))
        yield assertTrue(
          afterAdd.contains("\"scenarioName\":\"invoice\"") && afterAdd.contains("/added"),
          afterAdd.indexOf("/pay") < afterAdd.indexOf("/added"),
          afterRepl.contains("\"scenarioName\":\"invoice\"") && afterRepl.contains("/replaced"),
          !afterRepl.contains("/added"),
          afterRepl.indexOf("/pay") < afterRepl.indexOf("/replaced")
        )
      }
    },
    test("correlated: a space registers via rift_space_add_stub and destroy tears it down via rift_space_delete") {
      withControl(RiftMode.correlated) { (control, engine) =>
        val srcA =
          MockSource.Dsl(
            MockSpec(List(MockRule(RequestMatch(path = PathMatch.Exact("/a")), ResponseDef(status = 200))))
          )
        for
          a         <- control.provision(srcA).map(_.head)
          afterProv <- engine.spaceStubs.get
          recA      <- control.received(a) // routes over rift_space_recorded (→ [])
          _         <- control.destroy(a)
          afterDel  <- engine.spaceStubs.get
        yield assertTrue(
          control.isolation == Isolation.Correlated,
          afterProv.exists { case (_, stubs) => stubs.exists(_.contains("/a")) }, // registered via the space FFI
          recA.isEmpty, // received via the space FFI
          afterDel.forall { case (_, stubs) => !stubs.exists(_.contains("/a")) } // torn down via the space FFI
        )
      }
    },
    test("correlated: a fault + overlay rule rebuild the space via the FFI, first-match ahead of the base rule") {
      withControl(RiftMode.correlated) { (control, engine) =>
        val base =
          MockSource.Dsl(
            MockSpec(List(MockRule(RequestMatch(path = PathMatch.Exact("/base")), ResponseDef(status = 200))))
          )
        for
          space  <- control.provision(base).map(_.head)
          faults <- control.faults
          // corrInjectFault → rebuild (fault ahead of the base rule)
          _ <- faults.inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
          // corrAddRule(Overlay) → rebuild (overlay ahead of the base rule, behind the fault)
          _ <- control.addRule(
                 space,
                 MockRule(RequestMatch(path = PathMatch.Exact("/over")), ResponseDef(status = 200)),
                 Priority.Overlay
               )
          // one shared imposter, one flow → the last rebuild's registration order is the stub list
          stubs <- engine.spaceStubs.get.map(_.values.headOption.getOrElse(Vector.empty).mkString("\n"))
        yield assertTrue(
          stubs.contains("CONNECTION_RESET_BY_PEER") && stubs.contains("/boom"),
          stubs.contains("/over") && stubs.contains("/base"),
          stubs.indexOf("/boom") < stubs.indexOf("/over"), // fault registered ahead of the rules
          stubs.indexOf("/over") < stubs.indexOf("/base")  // overlay first-match over the base rule
        )
      }
    },
    test("destroy frees the port via rift_delete_imposter; later ops on the space fail SpaceNotFound") {
      withControl() { (control, engine) =>
        for
          space   <- control.provision(pingSource).map(_.head)
          port     = portFromUri(space.baseUri)
          _       <- control.destroy(space)
          deletes <- engine.deleted.get
          goneE   <- control.received(space).either
        yield assertTrue(
          deletes == Chunk(port),
          goneE == Left(MockError.SpaceNotFound(space.id))
        )
      }
    }
  ).provide(Provisioning.live) @@ TestAspect.withLiveClock
