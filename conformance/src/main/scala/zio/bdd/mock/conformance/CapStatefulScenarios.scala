package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

/**
 * The `cap-stateful` conformance feature (#129): the normative single-token-FSM
 * semantics, programmed against the neutral [[StatefulScenarios]] /
 * [[StateInspection]] capabilities. Each scenario is capability-gated, so it
 * runs (and validates the semantics) only on a backend that advertises the
 * capability — until #130 (WireMock) / #131 (Rift) land, every backend
 * justifiably SKIPs.
 */
object CapStatefulScenarios:

  lazy val all: List[ConformanceScenario] = core ++ inspection

  // ---- helpers ------------------------------------------------------------------

  private def asT(e: MockError): Throwable    = new RuntimeException(s"MockError: $e")
  private def asTU(u: Unsupported): Throwable = new RuntimeException(s"Unsupported: $u")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  // An empty space; the scenario's own rules are installed by `define`.
  private val emptySpace: MockSource = MockSource.Dsl(MockSpec(Nil))

  private def space(control: MockControl): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(emptySpace).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  // Started --GET /inv--> "created" --> Paid --GET /inv--> "paid" (stays).
  private val invoice: ScenarioDef =
    scenario("invoice")
      .when("Started", get("/inv"))
      .respond(ok.text("created"))
      .goTo("Paid")
      .when("Paid", get("/inv"))
      .respond(ok.text("paid"))
      .stay
      .build

  // Two rules eligible in "Started" for the same request: the first declared wins.
  private val firstMatch: ScenarioDef =
    scenario("fm")
      .when("Started", get("/fm"))
      .respond(ok.text("first"))
      .stay
      .when("Started", get("/fm"))
      .respond(ok.text("second"))
      .stay
      .build

  private def scen(name: String, requires: Set[Capability])(
    check: MockControl => ZIO[Scope, Throwable, Unit]
  ): ConformanceScenario =
    ConformanceScenario(name, requires, check)

  private val needsStateful = Set(Capability.StatefulScenarios)
  private val needsBoth     = Set(Capability.StatefulScenarios, Capability.StateInspection)

  // ---- core (StatefulScenarios) -------------------------------------------------

  private val core = List(
    scen("stateful: eligibility + transition advance the FSM", needsStateful) { control =>
      ZIO.scoped {
        for
          ss <- control.scenarios.mapError(asTU)
          s  <- space(control)
          _  <- ss.define(s, invoice).mapError(asT)
          r1 <- SutClient.make(s).send(Method.Get, "/inv") // Started -> "created", -> Paid
          r2 <- SutClient.make(s).send(Method.Get, "/inv") // Paid    -> "paid"
          _  <- ensure(r1.body == "created" && r2.body == "paid", s"transition: r1=${r1.body} r2=${r2.body}")
        yield ()
      }
    },
    scen("stateful: a request matching no eligible rule leaves the state unchanged", needsStateful) { control =>
      ZIO.scoped {
        for
          ss   <- control.scenarios.mapError(asTU)
          s    <- space(control)
          _    <- ss.define(s, invoice).mapError(asT)
          miss <- SutClient.make(s).send(Method.Get, "/other") // no eligible rule -> no transition
          hit  <- SutClient.make(s).send(Method.Get, "/inv")   // still in Started -> "created"
          _    <- ensure(miss.status == 404 && hit.body == "created", s"no-change: miss=${miss.status} hit=${hit.body}")
        yield ()
      }
    },
    scen("stateful: reset returns the FSM to its initial state", needsStateful) { control =>
      ZIO.scoped {
        for
          ss <- control.scenarios.mapError(asTU)
          s  <- space(control)
          _  <- ss.define(s, invoice).mapError(asT)
          _  <- SutClient.make(s).send(Method.Get, "/inv") // advance to Paid
          _  <- ss.reset(s, "invoice").mapError(asT)
          r  <- SutClient.make(s).send(Method.Get, "/inv") // back in Started -> "created"
          _  <- ensure(r.body == "created", s"reset: r=${r.body}")
        yield ()
      }
    },
    scen("stateful: two spaces sharing a scenario name keep independent state (locality)", needsStateful) { control =>
      ZIO.scoped {
        for
          ss <- control.scenarios.mapError(asTU)
          a  <- space(control)
          b  <- space(control)
          _  <- ss.define(a, invoice).mapError(asT)
          _  <- ss.define(b, invoice).mapError(asT)
          _  <- SutClient.make(a).send(Method.Get, "/inv") // advance A to Paid
          aR <- SutClient.make(a).send(Method.Get, "/inv") // A -> "paid"
          bR <- SutClient.make(b).send(Method.Get, "/inv") // B untouched -> "created"
          _  <- ensure(aR.body == "paid" && bR.body == "created", s"locality: a=${aR.body} b=${bR.body}")
        yield ()
      }
    },
    scen("stateful: among rules eligible in the same state, the first declared wins (first-match)", needsStateful) {
      control =>
        ZIO.scoped {
          for
            ss <- control.scenarios.mapError(asTU)
            s  <- space(control)
            _  <- ss.define(s, firstMatch).mapError(asT)
            r  <- SutClient.make(s).send(Method.Get, "/fm") // both rules eligible -> first wins
            _  <- ensure(r.body == "first", s"first-match: r=${r.body}")
          yield ()
        }
    },
    scen("stateful: concurrent advances on independent spaces stay isolated (concurrency)", needsStateful) { control =>
      ZIO.scoped {
        for
          ss     <- control.scenarios.mapError(asTU)
          spaces <- ZIO.foreachPar(1 to 8)(_ => space(control))
          _      <- ZIO.foreachPar(spaces)(s => ss.define(s, invoice).mapError(asT))
          // concurrently advance the even-indexed spaces to Paid; the odd ones stay in Started
          _ <- ZIO.foreachPar(spaces.zipWithIndex.filter((_, i) => i % 2 == 0)) { (s, _) =>
                 SutClient.make(s).send(Method.Get, "/inv")
               }
          bodies <-
            ZIO.foreachPar(spaces.zipWithIndex)((s, i) => SutClient.make(s).send(Method.Get, "/inv").map(i -> _.body))
          _ <- ensure(
                 bodies.forall((i, b) => b == (if i % 2 == 0 then "paid" else "created")),
                 s"concurrency: $bodies"
               )
        yield ()
      }
    }
  )

  // ---- StateInspection (gated separately) ---------------------------------------

  private val inspection = List(
    scen("inspection: currentState reflects the FSM; setState forces a transition", needsBoth) { control =>
      ZIO.scoped {
        for
          ss   <- control.scenarios.mapError(asTU)
          si   <- control.stateInspection.mapError(asTU)
          s    <- space(control)
          _    <- ss.define(s, invoice).mapError(asT)
          init <- si.currentState(s, "invoice").mapError(asT)
          _    <- si.setState(s, "invoice", ScenarioState("Paid")).mapError(asT)
          now  <- si.currentState(s, "invoice").mapError(asT)
          r    <- SutClient.make(s).send(Method.Get, "/inv") // forced to Paid -> "paid"
          _ <- ensure(
                 init == ScenarioState.Started && now == ScenarioState("Paid") && r.body == "paid",
                 s"inspection: init=$init now=$now r=${r.body}"
               )
        yield ()
      }
    }
  )
