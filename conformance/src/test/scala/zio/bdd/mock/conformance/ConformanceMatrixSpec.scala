package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.Rift
import zio.bdd.mock.wiremock.WireMock
import zio.http.Client
import zio.test.*

/**
 * The conformance matrix (#124): one portable scenario set run against every
 * registered backend, emitting pass/skip/fail. WireMock runs in-process always;
 * the Rift container backend is gated behind `RIFT_IT` (like
 * RiftContainerSpec), so `sbt test` stays Docker-free and CI runs the real
 * cross-adapter matrix.
 */
object ConformanceMatrixSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")
  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private val pingSource =
    MockSource.Dsl(
      MockSpec(
        List(
          MockRule(
            RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
            ResponseDef(status = 200, body = Body.Text("pong"))
          )
        )
      )
    )

  // ---- the portable scenarios (programmed against MockControl, never a backend) ----

  private val provisionServeRecord = "provision, serve, and record a request"
  private val isolationReceived    = "isolation: received(A) returns only A's traffic"
  private val injectFault          = "inject a fault"

  private val scenarios = List(
    ConformanceScenario(
      provisionServeRecord,
      Set.empty,
      control =>
        for
          spaces <- control.provision(pingSource).mapError(asT)
          space   = spaces.head
          resp   <- SutClient.make(space).send(Method.Get, "/ping")
          recv   <- control.received(space).mapError(asT)
          _      <- control.destroy(space).mapError(asT)
          _ <- ensure(
                 resp.status == 200 && resp.body == "pong" && recv.exists(_.uri == "/ping"),
                 s"basic: status=${resp.status} body=${resp.body} recv=$recv"
               )
        yield ()
    ),
    ConformanceScenario(
      isolationReceived,
      Set.empty,
      // Portable across modes: PerInstance (two ports) and Correlated (one server,
      // header-filtered) both yield received(A) = only A's traffic.
      control =>
        for
          a  <- control.provision(pingSource).mapError(asT).map(_.head)
          b  <- control.provision(pingSource).mapError(asT).map(_.head)
          _  <- SutClient.make(a).send(Method.Get, "/ping")
          _  <- SutClient.make(b).send(Method.Get, "/ping")
          _  <- SutClient.make(b).send(Method.Get, "/ping")
          ra <- control.received(a).mapError(asT)
          rb <- control.received(b).mapError(asT)
          _  <- control.destroy(a).mapError(asT)
          _  <- control.destroy(b).mapError(asT)
          _  <- ensure(ra.size == 1 && rb.size == 2, s"isolation: ra=${ra.size} rb=${rb.size}")
        yield ()
    ),
    // Requires Faults — neither adapter advertises it yet (M3), so it must SKIP, never FAIL.
    ConformanceScenario(
      injectFault,
      Set(Capability.Faults),
      control => control.faults.mapError(u => new RuntimeException(u.toString)).unit
    )
  )

  // ---- the backends ----

  private val wiremock =
    MockBackendUnderTest("wiremock", Provisioning.live >>> WireMock.correlated(), Set.empty, Isolation.Correlated)

  private val riftEnabled = sys.env.contains("RIFT_IT")
  private val rift =
    MockBackendUnderTest(
      "rift",
      (Client.default ++ Provisioning.live) >>> Rift.managed().mapError(asT),
      Set.empty,
      Isolation.PerInstance,
      available = riftEnabled
    )

  def spec = suite("ConformanceMatrix")(
    test("runs the portable scenario set across backends and emits a conformant matrix") {
      for
        matrix <- ConformanceHarness.run(List(wiremock, rift), scenarios)
        _      <- ZIO.logInfo(s"conformance matrix:\n${matrix.render}")
      yield assertTrue(
        // WireMock (in-process, always available)
        matrix.cell(provisionServeRecord, "wiremock").map(_.outcome).contains(Outcome.Pass),
        matrix.cell(isolationReceived, "wiremock").map(_.outcome).contains(Outcome.Pass),
        matrix
          .cell(injectFault, "wiremock")
          .map(_.outcome)
          .contains(Outcome.Skip),   // Faults un-advertised → SKIP, not FAIL
        matrix.conformant(wiremock), // zero FAIL, skip justified by capability
        // Rift: conformant when available (CI with RIFT_IT), else the column is SKIPPED-unavailable (local, no Docker)
        if riftEnabled then matrix.conformant(rift)
        else matrix.cell(provisionServeRecord, "rift").map(_.outcome).contains(Outcome.Skip),
        matrix.render.contains("wiremock")
      )
    } @@ TestAspect.withLiveClock,
    test("core conformance features pass on every adapter (#125)") {
      val all = CoreConformanceScenarios.all
      for
        matrix <- ConformanceHarness.run(List(wiremock, rift), all)
        _      <- ZIO.logInfo(s"core conformance matrix:\n${matrix.render}")
        // Surface any non-Pass cell's detail so a failure is debuggable from the log.
        wmFails   = nonPass(matrix, "wiremock", all)
        _        <- ZIO.logInfo(s"wiremock non-pass: $wmFails").when(wmFails.nonEmpty)
        riftCells = all.flatMap(s => matrix.cell(s.name, "rift").map(_.outcome))
      yield assertTrue(
        all.nonEmpty, // the catalog actually ran
        all.forall(s =>
          matrix.cell(s.name, "wiremock").exists(_.outcome == Outcome.Pass)
        ), // every scenario PASSed (presence, not absence-of-Fail)
        matrix.cells.count(
          _.backend == "wiremock"
        ) == all.size, // every scenario produced exactly one cell (no missing/aliased name)
        // Rift: every scenario PASSes in CI (RIFT_IT); else the whole column is SKIPPED-unavailable.
        if riftEnabled then all.forall(s => matrix.cell(s.name, "rift").exists(_.outcome == Outcome.Pass))
        else riftCells.size == all.size && riftCells.forall(_ == Outcome.Skip),
        matrix.conformant(wiremock)
      )
    } @@ TestAspect.withLiveClock,
    test("capability-negotiation + error-semantics features pass on every adapter (#127)") {
      val all = NegotiationErrorScenarios.all
      for
        matrix   <- ConformanceHarness.run(List(wiremock, rift), all)
        _        <- ZIO.logInfo(s"negotiation/error matrix:\n${matrix.render}")
        wmFails   = nonPass(matrix, "wiremock", all)
        _        <- ZIO.logInfo(s"wiremock non-pass: $wmFails").when(wmFails.nonEmpty)
        riftCells = all.flatMap(s => matrix.cell(s.name, "rift").map(_.outcome))
      yield assertTrue(
        all.nonEmpty,
        all.forall(s => matrix.cell(s.name, "wiremock").exists(_.outcome == Outcome.Pass)),
        matrix.cells.count(_.backend == "wiremock") == all.size,
        if riftEnabled then all.forall(s => matrix.cell(s.name, "rift").exists(_.outcome == Outcome.Pass))
        else riftCells.size == all.size && riftCells.forall(_ == Outcome.Skip)
      )
    } @@ TestAspect.withLiveClock
  )

  private def nonPass(matrix: Matrix, backend: String, scenarios: List[ConformanceScenario]): List[String] =
    scenarios.flatMap { s =>
      matrix
        .cell(s.name, backend)
        .filter(_.outcome != Outcome.Pass)
        .map(c => s"${c.scenario} -> ${c.outcome}: ${c.detail.getOrElse("")}")
    }
