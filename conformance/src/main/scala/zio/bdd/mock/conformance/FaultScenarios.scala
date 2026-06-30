package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * The portable fault-injection conformance scenarios (#128) — the `cap-faults`
 * feature. Each is programmed against the neutral [[MockControl]] (never a
 * concrete backend) and gated on [[Capability.Faults]], so a backend that does
 * not advertise Faults SKIPs them. Asserting via a real [[SutClient]]
 * round-trip makes each fault kind produce its specified *client-observable*
 * failure identically on every advertising adapter (WireMock natively; Rift via
 * `_rift.fault`, rift#239).
 *
 *   - The four connection kinds abort the transport, so the SUT's client throws
 *     (the `send` effect FAILs) — observed as `Exit.isFailure`, not a status.
 *   - [[FaultKind.LatencySpike]] delays an otherwise-normal 200 response —
 *     observed as a status-200 round-trip whose latency exceeds a baseline.
 */
object FaultScenarios:

  lazy val all: List[ConformanceScenario] =
    List(
      connectionFault("faults: ConnectionReset aborts the client connection", FaultKind.ConnectionReset),
      connectionFault("faults: EmptyResponse aborts the client connection", FaultKind.EmptyResponse),
      connectionFault("faults: MalformedChunk aborts the client connection", FaultKind.MalformedChunk),
      connectionFault("faults: RandomThenClose aborts the client connection", FaultKind.RandomThenClose),
      latencySpike
    )

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  // A baseline space serving GET /ping -> pong, torn down with the scenario scope.
  private val pingSource =
    MockSource.Dsl(
      MockSpec(List(MockRule(RequestMatch(path = PathMatch.Exact("/ping")), ResponseDef(body = Body.Text("pong")))))
    )

  private def space(control: MockControl): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(pingSource).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  private def faultsOf(control: MockControl): IO[Throwable, Faults] =
    control.faults.mapError(u => new RuntimeException(u.toString))

  private def connectionFault(name: String, kind: FaultKind): ConformanceScenario =
    ConformanceScenario(
      name,
      Set(Capability.Faults),
      control =>
        for
          s      <- space(control)
          faults <- faultsOf(control)
          _      <- faults.inject(s, RequestMatch(path = PathMatch.Exact("/boom")), kind).mapError(asT)
          result <- SutClient.make(s).send(Method.Get, "/boom").exit
          _      <- ensure(result.isFailure, s"$name: expected a client-side transport failure, got $result")
        yield ()
    )

  private val latencySpike =
    ConformanceScenario(
      "faults: LatencySpike delays an otherwise-normal response",
      Set(Capability.Faults),
      control =>
        // Measure /slow against a no-fault /ping baseline on the same space, so the
        // delta cancels connection/JIT noise (mirrors the core delay scenario): a 1s
        // spike with a 500ms floor and the MIN of two baselines keeps jitter from
        // flipping a genuinely-applied delay to a false failure.
        def elapsed(s: MockSpace, path: String): ZIO[Any, Throwable, (SutResponse, Duration)] =
          for
            t0 <- Clock.nanoTime
            r  <- SutClient.make(s).send(Method.Get, path)
            t1 <- Clock.nanoTime
          yield (r, Duration.fromNanos(t1 - t0))
        for
          s      <- space(control)
          faults <- faultsOf(control)
          _ <- faults
                 .inject(s, RequestMatch(path = PathMatch.Exact("/slow")), FaultKind.LatencySpike(1.second))
                 .mapError(asT)
          base1   <- elapsed(s, "/ping")
          base2   <- elapsed(s, "/ping")
          slow    <- elapsed(s, "/slow")
          baseline = math.min(base1._2.toNanos, base2._2.toNanos)
          delta    = Duration.fromNanos(slow._2.toNanos - baseline)
          _ <- ensure(
                 slow._1.status == 200 && delta >= 500.millis,
                 s"latency: status=${slow._1.status} slow=${slow._2.toMillis}ms baseline=${baseline / 1000000}ms delta=${delta.toMillis}ms"
               )
        yield ()
    )
