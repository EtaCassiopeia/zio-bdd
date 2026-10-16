package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 11 — fault injection (capability Faults): the four connection faults make the
 * SUT client observe a transport failure; LatencySpike delays an otherwise
 * normal response. Requires a backend advertising Faults (WireMock, Rift).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/11-faults.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Faults11Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def faultKind(name: String): IO[Throwable, FaultKind] = name match
    case "ConnectionReset" => ZIO.succeed(FaultKind.ConnectionReset)
    case "EmptyResponse"   => ZIO.succeed(FaultKind.EmptyResponse)
    case "MalformedChunk"  => ZIO.succeed(FaultKind.MalformedChunk)
    case "RandomThenClose" => ZIO.succeed(FaultKind.RandomThenClose)
    case other             => ZIO.fail(new IllegalArgumentException(s"unknown fault '$other'"))

  Given("the backend supports faults") {
    requiring(Capability.Faults)
  }

  Given("a " / string / " fault is injected for GET " / string) { (kind: String, path: String) =>
    for
      space <- activeSpace
      fk    <- faultKind(kind)
      fs    <- ZIO.serviceWithZIO[MockControl](_.faults).mapError(u => new RuntimeException(u.message))
      _     <- fs.inject(space, get(path), fk).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  Given("a latency fault of " / int / " millis is injected for GET " / string) { (ms: Int, path: String) =>
    for
      space <- activeSpace
      fs <- ZIO
              .serviceWithZIO[MockControl](_.faults)
              .mapError(u => new RuntimeException(u.message))
      _ <- fs
             .inject(space, get(path), FaultKind.LatencySpike(ms.millis))
             .mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  When("GET " / string / " is attempted") { (path: String) =>
    for
      t0  <- Clock.nanoTime
      res <- send(Method.Get, path).either
      t1  <- Clock.nanoTime
      _   <- Stage.put(AttemptOutcome(res.isLeft, (t1 - t0) / 1000000L))
    yield ()
  }

  Then("the request fails at the transport level") {
    Stage
      .get[AttemptOutcome]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(o => Assertions.assertTrue(o.failed, "expected a transport failure but the request succeeded"))
  }

  Then("the request succeeds after at least " / int / " millis") { (min: Int) =>
    Stage
      .get[AttemptOutcome]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(o =>
        Assertions.assertTrue(
          !o.failed && o.millis >= min.toLong,
          s"failed=${o.failed} elapsed=${o.millis}"
        )
      )
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class AttemptOutcome(failed: Boolean, millis: Long)
