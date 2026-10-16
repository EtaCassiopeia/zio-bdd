package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 18 — error paths: the typed MockError variants surface as values (never a
 * leaked Throwable). Each step runs an operation expected to fail and records
 * the error's variant name for assertion.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/18-error-paths.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Errors18Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def captureError(op: MockControl => IO[MockError, Any]) =
    ZIO
      .serviceWithZIO[MockControl](c => op(c).either)
      .flatMap {
        case Left(e)  => Stage.put(CapturedError(e.getClass.getSimpleName))
        case Right(_) => Stage.put(CapturedError("<no error>"))
      }

  Given("a provisioned space") {
    ensureProvisioned.unit
  }

  When("removeRule is called with an unknown id") {
    activeSpace.flatMap(space => captureError(_.removeRule(space, RuleId("does-not-exist"))))
  }

  When("the space is destroyed and then queried") {
    for
      space <- activeSpace
      _     <- ctl(_.destroy(space))
      _     <- captureError(_.received(space))
    yield ()
  }

  When("a malformed native document is provisioned") {
    captureError(_.provisionNative(NativeSpec.WireMock("this is not json {")))
  }

  Then("the error is a " / string) { (expected: String) =>
    Stage
      .get[CapturedError]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(ce => Assertions.assertEquals(ce.variant, expected, "MockError variant"))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class CapturedError(variant: String)
