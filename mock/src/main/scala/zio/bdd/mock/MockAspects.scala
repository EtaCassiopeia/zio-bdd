package zio.bdd.mock

import zio.*
import zio.test.{TestAspect, TestFailure, TestSuccess}

/**
 * `TestAspect`s that override a [[MockSpace]]'s behaviour for one test and
 * auto-revert afterward, mirroring zio-openfeature's `TestFeatureProvider`
 * delay/error aspects. Each is built on the scoped overlay ([[MockOverlay]],
 * #116): rules are added at [[Priority.Overlay]] on enter and removed on exit,
 * so the override is scoped to the aspected test — declare a base, override per
 * scenario, auto-revert.
 *
 * Requires `MockControl & MockSpace` in the test environment.
 */
object MockAspects:

  /** Add latency `d` to the space's responses for the duration of the test. */
  def withLatency(d: Duration): TestAspect[Nothing, MockControl & MockSpace, Nothing, Any] =
    overlay(MockRule(RequestMatch(), ResponseDef(delay = Some(d))))

  /**
   * Overlay `rules` (a scoped imposter) on the space for the duration of the
   * test.
   */
  def withImposter(rules: MockRule*): TestAspect[Nothing, MockControl & MockSpace, Nothing, Any] =
    overlay(rules*)

  // Wrap the test in the scoped overlay: rules are added on enter and removed
  // when the per-test scope closes. A setup failure becomes a defect so the
  // aspect's error stays Nothing (and thus applies to any test).
  private def overlay(rules: MockRule*): TestAspect[Nothing, MockControl & MockSpace, Nothing, Any] =
    new TestAspect.PerTest[Nothing, MockControl & MockSpace, Nothing, Any]:
      def perTest[R <: MockControl & MockSpace, E](
        test: ZIO[R, TestFailure[E], TestSuccess]
      )(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
        ZIO.serviceWithZIO[MockSpace] { space =>
          ZIO.scoped[R] {
            MockOverlay
              .scoped(space)(rules*)
              .orDieWith(e => new RuntimeException(s"MockAspects setup failed: $e")) *> test
          }
        }
