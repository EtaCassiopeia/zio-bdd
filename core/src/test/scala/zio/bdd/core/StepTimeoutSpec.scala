package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for #36 — per-step timeout.
 *
 * Verifies three flows:
 *   1. Annotation-injected timeout: `overrideStepTimeout` causes a slow step to
 *      time out. 2. Subclass-override timeout: a suite that overrides `def
 *      stepTimeout` cuts short a slow step. 3. No-timeout baseline: same slow
 *      step passes when no timeout is configured.
 */
object StepTimeoutSpec extends ZIOSpecDefault {

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  private val featureText =
    """Feature: Timeout
      |  Scenario: slow step
      |    Given a slow step
      |""".stripMargin

  // ── Annotation-injected timeout ───────────────────────────────────────────

  private val annotationFlow = suite("annotation-injected timeout via overrideStepTimeout")(
    test("slow step is reported as TimedOut when overrideStepTimeout is set") {
      val stepsImpl = new ZIOSteps[Any, S] {
        Given("a slow step") {
          ZIO.sleep(2.seconds)
        }
      }
      // Simulate what ZIOBDDFramework does when @Suite(stepTimeout=1) is read
      stepsImpl.overrideStepTimeout(Duration.fromSeconds(1))

      for {
        feature   <- GherkinParser.parseFeature(featureText, "timeout.feature")
        results   <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield assertTrue(
        stepResult.status.isInstanceOf[StepStatus.TimedOut],
        !stepResult.isPassed,
        results.head.scenarioResults.head.hasFailure
      )
    }
  )

  // ── Subclass-override timeout ─────────────────────────────────────────────

  private val subclassOverrideFlow = suite("subclass-override stepTimeout")(
    test("slow step is reported as TimedOut when suite overrides stepTimeout") {
      val stepsImpl = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(50.millis)

        Given("a slow step") {
          ZIO.sleep(2.seconds)
        }
      }

      for {
        feature   <- GherkinParser.parseFeature(featureText, "timeout.feature")
        results   <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield assertTrue(
        stepResult.status.isInstanceOf[StepStatus.TimedOut],
        !stepResult.isPassed
      )
    }
  )

  // ── No-timeout baseline ───────────────────────────────────────────────────

  private val noTimeoutFlow = suite("no timeout configured")(
    test("step that takes 200ms passes when no timeout is set") {
      val stepsImpl = new ZIOSteps[Any, S] {
        // stepTimeout returns None by default — no timeout
        Given("a slow step") {
          ZIO.sleep(200.millis)
        }
      }

      for {
        feature   <- GherkinParser.parseFeature(featureText, "timeout.feature")
        results   <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield assertTrue(
        stepResult.isPassed,
        stepResult.status == StepStatus.Passed
      )
    }
  )

  // ── TimedOut is a hard failure (not a pending) ────────────────────────────

  private val semanticsFlow = suite("TimedOut semantics")(
    test("timed-out step does not count as pending") {
      val stepsImpl = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(50.millis)
        Given("a slow step")(ZIO.sleep(2.seconds))
      }

      for {
        feature <- GherkinParser.parseFeature(featureText, "timeout.feature")
        results <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
        scResult = results.head.scenarioResults.head
      } yield assertTrue(
        !scResult.isPassed,
        scResult.hasFailure,
        !scResult.hasPending
      )
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepTimeout")(
    annotationFlow,
    subclassOverrideFlow,
    noTimeoutFlow,
    semanticsFlow
  ) @@ TestAspect.withLiveClock
}
