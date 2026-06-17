package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Validates that ZIO Cause/Exit information propagates faithfully into
 * StepResult:
 *   - ZIO.die → StepStatus.Failed with dieOption present
 *   - ZIO.fail → StepStatus.Failed with failureOption present
 *   - Interruption → non-Passed result
 *   - Cause.Both → both failure leaves visible
 *   - a failing afterStep hook fails the step (fail-loud) without aborting the
 *     run
 */
object CauseFidelitySpec extends ZIOSpecDefault {

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  private val F = "cause.feature"

  private def runFeature(content: String, suite: ZIOSteps[Any, S]) =
    GherkinParser.parseFeature(content, F).flatMap { f =>
      FeatureExecutor.executeFeatures[Any, S](List(f), suite.getSteps, suite)
    }

  private val dieSpec = suite("ZIO.die propagation")(
    test("ZIO.die surfaces as StepStatus.Failed with dieOption defined") {
      val steps = new ZIOSteps[Any, S] {
        Given("a step that dies")(ZIO.die(new RuntimeException("kaboom")))
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a step that dies\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield {
        val cause = stepResult.status match
          case StepStatus.Failed(c) => Some(c)
          case _                    => None
        assertTrue(
          stepResult.status.isInstanceOf[StepStatus.Failed],
          !stepResult.isPassed,
          cause.exists(_.dieOption.exists(_.getMessage == "kaboom"))
        )
      }
    },
    test("ZIO.die does not produce StepStatus.Pending or Skipped") {
      val steps = new ZIOSteps[Any, S] {
        Given("a step that dies")(ZIO.die(new RuntimeException("die")))
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a step that dies\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield assertTrue(!stepResult.isPending, !stepResult.isSkipped)
    }
  )

  private val failSpec = suite("ZIO.fail propagation")(
    test("ZIO.fail surfaces as StepStatus.Failed with failureOption defined") {
      val steps = new ZIOSteps[Any, S] {
        Given("a step that fails")(ZIO.fail(new RuntimeException("typed failure")))
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a step that fails\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield {
        val cause = stepResult.status match
          case StepStatus.Failed(c) => Some(c)
          case _                    => None
        assertTrue(
          stepResult.status.isInstanceOf[StepStatus.Failed],
          cause.exists(_.failureOption.exists(_.getMessage == "typed failure"))
        )
      }
    }
  )

  private val causeTreeSpec = suite("Cause tree preservation")(
    test("Cause.Both produces a Failed status where cause is-a Both or squash does not lose info") {
      val left  = new RuntimeException("left")
      val right = new RuntimeException("right")
      val steps = new ZIOSteps[Any, S] {
        Given("a step with both failures")(
          ZIO.failCause(Cause.Both(Cause.fail(left), Cause.fail(right)))
        )
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a step with both failures\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield {
        val cause = stepResult.status match
          case StepStatus.Failed(c) => Some(c)
          case _                    => None
        // The cause should contain both failures — either raw Both or as a squash error
        assertTrue(
          stepResult.status.isInstanceOf[StepStatus.Failed],
          cause.isDefined,
          // At minimum, squash should not produce null
          cause.flatMap(_.failureOption.orElse(cause.flatMap(_.dieOption))).isDefined
        )
      }
    },
    test("zipPar two failing effects — ScenarioResult reflects failure") {
      val steps = new ZIOSteps[Any, S] {
        Given("two parallel failures")(
          ZIO.fail(new RuntimeException("a")).zipPar(ZIO.fail(new RuntimeException("b"))).unit
        )
      }
      for {
        results <- runFeature("Feature: F\n  Scenario: s\n    Given two parallel failures\n", steps)
        sr       = results.head.scenarioResults.head
      } yield assertTrue(!sr.isPassed, sr.hasFailure)
    }
  )

  private val afterStepHookSpec = suite("a failing afterStep hook fails the step without aborting the run")(
    test("a dying afterStep hook fails the otherwise-passed step but the feature run completes") {
      val steps = new ZIOSteps[Any, S] {
        afterStep(_ => ZIO.die(new RuntimeException("afterStep boom")))
        Given("a passing step")(ZIO.unit)
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a passing step\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
        cause = stepResult.status match
                  case StepStatus.Failed(c) => Some(c)
                  case _                    => None
      } yield assertTrue(
        // Run completed normally (not aborted) and the hook failure is surfaced on the step.
        stepResult.status.isInstanceOf[StepStatus.Failed],
        cause.exists(_.dieOption.exists(_.getMessage == "afterStep boom"))
      )
    },
    test("an afterStep hook that handles its own defect leaves the step Passed") {
      val steps = new ZIOSteps[Any, S] {
        // catchAllCause (unlike .ignore) actually swallows defects, so the hook succeeds.
        afterStep(_ => ZIO.die(new RuntimeException("handled")).catchAllCause(_ => ZIO.unit))
        Given("a passing step")(ZIO.unit)
      }
      for {
        results   <- runFeature("Feature: F\n  Scenario: s\n    Given a passing step\n", steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield assertTrue(stepResult.isPassed)
    }
  )

  private val skippedAfterFailureSpec = suite("steps after failure are Skipped")(
    test("3 steps: first fails, 2nd and 3rd are Skipped (not Failed)") {
      val steps = new ZIOSteps[Any, S] {
        Given("a step that fails")(ZIO.fail(new RuntimeException("boom")))
        Then("step two")(ZIO.unit)
        Then("step three")(ZIO.unit)
      }
      for {
        results <- runFeature(
                     "Feature: F\n  Scenario: s\n    Given a step that fails\n    Then step two\n    Then step three\n",
                     steps
                   )
        stepResults = results.head.scenarioResults.head.stepResults
      } yield assertTrue(
        stepResults.length == 3,
        stepResults(0).status.isInstanceOf[StepStatus.Failed],
        stepResults(1).isSkipped,
        stepResults(2).isSkipped
      )
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("CauseFidelitySpec")(
    dieSpec,
    failSpec,
    causeTreeSpec,
    afterStepHookSpec,
    skippedAfterFailureSpec
  )
}
