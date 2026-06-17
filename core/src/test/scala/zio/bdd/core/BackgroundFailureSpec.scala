package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

import java.util.concurrent.atomic.AtomicInteger

/**
 * Validates the contract for failing Background steps:
 *   - A failing Background step causes all scenarios in the feature to have a
 *     setup error or skipped steps (contracts the current implementation).
 *   - A successful Background followed by a failing scenario step: only
 *     post-failure steps are Skipped.
 *   - Background failure does NOT prevent afterAll from running.
 */
object BackgroundFailureSpec extends ZIOSpecDefault {

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  private val F = "background-failure.feature"

  private def run(content: String, steps: ZIOSteps[Any, S]) =
    GherkinParser.parseFeature(content, F).flatMap { f =>
      FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps)
    }

  private val backgroundFailsAllSuite = suite("Background step failure")(
    test("Background step failure → scenarios are not passed") {
      val steps = new ZIOSteps[Any, S] {
        Given("a system is running")(ZIO.unit)
        Given("setup fails")(ZIO.fail(new RuntimeException("setup error")))
        Then("the user Alice should exist")(ZIO.unit)
        Then("the user Bob should exist")(ZIO.unit)
      }
      run(
        """Feature: F
          |  Background:
          |    Given a system is running
          |    Given setup fails
          |  Scenario: A
          |    Then the user Alice should exist
          |  Scenario: B
          |    Then the user Bob should exist
          |""".stripMargin,
        steps
      ).map { results =>
        val feature = results.head
        // All scenarios must not be passed
        assertTrue(!feature.isPassed)
      }
    },
    test("Background failure propagates to all scenarios in the feature") {
      val steps = new ZIOSteps[Any, S] {
        Given("setup fails")(ZIO.fail(new RuntimeException("setup error")))
        Then("the user Alice should exist")(ZIO.unit)
        Then("the user Bob should exist")(ZIO.unit)
        Then("the user Charlie should exist")(ZIO.unit)
      }
      run(
        """Feature: F
          |  Background:
          |    Given setup fails
          |  Scenario: A
          |    Then the user Alice should exist
          |  Scenario: B
          |    Then the user Bob should exist
          |  Scenario: C
          |    Then the user Charlie should exist
          |""".stripMargin,
        steps
      ).map { results =>
        // None of the scenarios should be passed
        val scenarios = results.head.scenarioResults
        assertTrue(
          scenarios.length == 3,
          !results.head.isPassed,
          scenarios.forall(!_.isPassed)
        )
      }
    }
  )

  private val successfulBackgroundSuite = suite("successful Background with failing scenario step")(
    test("only post-failure steps are Skipped when Background succeeds but a scenario step fails") {
      val steps = new ZIOSteps[Any, S] {
        Given("a system is running")(ScenarioContext.update(_ => S()))
        Then("an error occurs")(ZIO.fail(new RuntimeException("scenario error")))
        Then("this step is skipped")(ZIO.unit)
        Then("this step is also skipped")(ZIO.unit)
      }
      run(
        """Feature: F
          |  Background:
          |    Given a system is running
          |  Scenario: s
          |    Then an error occurs
          |    Then this step is skipped
          |    Then this step is also skipped
          |""".stripMargin,
        steps
      ).map { results =>
        // Background steps are prepended to the scenario's stepResults, so the list is:
        // [0] background "a system is running" passes; [1] "an error occurs" fails;
        // [2] and [3] are skipped because they follow the failing step.
        val stepResults = results.head.scenarioResults.head.stepResults
        assertTrue(
          stepResults.length == 4, // 1 background + 3 scenario steps
          stepResults(0).isPassed,
          stepResults(1).status.isInstanceOf[StepStatus.Failed],
          stepResults(2).isSkipped,
          stepResults(3).isSkipped
        )
      }
    }
  )

  private val afterAllWithBackgroundFailSuite = suite("afterAll runs despite Background failure")(
    test("afterAll fires even when a Background step fails in one feature") {
      val afterAllFired = new AtomicInteger(0)
      val steps = new ZIOSteps[Any, S] {
        afterAll(ZIO.succeed(afterAllFired.incrementAndGet()).unit)
        Given("setup fails")(ZIO.fail(new RuntimeException("setup error")))
        Then("a step")(ZIO.unit)
      }
      run(
        """Feature: F
          |  Background:
          |    Given setup fails
          |  Scenario: s
          |    Then a step
          |""".stripMargin,
        steps
      ).map(_ => assertTrue(afterAllFired.get() == 1))
    }
  )

  private val afterFeatureWithBackgroundFailSuite = suite("afterFeature runs despite Background failure")(
    test("afterFeature fires even when a Background step fails") {
      val afterFeatureFired = new AtomicInteger(0)
      val steps = new ZIOSteps[Any, S] {
        afterFeature(ZIO.succeed(afterFeatureFired.incrementAndGet()).unit)
        Given("setup fails")(ZIO.fail(new RuntimeException("setup error")))
        Then("a step")(ZIO.unit)
      }
      run(
        """Feature: F
          |  Background:
          |    Given setup fails
          |  Scenario: s
          |    Then a step
          |""".stripMargin,
        steps
      ).map(_ => assertTrue(afterFeatureFired.get() == 1))
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("BackgroundFailureSpec")(
    backgroundFailsAllSuite,
    successfulBackgroundSuite,
    afterAllWithBackgroundFailSuite,
    afterFeatureWithBackgroundFailSuite
  )
}
