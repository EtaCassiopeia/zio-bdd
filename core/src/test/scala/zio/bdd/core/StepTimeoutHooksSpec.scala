package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

import java.util.concurrent.atomic.AtomicInteger

/**
 * Validates the `.ensuring(afterScenarioHook)` contract in ScenarioExecutor and
 * the afterStep guarantee when a step times out:
 *   - afterStep still fires for a timed-out step
 *   - afterScenario still fires after a timed-out step
 *   - subsequent steps are Skipped
 *   - timed-out step carries StepStatus.TimedOut
 */
object StepTimeoutHooksSpec extends ZIOSpecDefault {

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  private val F       = "timeout-hooks.feature"
  private val timeout = 50.millis

  private val featureText =
    """Feature: Timeout hooks
      |  Scenario: timeout scenario
      |    Given a slow step
      |    Then a step after timeout
      |""".stripMargin

  private val afterStepFiresSuite = suite("afterStep fires on timeout")(
    test("afterStep hook fires even when step times out") {
      val afterStepCount = new AtomicInteger(0)
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        afterStep(_ => ZIO.succeed(afterStepCount.incrementAndGet()).unit)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature <- GherkinParser.parseFeature(featureText, F)
        _       <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
      } yield assertTrue(afterStepCount.get() >= 1)
    } @@ TestAspect.withLiveClock
  )

  private val afterScenarioFiresSuite = suite("afterScenario fires on timeout")(
    test("afterScenario hook fires even when a step times out") {
      val afterScenarioFired = new AtomicInteger(0)
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        afterScenario(_ => ZIO.succeed(afterScenarioFired.incrementAndGet()).unit)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature <- GherkinParser.parseFeature(featureText, F)
        _       <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
      } yield assertTrue(afterScenarioFired.get() == 1)
    } @@ TestAspect.withLiveClock
  )

  private val subsequentSkippedSuite = suite("steps after timeout are Skipped")(
    test("step following a timed-out step is Skipped, not Failed") {
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature    <- GherkinParser.parseFeature(featureText, F)
        results    <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
        stepResults = results.head.scenarioResults.head.stepResults
      } yield assertTrue(
        stepResults.length == 2,
        stepResults(0).status.isInstanceOf[StepStatus.TimedOut],
        stepResults(1).isSkipped
      )
    } @@ TestAspect.withLiveClock
  )

  private val timedOutStatusSuite = suite("timed-out step carries TimedOut status")(
    test("timed-out step has StepStatus.TimedOut with duration") {
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature   <- GherkinParser.parseFeature(featureText, F)
        results   <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
        stepResult = results.head.scenarioResults.head.stepResults.head
      } yield {
        val d = stepResult.status match
          case StepStatus.TimedOut(d, _) => Some(d)
          case _                         => None
        assertTrue(
          stepResult.status.isInstanceOf[StepStatus.TimedOut],
          d.isDefined,
          d.exists(_ == timeout)
        )
      }
    } @@ TestAspect.withLiveClock,
    test("timed-out step is a hard failure (hasFailure is true)") {
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature <- GherkinParser.parseFeature(featureText, F)
        results <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
        scResult = results.head.scenarioResults.head
      } yield assertTrue(!scResult.isPassed, scResult.hasFailure, !scResult.hasPending)
    } @@ TestAspect.withLiveClock
  )

  private val afterStepMetadataSuite = suite("afterStep receives StepMetadata for timed-out step")(
    test("afterStep captures the pattern of the timed-out step") {
      val capturedPatterns = new java.util.concurrent.CopyOnWriteArrayList[String]()
      val steps = new ZIOSteps[Any, S] {
        override def stepTimeout: Option[Duration] = Some(timeout)
        afterStep(meta => ZIO.succeed(capturedPatterns.add(meta.pattern)).unit)
        Given("a slow step")(ZIO.sleep(2.seconds))
        Then("a step after timeout")(ZIO.unit)
      }
      for {
        feature <- GherkinParser.parseFeature(featureText, F)
        _       <- FeatureExecutor.executeFeatures[Any, S](List(feature), steps.getSteps, steps)
      } yield {
        import scala.jdk.CollectionConverters.*
        val patterns = capturedPatterns.asScala.toList
        assertTrue(patterns.exists(_ == "a slow step"))
      }
    } @@ TestAspect.withLiveClock
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepTimeoutHooksSpec")(
    afterStepFiresSuite,
    afterScenarioFiresSuite,
    subsequentSkippedSuite,
    timedOutStatusSuite,
    afterStepMetadataSuite
  )
}
