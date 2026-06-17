package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.{GherkinParser, ScenarioMetadata}
import zio.schema.{DeriveSchema, Schema}

import java.util.concurrent.atomic.AtomicInteger

/**
 * Validates concurrent feature/scenario execution:
 *   - featureParallelism × scenarioParallelism: results pass and state is
 *     isolated
 *   - Per-scenario State[S] cannot leak to sibling scenarios
 *   - scenarioLayer is invoked once per scenario
 *   - Background steps execute for every scenario under parallelism
 */
object ConcurrencySpec extends ZIOSpecDefault {

  case class CountState(value: Int = 0)
  given Schema[CountState] = DeriveSchema.gen[CountState]

  private val F = "concurrent.feature"

  // Each scenario does: set state to a unique ID, sleep briefly to interleave, then read it back
  private def makeFeature(featureName: String, n: Int): String =
    s"""Feature: $featureName
       |${(1 to n).map { i =>
        s"""  Scenario: scenario $i
           |    Given state is set to $i
           |    Then state should be $i
           |""".stripMargin
      }.mkString}""".stripMargin

  private val isolationSuite = suite("per-scenario State isolation")(
    test("50 parallel scenarios each see only their own State[S] value") {
      val stateViolations = new AtomicInteger(0)

      class IsolationSteps extends ZIOSteps[Any, CountState] {
        Given("state is set to " / int) { (n: Int) =>
          ScenarioContext.update(_ => CountState(n)) *> ZIO.sleep(10.millis)
        }
        Then("state should be " / int) { (expected: Int) =>
          ScenarioContext.get.flatMap { s =>
            if (s.value != expected) {
              ZIO.succeed(stateViolations.incrementAndGet()).unit
            } else ZIO.unit
          }
        }
      }

      val steps = new IsolationSteps {}
      for {
        features <- ZIO.foreach(1 to 10) { i =>
                      GherkinParser.parseFeature(makeFeature(s"Feature $i", 5), F)
                    }
        _ <- FeatureExecutor.executeFeatures[Any, CountState](
               features.toList,
               steps.getSteps,
               steps,
               featureParallelism = 4,
               scenarioParallelism = 4
             )
      } yield assertTrue(stateViolations.get() == 0)
    } @@ TestAspect.withLiveClock,
    test("all 16 scenarios across 4 features pass under featureParallelism=4 x scenarioParallelism=4") {
      class SimpleSteps extends ZIOSteps[Any, CountState] {
        Given("state is set to " / int) { (n: Int) =>
          ScenarioContext.update(_ => CountState(n))
        }
        Then("state should be " / int) { (expected: Int) =>
          ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.value, expected))
        }
      }

      val steps = new SimpleSteps {}
      for {
        features <- ZIO.foreach(1 to 4) { i =>
                      GherkinParser.parseFeature(makeFeature(s"Feature $i", 4), F)
                    }
        results <- FeatureExecutor.executeFeatures[Any, CountState](
                     features.toList,
                     steps.getSteps,
                     steps,
                     featureParallelism = 4,
                     scenarioParallelism = 4
                   )
      } yield assertTrue(
        results.length == 4,
        results.flatMap(_.scenarioResults).length == 16,
        results.forall(_.isPassed)
      )
    }
  )

  private val scenarioLayerCountSuite = suite("scenarioLayer invocation count")(
    test("scenarioLayer is called exactly once per scenario under high parallelism") {
      val callCount = new AtomicInteger(0)

      class CountingSuite extends ZIOSteps[Any, CountState] {
        override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, Any] = {
          callCount.incrementAndGet()
          ZLayer.empty
        }
        Given("state is set to " / int)((n: Int) => ScenarioContext.update(_ => CountState(n)))
        Then("state should be " / int)((n: Int) =>
          ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.value, n))
        )
      }

      val steps = new CountingSuite {}
      for {
        features <- ZIO.foreach(1 to 4) { i =>
                      GherkinParser.parseFeature(makeFeature(s"Feature $i", 4), F)
                    }
        _ <- FeatureExecutor.executeFeatures[Any, CountState](
               features.toList,
               steps.getSteps,
               steps,
               featureParallelism = 4,
               scenarioParallelism = 4
             )
      } yield assertTrue(callCount.get() == 16)
    }
  )

  private val backgroundParallelSuite = suite("Background under parallelism")(
    test("Background steps run for every scenario under scenarioParallelism=4") {
      val backgroundFireCount = new AtomicInteger(0)

      class BackgroundSteps extends ZIOSteps[Any, CountState] {
        Given("the system is initialized") {
          ZIO.succeed(backgroundFireCount.incrementAndGet()).unit *>
            ScenarioContext.update(_ => CountState(0))
        }
        Then("state should be " / int) { (expected: Int) =>
          ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.value, expected))
        }
        Then("the system is ready")(ZIO.unit)
      }

      val steps = new BackgroundSteps {}
      val featureText =
        """Feature: Background
          |  Background:
          |    Given the system is initialized
          |  Scenario: s1
          |    Then the system is ready
          |  Scenario: s2
          |    Then the system is ready
          |  Scenario: s3
          |    Then the system is ready
          |  Scenario: s4
          |    Then the system is ready
          |""".stripMargin

      for {
        feature <- GherkinParser.parseFeature(featureText, F)
        results <- FeatureExecutor.executeFeatures[Any, CountState](
                     List(feature),
                     steps.getSteps,
                     steps,
                     scenarioParallelism = 4
                   )
      } yield assertTrue(
        results.head.isPassed,
        backgroundFireCount.get() == 4
      )
    } @@ TestAspect.withLiveClock
  )

  private val noStateBleedSuite = suite("no State bleed across feature runs")(
    test("FeatureContext is reset between features under parallel execution") {
      // Each feature sets a feature-context Int value, then reads it back.
      // Because FeatureContext is keyed by type, each feature just stores one Int.
      // The reset-between-features guarantee means each feature sees only its own value.
      val violations = new AtomicInteger(0)

      class FCtxSteps extends ZIOSteps[Any, CountState] {
        Given("feature context key is " / int) { (v: Int) =>
          FeatureContext.put[Int](v)
        }
        Then("feature context key should be " / int) { (expected: Int) =>
          FeatureContext.getOption[Int].flatMap {
            case Some(v) if v != expected => ZIO.succeed(violations.incrementAndGet()).unit
            case None                     => ZIO.succeed(violations.incrementAndGet()).unit
            case _                        => ZIO.unit
          }
        }
      }

      val steps = new FCtxSteps {}

      def mkF(name: String, v: Int) =
        s"""Feature: $name
           |  Scenario: check
           |    Given feature context key is $v
           |    Then feature context key should be $v
           |""".stripMargin

      for {
        f1 <- GherkinParser.parseFeature(mkF("F1", 1), F)
        f2 <- GherkinParser.parseFeature(mkF("F2", 2), F)
        f3 <- GherkinParser.parseFeature(mkF("F3", 3), F)
        f4 <- GherkinParser.parseFeature(mkF("F4", 4), F)
        results <- FeatureExecutor.executeFeatures[Any, CountState](
                     List(f1, f2, f3, f4),
                     steps.getSteps,
                     steps,
                     featureParallelism = 4
                   )
      } yield assertTrue(results.forall(_.isPassed), violations.get() == 0)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("ConcurrencySpec")(
    isolationSuite,
    scenarioLayerCountSuite,
    backgroundParallelSuite,
    noStateBleedSuite
  )
}
