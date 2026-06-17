package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Phase 3 stress test — run 1000+ scenarios across many features under high
 * parallelism to verify correctness and resource management at scale.
 *
 * Tagged @stress so normal `sbt test` skips it. Run with: sbt "core/testOnly
 * zio.bdd.core.StressSpec"
 *
 * Assertions:
 *   - No State[S] leaks between scenarios (each sees only its own value)
 *   - No exception propagates out of executeFeatures
 *   - All results are either passed or properly failed (no nulls, no missing
 *     results)
 *   - Total result count equals total input scenario count
 */
object StressSpec extends ZIOSpecDefault {

  case class StressState(value: Int = 0, scenarioId: Int = 0)
  given Schema[StressState] = DeriveSchema.gen[StressState]

  private val F = "stress.feature"

  private def makeFeature(featureIdx: Int, scenariosPerFeature: Int): String = {
    val scenarioLines = (1 to scenariosPerFeature).map { i =>
      s"""  Scenario: scenario-${featureIdx}-$i
         |    Given state is set to $i
         |    Then state equals $i
         |""".stripMargin
    }.mkString
    s"Feature: stress-feature-$featureIdx\n$scenarioLines"
  }

  private val stateIsolationUnderStress = suite("state isolation under stress")(
    test("1000 scenarios across 20 features: all pass with zero state leaks") {
      val violations = new java.util.concurrent.atomic.AtomicInteger(0)

      class StressSteps extends ZIOSteps[Any, StressState] {
        Given("state is set to " / int) { (n: Int) =>
          // Include a brief jitter to force interleaving
          ZIO.sleep(scala.util.Random.nextInt(3).millis) *>
            ScenarioContext.update(_ => StressState(n, n))
        }
        Then("state equals " / int) { (expected: Int) =>
          ScenarioContext.get.flatMap { s =>
            if (s.value != expected) {
              ZIO.succeed(violations.incrementAndGet()).unit
            } else ZIO.unit
          }
        }
      }

      val steps = new StressSteps {}

      for {
        features <- ZIO.foreach(1 to 20)(i => GherkinParser.parseFeature(makeFeature(i, 50), F))
        results <- FeatureExecutor.executeFeatures[Any, StressState](
                     features.toList,
                     steps.getSteps,
                     steps,
                     featureParallelism = 8,
                     scenarioParallelism = 8
                   )
        totalScenarios = results.flatMap(_.scenarioResults).length
      } yield assertTrue(
        violations.get() == 0,
        totalScenarios == 20 * 50,
        results.forall(_.isPassed)
      )
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds)
  )

  private val noResultLossUnderStress = suite("no result loss under stress")(
    test("result count equals input scenario count under max parallelism") {
      class CountSteps extends ZIOSteps[Any, StressState] {
        Given("state is set to " / int)((n: Int) => ScenarioContext.update(_ => StressState(n)))
        Then("state equals " / int)((n: Int) => ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.value, n)))
      }

      val steps       = new CountSteps {}
      val parallelism = java.lang.Runtime.getRuntime.availableProcessors().max(2)

      for {
        features <- ZIO.foreach(1 to 10)(i => GherkinParser.parseFeature(makeFeature(i, 20), F))
        results <- FeatureExecutor.executeFeatures[Any, StressState](
                     features.toList,
                     steps.getSteps,
                     steps,
                     featureParallelism = parallelism,
                     scenarioParallelism = parallelism
                   )
        totalIn  = 10 * 20
        totalOut = results.flatMap(_.scenarioResults).length
      } yield assertTrue(totalOut == totalIn)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StressSpec")(
    stateIsolationUnderStress,
    noResultLossUnderStress
  )
}
