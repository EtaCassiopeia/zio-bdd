package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.ZIOBDDTask
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.{Feature, Scenario, ScenarioMetadata, Step, StepType}
import zio.schema.{DeriveSchema, Schema}

import java.util.concurrent.ConcurrentLinkedQueue

/**
 * Regression tests for #337: `--exclude-tags <tag>` must drop a scenario from
 * selection *before* its scenario-tier layer (`scenarioLayer`) is built or
 * acquired — even when the scenario also carries a parameterized tag such as
 * `@mock(...)`. An excluded scenario must provision no fixtures and run no
 * scenario-tier layer.
 *
 * The bug lived in FeatureExecutor.runScenarios: `scenarioLayer(meta)` was
 * built and `.provideSomeLayer`-acquired around every scenario, and the
 * `isIgnored` short-circuit only fired *inside* that already-acquired scope —
 * so an excluded `@rift @mock(orders)` scenario still provisioned its
 * `@mock(orders)` source (and, for a raw source under the wrong backend, threw
 * and failed the whole run). A scenario carrying only plain tags never showed
 * the bug because its default `scenarioLayer` provisions nothing.
 */
object ExcludedScenarioFixtureSpec extends ZIOSpecDefault {

  case class FxState(v: Int = 0)
  given Schema[FxState] = DeriveSchema.gen[FxState]

  private val F = "excluded.feature"

  private def scenario(name: String, tags: String*): Scenario =
    Scenario(name, tags.toList, List(Step(StepType.GivenStep, "a step is present")), Some(F), Some(1))

  private def feature(scenarios: Scenario*): Feature =
    Feature("Capability-partitioned", Nil, scenarios.toList, Some(F), Some(1))

  // Mark scenarios exactly as `--exclude-tags rift` would (via the real filter path).
  private def excludeRift(features: List[Feature]): List[Feature] =
    ZIOBDDTask.filterFeatures(features, zio.bdd.BDDTestConfig(excludeTags = Set("rift")))

  def spec = suite("ExcludedScenarioFixture")(
    test("excluded scenario carrying a parameterized tag builds and acquires no scenarioLayer") {
      val built    = new ConcurrentLinkedQueue[String]()
      val acquired = new ConcurrentLinkedQueue[String]()

      class RecordingSuite extends ZIOSteps[Any, FxState] {
        override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, Any] = {
          built.add(meta.name)
          ZLayer.scoped(ZIO.succeed(acquired.add(meta.name)).unit)
        }
        Given("a step is present")(ZIO.unit)
      }

      val steps = new RecordingSuite {}
      val features =
        excludeRift(List(feature(scenario("rift-only", "rift", "mock(orders)"), scenario("portable"))))
      for {
        results  <- steps.run(features)
        scResults = results.flatMap(_.scenarioResults)
      } yield assertTrue(
        scResults.find(_.scenario.name == "rift-only").exists(_.isIgnored),
        scResults.find(_.scenario.name == "portable").exists(_.isPassed),
        !built.contains("rift-only"),
        !acquired.contains("rift-only"),
        built.contains("portable"),
        acquired.contains("portable")
      )
    },
    test("a failing (raw-doc) provisioning on the excluded scenario does not fail the whole run") {
      class FailingProvisionSuite extends ZIOSteps[Any, FxState] {
        override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, Any] =
          if (meta.tags.exists(_.startsWith("mock(")))
            // Simulates MockFixtures provisioning a raw/native source under the wrong backend:
            // it fails on acquire and, under the bug, dies the whole run (#337).
            ZLayer.scoped(ZIO.fail(new RuntimeException("a raw/native source must go through provisionNative")))
          else ZLayer.empty
        Given("a step is present")(ZIO.unit)
      }

      val steps = new FailingProvisionSuite {}
      val features =
        excludeRift(List(feature(scenario("rift-only", "rift", "mock(orders)"), scenario("portable"))))
      for {
        exit <- steps.run(features).exit
        scResults = exit match
                      case Exit.Success(results) => results.flatMap(_.scenarioResults)
                      case Exit.Failure(_)       => Nil
      } yield assertTrue(
        exit.isSuccess,
        scResults.find(_.scenario.name == "rift-only").exists(_.isIgnored),
        scResults.find(_.scenario.name == "portable").exists(_.isPassed)
      )
    }
  )
}
