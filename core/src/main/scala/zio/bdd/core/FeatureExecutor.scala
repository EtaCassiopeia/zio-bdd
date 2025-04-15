package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepDef, StepRegistry}
import zio.bdd.gherkin.Feature

object FeatureExecutor {
  def executeFeature[R: Tag, S: Tag](
    feature: Feature,
    initialState: => S,
    steps: List[StepDef[R, S]]
  ): ZIO[R, Nothing, FeatureResult] =
    if (feature.isIgnored) {
      // Handle ignored feature: create a FeatureResult with ignored scenarios
      val ignoredScenarios = feature.scenarios.map { scenario =>
        ScenarioResult(
          scenario = scenario,
          stepResults = List.empty, // No steps executed
          setupError = None
        )
      }
      ZIO.succeed(FeatureResult(feature, ignoredScenarios))
    } else {
      // Execute scenarios for non-ignored feature
      ZIO
        .logAnnotate("featureId", feature.id.toString) {
          ZIO
            .foreach(feature.scenarios) { scenario =>
              ZIO.logAnnotate("scenarioId", scenario.id.toString) {
                ScenarioExecutor.executeScenario[R, S](scenario, initialState)
              }
            }
            .map { scenarioResults =>
              FeatureResult(feature, scenarioResults)
            }
        }
        .provideSomeLayer[R](StepRegistry.layer[R, S](steps))
    }
}
