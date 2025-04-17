package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepDef, StepRegistry}
import zio.bdd.gherkin.Feature

object FeatureExecutor {
  def executeFeature[R: Tag, S: Tag: Default](
    feature: Feature,
    steps: List[StepDef[R, S]],
    hooks: Hooks[R, S]
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
          for {
            _ <- hooks.beforeFeatureHook
            featureResult <- ZIO
                               .foreach(feature.scenarios) { scenario =>
                                 ZIO.logAnnotate("scenarioId", scenario.id.toString) {
                                   ScenarioExecutor.executeScenario[R, S](scenario, hooks)
                                 }
                               }
                               .map { scenarioResults =>
                                 FeatureResult(feature, scenarioResults)
                               }
            _ <- hooks.afterFeatureHook
          } yield featureResult
        }
        .provideSomeLayer[R](StepRegistry.layer[R, S](steps))
    }

  def executeFeatures[R: Tag, S: Tag: Default](
    features: List[Feature],
    steps: List[StepDef[R, S]],
    hooks: Hooks[R, S]
  ): ZIO[R, Nothing, List[FeatureResult]] =
    ZIO.foreach(features) { feature =>
      executeFeature(feature, steps, hooks)
    }
}
