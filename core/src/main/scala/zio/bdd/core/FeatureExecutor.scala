package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepDef, StepRegistry, ZIOSteps}
import zio.bdd.gherkin.Feature

object FeatureExecutor {
  def executeFeature[R: Tag, S: Tag: Default](
    feature: Feature,
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S]
  ): ZIO[R, Nothing, FeatureResult] =
    if (feature.isIgnored) {
      // Handle ignored feature: create a FeatureResult with ignored scenarios
      val ignoredScenarios = feature.scenarios.map { scenario =>
        ScenarioResult(scenario = scenario, stepResults = List.empty, setupError = None)
      }
      ZIO.succeed(FeatureResult(feature, ignoredScenarios))
    } else {
      ZIO
        .logAnnotate("featureId", feature.id.toString) {
          for {
            _ <- FeatureContext.reset // per-feature staging is reset at the start of each feature
            _ <- suite.beforeFeatureHook
            featureResult <- ZIO
                               .foreach(feature.scenarios) { scenario =>
                                 ZIO.logAnnotate("scenarioId", scenario.id.toString) {
                                   ScenarioExecutor.executeScenario[R, S](
                                     scenario,
                                     suite,
                                     stepTimeout = suite.stepTimeout
                                   )
                                 }
                               }
                               .map(scenarioResults => FeatureResult(feature, scenarioResults))
            _ <- suite.afterFeatureHook
          } yield featureResult
        }
        .provideSomeLayer[R](StepRegistry.layer[R, S](steps))
    }

  def executeFeatures[R: Tag, S: Tag: Default](
    features: List[Feature],
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S]
  ): ZIO[R, Nothing, List[FeatureResult]] =
    ZIO.foreach(features)(feature => executeFeature(feature, steps, suite))
}
