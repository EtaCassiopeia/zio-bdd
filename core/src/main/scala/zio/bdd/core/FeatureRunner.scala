package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.Feature

import java.time.Instant

object FeatureRunner {

  private def matchesTagFilter(tags: List[String], includeTags: Set[String], excludeTags: Set[String]): Boolean = {
    val tagSet         = tags.toSet
    val matchesInclude = includeTags.isEmpty || includeTags.exists(tagSet.contains)
    val matchesExclude = excludeTags.nonEmpty && excludeTags.exists(tagSet.contains)
    matchesInclude && !matchesExclude
  }

  private def filterFeatures(
    features: List[Feature],
    includeTags: Set[String],
    excludeTags: Set[String]
  ): List[Feature] =
    features.filter { feature =>
      val featureTags     = feature.tags.toSet
      val allScenarioTags = feature.scenarios.flatMap(_.tags).toSet

      // Check if the feature or any scenario matches includeTags
      val featureMatchesInclude  = includeTags.isEmpty || includeTags.exists(featureTags.contains)
      val scenarioMatchesInclude = includeTags.isEmpty || includeTags.exists(allScenarioTags.contains)
      val shouldKeepFeature      = featureMatchesInclude || scenarioMatchesInclude

      shouldKeepFeature // Keep the feature if it or any scenario matches includeTags
    }.map { feature =>
      val filteredScenarios = feature.scenarios.filter { scenario =>
        val combinedTags = feature.tags ++ scenario.tags
        matchesTagFilter(combinedTags, includeTags, excludeTags)
      }
      feature.copy(scenarios = filteredScenarios)
    }.filter(_.scenarios.nonEmpty) // Only keep features with at least one scenario

  private def featureFailedResult(e: Throwable): Instant => StepResult =
    startTime =>
      StepResult(
        "Feature failed",
        succeeded = false,
        Some(e),
        (),
        Nil,
        Duration.Zero,
        startTime
      )

  def runFeatures[R](
    steps: ZIOSteps[R],
    features: List[Feature],
    config: TestConfig
  ): ZIO[R & LogCollector & Reporter, Throwable, List[StepResult]] =
    for {
      _               <- ZIO.logInfo(s"Loaded ${features.length} features with ${features.flatMap(_.scenarios).length} scenarios")
      filteredFeatures = filterFeatures(features, config.includeTags, config.excludeTags)
      _ <-
        ZIO.debug(
          s"After filtering: ${filteredFeatures.length} features with ${filteredFeatures.flatMap(_.scenarios).length} scenarios"
        )
      results <- if (filteredFeatures.isEmpty) {
                   ZIO.logWarning("No features or scenarios match the tag filters").as(Nil)
                 } else {
                   ZIO
                     .foreachPar(filteredFeatures) { feature =>
                       ScenarioRunner
                         .runScenarios(steps, feature, config.parallelism)
                         .map(_.flatten)
                         .tap { res =>
                           ZIO.logInfo(s"Feature '${feature.name}' produced ${res.length} results")
                         }
                         .catchAll { e =>
                           ZIO.succeed(List(featureFailedResult(e)(Instant.now())))
                         }
                     }
                     .map(_.flatten)
                     .withParallelism(config.parallelism)
                 }
    } yield results
}
