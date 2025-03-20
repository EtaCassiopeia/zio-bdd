package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.Feature
import java.time.Instant

case class TestConfig(
  includeTags: Set[String] = Set.empty,
  excludeTags: Set[String] = Set.empty,
  parallelism: Int = 1,
  logLevelConfig: LogLevelConfig = LogLevelConfig()
)

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
      featureMatchesInclude || scenarioMatchesInclude // Keep the feature if it or any scenario matches includeTags
    }.map { feature =>
      val filteredScenarios = feature.scenarios.filter { scenario =>
        val combinedTags = feature.tags ++ scenario.tags
        matchesTagFilter(combinedTags, includeTags, excludeTags)
      }
      feature.copy(scenarios = filteredScenarios)
    }.filter(_.scenarios.nonEmpty) // Only keep features with at least one scenario

  private def featureFailedResult(featureId: String, e: Cause[Throwable])(implicit
    trace: Trace
  ): Instant => StepResult =
    startTime =>
      StepResult( // TODO: Consider using a specific error type
        "Feature failed",
        succeeded = false,
        error = Some(TestError.GenericError(e.prettyPrint, None, Some(trace))),
        output = (),
        logs = Nil,
        featureId = Some(featureId),
        duration = Duration.Zero,
        startTime = startTime
      )

  def runFeatures[R](
    steps: ZIOSteps[R],
    features: List[Feature],
    config: TestConfig
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    for {
      logCollector    <- ZIO.service[LogCollector]
      reporter        <- ZIO.service[Reporter]
      _               <- logCollector.setLogLevelConfig(config.logLevelConfig)
      _               <- ZIO.debug(s"Loaded ${features.length} features")
      filteredFeatures = filterFeatures(features, config.includeTags, config.excludeTags)
      results <- if (filteredFeatures.isEmpty) {
                   ZIO.debug("No features or scenarios match the tag filters").as(Nil)
                 } else {
                   ZIO
                     .foreachPar(filteredFeatures) { feature =>
                       val featureId = feature.name.hashCode.toString
                       ScenarioRunner
                         .runScenarios(steps, feature, config.parallelism)
                         .map(_.flatten)
                         .tapDefect { cause =>
                           logCollector
                             .logFeature(featureId, s"Feature failed: ${cause.prettyPrint}", InternalLogLevel.Error)
                             .as(List(featureFailedResult(featureId, cause)(trace)(Instant.now())))
                         }
                     }
                     .map(_.flatten)
                     .withParallelism(config.parallelism)
                 }
      ignoredCount <-
        ZIO.foreach(filteredFeatures)(f => ZIO.succeed(f.scenarios.count(_.metadata.isIgnored))).map(_.sum)
      report <- reporter.generateFinalReport(features, results.groupByScenario, ignoredCount)
      _      <- ZIO.logInfo(report)
    } yield results

  implicit class StepResultListOps(results: List[StepResult]) {
    def groupByScenario: List[List[StepResult]] =
      results.groupBy(_.scenarioId.getOrElse("unknown")).values.toList
  }
}
