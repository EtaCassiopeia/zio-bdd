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
      val featureTags            = feature.tags.toSet
      val allScenarioTags        = feature.scenarios.flatMap(_.tags).toSet
      val featureMatchesInclude  = includeTags.isEmpty || includeTags.exists(featureTags.contains)
      val scenarioMatchesInclude = includeTags.isEmpty || includeTags.exists(allScenarioTags.contains)
      featureMatchesInclude || scenarioMatchesInclude
    }.map { feature =>
      val filteredScenarios = feature.scenarios.filter { scenario =>
        val combinedTags = feature.tags ++ scenario.tags
        matchesTagFilter(combinedTags, includeTags, excludeTags)
      }
      feature.copy(scenarios = filteredScenarios)
    }.filter(_.scenarios.nonEmpty)

  private def featureFailedResult(e: Cause[Throwable])(implicit trace: Trace): Instant => StepResult =
    startTime =>
      StepResult(
        "Feature failed",
        succeeded = false,
        error = Some(TestError.GenericError(e.prettyPrint, None, Some(trace))),
        output = (),
        logs = Nil,
        duration = Duration.Zero,
        startTime = startTime
      )

  def runFeatures[R](
    steps: ZIOSteps[R],
    features: List[Feature],
    config: TestConfig
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    for {
      logCollector <- ZIO.service[LogCollector]
      reporter     <- ZIO.service[Reporter]
      _            <- logCollector.setLogLevelConfig(config.logLevelConfig)
      _ <- logCollector.logFeature(
             "all",
             s"Loaded ${features.length} features with ${features.flatMap(_.scenarios).length} scenarios",
             InternalLogLevel.Info
           )
      filteredFeatures = filterFeatures(features, config.includeTags, config.excludeTags)
      _ <-
        logCollector.logFeature(
          "all",
          s"After filtering: ${filteredFeatures.length} features with ${filteredFeatures.flatMap(_.scenarios).length} scenarios",
          InternalLogLevel.Debug
        )
      results <- if (filteredFeatures.isEmpty) {
                   logCollector
                     .logFeature("all", "No features or scenarios match the tag filters", InternalLogLevel.Warning)
                     .as(Nil)
                 } else {
                   ZIO
                     .foreachPar(filteredFeatures) { feature =>
                       val featureId = feature.name.hashCode.toString
                       logCollector.logFeature(
                         featureId,
                         s"Starting feature: ${feature.name}",
                         InternalLogLevel.Info
                       ) *>
                         ScenarioRunner
                           .runScenarios(steps, feature, config.parallelism)
                           .map(_.flatten)
                           .tap { res =>
                             logCollector.logFeature(
                               featureId,
                               s"Feature '${feature.name}' produced ${res.length} results",
                               InternalLogLevel.Info
                             )
                           }
                           .tapDefect { cause =>
                             logCollector
                               .logFeature(featureId, s"Feature failed: ${cause.prettyPrint}", InternalLogLevel.Error)
                               .as(List(featureFailedResult(cause)(trace)(Instant.now())))
                           }
                     }
                     .map(_.flatten)
                     .withParallelism(config.parallelism)
                 }
      ignoredCount <-
        ZIO.foreach(filteredFeatures)(f => ZIO.succeed(f.scenarios.count(_.metadata.isIgnored))).map(_.sum)
      report <- reporter.generateFinalReport(features, results.groupedBy(_.step), ignoredCount)
      _      <- ZIO.logInfo(report)
    } yield results

  implicit class StepResultListOps(results: List[StepResult]) {
    def groupedBy(key: StepResult => String): List[List[StepResult]] =
      results.groupBy(key).values.toList
  }
}
