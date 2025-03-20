package zio.bdd.core.report

import zio.*
import zio.bdd.core.*
import zio.bdd.gherkin.{Feature, StepType}
import java.nio.file.{Files, Paths}
import java.time.Instant

case class JUnitReporterConfig(
  outputDir: String = "target/test-results",
  format: JUnitXMLFormatter.Format = JUnitXMLFormatter.Format.JUnit5
)

class JUnitReporter(config: JUnitReporterConfig) extends Reporter {
  private def ensureOutputDir: ZIO[Any, Throwable, Unit] =
    ZIO.attemptBlocking(Files.createDirectories(Paths.get(config.outputDir))).unit

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit]                                   = ZIO.unit
  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  override def startStep(step: String): ZIO[Any, Nothing, Unit]                                           = ZIO.unit
  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]                         = ZIO.unit
  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]                           = ZIO.unit

  override def generateFinalReport(
    features: List[Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String] =
    for {
      logCollector <- ZIO.service[LogCollector]
      _            <- ensureOutputDir.orDie
      reportPaths <- ZIO.foreach(features) { feature =>
                       val allSteps            = results.flatten.filter(_.featureId.contains(feature.name.hashCode.toString))
                       val executedScenarioIds = allSteps.flatMap(_.scenarioId).toSet

                       // Build test cases for executed scenarios
                       val scenarioTestCases = feature.scenarios
                         .filter(scenario =>
                           executedScenarioIds.contains(s"${feature.name}-${scenario.name}".hashCode.toString)
                         )
                         .map { scenario =>
                           val scenarioId    = s"${feature.name}-${scenario.name}".hashCode.toString
                           val scenarioSteps = allSteps.filter(_.scenarioId.contains(scenarioId))
                           for {
                             scenarioLogs <- logCollector.getScenarioLogs(scenarioId)
                             detailedLogs <-
                               ZIO.collectAll(scenarioSteps.map { step =>
                                 logCollector
                                   .getLogs(scenarioId, step.stepId.getOrElse(step.step.hashCode.toString))
                                   .map { stepLogs =>
                                     val stepType = scenario.steps
                                       .find(_.pattern == step.step)
                                       .map(_.stepType)
                                       .getOrElse(StepType.GivenStep)
                                     val stepPrefix = stepType match {
                                       case StepType.GivenStep => "Given"
                                       case StepType.WhenStep  => "When"
                                       case StepType.ThenStep  => "Then"
                                       case StepType.AndStep   => "And"
                                     }
                                     val status = if (step.succeeded) "[PASSED]" else "[FAILED]"
                                     val timing =
                                       s"(start: ${step.startTime}, duration: ${step.duration.toMillis}ms${step.file
                                           .map(f => s", file: $f:${step.line.getOrElse(-1)}")
                                           .getOrElse("")})"
                                     val stepHeader = s"$stepPrefix $status ${step.step} $timing"
                                     val stepLogEntries = stepLogs.entries.map { log =>
                                       LogEntry(
                                         message = s"[${log.level}] ${log.message}",
                                         timestamp = log.timestamp,
                                         source = log.source,
                                         level = log.level,
                                         stepId = step.stepId.getOrElse("unknown")
                                       )
                                     }
                                     val errorEntries = step.error.map { e =>
                                       val trace = e.trace
                                         .flatMap(Trace.unapply)
                                         .map { case (loc, file, line) =>
                                           LogEntry(
                                             s"Trace: $loc ($file:$line)",
                                             step.startTime,
                                             LogSource.Stderr,
                                             InternalLogLevel.Error,
                                             step.stepId.getOrElse("unknown")
                                           )
                                         }
                                         .toList
                                       val cause = e.cause.map { c =>
                                         val stack = c.getStackTrace
                                           .take(5)
                                           .map(st =>
                                             LogEntry(
                                               st.toString,
                                               step.startTime,
                                               LogSource.Stderr,
                                               InternalLogLevel.Error,
                                               step.stepId.getOrElse("unknown")
                                             )
                                           )
                                         LogEntry(
                                           s"Cause: ${c.getMessage}",
                                           step.startTime,
                                           LogSource.Stderr,
                                           InternalLogLevel.Error,
                                           step.stepId.getOrElse("unknown")
                                         ) :: stack.toList
                                       }.getOrElse(Nil)
                                       LogEntry(
                                         s"Error: ${e.message}",
                                         step.startTime,
                                         LogSource.Stderr,
                                         InternalLogLevel.Error,
                                         step.stepId.getOrElse("unknown")
                                       ) :: (trace ++ cause)
                                     }.getOrElse(Nil)
                                     (stepHeader, stepLogEntries ++ errorEntries)
                                   }
                               })
                           } yield {
                             val combinedLogs = CollectedLogs(
                               (scenarioLogs.entries ++ detailedLogs.flatMap(_._2)).sortBy(_.timestamp)
                             )
                             JUnitXMLFormatter.TestCase(
                               name = scenario.name,
                               succeeded = scenarioSteps.forall(_.succeeded),
                               logs = CollectedLogs(
                                 detailedLogs.map { case (header, logs) =>
                                   LogEntry(
                                     header,
                                     scenarioSteps.headOption.map(_.startTime).getOrElse(Instant.now()),
                                     LogSource.Stdout,
                                     InternalLogLevel.Info,
                                     "scenario"
                                   )
                                 } ++ combinedLogs.entries
                               ),
                               timestamp = scenarioSteps.headOption.map(_.startTime).getOrElse(Instant.now()),
                               duration = scenarioSteps.map(_.duration.toMillis).sum
                             )
                           }
                         }

                       // Handle ignored scenarios
                       val ignoredTestCases = feature.scenarios
                         .filter(scenario =>
                           !executedScenarioIds.contains(
                             s"${feature.name}-${scenario.name}".hashCode.toString
                           ) || scenario.metadata.isIgnored
                         )
                         .map { scenario =>
                           JUnitXMLFormatter.TestCase(
                             name = scenario.name,
                             succeeded = true,
                             logs = CollectedLogs(Nil),
                             timestamp = Instant.now(),
                             duration = 0
                           )
                         }

                       for {
                         testCases <- ZIO.collectAll(scenarioTestCases).map(_ ++ ignoredTestCases)
                         writeResult <- {
                           val suite    = JUnitXMLFormatter.TestSuite(feature.name, testCases.toList, Instant.now())
                           val filePath = s"${config.outputDir}/${feature.name.replaceAll("[^a-zA-Z0-9]", "_")}.xml"
                           JUnitXMLFormatter
                             .writeToFile(suite, filePath, config.format)
                             .foldZIO(
                               error => ZIO.succeed(s"Failed to write report for ${feature.name}: ${error.getMessage}"),
                               _ => ZIO.succeed(s"Generated report for ${feature.name} at $filePath")
                             )
                         }
                       } yield writeResult
                     }
    } yield reportPaths.mkString("\n")
}

object JUnitReporter {
  def live(config: JUnitReporterConfig = JUnitReporterConfig()): ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(new JUnitReporter(config))
}
