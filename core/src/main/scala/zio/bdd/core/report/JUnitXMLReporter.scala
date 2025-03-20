package zio.bdd.core.report

import zio.*
import zio.bdd.core.{LogCollector, CollectedLogs, StepResult}
import zio.bdd.gherkin.Feature
import java.time.Instant
import java.nio.file.{Files, Paths}

case class JUnitReporterConfig(
  outputDir: String = "target/test-results",
  format: JUnitXMLFormatter.Format = JUnitXMLFormatter.Format.JUnit5
)

class JUnitXMLReporter(config: JUnitReporterConfig) extends Reporter {

  override def generateFinalReport(
    features: List[Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String] =
    for {
      logCollector <- ZIO.service[LogCollector]
      reports      <- ZIO.foreach(features)(feature => buildJUnitReportForFeature(feature, logCollector, results))
      outputPaths  <- ZIO.foreach(reports)(report => writeReport(report))
      _            <- ZIO.foreachDiscard(outputPaths)(path => ZIO.logInfo(s"JUnit XML report written to $path"))
    } yield outputPaths.mkString(", ")

  private def ensureOutputDir: ZIO[Any, Nothing, Unit] = ZIO.attempt {
    val dir = Paths.get(config.outputDir)
    Files.createDirectories(dir)
    ()
  }.orDie

  private def writeReport(report: JUnitXMLFormatter.TestSuite): ZIO[Any, Nothing, String] =
    for {
      _         <- ensureOutputDir
      outputPath = s"${config.outputDir}/${sanitizeFileName(report.name)}.xml"
      _         <- JUnitXMLFormatter.writeToFile(report, outputPath, config.format).orDie
    } yield outputPath

  private def sanitizeFileName(name: String): String =
    // Replace invalid filename characters with underscores and trim to reasonable length
    name.replaceAll("[^a-zA-Z0-9-_]", "_").take(100)

  private def buildJUnitReportForFeature(
    feature: Feature,
    logCollector: LogCollector,
    results: List[List[StepResult]]
  ): ZIO[Any, Nothing, JUnitXMLFormatter.TestSuite] = {
    // Convert scenarios iterator to ZIO effect
    val testCasesEffect = ZIO.foreach(feature.scenarios) { scenario =>
      val scenarioId = s"${feature.name}-${scenario.name}".hashCode.toString
      val steps      = ZIO.succeed(results.find(_.headOption.exists(_.scenarioId.contains(scenarioId))).getOrElse(Nil))

      steps.flatMap { scenarioSteps =>
        val isIgnored = scenario.metadata.isIgnored || scenarioSteps.isEmpty

        if (isIgnored) {
          // For ignored scenarios, create a minimal test case
          ZIO.succeed(
            JUnitXMLFormatter.TestCase(
              name = s"Scenario: ${scenario.name}",
              succeeded = true, // Ignored tests are not failures in JUnit
              logs = CollectedLogs(List.empty),
              timestamp = Instant.now(),
              duration = 0L
            )
          )
        } else {
          val succeeded = ZIO.succeed(scenarioSteps.forall(_.succeeded))
          val startTime = ZIO.succeed(scenarioSteps.headOption.map(_.startTime).getOrElse(Instant.now()))
          val duration  = ZIO.succeed(scenarioSteps.map(_.duration.toMillis).sum)

          val logsEffect = ZIO
            .foreach(scenarioSteps) { step =>
              logCollector.getLogs(scenarioId, step.stepId.getOrElse(step.step.hashCode.toString))
            }
            .map(_.reduceOption(_ ++ _).getOrElse(CollectedLogs(List.empty)))

          for {
            succ <- succeeded
            time <- startTime
            dur  <- duration
            logs <- logsEffect
          } yield JUnitXMLFormatter.TestCase(
            name = s"Scenario: ${scenario.name}",
            succeeded = succ,
            logs = logs,
            timestamp = time,
            duration = dur
          )
        }
      }
    }

    testCasesEffect.map { testCases =>
      JUnitXMLFormatter.TestSuite(
        name = feature.name,
        cases = testCases,
        timestamp = Instant.now()
      )
    }
  }
}

object JUnitXMLReporter {
  def live(config: JUnitReporterConfig = JUnitReporterConfig()): ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(new JUnitXMLReporter(config))
}
