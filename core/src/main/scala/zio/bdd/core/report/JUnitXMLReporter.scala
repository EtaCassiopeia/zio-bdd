package zio.bdd.core.report

import zio.{ZIO, ZLayer}
import zio.bdd.core.{FeatureResult, LogCollector}

import java.time.Instant
import java.nio.file.{Files, Paths}

case class JUnitReporterConfig(
  outputDir: String = "target/test-results",
  format: JUnitXMLFormatter.Format = JUnitXMLFormatter.Format.JUnit5
)

case class JUnitXMLReporter(config: JUnitReporterConfig) extends Reporter {

  private def sanitizeFileName(name: String): String =
    name.replaceAll("[^a-zA-Z0-9-_.]", "_")

  private def ensureDirectoryExists(dir: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = Paths.get(dir)
      if (!Files.exists(path)) {
        Files.createDirectories(path)
      }
    }

  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    for {
      _            <- ensureDirectoryExists(config.outputDir)
      logCollector <- ZIO.service[LogCollector]
      _ <- ZIO.foreachDiscard(results) { feature =>
             val fileName = s"${sanitizeFileName(feature.feature.name)}.xml"
             val filePath = s"${config.outputDir}/$fileName"

             val testCases = feature.scenarioResults.map { scenarioResult =>
               for {
                 scenarioLogs <- logCollector.getScenarioLogs(scenarioResult.scenario.id.toString)
               } yield JUnitXMLFormatter.TestCase(
                 name = scenarioResult.scenario.name,
                 succeeded = scenarioResult.isPassed,
                 logs = scenarioLogs,
                 timestamp = Instant.now(),
                 duration = 0 // Duration could be enhanced if needed
               )
             }

             ZIO.collectAll(testCases).flatMap { cases =>
               val suite = JUnitXMLFormatter.TestSuite(
                 name = feature.feature.name,
                 cases = cases,
                 timestamp = Instant.now()
               )
               JUnitXMLFormatter.writeToFile(suite, filePath, config.format)
             }
           }
    } yield ()
}

object JUnitXMLReporter {
  def live(config: JUnitReporterConfig = JUnitReporterConfig()): ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(JUnitXMLReporter(config))
}
