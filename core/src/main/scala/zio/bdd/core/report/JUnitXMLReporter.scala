package zio.bdd.core.report

import zio.*
import zio.bdd.core.{CollectedLogs, FeatureResult, LogCollector}
import zio.bdd.core.report.JUnitXMLFormatter.{Format, TestSuiteRecord}

import java.nio.file.{Files, Paths}

case class JUnitReporterConfig(
  outputDir: String = "target/test-reports",
  suiteClass: String = "",
  format: Format = Format.JUnit5
)

/**
 * Writes one XML file per feature to `outputDir`.
 *
 * File naming follows the Jenkins JUnit plugin convention (TEST-*.xml):
 * TEST-ComponentSuite-Provision.xml
 *
 * Each file is a standard JUnit testsuite enriched with Gherkin metadata:
 *   - testsuite: feature name, tags, source file, total/failures/skipped counts
 *   - testcase: classname = suite simple name, tags, file, line
 *   - steps: keyword, name, status, duration per Gherkin step
 *   - failure: first failing step message + stack trace
 *   - skipped: ignored and pending scenarios
 *   - system-out / system-err: collected ZIO log entries
 */
case class JUnitXMLReporter(config: JUnitReporterConfig) extends Reporter:

  // "com.example.ComponentSuite$" → "ComponentSuite"
  private val suiteSimpleName: String =
    config.suiteClass.split('.').lastOption.getOrElse(config.suiteClass).stripSuffix("$")

  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    for {
      _            <- ensureDir(config.outputDir)
      logCollector <- ZIO.service[LogCollector]
      _            <- ZIO.foreachDiscard(results)(reportFeature(_, logCollector))
    } yield ()

  private def reportFeature(
    feature: FeatureResult,
    logCollector: LogCollector
  ): ZIO[Any, Throwable, Unit] =
    for {
      now  <- Clock.instant
      logs <- collectLogs(feature, logCollector)
      suite = JUnitXMLFormatter.buildSuite(
                feature.feature,
                feature.scenarioResults,
                logs,
                now,
                suiteSimpleName
              )
      _ <- writeFeature(suite, feature.feature.name)
    } yield ()

  private def collectLogs(
    feature: FeatureResult,
    logCollector: LogCollector
  ): ZIO[Any, Nothing, Map[String, CollectedLogs]] =
    ZIO
      .foreach(feature.scenarioResults) { sr =>
        logCollector.getScenarioLogs(sr.scenario.id.toString).map(sr.scenario.id.toString -> _)
      }
      .map(_.toMap)

  private def writeFeature(suite: TestSuiteRecord, featureName: String): ZIO[Any, Throwable, Unit] =
    // TEST-ComponentSuite-Provision.xml  matches **/TEST-*.xml Jenkins glob
    val prefix   = if (suiteSimpleName.nonEmpty) s"TEST-$suiteSimpleName-" else "TEST-"
    val fileName = prefix + sanitize(featureName) + ".xml"
    JUnitXMLFormatter.writeToFile(suite, s"${config.outputDir}/$fileName", config.format)

  private def sanitize(name: String): String =
    name.replaceAll("[^a-zA-Z0-9-_.]", "_")

  private def ensureDir(dir: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = Paths.get(dir)
      if (!Files.exists(path)) Files.createDirectories(path)
    }

object JUnitXMLReporter:
  def live(config: JUnitReporterConfig = JUnitReporterConfig()): ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(JUnitXMLReporter(config))
