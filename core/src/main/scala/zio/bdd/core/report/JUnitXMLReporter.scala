package zio.bdd.core.report

import zio.*
import zio.bdd.core.report.JUnitReporter.{Format, TestCase, TestSuite}
import zio.bdd.core.report.{JUnitReporter, Reporter}
import zio.bdd.core.{CollectedLogs, LogCollector, StepResult}

import java.time.Instant

case class JUnitXMLReporter(
  format: Format = Format.JUnit5,
  testCasesRef: Ref[List[TestCase]],
  outputDir: String = "target/test-results"
) extends Reporter {

  private def ensureOutputDir: ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val dir = new java.io.File(outputDir)
      if (!dir.exists()) {
        val created = dir.mkdirs()
        if (!created) throw new java.io.IOException(s"Failed to create directory: $outputDir")
      }
    }.tapError(e => ZIO.logError(s"Failed to ensure output dir: $e"))

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    ZIO.logInfo(s"Starting feature: $feature")

  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] =
    for {
      testCases <- testCasesRef.get
      _         <- ZIO.logDebug(s"Ending feature '$feature' with ${testCases.length} test cases")
      suite = TestSuite(
                name = feature,
                cases = testCases.reverse,
                timestamp = results.flatten.headOption.map(_.startTime).getOrElse(Instant.now())
              )
      _       <- ensureOutputDir.orDie
      filePath = s"$outputDir/${feature.replaceAll("[^a-zA-Z0-9]", "_")}-${format.toString.toLowerCase}.xml"
      _       <- JUnitReporter.writeToFile(suite, filePath, format).orDie
      _       <- ZIO.serviceWithZIO[LogCollector](_.clearLogs)
    } yield ()

  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    ZIO.logDebug(s"Starting scenario: $scenario").unit

  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] =
    for {
      _         <- ZIO.logDebug(s"Entering endScenario: $scenario with ${results.length} steps")
      _         <- Console.printLine(s"DEBUG: Ending scenario: $scenario").orDie
      startTime <- ZIO.succeed(results.headOption.map(_.startTime).getOrElse(Instant.now()))
      duration = results.lastOption
                   .map(r => java.time.Duration.between(startTime, r.startTime.plusNanos(r.duration.toNanos)).toMillis)
                   .getOrElse(0L)
      scenarioId <- ZIO.logAnnotations.map(_.getOrElse("scenarioId", s"${scenario}_default"))
      logs       <- LogCollector.getLogs(scenarioId)
      succeeded   = results.forall(_.succeeded)
      assertions  = results.count(_.succeeded)
      testCase = TestCase(
                   name = scenario,
                   succeeded = succeeded,
                   logs = logs,
                   timestamp = startTime,
                   duration = duration,
                   assertions = assertions
                 )
      _ <- testCasesRef.update(testCase :: _)
    } yield ()

  override def startStep(step: String): ZIO[Any, Nothing, Unit] =
    ZIO.unit

  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] =
    ZIO.unit

  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    for {
      startTime <- Clock.instant
      testCase = TestCase(
                   name = scenario,
                   succeeded = true,
                   logs = CollectedLogs(),
                   timestamp = startTime,
                   duration = 0L,
                   assertions = 0
                 )
      _ <- testCasesRef.update(testCase :: _)
    } yield ()
}

object JUnitXMLReporter {
  def live(format: Format, outputDir: String): ZLayer[Any, Nothing, Reporter] =
    ZLayer.fromZIO(
      Ref.make(List.empty[TestCase]).map { ref =>
        JUnitXMLReporter(format, ref, outputDir)
      }
    )

  val live: ZLayer[Any, Nothing, Reporter] =
    live(Format.JUnit5, "target/test-results")
}
