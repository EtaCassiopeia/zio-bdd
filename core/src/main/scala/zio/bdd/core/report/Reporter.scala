package zio.bdd.core.report

import zio.*
import zio.bdd.core.{CollectedLogs, InternalLogLevel, LogCollector, StepResult, TestError}

trait Reporter {
  def startFeature(feature: String): ZIO[Any, Nothing, Unit]
  def endFeature(feature: String, results: List[List[StepResult]], ignoredCount: Int): ZIO[LogCollector, Nothing, Unit]
  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]
  def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit]
  def startStep(step: String): ZIO[Any, Nothing, Unit]
  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]
  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]
  def generateFinalReport(
    features: List[zio.bdd.gherkin.Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String]
}

case class DefaultReporter() extends Reporter {
  case class ReportStats(
    features: Int,
    scenarios: Int,
    steps: Int,
    failedScenarios: Int,
    ignoredScenarios: Int,
    ignoredSteps: Int
  )

  def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
  def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]                                   = ZIO.unit
  def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  def startStep(step: String): ZIO[Any, Nothing, Unit]                                           = ZIO.unit
  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]                         = ZIO.unit
  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]                           = ZIO.unit

  def generateFinalReport(
    features: List[zio.bdd.gherkin.Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String] =
    for {
      logCollector <- ZIO.service[LogCollector]
      stats = ReportStats(
                features = features.length,
                scenarios = results.length,
                steps = results.flatten.length,
                failedScenarios = results.count(_.exists(!_.succeeded)),
                ignoredScenarios = ignoredCount,
                ignoredSteps = results.flatten.count(_.error.exists(_.message.contains("Skipped due to prior failure")))
              )
      report = s"""
                  |Final Test Report:
                  |------------------
                  |Total Features: ${stats.features}
                  |Total Scenarios: ${stats.scenarios}
                  |Total Steps: ${stats.steps}
                  |Failed Scenarios: ${stats.failedScenarios}
                  |Ignored Scenarios: ${stats.ignoredScenarios}
                  |Ignored Steps: ${stats.ignoredSteps}
                  |""".stripMargin
      _ <- logCollector.logFeature("report", report, InternalLogLevel.Info)
    } yield report
}
