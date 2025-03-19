package zio.bdd.core.report

import zio.*
import zio.bdd.core.{InternalLogLevel, LogCollector, StepResult}

object ConsoleReporter extends Reporter {
  // Lighter ANSI colors
  private val LightGreen  = "\u001b[92m" // Bright green for passed
  private val LightRed    = "\u001b[91m" // Bright red for failed
  private val LightBlue   = "\u001b[94m" // Bright blue for features and steps
  private val LightYellow = "\u001b[93m" // Bright yellow for scenarios and logs
  private val LightGray   = "\u001b[90m" // Gray for ignored scenarios
  private val Reset       = "\u001b[0m"

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightBlue}* Feature: $feature${Reset}").orDie

  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] = {
    val passed = results.flatten.count(_.succeeded)
    val failed = results.flatten.length - passed
    Console
      .printLine(
        s"${LightBlue}* Finished Feature: $feature - ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}, ${LightGray}$ignoredCount ignored${Reset}"
      )
      .orDie
  }

  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightYellow}  ◉ Scenario: $scenario${Reset}").orDie

  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = {
    val passed = results.count(_.succeeded)
    val failed = results.length - passed
    Console
      .printLine(
        s"${LightYellow}  ◉ Results: ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}"
      )
      .orDie
  }

  override def startStep(step: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightBlue}    ├─◑ $step${Reset}").orDie

  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = {
    val status   = if (result.succeeded) s"${LightGreen}PASSED${Reset}" else s"${LightRed}FAILED${Reset}"
    val errorMsg = result.error.map(e => s" - ${LightRed}Error: ${e.message}${Reset}").getOrElse("")
    val traceMsg = result.error
      .flatMap(_.trace)
      .map { t =>
        Trace.unapply(t) match {
          case Some((location, file, line)) => s"${LightRed}Trace: $location ($file:$line)${Reset}"
          case None                         => s"${LightRed}Trace unavailable${Reset}"
        }
      }
      .getOrElse("")
    val logs = if (result.logs.nonEmpty) {
      result.logs.map { case (msg, time, level) => s"${LightYellow}      ╰─ [$time] [$level] $msg${Reset}" }
        .mkString("\n")
    } else ""
    val timing =
      s" (start: ${result.startTime}, duration: ${result.duration.toMillis}ms, file: ${result.file.getOrElse("unknown")}:${result.line
          .getOrElse(-1)})"
    Console
      .printLine(
        s"${LightBlue}    ├─◑ [$status] $step$errorMsg$timing${Reset}" +
          (if (logs.nonEmpty || traceMsg.nonEmpty) s"\n$logs$traceMsg" else "")
      )
      .orDie
  }

  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightGray}  ◉ Scenario Ignored: $scenario${Reset}").orDie

  override def generateFinalReport(
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
                  |${LightBlue}Final Test Report:${Reset}
                  |${LightBlue}------------------${Reset}
                  |Total Features: ${stats.features}
                  |Total Scenarios: ${stats.scenarios}
                  |Total Steps: ${stats.steps}
                  |Failed Scenarios: ${stats.failedScenarios}
                  |Ignored Scenarios: ${stats.ignoredScenarios}
                  |Ignored Steps: ${stats.ignoredSteps}
                  |""".stripMargin
      _ <- logCollector.logFeature("report", report, InternalLogLevel.Info)
      _ <- Console.printLine(report).orDie
    } yield report

  private case class ReportStats(
    features: Int,
    scenarios: Int,
    steps: Int,
    failedScenarios: Int,
    ignoredScenarios: Int,
    ignoredSteps: Int
  )
}
