package zio.bdd.core

import zio.*

import java.io.File

trait Reporter {
  def startFeature(feature: String): ZIO[Any, Nothing, Unit]
  def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[Any, Nothing, Unit]
  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]
  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit]
  def startStep(step: String): ZIO[Any, Nothing, Unit]
  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]
  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]
}

object ConsoleReporter extends Reporter {
  // Lighter ANSI colors
  private val LightGreen  = "\u001b[92m" // Bright green for passed
  private val LightRed    = "\u001b[91m" // Bright red for failed
  private val LightBlue   = "\u001b[94m" // Bright blue for features and steps
  private val LightYellow = "\u001b[93m" // Bright yellow for scenarios and logs
  private val LightGray   = "\u001b[90m" // Gray for ignored scenarios
  private val Reset       = "\u001b[0m"

  def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightBlue}* Feature: $feature${Reset}").orDie

  def endFeature(feature: String, results: List[List[StepResult]], ignoredCount: Int): ZIO[Any, Nothing, Unit] = {
    val passed = results.flatten.count(_.succeeded)
    val failed = results.flatten.length - passed
    Console
      .printLine(
        s"${LightBlue}* Finished Feature: $feature - ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}, ${LightGray}$ignoredCount ignored${Reset}"
      )
      .orDie
  }

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightYellow}  ◉ Scenario: $scenario${Reset}").orDie

  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = {
    val passed = results.count(_.succeeded)
    val failed = results.length - passed
    Console
      .printLine(s"${LightYellow}  ◉ Results: ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}")
      .orDie
  }

  def startStep(step: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightBlue}    ├─◑ $step${Reset}").orDie

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = {
    val status   = if (result.succeeded) s"${LightGreen}PASSED${Reset}" else s"${LightRed}FAILED${Reset}"
    val errorMsg = result.error.map(e => s" - ${LightRed}Error: $e${Reset}").getOrElse("")
    val logs = if (result.logs.nonEmpty) {
      result.logs.map { case (msg, time) => s"${LightYellow}      ╰─ [$time] $msg${Reset}" }.mkString("\n")
    } else {
      "" // No logs, no extra lines
    }
    val timing = s" (start: ${result.startTime}, duration: ${result.duration.toMillis}ms)"
    Console
      .printLine(
        s"${LightBlue}    ├─◑ [$status] $step$errorMsg$timing${Reset}" + (if (logs.nonEmpty) s"\n$logs" else "")
      )
      .orDie
  }

  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightGray}  ◉ Scenario Ignored: $scenario${Reset}").orDie
}

object FileReporter extends Reporter {
  private def writeToFile(feature: String, content: String): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      val writer = new java.io.PrintWriter(new java.io.File(s"test-results-$feature.log"))
      writer.write(content)
      writer.close()
      ZIO.logInfo(s"Wrote results to test-results-$feature.log")
    }.orDie.flatten

  def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endFeature(feature: String, results: List[List[StepResult]], ignoredCount: Int): ZIO[Any, Nothing, Unit] = {
    val content = results.zipWithIndex.flatMap { case (scenarioResults, idx) =>
      val scenarioHeader = s"Scenario $idx:\n"
      scenarioResults.map { result =>
        val status   = if (result.succeeded) "PASSED" else "FAILED"
        val errorMsg = result.error.map(e => s" - Error: $e").getOrElse("")
        val logs     = result.logs.map { case (msg, time) => s"[$time] $msg" }.mkString("\n  ")
        s"[$status] ${result.step}$errorMsg\n  $logs"
      }.mkString(scenarioHeader, "\n", "\n")
    }.mkString("\n") + (if (ignoredCount > 0) s"\nIgnored Scenarios: $ignoredCount" else "")
    ZIO.logInfo(
      s"Preparing to write results for feature: $feature with ${results.length} scenarios and $ignoredCount ignored"
    ) *>
      writeToFile(feature, content)
  }

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit

  def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit

  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    ZIO.logInfo(s"Scenario ignored: $scenario")
}
