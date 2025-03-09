package zio.bdd.core

import zio.*

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

  def endFeature(
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

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightYellow}  ◉ Scenario: $scenario${Reset}").orDie

  def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = {
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
    val errorMsg = result.error.map(t => s" - ${LightRed}Error: ${t.getMessage}${Reset}").getOrElse("")
    val stackTrace =
      result.error.map(t => s"${LightRed}Stack trace:\n${t.getStackTrace.mkString("\n")}${Reset}").getOrElse("")
    val logs = if (result.logs.nonEmpty) {
      result.logs.map { case (msg, time) => s"${LightYellow}      ╰─ [$time] $msg${Reset}" }.mkString("\n")
    } else ""
    val timing =
      s" (start: ${result.startTime}, duration: ${result.duration.toMillis}ms, file: ${result.file}:${result.line})"
    Console
      .printLine(
        s"${LightBlue}    ├─◑ [$status] $step$errorMsg$timing${Reset}" +
          (if (logs.nonEmpty || stackTrace.nonEmpty) s"\n$logs$stackTrace" else "")
      )
      .orDie
  }

  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${LightGray}  ◉ Scenario Ignored: $scenario${Reset}").orDie
}
