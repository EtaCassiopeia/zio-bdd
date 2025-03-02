package zio.bdd.core

import zio.*

import java.io.File

trait Reporter {
  def startFeature(feature: String): ZIO[Any, Nothing, Unit]

  def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit]

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]

  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit]

  def startStep(step: String): ZIO[Any, Nothing, Unit]

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]
}

object ConsoleReporter extends Reporter {
  private val Green = "\u001b[32m"
  private val Red = "\u001b[31m"
  private val Yellow = "\u001b[33m"
  private val Blue = "\u001b[34m"
  private val Reset = "\u001b[0m"

  def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${Blue}✨ Feature: $feature ✨${Reset}").orDie *>
      ZIO.logInfo(s"Starting feature: $feature")

  def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = {
    val passed = results.flatten.count(_.succeeded)
    val failed = results.flatten.length - passed
    Console
      .printLine(
        s"${Blue}✨ Finished Feature: $feature - ${Green}$passed passed${Reset}, ${Red}$failed failed${Reset} ✨${Reset}"
      )
      .orDie *>
      ZIO.logInfo(s"Finished feature: $feature with $passed passed, $failed failed")
  }

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${Yellow}🌟 Scenario: $scenario 🌟${Reset}").orDie *>
      ZIO.logInfo(s"Starting scenario: $scenario")

  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = {
    val passed = results.count(_.succeeded)
    val failed = results.length - passed
    Console
      .printLine(s"${Yellow}🌟 Results: ${Green}$passed passed${Reset}, ${Red}$failed failed${Reset} 🌟${Reset}")
      .orDie *>
      ZIO.logInfo(s"Finished scenario: $scenario with $passed passed, $failed failed")
  }

  def startStep(step: String): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"${Blue}➡️ Step: $step${Reset}").orDie *>
      ZIO.logInfo(s"Starting step: $step")

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = {
    val status = if (result.succeeded) s"${Green}PASSED${Reset}" else s"${Red}FAILED${Reset}"
    val errorMsg = result.error.map(e => s" - ${Red}Error: $e${Reset}").getOrElse("")
    val logs = result.logs.map { case (msg, time) => s"${Yellow}[$time] $msg${Reset}" }.mkString("\n  ")
    Console.printLine(s"${Blue}[$status] $step$errorMsg${Reset}\n  $logs").orDie *>
      ZIO.logInfo(s"Finished step: $step with status $status")
  }
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

  def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = {
    val content = results.zipWithIndex.flatMap { case (scenarioResults, idx) =>
      val scenarioHeader = s"Scenario $idx:\n"
      scenarioResults.map { result =>
        val status = if (result.succeeded) "PASSED" else "FAILED"
        val errorMsg = result.error.map(e => s" - Error: $e").getOrElse("")
        val logs = result.logs.map { case (msg, time) => s"[$time] $msg" }.mkString("\n  ")
        s"[$status] ${result.step}$errorMsg\n  $logs"
      }.mkString(scenarioHeader, "\n", "\n")
    }.mkString("\n")
    ZIO.logInfo(s"Preparing to write results for feature: $feature with ${results.length} scenarios") *>
      writeToFile(feature, content)
  }

  def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit

  def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
}