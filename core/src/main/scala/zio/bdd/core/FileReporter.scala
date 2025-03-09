package zio.bdd.core

import zio.*

import java.io.File

object FileReporter extends Reporter {
  private def writeToFile(feature: String, content: String): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      val writer = new java.io.PrintWriter(new java.io.File(s"test-results-$feature.log"))
      writer.write(content)
      writer.close()
      ZIO.logInfo(s"Wrote results to test-results-$feature.log")
    }.orDie.flatten

  def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] = {
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

  def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = ZIO.unit

  def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit

  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    ZIO.logInfo(s"Scenario ignored: $scenario")
}
