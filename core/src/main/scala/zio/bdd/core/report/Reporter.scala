package zio.bdd.core.report

import zio.*
import zio.bdd.core.{LogCollector, StepResult}

trait Reporter {
  def startFeature(feature: String): ZIO[Any, Nothing, Unit]
  def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit]
  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]
  def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit]
  def startStep(step: String): ZIO[Any, Nothing, Unit]
  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]
  def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]
}
