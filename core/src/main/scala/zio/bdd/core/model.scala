package zio.bdd.core

import zio.*

// Define data structures to represent the parsed Gherkin syntax
case class Feature(name: String, background: List[String] = Nil, scenarios: List[Scenario])
case class Scenario(name: String, steps: List[String], examples: List[ExampleRow], metadata: ScenarioMetadata)
case class ExampleRow(data: Map[String, String])
case class ScenarioMetadata(retryCount: Int = 0, isFlaky: Boolean = false, repeatCount: Int = 1)

// TODO: This doesn't belong here
trait Reporter {
  def startFeature(feature: String): ZIO[Any, Nothing, Unit]
  def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit]
  def startScenario(scenario: String): ZIO[Any, Nothing, Unit]
  def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit]
  def startStep(step: String): ZIO[Any, Nothing, Unit]
  def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]
}
