package zio.bdd.core

import zio.Cause
import zio.bdd.gherkin.*

case class StepResult(step: Step, outcome: Either[Cause[Throwable], Unit]) {
  def isPassed: Boolean = outcome.isRight
}

case class ScenarioResult(
  scenario: Scenario,
  stepResults: List[StepResult],
  setupError: Option[Cause[Throwable]] = None
) {
  def isPassed: Boolean = stepResults.forall(_.isPassed)
}

case class FeatureResult(feature: Feature, scenarioResults: List[ScenarioResult]) {
  def isPassed: Boolean = scenarioResults.forall(_.isPassed)
}
