package zio.bdd.core

import zio.Cause
import zio.bdd.gherkin.*

case class StepResult(step: Step, outcome: Either[Cause[Throwable], Unit], duration: Long = 0L) {
  def isPassed: Boolean        = outcome.isRight
  def error: Option[Throwable] = outcome.left.toOption.map(_.squash)
}

case class ScenarioResult(
  scenario: Scenario,
  stepResults: List[StepResult],
  setupError: Option[Cause[Throwable]] = None,
  duration: Long = 0L
) {
  def isPassed: Boolean  = stepResults.forall(_.isPassed)
  def isIgnored: Boolean = scenario.isIgnored
  def error: Option[Throwable] =
    setupError.flatMap(_.failureOption).orElse(stepResults.find(!_.isPassed).flatMap(_.error))
}

case class FeatureResult(feature: Feature, scenarioResults: List[ScenarioResult], duration: Long = 0L) {
  def isPassed: Boolean        = scenarioResults.forall(_.isPassed)
  def isIgnored: Boolean       = feature.isIgnored || scenarioResults.forall(_.isIgnored)
  def error: Option[Throwable] = scenarioResults.find(!_.isPassed).flatMap(_.error)
}
