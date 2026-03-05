package zio.bdd.core

import zio.{Cause, Duration}
import zio.bdd.gherkin.*

/** Outcome of a single step execution. */
enum StepStatus:
  case Passed
  case Failed(cause: Cause[Throwable])
  case TimedOut(timeout: Duration, cause: Cause[Throwable])
  case Skipped
  case Pending(reason: String)

/** Thrown inside a step body to mark it as pending (known-unimplemented). */
class PendingException(val reason: String = "TODO") extends RuntimeException(s"Pending: $reason")

/** Thrown when a step exceeds its configured timeout duration. */
class StepTimeoutException(val pattern: String, val timeout: Duration)
    extends RuntimeException(s"Step '$pattern' timed out after ${timeout.toSeconds}s")

case class StepResult(step: Step, outcome: Either[Cause[Throwable], Unit], duration: Long = 0L):
  def isPassed: Boolean = outcome.isRight

  def status: StepStatus = outcome match
    case Right(_) => StepStatus.Passed
    case Left(cause) =>
      cause.squash match
        case p: PendingException     => StepStatus.Pending(p.reason)
        case t: StepTimeoutException => StepStatus.TimedOut(t.timeout, cause)
        case _                       => StepStatus.Failed(cause)

  def isSkipped: Boolean       = false
  def isPending: Boolean       = status.isInstanceOf[StepStatus.Pending]
  def error: Option[Throwable] = outcome.left.toOption.map(_.squash)

object StepResult:
  val skipped: Step => StepResult = step => SkippedStepResult(step)

/**
 * A step that was never attempted because a prior step failed. Not a case class
 * to avoid case-to-case inheritance.
 */
final class SkippedStepResult(step: Step) extends StepResult(step, Right(()), 0L):
  override def isPassed: Boolean        = false
  override def isSkipped: Boolean       = true
  override def status: StepStatus       = StepStatus.Skipped
  override def error: Option[Throwable] = None

object SkippedStepResult:
  def apply(step: Step): SkippedStepResult = new SkippedStepResult(step)

case class ScenarioResult(
  scenario: Scenario,
  stepResults: List[StepResult],
  setupError: Option[Cause[Throwable]] = None,
  duration: Long = 0L
):
  def isPassed: Boolean   = stepResults.forall(r => r.isPassed || r.isSkipped) && setupError.isEmpty
  def isIgnored: Boolean  = scenario.isIgnored
  def hasPending: Boolean = stepResults.exists(_.isPending)

  /**
   * A scenario is "complete" (does not block the build) if it is passed,
   * ignored, or pending. Only FAILED scenarios are considered incomplete.
   */
  def isComplete: Boolean = isPassed || isIgnored || (hasPending && !hasFailure)

  def hasFailure: Boolean =
    setupError.isDefined || stepResults.exists(r =>
      r.status.isInstanceOf[StepStatus.Failed] || r.status.isInstanceOf[StepStatus.TimedOut]
    )

  def error: Option[Throwable] =
    setupError.flatMap(_.failureOption).orElse(stepResults.find(!_.isPassed).flatMap(_.error))

case class FeatureResult(feature: Feature, scenarioResults: List[ScenarioResult], duration: Long = 0L):
  /**
   * A feature is "passed" when every scenario is passed or ignored. Pending
   * scenarios do not block the build — use `isComplete` for a build gate.
   */
  def isPassed: Boolean   = scenarioResults.forall(r => r.isPassed || r.isIgnored)
  def isIgnored: Boolean  = feature.isIgnored || scenarioResults.forall(_.isIgnored)
  def hasPending: Boolean = scenarioResults.exists(_.hasPending)

  /**
   * True when no scenario has a hard failure — PENDING is acceptable for a
   * green build gate.
   */
  def isComplete: Boolean = scenarioResults.forall(_.isComplete)

  def error: Option[Throwable] = scenarioResults.find(!_.isPassed).flatMap(_.error)
