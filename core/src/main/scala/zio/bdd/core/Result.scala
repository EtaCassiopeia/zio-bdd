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

/**
 * Thrown when an `And`/`But` step has no preceding step to inherit its keyword
 * from.
 */
class StepSequencingException(message: String) extends RuntimeException(message)

/**
 * Outcome of a single step. `status` is the single source of truth, derived
 * from `outcome` (the executed Either) plus the `skipped` flag (set for steps
 * that were never attempted because a prior step failed).
 */
case class StepResult(
  step: Step,
  outcome: Either[Cause[Throwable], Unit],
  duration: Long = 0L,
  skipped: Boolean = false
):
  def status: StepStatus =
    if (skipped) StepStatus.Skipped
    else
      outcome match
        case Right(_) => StepStatus.Passed
        case Left(cause) =>
          cause.squash match
            case p: PendingException     => StepStatus.Pending(p.reason)
            case t: StepTimeoutException => StepStatus.TimedOut(t.timeout, cause)
            case _                       => StepStatus.Failed(cause)

  def isPassed: Boolean  = !skipped && outcome.isRight
  def isSkipped: Boolean = skipped
  def isPending: Boolean = status match
    case StepStatus.Pending(_) => true
    case _                     => false
  def error: Option[Throwable] = if (skipped) None else outcome.left.toOption.map(_.squash)

object StepResult:
  /**
   * A step that was never attempted because a prior step failed or timed out.
   */
  def skipped(step: Step): StepResult = StepResult(step, Right(()), 0L, skipped = true)

case class ScenarioResult(
  scenario: Scenario,
  stepResults: List[StepResult],
  setupError: Option[Cause[Throwable]] = None,
  duration: Long = 0L,
  /**
   * How many times the scenario ran. 1 unless a retry aspect
   * (@retry/@flaky/@nonFlaky) applied.
   */
  attempts: Int = 1,
  /**
   * True when the scenario carried `@expectedFailure`/`@failing`: its raw
   * outcome is inverted — a raw failure is an expected (passing) result, and a
   * raw pass is an "unexpectedly passing" failure.
   */
  expectedFailure: Boolean = false
):
  // The scenario body outcome (every step passed or was skipped), ignoring setup.
  private def bodyPassed: Boolean = stepResults.forall(r => r.isPassed || r.isSkipped)
  private def rawPassed: Boolean  = bodyPassed && setupError.isEmpty

  def isIgnored: Boolean  = scenario.isIgnored
  def hasPending: Boolean = stepResults.exists(_.isPending)

  // The @expectedFailure inversion applies only to a BODY that ran to a definitive pass/fail with
  // setup succeeding. A setup/infra failure (a failing beforeScenario hook, bad env) is always a
  // real failure, and a PENDING (unimplemented) step is surfaced as PENDING — neither is masked
  // green by the tag.
  private def inverts: Boolean = expectedFailure && setupError.isEmpty && !hasPending

  def isPassed: Boolean = if (inverts) !bodyPassed else rawPassed

  /**
   * `@expectedFailure` scenario whose body failed as expected (a green result).
   */
  def isExpectedFailure: Boolean = inverts && !bodyPassed

  /** `@expectedFailure` scenario whose body unexpectedly passed (a failure). */
  def isUnexpectedlyPassing: Boolean = inverts && bodyPassed

  /**
   * A scenario is "complete" (does not block the build) if it is passed,
   * ignored, or pending. Only FAILED scenarios are considered incomplete.
   */
  def isComplete: Boolean = isPassed || isIgnored || (hasPending && !hasFailure)

  def hasFailure: Boolean =
    if (inverts) isUnexpectedlyPassing
    else
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
