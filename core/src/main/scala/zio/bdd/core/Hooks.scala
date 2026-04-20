package zio.bdd.core

import zio.*
import zio.bdd.core.step.{State, ZIOSteps}
import zio.bdd.gherkin.{ScenarioMetadata, StepType}

import scala.collection.mutable

/** Metadata about a step passed to BeforeStep/AfterStep hooks. */
case class StepMetadata(pattern: String, stepType: StepType, file: Option[String], line: Option[Int])

trait Hooks[R, S] { self: ZIOSteps[R, S] =>

  type FeatureHook         = URIO[R, Unit]
  type ScenarioHook        = ScenarioMetadata => URIO[R & State[S], Unit]
  type UnconditionalScHook = URIO[R & State[S], Unit]
  type StepHook            = StepMetadata => URIO[R & State[S], Unit]
  type RunHook             = URIO[R, Unit]

  // ── Before/After All ────────────────────────────────────────────────────
  // Uses ListBuffer accumulation (same pattern as step registration) so
  // multiple beforeAll { } calls compose rather than the last one winning.
  private val _beforeAllHooks = mutable.ListBuffer.empty[RunHook]
  private val _afterAllHooks  = mutable.ListBuffer.empty[RunHook]

  def beforeAll(hook: => RunHook): Unit = { _beforeAllHooks += hook; () }
  def afterAll(hook: => RunHook): Unit  = { _afterAllHooks += hook; () }

  private[core] def beforeAllHook: RunHook = ZIO.foreachDiscard(_beforeAllHooks.toList)(identity)
  private[core] def afterAllHook: RunHook  = ZIO.foreachDiscard(_afterAllHooks.toList)(identity)

  // ── Before/After Feature ─────────────────────────────────────────────────
  private val _beforeFeatureHooks = mutable.ListBuffer.empty[FeatureHook]
  private val _afterFeatureHooks  = mutable.ListBuffer.empty[FeatureHook]

  def beforeFeature(hook: => FeatureHook): Unit = { _beforeFeatureHooks += hook; () }
  def afterFeature(hook: => FeatureHook): Unit  = { _afterFeatureHooks += hook; () }

  private[core] def beforeFeatureHook: FeatureHook =
    ZIO.foreachDiscard(_beforeFeatureHooks.toList)(identity)
  private[core] def afterFeatureHook: FeatureHook =
    ZIO.foreachDiscard(_afterFeatureHooks.toList)(identity)

  // ── Before/After Scenario ────────────────────────────────────────────────
  private val _beforeScenarioHooks = mutable.ListBuffer.empty[ScenarioHook]
  private val _afterScenarioHooks  = mutable.ListBuffer.empty[ScenarioHook]

  /** Register a before-scenario hook that receives the scenario's metadata. */
  def beforeScenario(hook: ScenarioHook): Unit = { _beforeScenarioHooks += hook; () }

  /** Register an unconditional before-scenario hook (no metadata needed). */
  def beforeScenario(hook: => UnconditionalScHook): Unit =
    beforeScenario((_: ScenarioMetadata) => hook)

  /**
   * Register a before-scenario hook that only fires when the scenario has a
   * specific tag.
   */
  def beforeScenarioTagged(tag: String)(hook: ScenarioHook): Unit =
    beforeScenario((meta: ScenarioMetadata) => if (meta.tags.contains(tag)) hook(meta) else ZIO.unit)

  def afterScenario(hook: ScenarioHook): Unit           = { _afterScenarioHooks += hook; () }
  def afterScenario(hook: => UnconditionalScHook): Unit = afterScenario((_: ScenarioMetadata) => hook)
  def afterScenarioTagged(tag: String)(hook: ScenarioHook): Unit =
    afterScenario((meta: ScenarioMetadata) => if (meta.tags.contains(tag)) hook(meta) else ZIO.unit)

  private[core] def beforeScenarioHook(meta: ScenarioMetadata): URIO[R & State[S], Unit] =
    ZIO.foreachDiscard(_beforeScenarioHooks.toList)(_(meta))

  private[core] def afterScenarioHook(meta: ScenarioMetadata): URIO[R & State[S], Unit] =
    ZIO.foreachDiscard(_afterScenarioHooks.toList)(_(meta))

  // ── Before/After Step ────────────────────────────────────────────────────
  private val _beforeStepHooks = mutable.ListBuffer.empty[StepHook]
  private val _afterStepHooks  = mutable.ListBuffer.empty[StepHook]

  def beforeStep(hook: StepHook): Unit = { _beforeStepHooks += hook; () }
  def afterStep(hook: StepHook): Unit  = { _afterStepHooks += hook; () }

  private[core] def beforeStepHook(meta: StepMetadata): URIO[R & State[S], Unit] =
    ZIO.foreachDiscard(_beforeStepHooks.toList)(_(meta))

  private[core] def afterStepHook(meta: StepMetadata): URIO[R & State[S], Unit] =
    ZIO.foreachDiscard(_afterStepHooks.toList)(_(meta))
}
