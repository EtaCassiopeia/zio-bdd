package zio.bdd.core

import zio.*
import zio.bdd.core.step.{State, ZIOSteps}

trait Hooks[R, S] { self: ZIOSteps[R, S] =>

  // TODO: Consider using RIO
  type FeatureHook  = URIO[R, Unit]
  type ScenarioHook = URIO[R with State[S], Unit]

  private var _beforeFeatureHook: Option[FeatureHook]   = None
  private var _afterFeatureHook: Option[FeatureHook]    = None
  private var _beforeScenarioHook: Option[ScenarioHook] = None
  private var _afterScenarioHook: Option[ScenarioHook]  = None

  def beforeFeature(hook: => FeatureHook): Unit = {
    _beforeFeatureHook = Some(hook)
    ()
  }

  def afterFeature(hook: => FeatureHook): Unit = {
    _afterFeatureHook = Some(hook)
    ()
  }

  def beforeScenario(hook: => ScenarioHook): Unit = {
    _beforeScenarioHook = Some(hook)
    ()
  }

  def afterScenario(hook: => ScenarioHook): Unit = {
    _afterScenarioHook = Some(hook)
    ()
  }

  private[core] def beforeFeatureHook: FeatureHook   = _beforeFeatureHook.getOrElse(ZIO.unit)
  private[core] def afterFeatureHook: FeatureHook    = _afterFeatureHook.getOrElse(ZIO.unit)
  private[core] def beforeScenarioHook: ScenarioHook = _beforeScenarioHook.getOrElse(ZIO.unit)
  private[core] def afterScenarioHook: ScenarioHook  = _afterScenarioHook.getOrElse(ZIO.unit)

}
