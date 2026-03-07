package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{State, StepInput, StepRegistry}
import zio.bdd.gherkin.{Scenario, Step, StepType}

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag: Default](
    scenario: Scenario,
    hooks: Hooks[R, S],
    dryRun: Boolean = false,
    stepTimeout: Option[Duration] = None
  ): ZIO[R with StepRegistry[R, S], Nothing, ScenarioResult] =
    if (scenario.isIgnored) {
      ZIO.succeed(ScenarioResult(scenario, Nil, setupError = None))
    } else {
      ZIO.scoped {
        for {
          stateRef <- FiberRef.make(Default[S].default)
          scenarioResult <- (for {
                              _                    <- hooks.beforeScenarioHook
                              stepsWithTypesResult <- computeEffectiveStepTypes(scenario.steps).either
                              scenarioResult <- stepsWithTypesResult match {
                                                  case Left(throwable) =>
                                                    ZIO.succeed(
                                                      ScenarioResult(
                                                        scenario,
                                                        Nil,
                                                        setupError = Some(Cause.fail(throwable))
                                                      )
                                                    )
                                                  case Right(stepsWithTypes) =>
                                                    executeSteps[R, S](scenario, stepsWithTypes, dryRun, stepTimeout)
                                                }
                              _ <- hooks.afterScenarioHook
                            } yield scenarioResult).provideSomeLayer[StepRegistry[R, S] with R](State.layer(stateRef))
        } yield scenarioResult
      }
    }

  // Sequential step execution with short-circuit: once a step Fails or TimesOut, the
  // remaining steps are recorded as Skipped (not executed). Pending does NOT short-circuit.
  private def executeSteps[R: Tag, S: Tag: Default](
    scenario: Scenario,
    stepsWithTypes: List[(Step, StepType)],
    dryRun: Boolean,
    stepTimeout: Option[Duration]
  ): ZIO[R with StepRegistry[R, S] with State[S], Nothing, ScenarioResult] =
    ZIO
      .foldLeft(stepsWithTypes)((List.empty[StepResult], false)) { case ((acc, failed), (step, effectiveType)) =>
        if (failed) ZIO.succeed((acc :+ SkippedStepResult(step), true))
        else
          executeSingleStep[R, S](step, effectiveType, dryRun, stepTimeout).map { result =>
            val nowFailed = !result.isPassed && !result.isPending
            (acc :+ result, nowFailed)
          }
      }
      .map { case (results, _) => ScenarioResult(scenario, results) }

  private def executeSingleStep[R: Tag, S: Tag: Default](
    step: Step,
    effectiveType: StepType,
    dryRun: Boolean,
    stepTimeout: Option[Duration]
  ): ZIO[R with StepRegistry[R, S] with State[S], Nothing, StepResult] =
    ZIO.logAnnotate("stepId", step.id.toString) {
      val input    = StepInput(step.pattern, step.dataTable)
      val findStep = ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(effectiveType, input))
      findStep.foldCauseZIO(
        cause => ZIO.succeed(StepResult(step, Left(cause))),
        effect =>
          if (dryRun) ZIO.succeed(StepResult(step, Right(())))
          else {
            val timedEffect = stepTimeout match {
              case Some(duration) =>
                effect.timeout(duration).someOrFail(new StepTimeoutException(step.pattern, duration))
              case None => effect
            }
            timedEffect.foldCauseZIO(
              cause => ZIO.succeed(StepResult(step, Left(cause))),
              _ => ZIO.succeed(StepResult(step, Right(())))
            )
          }
      )
    }

  private def computeEffectiveStepTypes(steps: List[Step]): ZIO[Any, Throwable, List[(Step, StepType)]] =
    ZIO
      .foldLeft(steps)((List.empty[(Step, StepType)], Option.empty[StepType])) { case ((acc, prevType), step) =>
        val effectiveType = step.stepType match {
          case StepType.AndStep | StepType.ButStep =>
            prevType match {
              case Some(t) => ZIO.succeed(t)
              case None    => ZIO.fail(new Exception("And or But step without previous step"))
            }
          case other => ZIO.succeed(other)
        }
        effectiveType.map(et => (acc :+ (step, et), Some(et)))
      }
      .map(_._1)
}
