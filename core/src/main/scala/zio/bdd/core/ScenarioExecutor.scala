package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{Stage, State, StepInput, StepRegistry, ZIOSteps}
import zio.bdd.gherkin.{Scenario, ScenarioMetadata, Step, StepType}

object ScenarioExecutor {

  // Wall-clock duration measurement. Uses System.nanoTime directly rather than the ZIO
  // Clock service so durations are real even under zio-test's TestClock.
  private val nowNanos: UIO[Long] = ZIO.succeed(java.lang.System.nanoTime())

  def executeScenario[R: Tag, S: Tag: Default](
    scenario: Scenario,
    suite: ZIOSteps[R, S],
    dryRun: Boolean = false,
    flagValues: Map[String, String] = Map.empty,
    stepTimeout: Option[Duration] = None
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    if (scenario.isIgnored) {
      ZIO.succeed(ScenarioResult(scenario, Nil, setupError = None))
    } else {
      ZIO.scoped {
        for {
          stateRef <- FiberRef.make(Default[S].default)
          // Per-scenario staging is scoped to the scenario and auto-restored on scope close,
          // so values cannot leak into the next scenario even if a step is interrupted.
          _             <- Stage.ref.locallyScoped(Map.empty)
          _             <- Stage.currentStepLabel.locallyScoped("")
          scenarioScope <- ZIO.scope
          meta           = ScenarioMetadata.from(scenario, flagValues)
          scenarioResult <- (for {
                              startNanos <- nowNanos
                              // Run beforeScenario setup to completion BEFORE any step. A failing
                              // setup is recorded as the scenario's setupError instead of running steps.
                              beforeExit <- suite.beforeScenarioHook(meta).exit
                              result <- beforeExit match {
                                          case Exit.Failure(cause) =>
                                            ZIO.succeed(ScenarioResult(scenario, Nil, setupError = Some(cause)))
                                          case Exit.Success(_) =>
                                            computeEffectiveStepTypes(scenario.steps).either.flatMap {
                                              case Left(throwable) =>
                                                ZIO.succeed(
                                                  ScenarioResult(
                                                    scenario,
                                                    Nil,
                                                    setupError = Some(Cause.fail(throwable))
                                                  )
                                                )
                                              case Right(stepsWithTypes) =>
                                                executeSteps[R, S](scenario, stepsWithTypes, suite, dryRun, stepTimeout)
                                            }
                                        }
                              endNanos <- nowNanos
                              duration  = (endNanos - startNanos) / 1_000_000L
                            } yield result.copy(duration = duration))
                              // afterScenario teardown always runs, even on failure or interruption.
                              .ensuring(suite.afterScenarioHook(meta))
                              .provideSomeLayer[StepRegistry[R, S] & R](
                                State.layer(stateRef) ++ ZLayer.succeed(scenarioScope)
                              )
        } yield scenarioResult
      }
    }

  private def executeSteps[R: Tag, S: Tag: Default](
    scenario: Scenario,
    stepsWithTypes: List[(Step, StepType)],
    suite: ZIOSteps[R, S],
    dryRun: Boolean,
    stepTimeout: Option[Duration]
  ): ZIO[R & StepRegistry[R, S] & State[S] & Scope, Nothing, ScenarioResult] =
    ZIO
      .foldLeft(stepsWithTypes)((List.empty[StepResult], false)) { case ((acc, failed), (step, effectiveType)) =>
        if (failed) ZIO.succeed((acc :+ StepResult.skipped(step), true))
        else
          executeSingleStep[R, S](step, effectiveType, suite, dryRun, stepTimeout).map { result =>
            val nowFailed = !result.isPassed && !result.isPending
            (acc :+ result, nowFailed)
          }
      }
      .map { case (results, _) => ScenarioResult(scenario, results) }

  private def executeSingleStep[R: Tag, S: Tag: Default](
    step: Step,
    effectiveType: StepType,
    suite: ZIOSteps[R, S],
    dryRun: Boolean,
    stepTimeout: Option[Duration]
  ): ZIO[R & StepRegistry[R, S] & State[S] & Scope, Nothing, StepResult] =
    ZIO.logAnnotate("stepId", step.id.toString) {
      val input    = StepInput(step.pattern, step.dataTable, step.docString)
      val stepMeta = StepMetadata(step.pattern, effectiveType, step.file, step.line)
      val findStep = ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(effectiveType, input))
      for {
        startNanos <- nowNanos
        _          <- suite.beforeStepHook(stepMeta)
        _          <- Stage.currentStepLabel.set(step.pattern)
        result <- findStep.foldZIO(
                    lookupErr => ZIO.succeed(StepResult(step, Left(Cause.fail(lookupErr.toException)))),
                    effect =>
                      if (dryRun) ZIO.succeed(StepResult(step, Right(())))
                      else {
                        val timedEffect = stepTimeout match {
                          case Some(duration) =>
                            effect
                              .timeout(duration)
                              .someOrFail(new StepTimeoutException(step.pattern, duration))
                          case None =>
                            effect
                        }
                        timedEffect.foldCauseZIO(
                          cause => ZIO.succeed(StepResult(step, Left(cause))),
                          _ => ZIO.succeed(StepResult(step, Right(())))
                        )
                      }
                  )
        _        <- suite.afterStepHook(stepMeta)
        endNanos <- nowNanos
        duration  = (endNanos - startNanos) / 1_000_000L
      } yield result.copy(duration = duration)
    }

  private def computeEffectiveStepTypes(steps: List[Step]): ZIO[Any, Throwable, List[(Step, StepType)]] =
    ZIO
      .foldLeft(steps)((List.empty[(Step, StepType)], Option.empty[StepType])) { case ((acc, prevType), step) =>
        val effectiveType = step.stepType match {
          case StepType.AndStep | StepType.ButStep =>
            prevType match {
              case Some(t) => ZIO.succeed(t)
              case None =>
                ZIO.fail(
                  new StepSequencingException("'And'/'But' step has no preceding step to inherit a keyword from")
                )
            }
          case other => ZIO.succeed(other)
        }
        effectiveType.map(et => (acc :+ (step, et), Some(et)))
      }
      .map(_._1)
}
