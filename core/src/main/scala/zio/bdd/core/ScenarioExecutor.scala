package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{Stage, State, StepInput, StepLookupError, StepRegistry, ZIOSteps}
import zio.bdd.gherkin.{Scenario, ScenarioMetadata, Step, StepType}

object ScenarioExecutor {
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
          stateRef      <- FiberRef.make(Default[S].default)
          _             <- Stage.reset // clear any leaked staged values from previous scenario
          scenarioScope <- ZIO.scope
          meta           = ScenarioMetadata.from(scenario, flagValues)
          scenarioResult <- (for {
                              // Fire beforeScenarioHook asynchronously — progress output does not
                              // need to complete before steps begin, and does not affect results.
                              _                    <- suite.beforeScenarioHook(meta).forkDaemon
                              startNanos           <- Clock.nanoTime
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
                                                    executeSteps[R, S](
                                                      scenario,
                                                      stepsWithTypes,
                                                      suite,
                                                      dryRun,
                                                      stepTimeout
                                                    )
                                                }
                              endNanos <- Clock.nanoTime
                              duration  = (endNanos - startNanos) / 1_000_000L
                              // Fire afterScenarioHook asynchronously — progress bar output does not
                              // need to complete before the next scenario starts.
                              _ <- suite.afterScenarioHook(meta).forkDaemon
                            } yield scenarioResult.copy(duration = duration))
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
        if (failed) ZIO.succeed((acc :+ SkippedStepResult(step), true))
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
        startNanos <- Clock.nanoTime
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
        endNanos <- Clock.nanoTime
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
              case None    => ZIO.fail(new Exception("And or But step without previous step"))
            }
          case other => ZIO.succeed(other)
        }
        effectiveType.map(et => (acc :+ (step, et), Some(et)))
      }
      .map(_._1)
}
