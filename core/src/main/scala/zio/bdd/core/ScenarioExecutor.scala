package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{State, StepInput, StepRegistry}
import zio.bdd.gherkin.{Scenario, Step, StepType}

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag](
    scenario: Scenario,
    initialState: => S,
    hooks: Hooks[R, S]
  ): ZIO[R with StepRegistry[R, S], Nothing, ScenarioResult] =
    if (scenario.isIgnored) {
      // Skip execution for ignored scenarios
      ZIO.succeed(ScenarioResult(scenario, Nil, setupError = None))
    } else {
      ZIO.scoped {
        for {
          stateRef <- FiberRef.make(initialState)
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
                                                    ZIO
                                                      .foreach(stepsWithTypes) { case (step, effectiveType) =>
                                                        ZIO.logAnnotate("stepId", step.id.toString) {
                                                          val input = StepInput(step.pattern, step.dataTable)
                                                          val findStepEffect =
                                                            ZIO.serviceWithZIO[StepRegistry[R, S]](
                                                              _.findStep(effectiveType, input)
                                                            )
                                                          findStepEffect.foldCauseZIO(
                                                            cause => ZIO.succeed(StepResult(step, Left(cause))),
                                                            effect =>
                                                              effect.foldCauseZIO(
                                                                cause => ZIO.succeed(StepResult(step, Left(cause))),
                                                                _ => ZIO.succeed(StepResult(step, Right(())))
                                                              )
                                                          )
                                                        }
                                                      }
                                                      .map(stepResults => ScenarioResult(scenario, stepResults))
                                                }
                              _ <- hooks.afterScenarioHook
                            } yield scenarioResult).provideSomeLayer[StepRegistry[R, S] with R](State.layer(stateRef))
        } yield scenarioResult
      }
    }

  private def computeEffectiveStepTypes(steps: List[Step]): ZIO[Any, Throwable, List[(Step, StepType)]] =
    ZIO
      .foldLeft(steps)((List.empty[(Step, StepType)], Option.empty[StepType])) { case ((acc, prevType), step) =>
        val effectiveType = step.stepType match {
          case StepType.AndStep =>
            prevType match {
              case Some(t) => ZIO.succeed(t)
              case None    => ZIO.fail(new Exception("And step without previous step"))
            }
          case other => ZIO.succeed(other)
        }
        effectiveType.map { et =>
          (acc :+ (step, et), Some(et))
        }
      }
      .map(_._1)
}
