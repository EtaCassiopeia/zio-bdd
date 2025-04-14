package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{State, StepInput, StepRegistry}
import zio.bdd.gherkin.{Scenario, Step, StepType}

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag](
    scenario: Scenario,
    initialState: => S
  ): ZIO[R with StepRegistry[R, S], Nothing, ScenarioResult] =
    ZIO.scoped {
      for {
        _                    <- ZIO.debug(s"Executing scenario: ${scenario.name}")
        stateRef             <- FiberRef.make(initialState)
        stepsWithTypesResult <- computeEffectiveStepTypes(scenario.steps).either
        scenarioResult <- stepsWithTypesResult match {
                            case Left(throwable) =>
                              ZIO.succeed(ScenarioResult(scenario, Nil, setupError = Some(Cause.fail(throwable))))
                            case Right(stepsWithTypes) =>
                              for {
                                stepResults <- ZIO
                                                 .foreach(stepsWithTypes) { case (step, effectiveType) =>
                                                   ZIO.logAnnotate("stepId", step.id.toString) {
                                                     val input = StepInput(step.pattern, step.dataTable)
                                                     val findStepEffect = ZIO.serviceWithZIO[StepRegistry[R, S]](
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
                                                 .provideSomeLayer[StepRegistry[R, S] with R](State.layer(stateRef))
                              } yield ScenarioResult(scenario, stepResults)
                          }
      } yield scenarioResult
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
