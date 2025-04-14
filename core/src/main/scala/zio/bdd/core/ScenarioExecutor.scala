package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{State, StepInput, StepRegistry}
import zio.bdd.gherkin.{Scenario, Step}

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag](
    scenario: Scenario,
    initialState: => S
  ): ZIO[R with StepRegistry[R, S], Nothing, ScenarioResult] =
    ZIO.scoped {
      for {
        _        <- ZIO.debug(s"Executing scenario: ${scenario.name}")
        stateRef <- FiberRef.make(initialState)
        stepResults <- ZIO
                         .foreach(scenario.steps) { step =>
                           ZIO.logAnnotate("stepId", step.id.toString) {
                             val input = StepInput(step.pattern, step.dataTable)
                             val stepEffect =
                               ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input)).catchAll { error =>
                                 ZIO.fail(
                                   new Exception(
                                     s"Step execution failed for: ${step.stepType}: ${step.pattern} - ${error.getMessage}"
                                   )
                                 )
                               }
                             stepEffect.foldCauseZIO(
                               cause => ZIO.succeed(StepResult(step, Left(cause))),
                               _ => ZIO.succeed(StepResult(step, Right(())))
                             )
                           }
                         }
                         .provideSomeLayer(State.layer(stateRef))
        scenarioResult = ScenarioResult(scenario, stepResults)
      } yield scenarioResult
    }
}
