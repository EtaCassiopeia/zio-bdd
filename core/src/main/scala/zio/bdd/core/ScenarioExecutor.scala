package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{State, StepInput, StepRegistry}
import zio.bdd.gherkin.{Scenario, Step}

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag](scenario: Scenario, initialState: => S): RIO[R with StepRegistry[R, S], Unit] =
    ZIO.scoped {
      for {
        _        <- ZIO.debug(s"Executing scenario: ${scenario.name}")
        stateRef <- FiberRef.make(initialState)
        _ <- ZIO
               .foreach(scenario.steps) { step =>
                 println("Executing step: " + step.pattern)
                 val input = StepInput(step.pattern, step.dataTable)
                 ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input)).catchAll { error =>
                   ZIO.fail(
                     new Exception(
                       s"Step execution failed for: ${step.stepType}: ${step.pattern} - ${error.getMessage}"
                     )
                   )
                 }
               }
               .provideSomeLayer(State.layer(stateRef))
      } yield ()
    }
}
