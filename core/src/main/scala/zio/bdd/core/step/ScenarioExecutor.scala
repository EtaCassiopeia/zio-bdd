package zio.bdd.core.step

import zio.*
import zio.bdd.gherkin.{Scenario, Step}
import izumi.reflect.Tag

object ScenarioExecutor {
  def executeScenario[R: Tag, S: Tag](scenario: Scenario, initialState: => S): RIO[R with StepRegistry[R, S], Unit] =
    ZIO.scoped {
      for {
        _ <- ZIO.debug(s"Executing scenario: ${scenario.name}")
        stateRef <- FiberRef.make(initialState)
        _ <- ZIO.foreach(scenario.steps) { step =>
          val input = StepInput(step.pattern, step.dataTable)
          ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input)).flatMap { stepEffect =>
            stepEffect
          }.catchAll { error =>
            ZIO.fail(new Exception(s"Step execution failed for: ${step.pattern} - ${error.getMessage}"))
          }
        }.provideSomeLayer(State.layer(stateRef))
      } yield ()
    }
}

