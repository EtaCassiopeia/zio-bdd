package zio.bdd.core

import zio.*
import zio.bdd.gherkin.Step as GherkinStep

// Executes a sequence of Gherkin steps recursively, stopping on failure
case class ScenarioExecutor[R](
  stepExecutor: StepExecutor[R] // Delegate individual step execution to StepExecutor
) {

  // Recursively runs a list of steps, accumulating results
  def runSteps(
    gherkinSteps: List[GherkinStep],
    acc: List[StepResult] = Nil
  ): ZIO[R, Throwable, List[StepResult]] =
    gherkinSteps match {
      case Nil =>
        // Base case: no more steps, return accumulated results in reverse order
        ZIO.succeed(acc.reverse)
      case step :: rest =>
        // Execute the current step
        stepExecutor.executeStep(step).flatMap { result =>
          if (result.succeeded) {
            // If successful, continue with the remaining steps
            runSteps(rest, result :: acc)
          } else {
            // If failed, skip remaining steps and mark them as skipped
            ZIO.succeed(
              (result :: acc).reverse ++ rest.map(gherkinStep =>
                StepResult(
                  gherkinStep.toString,
                  succeeded = false,
                  error = Some("Skipped due to prior failure"),
                  output = (),
                  logs = Nil
                )
              )
            )
          }
        }
    }
}
