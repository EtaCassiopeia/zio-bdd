package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep}

object ScenarioRunner {

  // Runs a single scenario (list of steps) with the given step definitions
  def run[R](
    scenarioId: String,
    steps: ZIOSteps[R],
    gherkinSteps: List[GherkinStep],
    metadata: ScenarioMetadata = ScenarioMetadata()
  ): ZIO[R & LogCollector & Reporter, Throwable, List[StepResult]] =
    for {
      reporter     <- ZIO.service[Reporter]
      logCollector <- ZIO.service[LogCollector]
      stackRef     <- OutputStack.make // Initialize the output stack for tracking step results
      scenarioText  = gherkinSteps.mkString("\n")
      _            <- reporter.startScenario(scenarioId)
      results <- if (metadata.isIgnored) {
                   // If scenario is ignored, report it and return an empty result list
                   reporter.reportIgnoredScenario(scenarioText).as(Nil)
                 } else {
                   // Set up executors with dependencies and run the scenario
                   val stepExecutor     = StepExecutor(scenarioId, steps, stackRef, reporter, logCollector)
                   val scenarioExecutor = ScenarioExecutor(stepExecutor)
                   scenarioExecutor.runSteps(gherkinSteps)
                 }
      _ <- reporter.endScenario(scenarioId, results)
    } yield results

  // Runs all scenarios in a feature, handling parallelism and parameterization
  def runScenarios[R](
    steps: ZIOSteps[R],
    feature: Feature,
    parallelism: Int
  ): ZIO[R & LogCollector & Reporter, Throwable, List[List[StepResult]]] =
    for {
      reporter <- ZIO.service[Reporter]
      _        <- reporter.startFeature(feature.name)
      // Build parameterized scenarios from the feature (e.g., expanding Examples)
      scenariosWithMetadata <-
        ParameterizedScenarioBuilder
          .buildScenarios(feature)
          .mapError(e => new RuntimeException(e.toString))
      // Track ignored scenarios
      ignoredCountRef <- Ref.make(0)
      // Execute scenarios in parallel, respecting repeatCount
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (scenarioName, gherkinSteps, metadata) =>
                     val scenarioId = gherkinSteps.mkString("\n").hashCode.toString
                     if (metadata.isIgnored) {
                       // Increment ignored count and run (which will report and return empty results)
                       ignoredCountRef.update(_ + 1) *>
                         run(scenarioName, steps, gherkinSteps, metadata)
                     } else {
                       ZIO
                         .foreach(1 to metadata.repeatCount) { iteration =>
                           ZIO.logAnnotate("scenarioId", s"${scenarioId}_iteration_$iteration") {
                             // Retry the scenario up to retryCount times if it fails
                             run(scenarioName, steps, gherkinSteps, metadata)
                               .retryN(metadata.retryCount) // Retry if any step fails
                               .catchAll { error =>
                                 // After retries exhausted, run one last time to get the final failure result
                                 run(scenarioName, steps, gherkinSteps, metadata)
                               }
                           }
                         }
                         .map(_.flatten.toList)
                     }
                   }
                   .withParallelism(parallelism)
                   .map(_.toList)
      ignoredCount <- ignoredCountRef.get
      _            <- reporter.endFeature(feature.name, results, ignoredCount)
    } yield results
}
