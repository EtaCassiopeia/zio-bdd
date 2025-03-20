package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep}

import java.time.Instant

// Runs a single scenario (list of steps) with the given step definitions
object ScenarioRunner {
  def run[R](
    featureId: String,
    scenarioId: String,
    steps: ZIOSteps[R],
    gherkinSteps: List[GherkinStep],
    metadata: ScenarioMetadata = ScenarioMetadata()
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    for {
      reporter     <- ZIO.service[Reporter]
      logCollector <- ZIO.service[LogCollector]
      stackRef     <- OutputStack.make // Initialize the output stack for tracking step results
      _            <- reporter.startScenario(scenarioId)
      // TODO: Should we catch and handle hook errors, or let them fail as defects?
      _ <- steps.beforeScenario(scenarioId).orDie // Run beforeScenario hook, convert Throwable to defect
      results <- if (metadata.isIgnored) {
                   // If scenario is ignored, report it and return an empty result list
                   reporter.reportIgnoredScenario(scenarioId).as(Nil)
                 } else {
                   // Set up executors with dependencies and run the scenario
                   val stepExecutor     = StepExecutor(scenarioId, steps, stackRef, reporter, logCollector)
                   val scenarioExecutor = ScenarioExecutor(stepExecutor)
                   scenarioExecutor.runSteps(featureId, scenarioId, gherkinSteps)
                 }
      _ <- steps.afterScenario(scenarioId).orDie // Run afterScenario hook
      _ <- reporter.endScenario(scenarioId, results)
    } yield results

  // Runs all scenarios in a feature, handling parallelism and parameterization
  def runScenarios[R](
    steps: ZIOSteps[R],
    feature: Feature,
    parallelism: Int
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[List[StepResult]]] =
    for {
      reporter <- ZIO.service[Reporter]
      _        <- steps.beforeFeature.orDie
      _        <- reporter.startFeature(feature.name)
      // Build parameterized scenarios from the feature (e.g., expanding Examples)
      scenariosWithMetadata <-
        ParameterizedScenarioBuilder.buildScenarios(feature).mapError(e => new RuntimeException(e.toString)).orDie
      // Track ignored scenarios
      ignoredCountRef <- Ref.make(0)
      // Execute scenarios in parallel, respecting repeatCount
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (scenarioName, gherkinSteps, metadata) =>
                     // TODO: Add iteration number to scenarioId?
                     val scenarioId = s"${feature.name}-$scenarioName".hashCode.toString
                     ZIO.logAnnotate("scenarioId", scenarioId) {
                       if (metadata.isIgnored) {
                         // Increment ignored count and run (which will report and return empty results)
                         ignoredCountRef.update(_ + 1) *>
                           run(feature.name, scenarioId, steps, gherkinSteps, metadata)
                       } else {
                         ZIO
                           .foreach(1 to metadata.repeatCount) { iteration =>
                             // Removed .retryN since error type is Nothing; retries handled in step execution if needed
                             run(feature.name, scenarioId, steps, gherkinSteps, metadata).tapDefect { cause =>
                               ZIO.logError(s"Scenario $scenarioName failed: ${cause.prettyPrint}")
                             }
                           }
                           .map(_.flatten.toList)
                       }
                     }
                   }
                   .withParallelism(parallelism)
                   .map(_.toList)
      ignoredCount <- ignoredCountRef.get
      _            <- reporter.endFeature(feature.name, results, ignoredCount)
      _            <- steps.afterFeature.orDie // Run afterFeature hook
    } yield results
}

// Executes a sequence of Gherkin steps recursively, stopping on failure
case class ScenarioExecutor[R](stepExecutor: StepExecutor[R]) {

  // Recursively runs a list of steps, accumulating results
  def runSteps(
    featureId: String,
    scenarioId: String,
    steps: List[GherkinStep]
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    ZIO.foldLeft(steps)(List.empty[StepResult]) { (results, step) =>
      ZIO.logAnnotate("stepId", step.id) {
        results.lastOption match {
          // If previous step failed, skip remaining steps and mark them as skipped
          case Some(last) if !last.succeeded =>
            stepExecutor.logCollector
              .log(
                stepExecutor.scenarioId,
                step.id,
                s"Skipping step due to previous failure: ${step.pattern}",
                InternalLogLevel.Warning
              )
              .as {
                results :+ StepResult(
                  step.pattern,
                  succeeded = false,
                  error = Some(TestError.GenericError("Skipped due to prior failure", None, Some(trace))),
                  output = (),
                  logs = Nil,
                  duration = Duration.Zero,
                  startTime = Instant.now(),
                  stepId = Some(step.id),
                  scenarioId = Some(scenarioId),
                  featureId = Some(featureId),
                  file = step.file,
                  line = step.line
                )
              }
          case _ =>
            stepExecutor.executeStep(featureId, scenarioId, step).map(results :+ _)
        }
      }
    }
}
