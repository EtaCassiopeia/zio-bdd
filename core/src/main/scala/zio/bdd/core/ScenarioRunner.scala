package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep}

import java.time.Instant

object ScenarioRunner {
  def run[R](
    scenarioId: String,
    steps: ZIOSteps[R],
    gherkinSteps: List[GherkinStep],
    metadata: ScenarioMetadata = ScenarioMetadata()
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    for {
      reporter     <- ZIO.service[Reporter]
      logCollector <- ZIO.service[LogCollector]
      stackRef     <- OutputStack.make
      _            <- reporter.startScenario(scenarioId)
      _            <- steps.beforeScenario(scenarioId).orDie // Convert Throwable to defect
      results <- if (metadata.isIgnored) {
                   reporter.reportIgnoredScenario(scenarioId).as(Nil)
                 } else {
                   val stepExecutor     = StepExecutor(scenarioId, steps, stackRef, reporter, logCollector)
                   val scenarioExecutor = ScenarioExecutor(stepExecutor)
                   scenarioExecutor.runSteps(gherkinSteps)
                 }
      _ <- steps.afterScenario(scenarioId).orDie // Convert Throwable to defect
      _ <- reporter.endScenario(scenarioId, results)
    } yield results

  def runScenarios[R](
    steps: ZIOSteps[R],
    feature: Feature,
    parallelism: Int
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[List[StepResult]]] =
    for {
      reporter <- ZIO.service[Reporter]
      _        <- steps.beforeFeature.orDie // Convert Throwable to defect
      _        <- reporter.startFeature(feature.name)
      scenariosWithMetadata <-
        ParameterizedScenarioBuilder.buildScenarios(feature).mapError(e => new RuntimeException(e.toString)).orDie
      ignoredCountRef <- Ref.make(0)
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (scenarioName, gherkinSteps, metadata) =>
                     val scenarioId = s"${feature.name}-$scenarioName".hashCode.toString
                     if (metadata.isIgnored) {
                       ignoredCountRef.update(_ + 1) *>
                         run(scenarioName, steps, gherkinSteps, metadata)
                     } else {
                       ZIO
                         .foreach(1 to metadata.repeatCount) { iteration =>
                           ZIO.logAnnotate("scenarioId", scenarioId) {
                             run(scenarioName, steps, gherkinSteps, metadata).tapDefect { cause =>
                               ZIO.logError(s"Scenario $scenarioName failed: ${cause.prettyPrint}")
                             }
                             // Removed .retryN since error type is Nothing; retries handled in step execution if needed
                           }
                         }
                         .map(_.flatten.toList)
                     }
                   }
                   .withParallelism(parallelism)
                   .map(_.toList)
      ignoredCount <- ignoredCountRef.get
      _            <- reporter.endFeature(feature.name, results, ignoredCount)
      _            <- steps.afterFeature.orDie // Convert Throwable to defect
    } yield results
}

case class ScenarioExecutor[R](stepExecutor: StepExecutor[R]) {
  def runSteps(
    steps: List[GherkinStep]
  )(implicit trace: Trace): ZIO[R & LogCollector & Reporter, Nothing, List[StepResult]] =
    ZIO.foldLeft(steps)(List.empty[StepResult]) { (results, step) =>
      results.lastOption match {
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
                file = step.file,
                line = step.line
              )
            }
        case _ =>
          stepExecutor.executeStep(step).map(results :+ _)
      }
    }
}
