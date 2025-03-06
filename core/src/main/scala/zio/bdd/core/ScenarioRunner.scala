package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep}
import scala.util.matching.Regex

object ScenarioRunner {

  // Runs a single scenario (list of steps) with the given step definitions
  def run[R](
    steps: ZIOSteps[R],
    gherkinSteps: List[GherkinStep],
    metadata: ScenarioMetadata = ScenarioMetadata()
  ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] =
    for {
      reporter     <- ZIO.service[Reporter]
      logCollector <- ZIO.service[LogCollector]
      stackRef     <- OutputStack.make // Initialize the output stack for tracking step results
      scenarioText  = gherkinSteps.mkString("\n")
      _            <- reporter.startScenario(scenarioText)
      // Set up executors with dependencies
      stepExecutor     = StepExecutor(steps, stackRef, reporter, logCollector)
      scenarioExecutor = ScenarioExecutor(stepExecutor)
      results         <- scenarioExecutor.runSteps(gherkinSteps) // Delegate execution
      _               <- reporter.endScenario(scenarioText, results)
    } yield results

  // Runs all scenarios in a feature, handling parallelism and parameterization
  def runScenarios[R](
    steps: ZIOSteps[R],
    feature: Feature,
    parallelism: Int
  ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
    for {
      reporter <- ZIO.service[Reporter]
      _        <- reporter.startFeature(feature.name)
      // Build parameterized scenarios from the feature (e.g., expanding Examples)
      scenariosWithMetadata <-
        ParameterizedScenarioBuilder
          .buildScenarios(feature)
          .mapError(e =>
            new RuntimeException(e.toString)
          ) // Map BuildError to Throwable until we have a better error handling for the rest of the code
      // Execute scenarios in parallel, respecting repeatCount
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (gherkinSteps, metadata) =>
                     val scenarioId = gherkinSteps.mkString("\n").hashCode.toString
                     ZIO
                       .foreach(1 to metadata.repeatCount) { iteration =>
                         // Annotate logs with scenario ID and iteration for traceability
                         ZIO.logAnnotate("scenarioId", s"${scenarioId}_iteration_$iteration") {
                           run(steps, gherkinSteps, metadata)
                         }
                       }
                       .map(_.flatten.toList)
                   }
                   .withParallelism(parallelism)
                   .map(_.toList)
      _ <- reporter.endFeature(feature.name, results)
    } yield results

  // Combines previous output with step parameters into a single input value
  def combine(prev: Any, params: List[String]): Any = {
    val flattenedPrev = OutputStack.flattenOutput(prev)
    params match {
      case Nil => flattenedPrev
      case head :: Nil =>
        flattenedPrev match {
          case ()     => parseParam(head)
          case single => (single, parseParam(head)) // Pair previous output with single param
        }
      case many =>
        flattenedPrev match {
          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray) // Prepend previous output
        }
    }
  }

  // Extracts parameters from a step's pattern match
  def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = param.trim
}
