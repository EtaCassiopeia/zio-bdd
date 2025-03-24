package zio.bdd.core

import zio.bdd.gherkin.StepType
import zio.*

import java.time.Instant

sealed trait TestError {
  def message: String
  def cause: Option[Throwable]
  def trace: Option[Trace]
}

object TestError {
  case class GenericError(message: String, cause: Option[Throwable], trace: Option[Trace])             extends TestError
  case class TypeMismatch(message: String, input: Any, cause: Option[Throwable], trace: Option[Trace]) extends TestError
  case class MissingStep(step: String, cause: Option[Throwable], trace: Option[Trace]) extends TestError {
    def message: String = s"No step definition matches: $step"
  }

  def fromThrowable(t: Throwable)(implicit trace: Trace): TestError =
    GenericError(t.getMessage, Some(t), Some(trace))
}

// Represents the result of executing a single step
case class StepResult(
  step: String,                                    // The step's text (e.g., "Given I have 5 items")
  succeeded: Boolean,                              // Whether the step succeeded
  error: Option[TestError],                        // Error message if the step failed
  output: Any,                                     // The output produced by the step
  logs: List[(String, Instant, InternalLogLevel)], // Logs collected during execution
  duration: Duration,                              // Execution time
  startTime: Instant,                              // Start timestamp
  stepId: Option[String] = None,                   // Step ID
  scenarioId: Option[String] = None,               // Scenario ID
  featureId: Option[String] = None,                // Feature ID
  file: Option[String] = None,                     // Source file path
  line: Option[Int] = None                         // Line number
)

// Represents a recorded step in the output stack
case class StepRecord(
  stepType: StepType,     // The type of step (Given, When, Then, And)
  stepText: String,       // The step's text
  output: Any,            // The output produced by the step
  scenarioId: String,     //  Scenario ID used to look up the steps of a scenario
  outputTag: LightTypeTag // To store the output type tag
)
