package zio.bdd.core

import zio.bdd.gherkin.StepType
import java.time.Instant

// Represents the result of executing a single step
case class StepResult(
  step: String,                 // The step's text (e.g., "Given I have 5 items")
  succeeded: Boolean,           // Whether the step succeeded
  error: Option[String],        // Error message if the step failed
  output: Any,                  // The output produced by the step
  logs: List[(String, Instant)] // Logs collected during execution
)

// Represents a recorded step in the output stack
case class StepRecord(
  stepType: StepType, // The type of step (Given, When, Then, And)
  line: String,       // The step's text
  output: Any         // The output produced by the step
)
