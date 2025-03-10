package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.{StepType, Step as GherkinStep}

import java.time.Instant
import scala.util.matching.Regex

// Executes individual Gherkin steps by matching them to step definitions
case class StepExecutor[R](
  scenarioId: String,
  steps: ZIOSteps[R],               // Provided step definitions
  stackRef: Ref[Chunk[StepRecord]], // Stack tracking outputs of executed steps
  reporter: Reporter,               // Handles reporting of step start/end and results
  logCollector: LogCollector        // Collects logs during execution
) {

  // Main method to execute a single Gherkin step
  def executeStep(gherkinStep: GherkinStep): ZIO[R, Throwable, StepResult] = {
    val line            = gherkinStep.pattern
    val isAnd           = gherkinStep.stepType == StepType.AndStep
    val currentStepType = gherkinStep.stepType

    for {
      // Determine the expected step type for matching (e.g., "And" inherits from last non-"And")
      lastNonAndStepType <- OutputStack.findLastNonAndStepType(stackRef)
      expectedStepType    = if (isAnd) lastNonAndStepType else currentStepType
      stepDefOpt         <- findMatchingStepDef(expectedStepType, gherkinStep)
      start              <- Clock.instant
      result <- stepDefOpt match {
                  case Some(stepDef) =>
                    // If a step definition matches, execute it
                    executeMatchedStep(stepDef, gherkinStep, line, isAnd, currentStepType, start)
                  case None =>
                    // If no match, report a failure
                    executeUnmatchedStep(gherkinStep, line, currentStepType, start)
                }
      end     <- Clock.instant
      duration = Duration.fromInterval(start, end)
      finalResult =
        result.copy(duration = duration, startTime = start, file = gherkinStep.file, line = gherkinStep.line)
      // Record the step's result in the stack for use by subsequent steps
      _            <- OutputStack.push(stackRef, StepRecord(currentStepType, line, finalResult.output))
      updatedStack <- stackRef.get
      _            <- logCollector.logStdout(scenarioId, s"After $line, NewStack: $updatedStack, Duration: $duration")
    } yield finalResult
  }

  // Finds a step definition that matches the Gherkin step's type and pattern
  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  ): ZIO[Any, Nothing, Option[ZIOSteps[R]#StepDef[?, ?]]] =
    ZIO.succeed {
      steps.getSteps.find { stepDef =>
        val matchesStepType = if (gherkinStep.stepType == StepType.AndStep) {
          // "And" steps can match either the inherited type or AndStep definitions
          stepDef.stepType == expectedStepType || stepDef.stepType == StepType.AndStep
        } else {
          stepDef.stepType == expectedStepType
        }
        val patternMatches = stepDef.pattern.findFirstIn(gherkinStep.pattern).isDefined
        matchesStepType && patternMatches
      }
    }

  // Executes a matched step definition with its parameters and input
  private def executeMatchedStep(
    stepDef: ZIOSteps[R]#StepDef[?, ?],
    gherkinStep: GherkinStep,
    line: String,
    isAnd: Boolean,
    currentStepType: StepType,
    start: Instant
  ): ZIO[R, Throwable, StepResult] = {
    val pattern = stepDef.pattern
    // Cast fn to Any => ZIO[R, Throwable, Any] to match executeStepFunction's expected type
    // This is safe because I => O is compatible with Any => Any in this dynamic context
    val fn     = stepDef.fn.asInstanceOf[Any => ZIO[R, Throwable, Any]]
    val params = extractParams(pattern, gherkinStep.pattern)

    for {
      currentStack <- stackRef.get
      _            <- logCollector.logStdout(scenarioId, s"Step: $line, OutputStack: $currentStack, Params: $params")
      // Determine the input for the step (from params or previous outputs)
      input <- determineInput(params, currentStepType, isAnd)
      _     <- logCollector.logStdout(scenarioId, s"Selected Input for $line: $input")
      _     <- reporter.startStep(line)
      // Execute the step function with the prepared input
      result <- executeStepFunction(fn, line, input, isAnd, start)
      _      <- reporter.endStep(line, result)
    } yield result
  }

  // Determines the input for a step, either from parameters or the stack
  private def determineInput(
    params: List[String],
    currentStepType: StepType,
    isAnd: Boolean
  ): ZIO[Any, Nothing, Any] =
    if (params.nonEmpty) {
      // If parameters are provided, combine them into a single value or tuple
      ZIO.succeed(combine((), params))
    } else {
      // Otherwise, use the last relevant output from the stack (e.g., Given for When/And)
      stackRef.get.map { stack =>
        if (currentStepType == StepType.WhenStep || isAnd) {
          stack.find(_.stepType == StepType.GivenStep).map(_.output).getOrElse(())
        } else {
          OutputStack.flattenOutput(stack.headOption.map(_.output).getOrElse(()))
        }
      }
    }

  // Runs the step's function and constructs the result, handling success and failure
  private def executeStepFunction(
    fn: Any => ZIO[R, Throwable, Any],
    line: String,
    input: Any,
    isAnd: Boolean,
    start: Instant
  ): ZIO[R, Throwable, StepResult] =
    (for {
      _      <- logCollector.logStdout(scenarioId, s"Executing: $line with input: $input")
      output <- fn(input) // Execute the step's logic
      logs   <- logCollector.getLogs(scenarioId)
      // For "And" steps, combine with previous non-unit output if applicable
      finalOutput <- if (isAnd && OutputStack.flattenOutput(output) != ()) {
                       OutputStack.peek(stackRef).flatMap {
                         case Some(prev) if OutputStack.flattenOutput(prev.output) != () =>
                           ZIO.succeed((prev.output, output))
                         case _ => ZIO.succeed(output)
                       }
                     } else {
                       ZIO.succeed(output)
                     }
    } yield StepResult(
      line,
      succeeded = true,
      error = None,
      output = finalOutput,
      logs = logs.toStepResultLogs,
      duration = Duration.Zero,
      startTime = start
    )).catchAll { error =>
      // On failure, capture logs and report the error
      logCollector.logStderr(scenarioId, s"Step failed: $line - ${error.getMessage}") *>
        logCollector.getLogs(scenarioId).flatMap { logs =>
          logCollector.clearLogs.as {
            StepResult(
              line,
              succeeded = false,
              error = Some(error),
              output = (),
              logs = logs.toStepResultLogs,
              duration = Duration.Zero,
              startTime = start
            )
          }
        }
    }

  // Handles the case where no step definition matches the Gherkin step
  private def executeUnmatchedStep(
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  ): ZIO[Any, Nothing, StepResult] =
    for {
      _    <- logCollector.logStderr(scenarioId, s"No step definition matches: $line")
      logs <- logCollector.getLogs(scenarioId)
      result = StepResult(
                 line,
                 succeeded = false,
                 error = Some(new Exception("No step definition matches")),
                 output = (),
                 logs = logs.toStepResultLogs,
                 duration = Duration.Zero,
                 startTime = start,
                 file = gherkinStep.file,
                 line = gherkinStep.line
               )
      _ <- reporter.startStep(line)
      _ <- reporter.endStep(line, result)
    } yield result

  // Combines previous output with step parameters into a single input value
  private def combine(prev: Any, params: List[String]): Any = {
    val flattenedPrev = OutputStack.flattenOutput(prev)
    params match {
      case Nil => flattenedPrev
      case head :: Nil =>
        flattenedPrev match {
          case ()     => parseParam(head)
          case single => (single, parseParam(head))
        }
      case many =>
        flattenedPrev match {
          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
        }
    }
  }

  // Extracts parameters from a step's pattern match
  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = param.trim
}
