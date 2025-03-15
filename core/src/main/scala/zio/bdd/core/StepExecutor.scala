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
    val stepId          = gherkinStep.id
    val isAnd           = gherkinStep.stepType == StepType.AndStep
    val currentStepType = gherkinStep.stepType
    ZIO.logAnnotate("stepId", stepId) {
      for {
        // Determine the expected step type for matching (e.g., "And" inherits from last non-"And")
        lastNonAndStepType <- OutputStack.findLastNonAndStepType(stackRef)
        expectedStepType    = if (isAnd) lastNonAndStepType else currentStepType
        stepDefOpt         <- findMatchingStepDef(expectedStepType, gherkinStep)
        start              <- Clock.instant
        _                  <- reporter.startStep(line)
        result <- stepDefOpt match {
                    case Some(stepDef) =>
                      // If a step definition matches, execute it
                      executeMatchedStep(stepDef, gherkinStep, line, currentStepType, start)
                    case None =>
                      // If no match, report a failure
                      executeUnmatchedStep(gherkinStep, line, currentStepType, start)
                  }
        end     <- Clock.instant
        duration = Duration.fromInterval(start, end)
        finalResult =
          result.copy(duration = duration, startTime = start, file = gherkinStep.file, line = gherkinStep.line)
        _ <- reporter.endStep(line, finalResult)
        // Record the step's result in the stack for use by subsequent steps
        _            <- OutputStack.push(stackRef, StepRecord(currentStepType, line, finalResult.output))
        updatedStack <- stackRef.get
        _            <- logCollector.logStdout(scenarioId, stepId, s"After $line, NewStack: $updatedStack, Duration: $duration")
      } yield finalResult
    }
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
        val patternRegex   = convertToRegex(stepDef.pattern)
        val patternMatches = patternRegex.findFirstIn(gherkinStep.pattern).isDefined
        matchesStepType && patternMatches
      }
    }

  // Executes a matched step definition with its parameters and input
  private def executeMatchedStep(
    stepDef: ZIOSteps[R]#StepDef[?, ?],
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  ): ZIO[R, Throwable, StepResult] = {
    val stepId  = gherkinStep.id
    val pattern = convertToRegex(stepDef.pattern)
    // Cast fn to Any => ZIO[R, Throwable, Any] to match executeStepFunction's expected type
    // This is safe because I => O is compatible with Any => Any in this dynamic context
    val fn     = stepDef.fn.asInstanceOf[Any => ZIO[R, Throwable, Any]]
    val params = extractParams(pattern, gherkinStep.pattern, stepDef.pattern)

    for {
      currentStack <- stackRef.get
      _            <- logCollector.logStdout(scenarioId, stepId, s"Step: $line, OutputStack: $currentStack, Params: $params")
      // Determine the input for the step (from params or previous outputs)
      input <- determineInput(params, currentStepType)
      _     <- logCollector.logStdout(scenarioId, stepId, s"Selected Input for $line: ${input.toString}")
      // Execute the step function with the prepared input
      result <- executeStepFunction(fn, line, input, start, stepId)
    } yield result
  }

  // Determines the input for a step, either from parameters or the stack
  private def determineInput(
    params: List[Any],
    currentStepType: StepType
  ): ZIO[Any, Nothing, Any] =
    stackRef.get.map { stack =>
      val priorOutput = if (currentStepType == StepType.GivenStep) {
        () // Given starts with no prior input
      } else {
        stack.headOption.map(_.output).getOrElse(()) // Use previous step's output as-is
      }
      if (params.nonEmpty) {
        // Combine prior output with params without flattening the prior output
        priorOutput match {
          case ()    => if (params.length == 1) params.head else Tuple.fromArray(params.toArray)
          case prior => Tuple.fromArray((prior :: params).toArray)
        }
      } else {
        priorOutput
      }
    }

  // Runs the step's function and constructs the result, handling success and failure
  private def executeStepFunction(
    fn: Any => ZIO[R, Throwable, Any],
    line: String,
    input: Any,
    start: Instant,
    stepId: String
  ): ZIO[R, Throwable, StepResult] =
    (for {
      _      <- logCollector.logStdout(scenarioId, stepId, s"Executing: $line with input: ${input.toString}")
      output <- fn(input)
      logs   <- logCollector.getLogs(scenarioId, stepId)
    } yield StepResult(
      line,
      succeeded = true,
      error = None,
      output = output,
      logs = logs.toStepResultLogs,
      duration = Duration.Zero,
      startTime = start
    )).catchAll { error =>
      logCollector.getLogs(scenarioId, stepId).flatMap { logs =>
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
  ): ZIO[Any, Nothing, StepResult] = {
    val stepId = gherkinStep.id
    for {
      _    <- logCollector.logStderr(scenarioId, stepId, s"No step definition matches: $line")
      logs <- logCollector.getLogs(scenarioId, stepId)
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
    } yield result
  }

  // Extracts parameters from a step's pattern match
  private def extractParams(pattern: Regex, line: String, patternString: String): List[Any] = {
    val trimmedLine  = line.trim
    val matchResult  = pattern.findFirstMatchIn(trimmedLine)
    val subgroups    = matchResult.map(_.subgroups).getOrElse(Nil)
    val placeholders = patternString.split("\\s+").filter(_.startsWith("{")).toList

    if (placeholders.isEmpty) {
      List()
    } else {
      if (subgroups.length != placeholders.length || subgroups.isEmpty) {
        List()
      } else {
        subgroups.zip(placeholders).map { case (param, placeholder) =>
          parseParam(param, placeholder)
        }
      }
    }
  }

  private def parseParam(param: String, placeholder: String): Any = {
    val placeholderType = placeholder.stripPrefix("{").stripSuffix("}").split(":").last.toLowerCase
    placeholderType match {
      case "int"     => param.toInt
      case "float"   => param.toFloat
      case "double"  => param.toDouble
      case "boolean" => param.toBoolean
      case "string"  => param.trim
      case _         => param.trim
    }
  }

  private def convertToRegex(pattern: String): Regex =
    if (pattern.contains("{") || pattern.contains("}")) {
      pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(\\d+)")
        .replace("{float}", "(\\d+\\.\\d+)")
        .replace("{double}", "([-+]?\\d*\\.\\d+([eE][-+]?\\d+)?)") // Handles scientific notation
        .replace("{boolean}", "(true|false)")
        .replaceAll("\\{[^:]+:[^}]+\\}", "(.+)")
        .r
    } else {
      pattern.r
    }
}
