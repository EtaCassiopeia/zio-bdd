package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.{StepType, Step as GherkinStep}
import zio.bdd.core.report.Reporter
import java.time.Instant

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
    val line   = gherkinStep.pattern
    val stepId = gherkinStep.id
    ZIO.logAnnotate("stepId", stepId) {
      for {
        // Determine the expected step type for matching (e.g., "And" inherits from last non-"And")
        lastNonAndStepType <- OutputStack.findLastNonAndStepType(stackRef)
        expectedStepType    = if (gherkinStep.stepType == StepType.AndStep) lastNonAndStepType else gherkinStep.stepType
        stepDefOpt         <- findMatchingStepDef(expectedStepType, gherkinStep)
        start              <- Clock.instant
        _                  <- reporter.startStep(line)
        result <- stepDefOpt match {
                    case Some(stepDef) =>
                      // If a step definition matches, execute it
                      executeMatchedStep(stepDef, gherkinStep, line, gherkinStep.stepType, start)(
                        stepDef.iTag,
                        stepDef.oTag
                      )
                    case None =>
                      // If no match, report a failure
                      executeUnmatchedStep(gherkinStep, line, gherkinStep.stepType, start)
                  }
        end     <- Clock.instant
        duration = Duration.fromInterval(start, end)
        finalResult =
          result.copy(duration = duration, startTime = start, file = gherkinStep.file, line = gherkinStep.line)
        _ <- reporter.endStep(line, finalResult)
        // Record the step's result in the stack for use by subsequent steps
        _            <- OutputStack.push(stackRef, StepRecord(gherkinStep.stepType, line, finalResult.output))
        updatedStack <- stackRef.get
        _            <- logCollector.logStdout(scenarioId, stepId, s"After $line, NewStack: $updatedStack, Duration: $duration")
      } yield finalResult
    }
  }

  // Finds a step definition that matches the Gherkin step's type and pattern
  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  ): ZIO[Any, Nothing, Option[ZIOSteps[R]#StepDef[? <: Matchable, ?]]] =
    ZIO.succeed {
      steps.getSteps.find { stepDef =>
        val matchesStepType = if (gherkinStep.stepType == StepType.AndStep) {
          // "And" steps can match either the inherited type or AndStep definitions
          stepDef.stepType == expectedStepType || stepDef.stepType == StepType.AndStep
        } else {
          stepDef.stepType == expectedStepType
        }
        val patternRegex   = StepUtils.convertToRegex(stepDef.pattern)
        val patternMatches = patternRegex.findFirstIn(gherkinStep.pattern).isDefined
        matchesStepType && patternMatches
      }
    }

  // Executes a matched step definition with its parameters and input
  private def executeMatchedStep[I <: Matchable, O](
    stepDef: ZIOSteps[R]#StepDef[I, O],
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  )(implicit iTag: Tag[I], oTag: Tag[O]): ZIO[R, Throwable, StepResult] = {
    val stepId  = gherkinStep.id
    val pattern = StepUtils.convertToRegex(stepDef.pattern)
    val params  = StepUtils.extractParams(pattern, gherkinStep.pattern, stepDef.pattern)
    val fn      = stepDef.fn

    for {
      currentStack <- stackRef.get
      // _            <- logCollector.logStdout(scenarioId, stepId, s"Step: $line, OutputStack: $currentStack, Params: $params")
      // Determine the input for the step (from params or previous outputs)
      input <- determineInput(params, currentStepType, stepDef.iTag)
      // _            <- logCollector.logStdout(scenarioId, stepId, s"Selected Input for $line: ${input.toString}")
      // Execute the step function with the prepared input
      result <- executeStepFunction(fn, line, input, start, stepId)
    } yield result
  }

  // Determines the input for a step, either from parameters or the stack
  private def determineInput[I](
    params: List[Any],
    currentStepType: StepType,
    iTag: Tag[I]
  ): ZIO[Any, Throwable, I] =
    stackRef.get.flatMap { stack =>
      val priorOutput = stack.headOption.map(_.output).getOrElse(())
      OutputStack.combineTyped(priorOutput, params)(iTag)
    }

  // Runs the step's function and constructs the result, handling success and failure
  private def executeStepFunction[I <: Matchable, O](
    fn: I => ZIO[R, Throwable, O],
    line: String,
    input: I,
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
      output = output.asInstanceOf[Any],
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
}
