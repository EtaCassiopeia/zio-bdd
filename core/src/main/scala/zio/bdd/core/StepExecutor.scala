package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.{StepType, Step => GherkinStep}
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
  def executeStep(featureId: String, scenarioId: String, gherkinStep: GherkinStep)(implicit
    trace: Trace
  ): ZIO[R, Nothing, StepResult] =
    ZIO.logAnnotate("stepId", gherkinStep.id) {
      for {
        stepDefOpt <- findMatchingStepDef(gherkinStep.stepType, gherkinStep)
        start      <- Clock.instant
        _          <- reporter.startStep(gherkinStep.pattern)
        result <- stepDefOpt match {
                    case Some(stepDef) =>
                      // If a step definition matches, execute it
                      executeMatchedStep(
                        stepDef,
                        gherkinStep,
                        gherkinStep.pattern,
                        gherkinStep.stepType,
                        start,
                        featureId,
                        scenarioId
                      )(
                        stepDef.iTag,
                        stepDef.oTag
                      ).tapDefect(cause => handleStepFailure(gherkinStep, cause, start))
                    case None =>
                      // If no match, report a failure
                      executeUnmatchedStep(
                        featureId,
                        scenarioId,
                        gherkinStep,
                        gherkinStep.pattern,
                        gherkinStep.stepType,
                        start
                      )
                  }
        end <- Clock.instant
        finalResult = result.copy(
                        duration = Duration.fromInterval(start, end),
                        startTime = start,
                        file = gherkinStep.file,
                        line = gherkinStep.line
                      )
        _ <- reporter.endStep(gherkinStep.pattern, finalResult)
        // Record the step's result in the stack for use by subsequent steps
        _            <- OutputStack.push(stackRef, StepRecord(gherkinStep.stepType, gherkinStep.pattern, finalResult.output))
        updatedStack <- stackRef.get
//        _ <- logCollector.log(
//               scenarioId,
//               gherkinStep.id,
//               s"After ${gherkinStep.pattern}, Stack: $updatedStack, Duration: ${finalResult.duration}",
//               InternalLogLevel.Debug
//             )
      } yield finalResult
    }

  // Finds a step definition that matches the Gherkin step's type and pattern
  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  )(implicit trace: Trace): ZIO[Any, Nothing, Option[ZIOSteps[R]#StepDef[? <: Matchable, ?]]] =
    ZIO.succeed {
      steps.getSteps.find { stepDef =>
        // Determine if step types match
        val matchesStepType = gherkinStep.stepType match {
          case StepType.AndStep =>
            // For And steps, ignore expectedStepType and allow any step type with an exact match
            true
          case _ =>
            // For non-And steps, require exact step type match
            stepDef.stepType == expectedStepType
        }

        // Convert pattern to regex and ensure exact match
        val patternRegex   = StepUtils.convertToRegex(stepDef.pattern)
        val patternMatches = patternRegex.pattern.matcher(gherkinStep.pattern).matches()
        matchesStepType && patternMatches
      }
    }

  // Executes a matched step definition with its parameters and input
  private def executeMatchedStep[I <: Matchable, O](
    stepDef: ZIOSteps[R]#StepDef[I, O],
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant,
    featureId: String,
    scenarioId: String
  )(implicit iTag: Tag[I], oTag: Tag[O], trace: Trace): ZIO[R, Nothing, StepResult] =
    for {
      params <- ZIO.succeed(StepUtils.extractParams(StepUtils.convertToRegex(stepDef.pattern), line, stepDef.pattern))
      // Determine the input for the step (from params or previous outputs)
      input <- determineInput(params, currentStepType, iTag)
                 .map(Right(_))
                 .catchAll { e =>
                   ZIO.left(TestError.TypeMismatch(iTag.tag.toString, e.getMessage, params, Some(e), Some(trace)))
                 }
      result <- input match {
                  case Left(error) =>
                    ZIO.succeed(
                      StepResult(
                        line,
                        succeeded = false,
                        error = Some(error),
                        output = (),
                        logs = Nil,
                        stepId = Some(gherkinStep.id),
                        scenarioId = Some(scenarioId),
                        featureId = Some(featureId),
                        duration = Duration.Zero,
                        startTime = start
                      )
                    )
                  case Right(value) =>
                    // Execute the step function with the prepared input
                    executeStepFunction(stepDef.fn, line, value, start, featureId, scenarioId, gherkinStep.id)
                }
    } yield result

  // Determines the input for a step, either from parameters or the stack
  private def determineInput[I](
    params: List[Any],
    currentStepType: StepType,
    iTag: Tag[I]
  )(implicit trace: Trace): ZIO[Any, Throwable, I] =
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
    featureId: String,
    scenarioId: String,
    stepId: String
  )(implicit trace: Trace): ZIO[R, Nothing, StepResult] =
    for {
      startTime <- Clock.instant
      result <- fn(input).map { output =>
                  StepResult(
                    line,
                    succeeded = true,
                    error = None,
                    output = output.asInstanceOf[Any],
                    logs = Nil,
                    stepId = Some(stepId),
                    scenarioId = Some(scenarioId),
                    featureId = Some(featureId),
                    duration = Duration.Zero, // Will be updated
                    startTime = startTime
                  )
                }.catchAll { error =>
                  for {
                    logs <- logCollector.getLogs(scenarioId, stepId)
                    _    <- logCollector.log(scenarioId, stepId, error.getMessage, InternalLogLevel.Error)
                  } yield StepResult(
                    line,
                    succeeded = false,
                    error = Some(TestError.fromThrowable(error)),
                    output = (),
                    logs = logs.toStepResultLogs,
                    stepId = Some(stepId),
                    scenarioId = Some(scenarioId),
                    featureId = Some(featureId),
                    duration = Duration.Zero, // Will be updated
                    startTime = startTime
                  )
                }
      endTime <- Clock.instant
    } yield result.copy(duration = Duration.fromInterval(startTime, endTime))

  // Handles the case where no step definition matches the Gherkin step
  private def executeUnmatchedStep(
    featureId: String,
    scenarioId: String,
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  )(implicit trace: Trace): ZIO[Any, Nothing, StepResult] =
    for {
      _    <- logCollector.log(scenarioId, gherkinStep.id, s"No step definition matches: $line", InternalLogLevel.Error)
      logs <- logCollector.getLogs(scenarioId, gherkinStep.id)
    } yield StepResult(
      line,
      succeeded = false,
      error = Some(TestError.MissingStep(line, None, Some(trace))),
      output = (),
      logs = logs.toStepResultLogs,
      duration = Duration.Zero,
      startTime = start,
      stepId = Some(gherkinStep.id),
      scenarioId = Some(scenarioId),
      featureId = Some(featureId),
      file = gherkinStep.file,
      line = gherkinStep.line
    )

  private def handleStepFailure(gherkinStep: GherkinStep, cause: Cause[Throwable], start: Instant)(implicit
    trace: Trace
  ): ZIO[R, Nothing, Unit] =
    for {
      logs <- logCollector.getLogs(scenarioId, gherkinStep.id)
      _    <- logCollector.log(scenarioId, gherkinStep.id, s"Step failed: ${cause.prettyPrint}", InternalLogLevel.Error)
    } yield ()
}
