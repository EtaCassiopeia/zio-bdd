package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.{StepType, Step => GherkinStep}
import zio.bdd.core.report.Reporter
import java.time.Instant

case class StepExecutor[R](
  scenarioId: String,
  steps: ZIOSteps[R],
  stackRef: Ref[Chunk[StepRecord]],
  reporter: Reporter,
  logCollector: LogCollector
) {
  def executeStep(gherkinStep: GherkinStep)(implicit trace: Trace): ZIO[R, Nothing, StepResult] =
    ZIO.logAnnotate("stepId", gherkinStep.pattern.hashCode.toString) {
      for {
        stepDefOpt <- findMatchingStepDef(gherkinStep.stepType, gherkinStep)
        start      <- Clock.instant
        _          <- reporter.startStep(gherkinStep.pattern)
        result <- stepDefOpt match {
                    case Some(stepDef) =>
                      executeMatchedStep(stepDef, gherkinStep, gherkinStep.pattern, gherkinStep.stepType, start)(
                        stepDef.iTag,
                        stepDef.oTag
                      ).tapDefect(cause => handleStepFailure(gherkinStep, cause, start))
                    case None =>
                      executeUnmatchedStep(gherkinStep, gherkinStep.pattern, gherkinStep.stepType, start)
                  }
        end <- Clock.instant
        finalResult = result.copy(
                        duration = Duration.fromInterval(start, end),
                        startTime = start,
                        file = gherkinStep.file,
                        line = gherkinStep.line
                      )
        _            <- reporter.endStep(gherkinStep.pattern, finalResult)
        _            <- OutputStack.push(stackRef, StepRecord(gherkinStep.stepType, gherkinStep.pattern, finalResult.output))
        updatedStack <- stackRef.get
        _ <- logCollector.log(
               scenarioId,
               gherkinStep.id,
               s"After ${gherkinStep.pattern}, Stack: $updatedStack, Duration: ${finalResult.duration}",
               InternalLogLevel.Debug
             )
      } yield finalResult
    }

  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  )(implicit trace: Trace): ZIO[Any, Nothing, Option[ZIOSteps[R]#StepDef[? <: Matchable, ?]]] =
    ZIO.succeed {
      steps.getSteps.find { stepDef =>
        val matchesStepType = gherkinStep.stepType match {
          case StepType.AndStep => true
          case _                => stepDef.stepType == expectedStepType
        }
        val patternRegex   = StepUtils.convertToRegex(stepDef.pattern)
        val patternMatches = patternRegex.pattern.matcher(gherkinStep.pattern).matches()
        matchesStepType && patternMatches
      }
    }

  private def executeMatchedStep[I <: Matchable, O](
    stepDef: ZIOSteps[R]#StepDef[I, O],
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  )(implicit iTag: Tag[I], oTag: Tag[O], trace: Trace): ZIO[R, Nothing, StepResult] =
    for {
      params <- ZIO.succeed(StepUtils.extractParams(StepUtils.convertToRegex(stepDef.pattern), line, stepDef.pattern))
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
                        duration = Duration.Zero,
                        startTime = start
                      )
                    )
                  case Right(value) =>
                    executeStepFunction(stepDef.fn, line, value, start, gherkinStep.id)
                }
    } yield result

  private def determineInput[I](
    params: List[Any],
    currentStepType: StepType,
    iTag: Tag[I]
  )(implicit trace: Trace): ZIO[Any, Throwable, I] =
    stackRef.get.flatMap { stack =>
      val priorOutput = stack.headOption.map(_.output).getOrElse(())
      OutputStack.combineTyped(priorOutput, params)(iTag)
    }

  private def executeStepFunction[I <: Matchable, O](
    fn: I => ZIO[R, Throwable, O],
    line: String,
    input: I,
    start: Instant,
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
                    duration = Duration.Zero, // Will be updated
                    startTime = startTime
                  )
                }
      endTime <- Clock.instant
    } yield result.copy(duration = Duration.fromInterval(startTime, endTime))

//  private def executeStepFunction[I <: Matchable, O](
//    fn: I => ZIO[R, Throwable, O],
//    line: String,
//    input: I,
//    start: Instant,
//    stepId: String
//  )(implicit trace: Trace): ZIO[R, Nothing, StepResult] =
//    fn(input).map { output =>
//      StepResult(
//        line,
//        succeeded = true,
//        error = None,
//        output = output.asInstanceOf[Any],
//        logs = Nil, // Logs will be fetched later if needed
//        duration = Duration.Zero,
//        startTime = start
//      )
//    }.catchAll { error =>
//      for {
//        logs <- logCollector.getLogs(scenarioId, stepId)
//        _    <- logCollector.log(scenarioId, stepId, error.getMessage, InternalLogLevel.Error)
//      } yield StepResult(
//        line,
//        succeeded = false,
//        error = Some(TestError.fromThrowable(error)), // Propagate the actual error
//        output = (),
//        logs = logs.toStepResultLogs,
//        duration = Duration.Zero,
//        startTime = start
//      )
//    }

  private def executeUnmatchedStep(
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
