package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{StepType, Step => GherkinStep}

import java.time.Instant
import scala.util.matching.Regex

case class StepExecutor[R](
  scenarioId: String,
  steps: ZIOSteps[R],
  stackRef: Ref[Chunk[StepRecord]],
  reporter: Reporter,
  logCollector: LogCollector
) {

  def executeStep(gherkinStep: GherkinStep): ZIO[R, Throwable, StepResult] = {
    val line            = gherkinStep.pattern
    val isAnd           = gherkinStep.stepType == StepType.AndStep
    val currentStepType = gherkinStep.stepType

    for {
      lastNonAndStepType <- OutputStack.findLastNonAndStepType(stackRef)
      expectedStepType    = if (isAnd) lastNonAndStepType else currentStepType
      stepDefOpt         <- findMatchingStepDef(expectedStepType, gherkinStep)
      start              <- Clock.instant
      result <- stepDefOpt match {
                  case Some(stepDef) => executeMatchedStep(stepDef, gherkinStep, line, isAnd, currentStepType, start)
                  case None          => executeUnmatchedStep(gherkinStep, line, currentStepType, start)
                }
      end     <- Clock.instant
      duration = Duration.fromInterval(start, end)
      finalResult =
        result.copy(duration = duration, startTime = start, file = gherkinStep.file, line = gherkinStep.line)
      _            <- OutputStack.push(stackRef, StepRecord(currentStepType, line, finalResult.output))
      updatedStack <- stackRef.get
      _            <- logCollector.logStdout(scenarioId, s"After $line, NewStack: $updatedStack, Duration: $duration")
    } yield finalResult
  }

  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  ): ZIO[Any, Nothing, Option[ZIOSteps[R]#StepDef[?, ?]]] =
    ZIO.succeed {
      steps.getSteps.find { stepDef =>
        val matchesStepType = if (gherkinStep.stepType == StepType.AndStep) {
          stepDef.stepType == expectedStepType || stepDef.stepType == StepType.AndStep
        } else {
          stepDef.stepType == expectedStepType
        }
        val patternMatches = stepDef.pattern.findFirstIn(gherkinStep.pattern).isDefined
        matchesStepType && patternMatches
      }
    }

  private def executeMatchedStep(
    stepDef: ZIOSteps[R]#StepDef[?, ?],
    gherkinStep: GherkinStep,
    line: String,
    isAnd: Boolean,
    currentStepType: StepType,
    start: Instant
  ): ZIO[R, Throwable, StepResult] = {
    val pattern = stepDef.pattern
    val fn      = stepDef.fn.asInstanceOf[Any => ZIO[R, Throwable, Any]]
    val params  = extractParams(pattern, gherkinStep.pattern)

    for {
      currentStack <- stackRef.get
      _            <- logCollector.logStdout(scenarioId, s"Step: $line, OutputStack: $currentStack, Params: $params")
      input        <- determineInput(params, currentStepType, isAnd)
      _            <- logCollector.logStdout(scenarioId, s"Selected Input for $line: $input")
      _            <- reporter.startStep(line)
      result       <- executeStepFunction(fn, line, input, isAnd, start)
      _            <- reporter.endStep(line, result)
    } yield result
  }

  private def determineInput(
    params: List[String],
    currentStepType: StepType,
    isAnd: Boolean
  ): ZIO[Any, Nothing, Any] =
    if (params.nonEmpty) {
      ZIO.succeed(combine((), params))
    } else {
      stackRef.get.map { stack =>
        if (currentStepType == StepType.WhenStep || isAnd) {
          stack.find(_.stepType == StepType.GivenStep).map(_.output).getOrElse(())
        } else {
          OutputStack.flattenOutput(stack.headOption.map(_.output).getOrElse(()))
        }
      }
    }

  private def executeStepFunction(
    fn: Any => ZIO[R, Throwable, Any],
    line: String,
    input: Any,
    isAnd: Boolean,
    start: Instant
  ): ZIO[R, Throwable, StepResult] =
    (for {
      _      <- logCollector.logStdout(scenarioId, s"Executing: $line with input: $input")
      output <- fn(input)
      logs   <- logCollector.getLogs(scenarioId)
      // _      <- logCollector.clearLogs
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

  private def executeUnmatchedStep(
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant
  ): ZIO[Any, Nothing, StepResult] =
    for {
      _    <- logCollector.logStderr(scenarioId, s"No step definition matches: $line")
      logs <- logCollector.getLogs(scenarioId)
      // _    <- logCollector.clearLogs
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

  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = param.trim
}
