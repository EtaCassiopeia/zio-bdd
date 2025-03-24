package zio.bdd.core

import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTagRef.AbstractReference
import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.{StepType, Step as GherkinStep}

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
                      ).tap { result =>
                        // Record the step's result in the stack for use by subsequent steps
                        OutputStack.push(
                          stackRef,
                          StepRecord(
                            gherkinStep.stepType,
                            gherkinStep.pattern,
                            result.output,
                            scenarioId,
                            stepDef.oTag.tag
                          )
                        )
                      }
                        .tapDefect(cause => handleStepFailure(gherkinStep, cause, start))
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
      } yield finalResult
    }

  // Finds a step definition that matches the Gherkin step's type, pattern and input type
  private def findMatchingStepDef(
    expectedStepType: StepType,
    gherkinStep: GherkinStep
  )(implicit trace: Trace): ZIO[Any, Nothing, Option[StepDef[R, ?, ?]]] =
    for {
      maybePriorRecord <- OutputStack.peek(stackRef, scenarioId)
      priorOutput       = maybePriorRecord.map(_.output).getOrElse(())
      priorArity       <- ZIO.succeed(getArity(priorOutput))
      priorOutputTag   <- ZIO.succeed(maybePriorRecord.map(_.outputTag).getOrElse(Tag[Unit].tag))
    } yield {
      steps.getSteps.find { stepDef =>
        // Convert pattern to regex and ensure exact match
        val patternRegex   = StepUtils.convertToRegex(stepDef.pattern)
        val patternMatches = patternRegex.pattern.matcher(gherkinStep.pattern).matches()
        if (!patternMatches) {
          false
        } else {
          val (stepParams, paramTags) = StepUtils.extractParams(patternRegex, gherkinStep.pattern, stepDef.pattern)
          val paramCount              = stepParams.length
          val totalInputCount         = priorArity + paramCount
          val expectedInputCount      = getInputArity(stepDef)
          val combinedInputTag        = constructCombinedTag(priorOutputTag, paramTags)
          val inputTypeMatches        = stepDef.iTag.tag == combinedInputTag

          // Determine if step types match
          val matchesStepType = gherkinStep.stepType match {
            case StepType.AndStep =>
              // For And steps, ignore expectedStepType and allow any step type with an exact match
              true
            case _ =>
              // For non-And steps, require exact step type match
              stepDef.stepType == expectedStepType
          }

          matchesStepType && totalInputCount == expectedInputCount && inputTypeMatches
        }
      }
    }

  private def getArity(value: Any): Int = value match {
    case ()           => 0
    case tuple: Tuple => tuple.productArity
    case _            => 1
  }

  private def getInputArity[I, O](stepDef: StepDef[R, I, O]): Int = {
    val tag = stepDef.iTag.tag
    if (tag == Tag[Unit].tag) 0
    else if (tag.longName.startsWith("scala.Tuple")) tag.typeArgs.length
    else 1
  }

  private def constructCombinedTag(priorOutputTag: LightTypeTag, paramTags: List[Tag[Any]]): LightTypeTag = {
    import izumi.reflect.macrortti.LightTypeTagRef.{FullReference, SymName, TypeParam, Variance}
    import izumi.reflect.macrortti.{LightTypeTag, LightTypeTagRef}

    if (priorOutputTag == Tag[Unit].tag && paramTags.isEmpty) {
      Tag[Unit].tag
    } else if (priorOutputTag == Tag[Unit].tag && paramTags.length == 1) {
      paramTags.head.tag
    } else if (priorOutputTag == Tag[Unit].tag) {
      // Case: Only parameters from paramTags, no prior output
      val tupleName = s"scala.Tuple${paramTags.length}"
      // Use Variance.Covariant to match Scala's tuple variance
      val typeParams = paramTags.map(tag => TypeParam(tag.tag.ref.asInstanceOf[AbstractReference], Variance.Covariant))
      val fullRef    = FullReference(SymName.SymTypeName(tupleName), typeParams, None)
      LightTypeTag(fullRef, Map.empty, Map.empty)
    } else if (paramTags.isEmpty) {
      priorOutputTag
    } else {
      // Case: Combine priorOutputTag with paramTags
      val combinedArgs =
        priorOutputTag.ref.asInstanceOf[AbstractReference] :: paramTags.map(_.tag.ref.asInstanceOf[AbstractReference])
      val tupleName = s"scala.Tuple${combinedArgs.length}"
      // Use Variance.Covariant for all tuple parameters
      val typeParams = combinedArgs.map(arg => TypeParam(arg, Variance.Covariant))
      val fullRef    = FullReference(SymName.SymTypeName(tupleName), typeParams, None)
      LightTypeTag(fullRef, Map.empty, Map.empty)
    }
  }

  // Executes a matched step definition with its parameters and input
  private def executeMatchedStep[I, O](
    stepDef: StepDef[R, I, O],
    gherkinStep: GherkinStep,
    line: String,
    currentStepType: StepType,
    start: Instant,
    featureId: String,
    scenarioId: String
  )(implicit trace: Trace): ZIO[R, Nothing, StepResult] =
    for {
      params <-
        ZIO.succeed(StepUtils.extractParams(StepUtils.convertToRegex(stepDef.pattern), line, stepDef.pattern)._1)
      // Determine the input for the step (from params or previous outputs)
      input <- determineInput[I](params, currentStepType)(stepDef.iTag, trace)
                 .map(Right(_))
                 .catchAll { e =>
                   ZIO.left(TestError.TypeMismatch(e.getMessage, params, Some(e), Some(trace)))
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
    currentStepType: StepType
  )(implicit iTag: Tag[I], trace: Trace): ZIO[Any, Throwable, I] =
    OutputStack.getPriorOutput(stackRef, scenarioId, params)

  // Runs the step's function and constructs the result, handling success and failure
  private def executeStepFunction[I, O](
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
                    duration = Duration.Zero,
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
                    duration = Duration.Zero,
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
