package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep, StepType}

import java.time.Instant
import scala.util.matching.Regex

case class StepResult(
  step: String,
  succeeded: Boolean,
  error: Option[String],
  output: Any,
  logs: List[(String, Instant)]
)

case class StepRecord(
  stepType: StepType,
  line: String,
  output: Any
)

object OutputStack {
  def make: UIO[Ref[Chunk[StepRecord]]] = Ref.make(Chunk.empty[StepRecord])

  def push(stackRef: Ref[Chunk[StepRecord]], record: StepRecord): UIO[Unit] =
    stackRef.update(record +: _)

  def peek(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[StepRecord]] =
    stackRef.get.map(_.headOption)

  def pop(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[StepRecord]] =
    stackRef.modify { chunk =>
      if (chunk.isEmpty) (None, chunk)
      else (Some(chunk.head), chunk.tail)
    }

  def clear(stackRef: Ref[Chunk[StepRecord]]): UIO[Unit] =
    stackRef.set(Chunk.empty)

  def isEmpty(stackRef: Ref[Chunk[StepRecord]]): UIO[Boolean] =
    stackRef.get.map(_.isEmpty)

  def findNonUnitOutput(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[Any]] =
    stackRef.get.map { chunk =>
      chunk.find(record => flattenOutput(record.output) != ()).map(_.output)
    }

  def findLastNonAndStepType(stackRef: Ref[Chunk[StepRecord]]): UIO[StepType] =
    stackRef.get.map { chunk =>
      chunk.find(_.stepType != StepType.AndStep).map(_.stepType).getOrElse(StepType.GivenStep)
    }

  private[core] def flattenOutput(value: Any): Any = value match {
    case () => ()
    case (a, b) =>
      (flattenOutput(a), flattenOutput(b)) match {
        case ((), ()) => ()
        case ((), b)  => b
        case (a, ())  => a
        case (a, b)   => (a, b)
      }
    case other => other
  }
}

object ScenarioRunner {
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

  def run[R](
    steps: ZIOSteps[R],
    gherkinSteps: List[GherkinStep],
    metadata: ScenarioMetadata = ScenarioMetadata()
  ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] = {

    def runSteps(
      remaining: List[GherkinStep],
      stackRef: Ref[Chunk[StepRecord]],
      acc: List[StepResult]
    ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] =
      remaining match {
        case Nil => ZIO.succeed(acc.reverse)
        case gherkinStep :: rest =>
          val line            = gherkinStep.pattern
          val isAnd           = gherkinStep.stepType == StepType.AndStep
          val currentStepType = gherkinStep.stepType
          OutputStack.findLastNonAndStepType(stackRef).flatMap { lastNonAndStepType =>
            val expectedStepType = if (isAnd) lastNonAndStepType else currentStepType
            steps.getSteps.find { stepDef =>
              val matchesStepType = if (isAnd) {
                stepDef.stepType == expectedStepType || stepDef.stepType == StepType.AndStep
              } else {
                stepDef.stepType == expectedStepType
              }
              val patternMatches = stepDef.pattern.findFirstIn(gherkinStep.pattern).isDefined
              patternMatches && matchesStepType
            } match {
              case Some(stepDef) =>
                val pattern = stepDef.pattern
                val fn      = stepDef.fn
                val params  = extractParams(pattern, gherkinStep.pattern)
                for {
                  currentStack <- stackRef.get
                  _ <- ZIO.serviceWithZIO[LogCollector](
                         _.log(s"Step: $line, OutputStack: $currentStack, Params: $params")
                       )
                  input <- if (params.nonEmpty) {
                             ZIO.succeed(combine((), params))
                           } else {
                             // For When/And steps, prefer the last Given output if available
                             stackRef.get.map { stack =>
                               if (currentStepType == StepType.WhenStep || isAnd) {
                                 stack.find(_.stepType == StepType.GivenStep).map(_.output).getOrElse(())
                               } else {
                                 OutputStack.flattenOutput(stack.headOption.map(_.output).getOrElse(()))
                               }
                             }
                           }
                  _         <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
                  reporter  <- ZIO.service[Reporter]
                  _         <- reporter.startStep(line)
                  collector <- ZIO.service[LogCollector]
                  result <- (for {
                              _      <- collector.log(s"Executing: $line with input: $input")
                              output <- fn.asInstanceOf[Any => ZIO[R, Throwable, Any]](input)
                              logs   <- collector.getLogs
                              _      <- collector.clearLogs
                              finalOutput <- if (isAnd && OutputStack.flattenOutput(output) != ()) {
                                               OutputStack.peek(stackRef).flatMap {
                                                 case Some(prev) if OutputStack.flattenOutput(prev.output) != () =>
                                                   ZIO.succeed((prev.output, output))
                                                 case _ =>
                                                   ZIO.succeed(output)
                                               }
                                             } else {
                                               ZIO.succeed(output)
                                             }
                            } yield StepResult(
                              line,
                              succeeded = true,
                              error = None,
                              output = finalOutput,
                              logs = logs
                            )).catchAll { error =>
                              collector.getLogs.flatMap { logs =>
                                collector.clearLogs.as {
                                  StepResult(
                                    line,
                                    succeeded = false,
                                    error = Some(error.getMessage),
                                    output = (),
                                    logs = logs
                                  )
                                }
                              }
                            }
                  _            <- reporter.endStep(line, result)
                  _            <- OutputStack.push(stackRef, StepRecord(currentStepType, line, result.output))
                  updatedStack <- stackRef.get
                  _ <- ZIO.serviceWithZIO[LogCollector](
                         _.log(s"After $line, NewStack: $updatedStack")
                       )
                  finalResult <- if (result.succeeded) {
                                   runSteps(rest, stackRef, result :: acc)
                                 } else {
                                   ZIO.succeed(
                                     (result :: acc).reverse ++ rest.map(gherkinStep =>
                                       StepResult(
                                         gherkinStep.toString,
                                         succeeded = false,
                                         error = Some("Skipped due to prior failure"),
                                         output = (),
                                         logs = Nil
                                       )
                                     )
                                   )
                                 }
                } yield finalResult
              case None =>
                for {
                  _         <- ZIO.serviceWithZIO[LogCollector](_.log(s"No step definition matches: $line"))
                  collector <- ZIO.service[LogCollector]
                  logs      <- collector.getLogs
                  _         <- collector.clearLogs
                  result = StepResult(
                             line,
                             succeeded = false,
                             error = Some("No step definition matches"),
                             output = (),
                             logs = logs
                           )
                  reporter <- ZIO.service[Reporter]
                  _        <- reporter.startStep(line)
                  _        <- reporter.endStep(line, result)
                  _        <- OutputStack.push(stackRef, StepRecord(currentStepType, line, result.output))
                  next     <- runSteps(rest, stackRef, result :: acc)
                } yield next
            }
          }
      }

    for {
      reporter    <- ZIO.service[Reporter]
      stackRef    <- OutputStack.make
      scenarioText = gherkinSteps.mkString("\n")
      _           <- reporter.startScenario(scenarioText)
      results     <- runSteps(gherkinSteps, stackRef, Nil)
      _           <- reporter.endScenario(scenarioText, results)
    } yield results
  }

  def runScenarios[R](
    steps: ZIOSteps[R],
    feature: Feature,
    parallelism: Int
  ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
    val allExamples = feature.scenarios.flatMap(_.examples)
    val scenariosWithMetadata = if (allExamples.isEmpty) {
      feature.scenarios.map { scenario =>
        val baseSteps = feature.background ++ scenario.steps
        (baseSteps, scenario.metadata)
      }
    } else {
      allExamples.flatMap { row =>
        val exampleData = row.data
        feature.scenarios.map { scenario =>
          val baseSteps = feature.background ++ scenario.steps
          val parameterizedSteps = baseSteps.map { step =>
            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
            val newPattern = placeholderPattern.findAllMatchIn(step.pattern).foldLeft(step.pattern) {
              (currentPattern, placeholderMatch) =>
                val placeholderName = placeholderMatch.group(1)
                val placeholderType = placeholderMatch.group(2)
                val rawValue = exampleData.getOrElse(
                  placeholderName,
                  throw new IllegalArgumentException(
                    s"Placeholder '{$placeholderName}' in step '${step.pattern}' not found in Examples row: $exampleData"
                  )
                )
                val validatedValue = placeholderType.toLowerCase match {
                  case "string" => rawValue
                  case "int" =>
                    try rawValue.toInt.toString
                    catch {
                      case _: NumberFormatException =>
                        throw new IllegalArgumentException(
                          s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}' is not an Int"
                        )
                    }
                  case "double" =>
                    try rawValue.toDouble.toString
                    catch {
                      case _: NumberFormatException =>
                        throw new IllegalArgumentException(
                          s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}' is not a Double"
                        )
                    }
                  case unknown =>
                    throw new IllegalArgumentException(
                      s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}'"
                    )
                }
                currentPattern.replace(s"{$placeholderName:$placeholderType}", validatedValue)
            }
            GherkinStep(step.stepType, newPattern)
          }
          (parameterizedSteps, scenario.metadata)
        }
      }
    }

    for {
      reporter <- ZIO.service[Reporter]
      _        <- reporter.startFeature(feature.name)
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (gherkinSteps, metadata) =>
                     val scenarioId = gherkinSteps.mkString("\n").hashCode.toString
                     ZIO
                       .foreach(1 to metadata.repeatCount) { iteration =>
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
  }

  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = {
    val trimmed = param.trim
    trimmed
  }
}
