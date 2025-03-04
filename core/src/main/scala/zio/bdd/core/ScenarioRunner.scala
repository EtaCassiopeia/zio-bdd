package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata}

import java.time.Instant
import scala.util.matching.Regex

case class StepResult(
  step: String,
  succeeded: Boolean,
  error: Option[String],
  output: Any,
  logs: List[(String, Instant)]
)

object ScenarioRunner {
  private def flattenOutput(value: Any): Any = value match {
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

  private def combine(prev: Any, params: List[String]): Any = {
    val flattenedPrev = flattenOutput(prev)
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
    scenario: String,
    metadata: ScenarioMetadata = ScenarioMetadata()
  ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] = {
    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList

    def runSteps(
      remaining: List[String],
      context: Option[Any], // Context from last non-Unit output of non-And steps
      previousOutput: Any,
      acc: List[StepResult]
    ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] =
      remaining match {
        case Nil => ZIO.succeed(acc.reverse)
        case line :: rest =>
          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
            case Some(stepDef) =>
              val pattern = stepDef.pattern
              val fn      = stepDef.fn
              val params  = extractParams(pattern, line)
              val isAnd   = line.trim.startsWith("And")
              for {
                _ <- ZIO.serviceWithZIO[LogCollector](
                       _.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params")
                     )
                // Input: params if present, context for non-And if non-Unit, else previousOutput
                input = if (params.nonEmpty) {
                          combine((), params)
                        } else if (!isAnd && context.isDefined && flattenOutput(context.get) != ()) {
                          context.get
                        } else {
                          previousOutput
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
                            // Compose output with context if And and context exists
                            finalOutput <- if (isAnd && context.isDefined && flattenOutput(output) != ()) {
                                             ZIO.succeed((context.get, output))
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
                _         <- reporter.endStep(line, result)
                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
                _ <- ZIO.serviceWithZIO[LogCollector](
                       _.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}")
                     )
                finalResult <- if (result.succeeded) {
                                 runSteps(rest, newContext, result.output, result :: acc)
                               } else {
                                 ZIO.succeed(
                                   (result :: acc).reverse ++ rest.map(line =>
                                     StepResult(
                                       line,
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
                next     <- runSteps(rest, context, previousOutput, result :: acc)
              } yield next
          }
      }

    for {
      reporter <- ZIO.service[Reporter]
      _        <- reporter.startScenario(scenario)
      results  <- runSteps(lines, None, (), Nil)
      _        <- reporter.endScenario(scenario, results)
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
        (baseSteps.mkString("\n"), scenario.metadata)
      }
    } else {
      allExamples.flatMap { row =>
        val exampleData = row.data
        feature.scenarios.map { scenario =>
          val baseSteps = feature.background ++ scenario.steps
          val parameterizedText = baseSteps.map { step =>
            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
              val placeholderName = placeholderMatch.group(1)
              val placeholderType = placeholderMatch.group(2)
              val rawValue = exampleData.getOrElse(
                placeholderName,
                throw new IllegalArgumentException(
                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
                )
              )
              val validatedValue = placeholderType.toLowerCase match {
                case "string" => rawValue
                case "int" =>
                  try rawValue.toInt.toString
                  catch {
                    case _: NumberFormatException =>
                      throw new IllegalArgumentException(
                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
                      )
                  }
                case "double" =>
                  try rawValue.toDouble.toString
                  catch {
                    case _: NumberFormatException =>
                      throw new IllegalArgumentException(
                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
                      )
                  }
                case unknown =>
                  throw new IllegalArgumentException(
                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
                  )
              }
              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
            }
          }.mkString("\n")
          // println(s"Parameterized text for ${row.data}: $parameterizedText")
          (parameterizedText, scenario.metadata)
        }
      }
    }

    for {
      reporter <- ZIO.service[Reporter]
      _        <- reporter.startFeature(feature.name)
      results <- ZIO
                   .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
                     val scenarioId = scenarioText.hashCode.toString
                     ZIO
                       .foreach(1 to metadata.repeatCount) { iteration =>
                         ZIO.logAnnotate("scenarioId", s"${scenarioId}_iteration_$iteration") {
                           run(steps, scenarioText, metadata)
                         }
                       }
                       .map(_.flatten.toList)
                   }
                   .withParallelism(parallelism)
                   .map(_.toList)
      _ <- reporter.endFeature(feature.name, results)
    } yield results
  }

  def runScenarios[R](
    steps: ZIOSteps[R],
    scenarios: List[(String, ScenarioMetadata)],
    parallelism: Int = 4
  ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
    ZIO
      .foreachPar(scenarios) { case (scenario, metadata) =>
        val scenarioId = scenario.hashCode.toString
        ZIO.logAnnotate("scenarioId", scenarioId) {
          for {
            reporter <- ZIO.service[Reporter]
            _        <- reporter.startScenario(scenario)
            results  <- run(steps, scenario, metadata)
            _        <- reporter.endScenario(scenario, results)
          } yield results
        }
      }
      .withParallelism(parallelism)

  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = {
    val trimmed = param.trim
    trimmed
  }
}
