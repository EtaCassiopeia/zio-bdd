package zio.bdd.core

import zio.*

import scala.util.matching.Regex
import java.time.Instant
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}

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
              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
              val params  = extractParams(pattern, line)
              val isAnd   = line.trim.startsWith("And")
              for {
                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params"))
                // Input: params if present, context for non-And if non-Unit, else previousOutput
                input = if (params.nonEmpty) {
                  combine((), params)
                } else if (!isAnd && context.isDefined && flattenOutput(context.get) != ()) {
                  context.get
                } else {
                  previousOutput
                }
                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
                reporter <- ZIO.service[Reporter]
                _        <- reporter.startStep(line)
                collector <- ZIO.service[LogCollector]
                result <- (for {
                  _ <- collector.log(s"Executing: $line with input: $input")
                  output <- fn(input)
                  logs <- collector.getLogs
                  _ <- collector.clearLogs
                } yield StepResult(line, succeeded = true, error = None, output = output, logs = logs))
                  .catchAll { error =>
                    collector.getLogs.map { logs =>
                      collector.clearLogs.map { _ =>
                        StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
                      }
                    }.flatten
                  }
                _ <- reporter.endStep(line, result)
                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}"))
                finalResult <- if (result.succeeded) {
                  runSteps(rest, newContext, result.output, result :: acc)
                } else {
                  ZIO.succeed(
                    (result :: acc).reverse ++ rest.map(line =>
                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
                    )
                  )
                }
              } yield finalResult
            case None =>
              for {
                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"No step definition matches: $line"))
                collector <- ZIO.service[LogCollector]
                logs      <- collector.getLogs
                _         <- collector.clearLogs
                result    = StepResult(line, succeeded = false, error = Some("No step definition matches"), output = (), logs = logs)
                reporter  <- ZIO.service[Reporter]
                _         <- reporter.startStep(line)
                _         <- reporter.endStep(line, result)
                next      <- runSteps(rest, context, previousOutput, result :: acc)
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
          println(s"Parameterized text for ${row.data}: $parameterizedText")
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
          ZIO.logAnnotate("scenarioId", scenarioId) {
            run(steps, scenarioText, metadata)
          }
        }
        .withParallelism(parallelism)
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

//package zio.bdd.core
//
//import zio.*
//
//import scala.util.matching.Regex
//import java.time.Instant
//import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//
//case class StepResult(
//                       step: String,
//                       succeeded: Boolean,
//                       error: Option[String],
//                       output: Any,
//                       logs: List[(String, Instant)]
//                     )
//
//object ScenarioRunner {
//  private def flattenOutput(value: Any): Any = value match {
//    case () => ()
//    case (a, b) =>
//      (flattenOutput(a), flattenOutput(b)) match {
//        case ((), ()) => ()
//        case ((), b)  => b
//        case (a, ())  => a
//        case (a, b)   => (a, b)
//      }
//    case other => other
//  }
//
//  private def combine(prev: Any, params: List[String]): Any = {
//    val flattenedPrev = flattenOutput(prev)
//    params match {
//      case Nil => flattenedPrev
//      case head :: Nil =>
//        flattenedPrev match {
//          case ()     => parseParam(head)
//          case single => (single, parseParam(head))
//        }
//      case many =>
//        flattenedPrev match {
//          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//        }
//    }
//  }
//
//  def run[R](
//              steps: ZIOSteps[R],
//              scenario: String,
//              metadata: ScenarioMetadata = ScenarioMetadata()
//            ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] = {
//    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//
//    def runSteps(
//                  remaining: List[String],
//                  context: Option[Any], // Context from last non-Unit output
//                  previousOutput: Any,
//                  acc: List[StepResult]
//                ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] =
//      remaining match {
//        case Nil => ZIO.succeed(acc.reverse)
//        case line :: rest =>
//          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//            case Some(stepDef) =>
//              val pattern = stepDef.pattern
//              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//              val params  = extractParams(pattern, line)
//              val isAnd   = line.trim.startsWith("And")
//              for {
//                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params"))
//                // Input: params if present, context for non-And if non-Unit, else previousOutput
//                input = if (params.nonEmpty) {
//                  combine((), params)
//                } else if (!isAnd && context.isDefined && flattenOutput(context.get) != ()) {
//                  context.get
//                } else {
//                  previousOutput
//                }
//                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
//                reporter <- ZIO.service[Reporter]
//                _        <- reporter.startStep(line)
//                collector <- ZIO.service[LogCollector]
//                _         <- collector.log(s"Executing: $line with input: $input")
//                result <- fn(input).map { output =>
//                  val logs = List((s"Executing: $line with input: $input", Instant.now()))
//                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//                }.catchAll { error =>
//                  val logs = List((s"Executing: $line with input: $input", Instant.now()))
//                  ZIO.succeed(
//                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//                  )
//                }
//                _ <- reporter.endStep(line, result)
//                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
//                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}"))
//                finalResult <- if (result.succeeded) {
//                  runSteps(rest, newContext, result.output, result :: acc)
//                } else {
//                  ZIO.succeed(
//                    (result :: acc).reverse ++ rest.map(line =>
//                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//                    )
//                  )
//                }
//              } yield finalResult
//            case None =>
//              for {
//                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"No step definition matches: $line"))
//                collector <- ZIO.service[LogCollector]
//                logs      <- collector.getLogs
//                _         <- collector.clearLogs
//                result    = StepResult(line, succeeded = false, error = Some("No step definition matches"), output = (), logs = logs)
//                reporter  <- ZIO.service[Reporter]
//                _         <- reporter.startStep(line)
//                _         <- reporter.endStep(line, result)
//                next      <- runSteps(rest, context, previousOutput, result :: acc)
//              } yield next
//          }
//      }
//
//    for {
//      reporter <- ZIO.service[Reporter]
//      _        <- reporter.startScenario(scenario)
//      results  <- runSteps(lines, None, (), Nil)
//      _        <- reporter.endScenario(scenario, results)
//    } yield results
//  }
//
//  def runScenarios[R](
//                       steps: ZIOSteps[R],
//                       feature: Feature,
//                       parallelism: Int
//                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//    val allExamples = feature.scenarios.flatMap(_.examples)
//    val scenariosWithMetadata = if (allExamples.isEmpty) {
//      feature.scenarios.map { scenario =>
//        val baseSteps = feature.background ++ scenario.steps
//        (baseSteps.mkString("\n"), scenario.metadata)
//      }
//    } else {
//      allExamples.flatMap { row =>
//        val exampleData = row.data
//        feature.scenarios.map { scenario =>
//          val baseSteps = feature.background ++ scenario.steps
//          val parameterizedText = baseSteps.map { step =>
//            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//              val placeholderName = placeholderMatch.group(1)
//              val placeholderType = placeholderMatch.group(2)
//              val rawValue = exampleData.getOrElse(
//                placeholderName,
//                throw new IllegalArgumentException(
//                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//                )
//              )
//              val validatedValue = placeholderType.toLowerCase match {
//                case "string" => rawValue
//                case "int" =>
//                  try rawValue.toInt.toString
//                  catch {
//                    case _: NumberFormatException =>
//                      throw new IllegalArgumentException(
//                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//                      )
//                  }
//                case "double" =>
//                  try rawValue.toDouble.toString
//                  catch {
//                    case _: NumberFormatException =>
//                      throw new IllegalArgumentException(
//                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//                      )
//                  }
//                case unknown =>
//                  throw new IllegalArgumentException(
//                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//                  )
//              }
//              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//            }
//          }.mkString("\n")
//          println(s"Parameterized text for ${row.data}: $parameterizedText")
//          (parameterizedText, scenario.metadata)
//        }
//      }
//    }
//
//    for {
//      reporter <- ZIO.service[Reporter]
//      _        <- reporter.startFeature(feature.name)
//      results <- ZIO
//        .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//          val scenarioId = scenarioText.hashCode.toString
//          ZIO.logAnnotate("scenarioId", scenarioId) {
//            run(steps, scenarioText, metadata)
//          }
//        }
//        .withParallelism(parallelism)
//      _ <- reporter.endFeature(feature.name, results)
//    } yield results
//  }
//
//  def runScenarios[R](
//                       steps: ZIOSteps[R],
//                       scenarios: List[(String, ScenarioMetadata)],
//                       parallelism: Int = 4
//                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//    ZIO
//      .foreachPar(scenarios) { case (scenario, metadata) =>
//        val scenarioId = scenario.hashCode.toString
//        ZIO.logAnnotate("scenarioId", scenarioId) {
//          for {
//            reporter <- ZIO.service[Reporter]
//            _        <- reporter.startScenario(scenario)
//            results  <- run(steps, scenario, metadata)
//            _        <- reporter.endScenario(scenario, results)
//          } yield results
//        }
//      }
//      .withParallelism(parallelism)
//
//  private def extractParams(pattern: Regex, line: String): List[String] =
//    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//
//  private def parseParam(param: String): Any = {
//    val trimmed = param.trim
//    trimmed
//  }
//}
//
//
//
//
//
////package zio.bdd.core
////
////import zio.*
////
////import scala.util.matching.Regex
////import java.time.Instant
////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////
////case class StepResult(
////                       step: String,
////                       succeeded: Boolean,
////                       error: Option[String],
////                       output: Any,
////                       logs: List[(String, Instant)]
////                     )
////
////object ScenarioRunner {
////  private def flattenOutput(value: Any): Any = value match {
////    case () => ()
////    case (a, b) =>
////      (flattenOutput(a), flattenOutput(b)) match {
////        case ((), ()) => ()
////        case ((), b)  => b
////        case (a, ())  => a
////        case (a, b)   => (a, b)
////      }
////    case other => other
////  }
////
////  private def combine(prev: Any, params: List[String]): Any = {
////    val flattenedPrev = flattenOutput(prev)
////    params match {
////      case Nil => flattenedPrev
////      case head :: Nil =>
////        flattenedPrev match {
////          case ()     => parseParam(head)
////          case single => (single, parseParam(head))
////        }
////      case many =>
////        flattenedPrev match {
////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////        }
////    }
////  }
////
////  def run[R](
////              steps: ZIOSteps[R],
////              scenario: String,
////              metadata: ScenarioMetadata = ScenarioMetadata()
////            ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] = {
////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////
////    def runSteps(
////                  remaining: List[String],
////                  context: Option[Any], // Context from last non-Unit output
////                  previousOutput: Any,
////                  acc: List[StepResult]
////                ): ZIO[R with LogCollector with Reporter, Throwable, List[StepResult]] =
////      remaining match {
////        case Nil => ZIO.succeed(acc.reverse)
////        case line :: rest =>
////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////            case Some(stepDef) =>
////              val pattern = stepDef.pattern
////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////              val params  = extractParams(pattern, line)
////              val isAnd   = line.trim.startsWith("And")
////              for {
////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params"))
////                // Input: params if present, context for non-And if non-Unit, else previousOutput
////                input = if (params.nonEmpty) {
////                  combine((), params)
////                } else if (!isAnd && context.isDefined && flattenOutput(context.get) != ()) {
////                  context.get
////                } else {
////                  previousOutput
////                }
////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
////                reporter <- ZIO.service[Reporter]
////                _        <- reporter.startStep(line)
////                collector <- ZIO.service[LogCollector]
////                _         <- collector.log(s"Executing: $line with input: $input")
////                logs      <- collector.getLogs
////                _         <- collector.clearLogs
////                result <- fn(input).map { output =>
////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////                }.catchAll { error =>
////                  ZIO.succeed(
////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////                  )
////                }
////                _ <- reporter.endStep(line, result)
////                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}"))
////                finalResult <- if (result.succeeded) {
////                  runSteps(rest, newContext, result.output, result :: acc)
////                } else {
////                  ZIO.succeed(
////                    (result :: acc).reverse ++ rest.map(line =>
////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////                    )
////                  )
////                }
////              } yield finalResult
////            case None =>
////              for {
////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"No step definition matches: $line"))
////                collector <- ZIO.service[LogCollector]
////                logs      <- collector.getLogs
////                _         <- collector.clearLogs
////                result    = StepResult(line, succeeded = false, error = Some("No step definition matches"), output = (), logs = logs)
////                reporter  <- ZIO.service[Reporter]
////                _         <- reporter.startStep(line)
////                _         <- reporter.endStep(line, result)
////                next      <- runSteps(rest, context, previousOutput, result :: acc)
////              } yield next
////          }
////      }
////
////    for {
////      reporter <- ZIO.service[Reporter]
////      _        <- reporter.startScenario(scenario)
////      results  <- runSteps(lines, None, (), Nil)
////      _        <- reporter.endScenario(scenario, results)
////    } yield results
////  }
////
////  def runScenarios[R](
////                       steps: ZIOSteps[R],
////                       feature: Feature,
////                       parallelism: Int
////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////    val allExamples = feature.scenarios.flatMap(_.examples)
////    val scenariosWithMetadata = if (allExamples.isEmpty) {
////      feature.scenarios.map { scenario =>
////        val baseSteps = feature.background ++ scenario.steps
////        (baseSteps.mkString("\n"), scenario.metadata)
////      }
////    } else {
////      allExamples.flatMap { row =>
////        val exampleData = row.data
////        feature.scenarios.map { scenario =>
////          val baseSteps = feature.background ++ scenario.steps
////          val parameterizedText = baseSteps.map { step =>
////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////              val placeholderName = placeholderMatch.group(1)
////              val placeholderType = placeholderMatch.group(2)
////              val rawValue = exampleData.getOrElse(
////                placeholderName,
////                throw new IllegalArgumentException(
////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////                )
////              )
////              val validatedValue = placeholderType.toLowerCase match {
////                case "string" => rawValue
////                case "int" =>
////                  try rawValue.toInt.toString
////                  catch {
////                    case _: NumberFormatException =>
////                      throw new IllegalArgumentException(
////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////                      )
////                  }
////                case "double" =>
////                  try rawValue.toDouble.toString
////                  catch {
////                    case _: NumberFormatException =>
////                      throw new IllegalArgumentException(
////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////                      )
////                  }
////                case unknown =>
////                  throw new IllegalArgumentException(
////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////                  )
////              }
////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////            }
////          }.mkString("\n")
////          println(s"Parameterized text for ${row.data}: $parameterizedText")
////          (parameterizedText, scenario.metadata)
////        }
////      }
////    }
////
////    for {
////      reporter <- ZIO.service[Reporter]
////      _        <- reporter.startFeature(feature.name)
////      results <- ZIO
////        .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////          val scenarioId = scenarioText.hashCode.toString
////          ZIO.logAnnotate("scenarioId", scenarioId) {
////            run(steps, scenarioText, metadata)
////          }
////        }
////        .withParallelism(parallelism)
////      _ <- reporter.endFeature(feature.name, results)
////    } yield results
////  }
////
////  def runScenarios[R](
////                       steps: ZIOSteps[R],
////                       scenarios: List[(String, ScenarioMetadata)],
////                       parallelism: Int = 4
////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////    ZIO
////      .foreachPar(scenarios) { case (scenario, metadata) =>
////        val scenarioId = scenario.hashCode.toString
////        ZIO.logAnnotate("scenarioId", scenarioId) {
////          for {
////            reporter <- ZIO.service[Reporter]
////            _        <- reporter.startScenario(scenario)
////            results  <- run(steps, scenario, metadata)
////            _        <- reporter.endScenario(scenario, results)
////          } yield results
////        }
////      }
////      .withParallelism(parallelism)
////
////  private def extractParams(pattern: Regex, line: String): List[String] =
////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////
////  private def parseParam(param: String): Any = {
////    val trimmed = param.trim
////    trimmed
////  }
////}
////
////
////
//////package zio.bdd.core
//////
//////import zio.*
//////
//////import scala.util.matching.Regex
//////import java.time.Instant
//////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////
//////case class StepResult(
//////                       step: String,
//////                       succeeded: Boolean,
//////                       error: Option[String],
//////                       output: Any,
//////                       logs: List[(String, Instant)]
//////                     )
//////
//////object ScenarioRunner {
//////  private def flattenOutput(value: Any): Any = value match {
//////    case () => ()
//////    case (a, b) =>
//////      (flattenOutput(a), flattenOutput(b)) match {
//////        case ((), ()) => ()
//////        case ((), b)  => b
//////        case (a, ())  => a
//////        case (a, b)   => (a, b)
//////      }
//////    case other => other
//////  }
//////
//////  private def combine(prev: Any, params: List[String]): Any = {
//////    val flattenedPrev = flattenOutput(prev)
//////    params match {
//////      case Nil => flattenedPrev
//////      case head :: Nil =>
//////        flattenedPrev match {
//////          case ()     => parseParam(head)
//////          case single => (single, parseParam(head))
//////        }
//////      case many =>
//////        flattenedPrev match {
//////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////        }
//////    }
//////  }
//////
//////  def run[R](
//////              steps: ZIOSteps[R],
//////              scenario: String,
//////              metadata: ScenarioMetadata = ScenarioMetadata()
//////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////
//////    def runSteps(
//////                  remaining: List[String],
//////                  context: Option[Any], // Context from last non-Unit output
//////                  previousOutput: Any,
//////                  acc: List[StepResult]
//////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////      remaining match {
//////        case Nil => ZIO.succeed(acc.reverse)
//////        case line :: rest =>
//////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////            case Some(stepDef) =>
//////              val pattern = stepDef.pattern
//////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////              val params  = extractParams(pattern, line)
//////              val isAnd   = line.trim.startsWith("And")
//////              for {
//////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params"))
//////                // Input: params if present, context if non-Unit and not overridden by previous And, else previousOutput
//////                input = if (params.nonEmpty) {
//////                  combine((), params)
//////                } else if (context.isDefined && flattenOutput(context.get) != () && (!isAnd || flattenOutput(previousOutput) == ())) {
//////                  context.get // Use context for non-And or if And follows Unit
//////                } else {
//////                  previousOutput
//////                }
//////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
//////                collector <- ZIO.service[LogCollector]
//////                _         <- collector.log(s"Executing: $line with input: $input")
//////                logs      <- collector.getLogs
//////                _         <- collector.clearLogs
//////                result <- fn(input).map { output =>
//////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////                }.catchAll { error =>
//////                  ZIO.succeed(
//////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////                  )
//////                }
//////                newContext = if (flattenOutput(result.output) == ()) context else Some(result.output)
//////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}"))
//////                finalResult <- if (result.succeeded) {
//////                  runSteps(rest, newContext, result.output, result :: acc)
//////                } else {
//////                  ZIO.succeed(
//////                    (result :: acc).reverse ++ rest.map(line =>
//////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////                    )
//////                  )
//////                }
//////              } yield finalResult
//////            case None =>
//////              for {
//////                collector <- ZIO.service[LogCollector]
//////                _         <- collector.log(s"No step definition matches: $line")
//////                logs      <- collector.getLogs
//////                _         <- collector.clearLogs
//////                next <- runSteps(
//////                  rest,
//////                  context,
//////                  previousOutput,
//////                  StepResult(
//////                    line,
//////                    succeeded = false,
//////                    error = Some("No step definition matches"),
//////                    output = (),
//////                    logs = logs
//////                  ) :: acc
//////                )
//////              } yield next
//////          }
//////      }
//////
//////    val effect = runSteps(lines, None, (), Nil)
//////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////  }
//////
//////  def runScenarios[R](
//////                       steps: ZIOSteps[R],
//////                       feature: Feature,
//////                       parallelism: Int
//////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////    val allExamples = feature.scenarios.flatMap(_.examples)
//////    val scenariosWithMetadata = if (allExamples.isEmpty) {
//////      feature.scenarios.map { scenario =>
//////        val baseSteps = feature.background ++ scenario.steps
//////        (baseSteps.mkString("\n"), scenario.metadata)
//////      }
//////    } else {
//////      allExamples.flatMap { row =>
//////        val exampleData = row.data
//////        feature.scenarios.map { scenario =>
//////          val baseSteps = feature.background ++ scenario.steps
//////          val parameterizedText = baseSteps.map { step =>
//////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////              val placeholderName = placeholderMatch.group(1)
//////              val placeholderType = placeholderMatch.group(2)
//////              val rawValue = exampleData.getOrElse(
//////                placeholderName,
//////                throw new IllegalArgumentException(
//////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////                )
//////              )
//////              val validatedValue = placeholderType.toLowerCase match {
//////                case "string" => rawValue
//////                case "int" =>
//////                  try rawValue.toInt.toString
//////                  catch {
//////                    case _: NumberFormatException =>
//////                      throw new IllegalArgumentException(
//////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////                      )
//////                  }
//////                case "double" =>
//////                  try rawValue.toDouble.toString
//////                  catch {
//////                    case _: NumberFormatException =>
//////                      throw new IllegalArgumentException(
//////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////                      )
//////                  }
//////                case unknown =>
//////                  throw new IllegalArgumentException(
//////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////                  )
//////              }
//////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////            }
//////          }.mkString("\n")
//////          println(s"Parameterized text for ${row.data}: $parameterizedText")
//////          (parameterizedText, scenario.metadata)
//////        }
//////      }
//////    }
//////
//////    ZIO
//////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////        val scenarioId = scenarioText.hashCode.toString
//////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////          for {
//////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////            results <- run(steps, scenarioText, metadata)
//////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////          } yield results
//////        }
//////      }
//////      .withParallelism(parallelism)
//////  }
//////
//////  def runScenarios[R](
//////                       steps: ZIOSteps[R],
//////                       scenarios: List[(String, ScenarioMetadata)],
//////                       parallelism: Int = 4
//////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////    ZIO
//////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////        val scenarioId = scenario.hashCode.toString
//////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////          for {
//////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////            results <- run(steps, scenario, metadata)
//////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////          } yield results
//////        }
//////      }
//////      .withParallelism(parallelism)
//////
//////  private def extractParams(pattern: Regex, line: String): List[String] =
//////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////
//////  private def parseParam(param: String): Any = {
//////    val trimmed = param.trim
//////    trimmed
//////  }
//////}
//////
////////package zio.bdd.core
////////
////////import zio.*
////////
////////import scala.util.matching.Regex
////////import java.time.Instant
////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////
////////case class StepResult(
////////                       step: String,
////////                       succeeded: Boolean,
////////                       error: Option[String],
////////                       output: Any,
////////                       logs: List[(String, Instant)]
////////                     )
////////
////////object ScenarioRunner {
////////  private def flattenOutput(value: Any): Any = value match {
////////    case () => ()
////////    case (a, b) =>
////////      (flattenOutput(a), flattenOutput(b)) match {
////////        case ((), ()) => ()
////////        case ((), b)  => b
////////        case (a, ())  => a
////////        case (a, b)   => (a, b)
////////      }
////////    case other => other
////////  }
////////
////////  private def combine(prev: Any, params: List[String]): Any = {
////////    val flattenedPrev = flattenOutput(prev)
////////    params match {
////////      case Nil => flattenedPrev
////////      case head :: Nil =>
////////        flattenedPrev match {
////////          case ()     => parseParam(head)
////////          case single => (single, parseParam(head))
////////        }
////////      case many =>
////////        flattenedPrev match {
////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////        }
////////    }
////////  }
////////
////////  def run[R](
////////              steps: ZIOSteps[R],
////////              scenario: String,
////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////
////////    def runSteps(
////////                  remaining: List[String],
////////                  context: Option[Any], // Context from last non-And, non-Unit output
////////                  previousOutput: Any,
////////                  acc: List[StepResult]
////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////      remaining match {
////////        case Nil => ZIO.succeed(acc.reverse)
////////        case line :: rest =>
////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////            case Some(stepDef) =>
////////              val pattern = stepDef.pattern
////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////              val params  = extractParams(pattern, line)
////////              val isAnd   = line.trim.startsWith("And")
////////              // Debug logging before determining input
////////              for {
////////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Step: $line, Context: $context, PreviousOutput: $previousOutput, Params: $params"))
////////                // Input: params if present, context if non-Unit, else previousOutput
////////                input = if (params.nonEmpty) {
////////                  combine((), params)
////////                } else if (context.isDefined && flattenOutput(context.get) != ()) {
////////                  context.get // Always use context if available and non-Unit
////////                } else {
////////                  previousOutput
////////                }
////////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Selected Input for $line: $input"))
////////                collector <- ZIO.service[LogCollector]
////////                _         <- collector.log(s"Executing: $line with input: $input")
////////                logs      <- collector.getLogs
////////                _         <- collector.clearLogs
////////                result <- fn(input).map { output =>
////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////                }.catchAll { error =>
////////                  ZIO.succeed(
////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////                  )
////////                }
////////                // Update context only for non-And steps producing non-Unit output
////////                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
////////                _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"After $line, NewContext: $newContext, NewPreviousOutput: ${result.output}"))
////////                finalResult <- if (result.succeeded) {
////////                  runSteps(rest, newContext, result.output, result :: acc)
////////                } else {
////////                  ZIO.succeed(
////////                    (result :: acc).reverse ++ rest.map(line =>
////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////                    )
////////                  )
////////                }
////////              } yield finalResult
////////            case None =>
////////              for {
////////                collector <- ZIO.service[LogCollector]
////////                _         <- collector.log(s"No step definition matches: $line")
////////                logs      <- collector.getLogs
////////                _         <- collector.clearLogs
////////                next <- runSteps(
////////                  rest,
////////                  context,
////////                  previousOutput,
////////                  StepResult(
////////                    line,
////////                    succeeded = false,
////////                    error = Some("No step definition matches"),
////////                    output = (),
////////                    logs = logs
////////                  ) :: acc
////////                )
////////              } yield next
////////          }
////////      }
////////
////////    val effect = runSteps(lines, None, (), Nil)
////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////  }
////////
////////  def runScenarios[R](
////////                       steps: ZIOSteps[R],
////////                       feature: Feature,
////////                       parallelism: Int
////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////    val allExamples = feature.scenarios.flatMap(_.examples)
////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
////////      feature.scenarios.map { scenario =>
////////        val baseSteps = feature.background ++ scenario.steps
////////        (baseSteps.mkString("\n"), scenario.metadata)
////////      }
////////    } else {
////////      allExamples.flatMap { row =>
////////        val exampleData = row.data
////////        feature.scenarios.map { scenario =>
////////          val baseSteps = feature.background ++ scenario.steps
////////          val parameterizedText = baseSteps.map { step =>
////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////              val placeholderName = placeholderMatch.group(1)
////////              val placeholderType = placeholderMatch.group(2)
////////              val rawValue = exampleData.getOrElse(
////////                placeholderName,
////////                throw new IllegalArgumentException(
////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////                )
////////              )
////////              val validatedValue = placeholderType.toLowerCase match {
////////                case "string" => rawValue
////////                case "int" =>
////////                  try rawValue.toInt.toString
////////                  catch {
////////                    case _: NumberFormatException =>
////////                      throw new IllegalArgumentException(
////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////                      )
////////                  }
////////                case "double" =>
////////                  try rawValue.toDouble.toString
////////                  catch {
////////                    case _: NumberFormatException =>
////////                      throw new IllegalArgumentException(
////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////                      )
////////                  }
////////                case unknown =>
////////                  throw new IllegalArgumentException(
////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////                  )
////////              }
////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////            }
////////          }.mkString("\n")
////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
////////          (parameterizedText, scenario.metadata)
////////        }
////////      }
////////    }
////////
////////    ZIO
////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////        val scenarioId = scenarioText.hashCode.toString
////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////          for {
////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////            results <- run(steps, scenarioText, metadata)
////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////          } yield results
////////        }
////////      }
////////      .withParallelism(parallelism)
////////  }
////////
////////  def runScenarios[R](
////////                       steps: ZIOSteps[R],
////////                       scenarios: List[(String, ScenarioMetadata)],
////////                       parallelism: Int = 4
////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////    ZIO
////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////        val scenarioId = scenario.hashCode.toString
////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////          for {
////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////            results <- run(steps, scenario, metadata)
////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////          } yield results
////////        }
////////      }
////////      .withParallelism(parallelism)
////////
////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////
////////  private def parseParam(param: String): Any = {
////////    val trimmed = param.trim
////////    trimmed
////////  }
////////}
////////
//////////package zio.bdd.core
//////////
//////////import zio.*
//////////
//////////import scala.util.matching.Regex
//////////import java.time.Instant
//////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////
//////////case class StepResult(
//////////                       step: String,
//////////                       succeeded: Boolean,
//////////                       error: Option[String],
//////////                       output: Any,
//////////                       logs: List[(String, Instant)]
//////////                     )
//////////
//////////object ScenarioRunner {
//////////  private def flattenOutput(value: Any): Any = value match {
//////////    case () => ()
//////////    case (a, b) =>
//////////      (flattenOutput(a), flattenOutput(b)) match {
//////////        case ((), ()) => ()
//////////        case ((), b)  => b
//////////        case (a, ())  => a
//////////        case (a, b)   => (a, b)
//////////      }
//////////    case other => other
//////////  }
//////////
//////////  private def combine(prev: Any, params: List[String]): Any = {
//////////    val flattenedPrev = flattenOutput(prev)
//////////    params match {
//////////      case Nil => flattenedPrev
//////////      case head :: Nil =>
//////////        flattenedPrev match {
//////////          case ()     => parseParam(head)
//////////          case single => (single, parseParam(head))
//////////        }
//////////      case many =>
//////////        flattenedPrev match {
//////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////        }
//////////    }
//////////  }
//////////
//////////  def run[R](
//////////              steps: ZIOSteps[R],
//////////              scenario: String,
//////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////
//////////    def runSteps(
//////////                  remaining: List[String],
//////////                  context: Option[Any], // Context from last non-And, non-Unit output
//////////                  previousOutput: Any,
//////////                  acc: List[StepResult]
//////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////      remaining match {
//////////        case Nil => ZIO.succeed(acc.reverse)
//////////        case line :: rest =>
//////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////            case Some(stepDef) =>
//////////              val pattern = stepDef.pattern
//////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////              val params  = extractParams(pattern, line)
//////////              val isAnd   = line.trim.startsWith("And")
//////////              // Input: params if present, context if non-Unit, else previousOutput
//////////              val input = if (params.nonEmpty) {
//////////                combine((), params)
//////////              } else if (context.isDefined && flattenOutput(context.get) != ()) {
//////////                context.get // Always use context if available and non-Unit
//////////              } else {
//////////                previousOutput
//////////              }
//////////              for {
//////////                collector <- ZIO.service[LogCollector]
//////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////                logs      <- collector.getLogs
//////////                _         <- collector.clearLogs
//////////                result <- fn(input).map { output =>
//////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////                }.catchAll { error =>
//////////                  ZIO.succeed(
//////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////                  )
//////////                }
//////////                // Update context only for non-And steps producing non-Unit output
//////////                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
//////////                finalResult <- if (result.succeeded) {
//////////                  runSteps(rest, newContext, result.output, result :: acc)
//////////                } else {
//////////                  ZIO.succeed(
//////////                    (result :: acc).reverse ++ rest.map(line =>
//////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////                    )
//////////                  )
//////////                }
//////////              } yield finalResult
//////////            case None =>
//////////              for {
//////////                collector <- ZIO.service[LogCollector]
//////////                _         <- collector.log(s"No step definition matches: $line")
//////////                logs      <- collector.getLogs
//////////                _         <- collector.clearLogs
//////////                next <- runSteps(
//////////                  rest,
//////////                  context,
//////////                  previousOutput,
//////////                  StepResult(
//////////                    line,
//////////                    succeeded = false,
//////////                    error = Some("No step definition matches"),
//////////                    output = (),
//////////                    logs = logs
//////////                  ) :: acc
//////////                )
//////////              } yield next
//////////          }
//////////      }
//////////
//////////    val effect = runSteps(lines, None, (), Nil)
//////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////  }
//////////
//////////  def runScenarios[R](
//////////                       steps: ZIOSteps[R],
//////////                       feature: Feature,
//////////                       parallelism: Int
//////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////    val allExamples = feature.scenarios.flatMap(_.examples)
//////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
//////////      feature.scenarios.map { scenario =>
//////////        val baseSteps = feature.background ++ scenario.steps
//////////        (baseSteps.mkString("\n"), scenario.metadata)
//////////      }
//////////    } else {
//////////      allExamples.flatMap { row =>
//////////        val exampleData = row.data
//////////        feature.scenarios.map { scenario =>
//////////          val baseSteps = feature.background ++ scenario.steps
//////////          val parameterizedText = baseSteps.map { step =>
//////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////////              val placeholderName = placeholderMatch.group(1)
//////////              val placeholderType = placeholderMatch.group(2)
//////////              val rawValue = exampleData.getOrElse(
//////////                placeholderName,
//////////                throw new IllegalArgumentException(
//////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////////                )
//////////              )
//////////              val validatedValue = placeholderType.toLowerCase match {
//////////                case "string" => rawValue
//////////                case "int" =>
//////////                  try rawValue.toInt.toString
//////////                  catch {
//////////                    case _: NumberFormatException =>
//////////                      throw new IllegalArgumentException(
//////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////////                      )
//////////                  }
//////////                case "double" =>
//////////                  try rawValue.toDouble.toString
//////////                  catch {
//////////                    case _: NumberFormatException =>
//////////                      throw new IllegalArgumentException(
//////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////////                      )
//////////                  }
//////////                case unknown =>
//////////                  throw new IllegalArgumentException(
//////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////////                  )
//////////              }
//////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////////            }
//////////          }.mkString("\n")
//////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
//////////          (parameterizedText, scenario.metadata)
//////////        }
//////////      }
//////////    }
//////////
//////////    ZIO
//////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////        val scenarioId = scenarioText.hashCode.toString
//////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////          for {
//////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////            results <- run(steps, scenarioText, metadata)
//////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////          } yield results
//////////        }
//////////      }
//////////      .withParallelism(parallelism)
//////////  }
//////////
//////////  def runScenarios[R](
//////////                       steps: ZIOSteps[R],
//////////                       scenarios: List[(String, ScenarioMetadata)],
//////////                       parallelism: Int = 4
//////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////    ZIO
//////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////        val scenarioId = scenario.hashCode.toString
//////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////          for {
//////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////            results <- run(steps, scenario, metadata)
//////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////          } yield results
//////////        }
//////////      }
//////////      .withParallelism(parallelism)
//////////
//////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////
//////////  private def parseParam(param: String): Any = {
//////////    val trimmed = param.trim
//////////    trimmed
//////////  }
//////////}
//////////
////////////package zio.bdd.core
////////////
////////////import zio.*
////////////
////////////import scala.util.matching.Regex
////////////import java.time.Instant
////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////
////////////case class StepResult(
////////////                       step: String,
////////////                       succeeded: Boolean,
////////////                       error: Option[String],
////////////                       output: Any,
////////////                       logs: List[(String, Instant)]
////////////                     )
////////////
////////////object ScenarioRunner {
////////////  private def flattenOutput(value: Any): Any = value match {
////////////    case () => ()
////////////    case (a, b) =>
////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////        case ((), ()) => ()
////////////        case ((), b)  => b
////////////        case (a, ())  => a
////////////        case (a, b)   => (a, b)
////////////      }
////////////    case other => other
////////////  }
////////////
////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////    val flattenedPrev = flattenOutput(prev)
////////////    params match {
////////////      case Nil => flattenedPrev
////////////      case head :: Nil =>
////////////        flattenedPrev match {
////////////          case ()     => parseParam(head)
////////////          case single => (single, parseParam(head))
////////////        }
////////////      case many =>
////////////        flattenedPrev match {
////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////        }
////////////    }
////////////  }
////////////
////////////  def run[R](
////////////              steps: ZIOSteps[R],
////////////              scenario: String,
////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////
////////////    def runSteps(
////////////                  remaining: List[String],
////////////                  context: Option[Any], // Context from last non-And, non-Unit output
////////////                  previousOutput: Any,
////////////                  acc: List[StepResult]
////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////      remaining match {
////////////        case Nil => ZIO.succeed(acc.reverse)
////////////        case line :: rest =>
////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////            case Some(stepDef) =>
////////////              val pattern = stepDef.pattern
////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////              val params  = extractParams(pattern, line)
////////////              val isAnd   = line.trim.startsWith("And")
////////////              // Input: params if present, context for non-And steps if non-Unit, else previousOutput
////////////              val input = if (params.nonEmpty) {
////////////                combine((), params)
////////////              } else if (!isAnd && context.isDefined && flattenOutput(context.get) != ()) {
////////////                context.get
////////////              } else {
////////////                previousOutput
////////////              }
////////////              for {
////////////                collector <- ZIO.service[LogCollector]
////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////                logs      <- collector.getLogs
////////////                _         <- collector.clearLogs
////////////                result <- fn(input).map { output =>
////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////                }.catchAll { error =>
////////////                  ZIO.succeed(
////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////                  )
////////////                }
////////////                // Update context only for non-And steps producing non-Unit output
////////////                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
////////////                finalResult <- if (result.succeeded) {
////////////                  runSteps(rest, newContext, result.output, result :: acc)
////////////                } else {
////////////                  ZIO.succeed(
////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////                    )
////////////                  )
////////////                }
////////////              } yield finalResult
////////////            case None =>
////////////              for {
////////////                collector <- ZIO.service[LogCollector]
////////////                _         <- collector.log(s"No step definition matches: $line")
////////////                logs      <- collector.getLogs
////////////                _         <- collector.clearLogs
////////////                next <- runSteps(
////////////                  rest,
////////////                  context,
////////////                  previousOutput,
////////////                  StepResult(
////////////                    line,
////////////                    succeeded = false,
////////////                    error = Some("No step definition matches"),
////////////                    output = (),
////////////                    logs = logs
////////////                  ) :: acc
////////////                )
////////////              } yield next
////////////          }
////////////      }
////////////
////////////    val effect = runSteps(lines, None, (), Nil)
////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////  }
////////////
////////////  def runScenarios[R](
////////////                       steps: ZIOSteps[R],
////////////                       feature: Feature,
////////////                       parallelism: Int
////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////    val allExamples = feature.scenarios.flatMap(_.examples)
////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
////////////      feature.scenarios.map { scenario =>
////////////        val baseSteps = feature.background ++ scenario.steps
////////////        (baseSteps.mkString("\n"), scenario.metadata)
////////////      }
////////////    } else {
////////////      allExamples.flatMap { row =>
////////////        val exampleData = row.data
////////////        feature.scenarios.map { scenario =>
////////////          val baseSteps = feature.background ++ scenario.steps
////////////          val parameterizedText = baseSteps.map { step =>
////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////////              val placeholderName = placeholderMatch.group(1)
////////////              val placeholderType = placeholderMatch.group(2)
////////////              val rawValue = exampleData.getOrElse(
////////////                placeholderName,
////////////                throw new IllegalArgumentException(
////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////////                )
////////////              )
////////////              val validatedValue = placeholderType.toLowerCase match {
////////////                case "string" => rawValue
////////////                case "int" =>
////////////                  try rawValue.toInt.toString
////////////                  catch {
////////////                    case _: NumberFormatException =>
////////////                      throw new IllegalArgumentException(
////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////////                      )
////////////                  }
////////////                case "double" =>
////////////                  try rawValue.toDouble.toString
////////////                  catch {
////////////                    case _: NumberFormatException =>
////////////                      throw new IllegalArgumentException(
////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////////                      )
////////////                  }
////////////                case unknown =>
////////////                  throw new IllegalArgumentException(
////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////////                  )
////////////              }
////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////////            }
////////////          }.mkString("\n")
////////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
////////////          (parameterizedText, scenario.metadata)
////////////        }
////////////      }
////////////    }
////////////
////////////    ZIO
////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////        val scenarioId = scenarioText.hashCode.toString
////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////          for {
////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////            results <- run(steps, scenarioText, metadata)
////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////          } yield results
////////////        }
////////////      }
////////////      .withParallelism(parallelism)
////////////  }
////////////
////////////  def runScenarios[R](
////////////                       steps: ZIOSteps[R],
////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////                       parallelism: Int = 4
////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////    ZIO
////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////        val scenarioId = scenario.hashCode.toString
////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////          for {
////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////            results <- run(steps, scenario, metadata)
////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////          } yield results
////////////        }
////////////      }
////////////      .withParallelism(parallelism)
////////////
////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////
////////////  private def parseParam(param: String): Any = {
////////////    val trimmed = param.trim
////////////    trimmed
////////////  }
////////////}
////////////
//////////////package zio.bdd.core
//////////////
//////////////import zio.*
//////////////
//////////////import scala.util.matching.Regex
//////////////import java.time.Instant
//////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////
//////////////case class StepResult(
//////////////                       step: String,
//////////////                       succeeded: Boolean,
//////////////                       error: Option[String],
//////////////                       output: Any,
//////////////                       logs: List[(String, Instant)]
//////////////                     )
//////////////
//////////////object ScenarioRunner {
//////////////  private def flattenOutput(value: Any): Any = value match {
//////////////    case () => ()
//////////////    case (a, b) =>
//////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////        case ((), ()) => ()
//////////////        case ((), b)  => b
//////////////        case (a, ())  => a
//////////////        case (a, b)   => (a, b)
//////////////      }
//////////////    case other => other
//////////////  }
//////////////
//////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////    val flattenedPrev = flattenOutput(prev)
//////////////    params match {
//////////////      case Nil => flattenedPrev
//////////////      case head :: Nil =>
//////////////        flattenedPrev match {
//////////////          case ()     => parseParam(head)
//////////////          case single => (single, parseParam(head))
//////////////        }
//////////////      case many =>
//////////////        flattenedPrev match {
//////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////        }
//////////////    }
//////////////  }
//////////////
//////////////  def run[R](
//////////////              steps: ZIOSteps[R],
//////////////              scenario: String,
//////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////
//////////////    def runSteps(
//////////////                  remaining: List[String],
//////////////                  context: Option[Any], // Generic context from Given/When/Then
//////////////                  previousOutput: Any,
//////////////                  acc: List[StepResult]
//////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////      remaining match {
//////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////        case line :: rest =>
//////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////            case Some(stepDef) =>
//////////////              val pattern = stepDef.pattern
//////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////              val params  = extractParams(pattern, line)
//////////////              val isAnd   = line.trim.startsWith("And")
//////////////              // Use params if present, context for non-parameterized non-And steps, else previousOutput
//////////////              val input = if (params.nonEmpty) {
//////////////                combine((), params)
//////////////              } else if (context.isDefined && flattenOutput(context.get) != () && !isAnd) {
//////////////                context.get
//////////////              } else {
//////////////                previousOutput
//////////////              }
//////////////              for {
//////////////                collector <- ZIO.service[LogCollector]
//////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////                logs      <- collector.getLogs
//////////////                _         <- collector.clearLogs
//////////////                result <- fn(input).map { output =>
//////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////                }.catchAll { error =>
//////////////                  ZIO.succeed(
//////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////                  )
//////////////                }
//////////////                // Update context only for non-And steps producing non-Unit output
//////////////                newContext = if (isAnd || flattenOutput(result.output) == ()) context else Some(result.output)
//////////////                finalResult <- if (result.succeeded) {
//////////////                  runSteps(rest, newContext, result.output, result :: acc)
//////////////                } else {
//////////////                  ZIO.succeed(
//////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////                    )
//////////////                  )
//////////////                }
//////////////              } yield finalResult
//////////////            case None =>
//////////////              for {
//////////////                collector <- ZIO.service[LogCollector]
//////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////                logs      <- collector.getLogs
//////////////                _         <- collector.clearLogs
//////////////                next <- runSteps(
//////////////                  rest,
//////////////                  context,
//////////////                  previousOutput,
//////////////                  StepResult(
//////////////                    line,
//////////////                    succeeded = false,
//////////////                    error = Some("No step definition matches"),
//////////////                    output = (),
//////////////                    logs = logs
//////////////                  ) :: acc
//////////////                )
//////////////              } yield next
//////////////          }
//////////////      }
//////////////
//////////////    val effect = runSteps(lines, None, (), Nil)
//////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////  }
//////////////
//////////////  def runScenarios[R](
//////////////                       steps: ZIOSteps[R],
//////////////                       feature: Feature,
//////////////                       parallelism: Int
//////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////    val allExamples = feature.scenarios.flatMap(_.examples)
//////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
//////////////      feature.scenarios.map { scenario =>
//////////////        val baseSteps = feature.background ++ scenario.steps
//////////////        (baseSteps.mkString("\n"), scenario.metadata)
//////////////      }
//////////////    } else {
//////////////      allExamples.flatMap { row =>
//////////////        val exampleData = row.data
//////////////        feature.scenarios.map { scenario =>
//////////////          val baseSteps = feature.background ++ scenario.steps
//////////////          val parameterizedText = baseSteps.map { step =>
//////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////////////              val placeholderName = placeholderMatch.group(1)
//////////////              val placeholderType = placeholderMatch.group(2)
//////////////              val rawValue = exampleData.getOrElse(
//////////////                placeholderName,
//////////////                throw new IllegalArgumentException(
//////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////////////                )
//////////////              )
//////////////              val validatedValue = placeholderType.toLowerCase match {
//////////////                case "string" => rawValue
//////////////                case "int" =>
//////////////                  try rawValue.toInt.toString
//////////////                  catch {
//////////////                    case _: NumberFormatException =>
//////////////                      throw new IllegalArgumentException(
//////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////////////                      )
//////////////                  }
//////////////                case "double" =>
//////////////                  try rawValue.toDouble.toString
//////////////                  catch {
//////////////                    case _: NumberFormatException =>
//////////////                      throw new IllegalArgumentException(
//////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////////////                      )
//////////////                  }
//////////////                case unknown =>
//////////////                  throw new IllegalArgumentException(
//////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////////////                  )
//////////////              }
//////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////////////            }
//////////////          }.mkString("\n")
//////////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
//////////////          (parameterizedText, scenario.metadata)
//////////////        }
//////////////      }
//////////////    }
//////////////
//////////////    ZIO
//////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////        val scenarioId = scenarioText.hashCode.toString
//////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////          for {
//////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////            results <- run(steps, scenarioText, metadata)
//////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////          } yield results
//////////////        }
//////////////      }
//////////////      .withParallelism(parallelism)
//////////////  }
//////////////
//////////////  def runScenarios[R](
//////////////                       steps: ZIOSteps[R],
//////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////                       parallelism: Int = 4
//////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////    ZIO
//////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////        val scenarioId = scenario.hashCode.toString
//////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////          for {
//////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////            results <- run(steps, scenario, metadata)
//////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////          } yield results
//////////////        }
//////////////      }
//////////////      .withParallelism(parallelism)
//////////////
//////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////
//////////////  private def parseParam(param: String): Any = {
//////////////    val trimmed = param.trim
//////////////    trimmed
//////////////  }
//////////////}
//////////////
////////////////package zio.bdd.core
////////////////
////////////////import zio.*
////////////////
////////////////import scala.util.matching.Regex
////////////////import java.time.Instant
////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////
////////////////case class StepResult(
////////////////                       step: String,
////////////////                       succeeded: Boolean,
////////////////                       error: Option[String],
////////////////                       output: Any,
////////////////                       logs: List[(String, Instant)]
////////////////                     )
////////////////
////////////////object ScenarioRunner {
////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////    case () => ()
////////////////    case (a, b) =>
////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////        case ((), ()) => ()
////////////////        case ((), b)  => b
////////////////        case (a, ())  => a
////////////////        case (a, b)   => (a, b)
////////////////      }
////////////////    case other => other
////////////////  }
////////////////
////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////    val flattenedPrev = flattenOutput(prev)
////////////////    params match {
////////////////      case Nil => flattenedPrev
////////////////      case head :: Nil =>
////////////////        flattenedPrev match {
////////////////          case ()     => parseParam(head)
////////////////          case single => (single, parseParam(head))
////////////////        }
////////////////      case many =>
////////////////        flattenedPrev match {
////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////        }
////////////////    }
////////////////  }
////////////////
////////////////  def run[R](
////////////////              steps: ZIOSteps[R],
////////////////              scenario: String,
////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////
////////////////    def runSteps(
////////////////                  remaining: List[String],
////////////////                  context: Option[Any], // Generic context for meaningful state
////////////////                  previousOutput: Any,
////////////////                  acc: List[StepResult]
////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////      remaining match {
////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////        case line :: rest =>
////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////            case Some(stepDef) =>
////////////////              val pattern = stepDef.pattern
////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////              val params  = extractParams(pattern, line)
////////////////              // Use params if present, otherwise context if non-Unit, else previousOutput
////////////////              val input = if (params.nonEmpty) {
////////////////                combine((), params)
////////////////              } else if (context.isDefined && flattenOutput(context.get) != ()) {
////////////////                context.get // Prefer context for non-parameterized steps
////////////////              } else {
////////////////                previousOutput
////////////////              }
////////////////              for {
////////////////                collector <- ZIO.service[LogCollector]
////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////                logs      <- collector.getLogs
////////////////                _         <- collector.clearLogs
////////////////                result <- fn(input).map { output =>
////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////                }.catchAll { error =>
////////////////                  ZIO.succeed(
////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////                  )
////////////////                }
////////////////                newContext = if (flattenOutput(result.output) == ()) context else Some(result.output)
////////////////                finalResult <- if (result.succeeded) {
////////////////                  runSteps(rest, newContext, result.output, result :: acc)
////////////////                } else {
////////////////                  ZIO.succeed(
////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////                    )
////////////////                  )
////////////////                }
////////////////              } yield finalResult
////////////////            case None =>
////////////////              for {
////////////////                collector <- ZIO.service[LogCollector]
////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////                logs      <- collector.getLogs
////////////////                _         <- collector.clearLogs
////////////////                next <- runSteps(
////////////////                  rest,
////////////////                  context,
////////////////                  previousOutput,
////////////////                  StepResult(
////////////////                    line,
////////////////                    succeeded = false,
////////////////                    error = Some("No step definition matches"),
////////////////                    output = (),
////////////////                    logs = logs
////////////////                  ) :: acc
////////////////                )
////////////////              } yield next
////////////////          }
////////////////      }
////////////////
////////////////    val effect = runSteps(lines, None, (), Nil)
////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////////  }
////////////////
////////////////  def runScenarios[R](
////////////////                       steps: ZIOSteps[R],
////////////////                       feature: Feature,
////////////////                       parallelism: Int
////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////    val allExamples = feature.scenarios.flatMap(_.examples)
////////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
////////////////      feature.scenarios.map { scenario =>
////////////////        val baseSteps = feature.background ++ scenario.steps
////////////////        (baseSteps.mkString("\n"), scenario.metadata)
////////////////      }
////////////////    } else {
////////////////      allExamples.flatMap { row =>
////////////////        val exampleData = row.data
////////////////        feature.scenarios.map { scenario =>
////////////////          val baseSteps = feature.background ++ scenario.steps
////////////////          val parameterizedText = baseSteps.map { step =>
////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////////////              val placeholderName = placeholderMatch.group(1)
////////////////              val placeholderType = placeholderMatch.group(2)
////////////////              val rawValue = exampleData.getOrElse(
////////////////                placeholderName,
////////////////                throw new IllegalArgumentException(
////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////////////                )
////////////////              )
////////////////              val validatedValue = placeholderType.toLowerCase match {
////////////////                case "string" => rawValue
////////////////                case "int" =>
////////////////                  try rawValue.toInt.toString
////////////////                  catch {
////////////////                    case _: NumberFormatException =>
////////////////                      throw new IllegalArgumentException(
////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////////////                      )
////////////////                  }
////////////////                case "double" =>
////////////////                  try rawValue.toDouble.toString
////////////////                  catch {
////////////////                    case _: NumberFormatException =>
////////////////                      throw new IllegalArgumentException(
////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////////////                      )
////////////////                  }
////////////////                case unknown =>
////////////////                  throw new IllegalArgumentException(
////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////////////                  )
////////////////              }
////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////////////            }
////////////////          }.mkString("\n")
////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
////////////////          (parameterizedText, scenario.metadata)
////////////////        }
////////////////      }
////////////////    }
////////////////
////////////////    ZIO
////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////          for {
////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////            results <- run(steps, scenarioText, metadata)
////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////          } yield results
////////////////        }
////////////////      }
////////////////      .withParallelism(parallelism)
////////////////  }
////////////////
////////////////  def runScenarios[R](
////////////////                       steps: ZIOSteps[R],
////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////                       parallelism: Int = 4
////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////    ZIO
////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////        val scenarioId = scenario.hashCode.toString
////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////          for {
////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////            results <- run(steps, scenario, metadata)
////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////          } yield results
////////////////        }
////////////////      }
////////////////      .withParallelism(parallelism)
////////////////
////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////
////////////////  private def parseParam(param: String): Any = {
////////////////    val trimmed = param.trim
////////////////    trimmed
////////////////  }
////////////////}
////////////////
//////////////////package zio.bdd.core
//////////////////
//////////////////import zio.*
//////////////////
//////////////////import scala.util.matching.Regex
//////////////////import java.time.Instant
//////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////
//////////////////case class StepResult(
//////////////////                       step: String,
//////////////////                       succeeded: Boolean,
//////////////////                       error: Option[String],
//////////////////                       output: Any,
//////////////////                       logs: List[(String, Instant)]
//////////////////                     )
//////////////////
//////////////////object ScenarioRunner {
//////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////    case () => ()
//////////////////    case (a, b) =>
//////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////        case ((), ()) => ()
//////////////////        case ((), b)  => b
//////////////////        case (a, ())  => a
//////////////////        case (a, b)   => (a, b)
//////////////////      }
//////////////////    case other => other
//////////////////  }
//////////////////
//////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////    params match {
//////////////////      case Nil => flattenedPrev
//////////////////      case head :: Nil =>
//////////////////        flattenedPrev match {
//////////////////          case ()     => parseParam(head)
//////////////////          case single => (single, parseParam(head))
//////////////////        }
//////////////////      case many =>
//////////////////        flattenedPrev match {
//////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////        }
//////////////////    }
//////////////////  }
//////////////////
//////////////////  def run[R](
//////////////////              steps: ZIOSteps[R],
//////////////////              scenario: String,
//////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////
//////////////////    def runSteps(
//////////////////                  remaining: List[String],
//////////////////                  context: Option[Any], // Generic context for meaningful state
//////////////////                  previousOutput: Any,
//////////////////                  acc: List[StepResult]
//////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////      remaining match {
//////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////        case line :: rest =>
//////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////            case Some(stepDef) =>
//////////////////              val pattern = stepDef.pattern
//////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////              val params  = extractParams(pattern, line)
//////////////////              // Determine input: params if present, context if non-Unit and step expects it, else previousOutput
//////////////////              val input = if (params.nonEmpty) {
//////////////////                combine((), params)
//////////////////              } else if (context.isDefined && flattenOutput(context.get) != () &&
//////////////////                (line.contains("the user requests a password reset") || line.contains("an email should be sent to"))) {
//////////////////                context.get // Prefer context for steps expecting structured input
//////////////////              } else {
//////////////////                previousOutput
//////////////////              }
//////////////////              for {
//////////////////                collector <- ZIO.service[LogCollector]
//////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////                logs      <- collector.getLogs
//////////////////                _         <- collector.clearLogs
//////////////////                result <- fn(input).map { output =>
//////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////////                }.catchAll { error =>
//////////////////                  ZIO.succeed(
//////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////////                  )
//////////////////                }
//////////////////                newContext = if (flattenOutput(result.output) == ()) context else Some(result.output)
//////////////////                finalResult <- if (result.succeeded) {
//////////////////                  runSteps(rest, newContext, result.output, result :: acc)
//////////////////                } else {
//////////////////                  ZIO.succeed(
//////////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////////                    )
//////////////////                  )
//////////////////                }
//////////////////              } yield finalResult
//////////////////            case None =>
//////////////////              for {
//////////////////                collector <- ZIO.service[LogCollector]
//////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////                logs      <- collector.getLogs
//////////////////                _         <- collector.clearLogs
//////////////////                next <- runSteps(
//////////////////                  rest,
//////////////////                  context,
//////////////////                  previousOutput,
//////////////////                  StepResult(
//////////////////                    line,
//////////////////                    succeeded = false,
//////////////////                    error = Some("No step definition matches"),
//////////////////                    output = (),
//////////////////                    logs = logs
//////////////////                  ) :: acc
//////////////////                )
//////////////////              } yield next
//////////////////          }
//////////////////      }
//////////////////
//////////////////    val effect = runSteps(lines, None, (), Nil)
//////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////////  }
//////////////////
//////////////////  def runScenarios[R](
//////////////////                       steps: ZIOSteps[R],
//////////////////                       feature: Feature,
//////////////////                       parallelism: Int
//////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////    val allExamples = feature.scenarios.flatMap(_.examples)
//////////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
//////////////////      feature.scenarios.map { scenario =>
//////////////////        val baseSteps = feature.background ++ scenario.steps
//////////////////        (baseSteps.mkString("\n"), scenario.metadata)
//////////////////      }
//////////////////    } else {
//////////////////      allExamples.flatMap { row =>
//////////////////        val exampleData = row.data
//////////////////        feature.scenarios.map { scenario =>
//////////////////          val baseSteps = feature.background ++ scenario.steps
//////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////////////////              val placeholderName = placeholderMatch.group(1)
//////////////////              val placeholderType = placeholderMatch.group(2)
//////////////////              val rawValue = exampleData.getOrElse(
//////////////////                placeholderName,
//////////////////                throw new IllegalArgumentException(
//////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////////////////                )
//////////////////              )
//////////////////              val validatedValue = placeholderType.toLowerCase match {
//////////////////                case "string" => rawValue
//////////////////                case "int" =>
//////////////////                  try rawValue.toInt.toString
//////////////////                  catch {
//////////////////                    case _: NumberFormatException =>
//////////////////                      throw new IllegalArgumentException(
//////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////////////////                      )
//////////////////                  }
//////////////////                case "double" =>
//////////////////                  try rawValue.toDouble.toString
//////////////////                  catch {
//////////////////                    case _: NumberFormatException =>
//////////////////                      throw new IllegalArgumentException(
//////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////////////////                      )
//////////////////                  }
//////////////////                case unknown =>
//////////////////                  throw new IllegalArgumentException(
//////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////////////////                  )
//////////////////              }
//////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////////////////            }
//////////////////          }.mkString("\n")
//////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
//////////////////          (parameterizedText, scenario.metadata)
//////////////////        }
//////////////////      }
//////////////////    }
//////////////////
//////////////////    ZIO
//////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////          for {
//////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////            results <- run(steps, scenarioText, metadata)
//////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////          } yield results
//////////////////        }
//////////////////      }
//////////////////      .withParallelism(parallelism)
//////////////////  }
//////////////////
//////////////////  def runScenarios[R](
//////////////////                       steps: ZIOSteps[R],
//////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////                       parallelism: Int = 4
//////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////    ZIO
//////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////        val scenarioId = scenario.hashCode.toString
//////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////          for {
//////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////            results <- run(steps, scenario, metadata)
//////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////          } yield results
//////////////////        }
//////////////////      }
//////////////////      .withParallelism(parallelism)
//////////////////
//////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////
//////////////////  private def parseParam(param: String): Any = {
//////////////////    val trimmed = param.trim
//////////////////    trimmed
//////////////////  }
//////////////////}
//////////////////
////////////////////package zio.bdd.core
////////////////////
////////////////////import zio.*
////////////////////
////////////////////import scala.util.matching.Regex
////////////////////import java.time.Instant
////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////
////////////////////case class StepResult(
////////////////////                       step: String,
////////////////////                       succeeded: Boolean,
////////////////////                       error: Option[String],
////////////////////                       output: Any,
////////////////////                       logs: List[(String, Instant)]
////////////////////                     )
////////////////////
////////////////////object ScenarioRunner {
////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////    case () => ()
////////////////////    case (a, b) =>
////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////        case ((), ()) => ()
////////////////////        case ((), b)  => b
////////////////////        case (a, ())  => a
////////////////////        case (a, b)   => (a, b)
////////////////////      }
////////////////////    case other => other
////////////////////  }
////////////////////
////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////    params match {
////////////////////      case Nil => flattenedPrev
////////////////////      case head :: Nil =>
////////////////////        flattenedPrev match {
////////////////////          case ()     => parseParam(head)
////////////////////          case single => (single, parseParam(head))
////////////////////        }
////////////////////      case many =>
////////////////////        flattenedPrev match {
////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////        }
////////////////////    }
////////////////////  }
////////////////////
////////////////////  def run[R](
////////////////////              steps: ZIOSteps[R],
////////////////////              scenario: String,
////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////
////////////////////    def runSteps(
////////////////////                  remaining: List[String],
////////////////////                  context: Option[Any], // Generic context to hold any meaningful state
////////////////////                  previousOutput: Any,
////////////////////                  acc: List[StepResult]
////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////      remaining match {
////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////        case line :: rest =>
////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////            case Some(stepDef) =>
////////////////////              val pattern = stepDef.pattern
////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////              val params  = extractParams(pattern, line)
////////////////////              // Determine input: params if present, context if non-Unit and applicable, else previousOutput
////////////////////              val input = if (params.nonEmpty) {
////////////////////                combine((), params)
////////////////////              } else if (context.isDefined && flattenOutput(context.get) != ()) {
////////////////////                context.get // Use context if it’s not Unit
////////////////////              } else {
////////////////////                previousOutput
////////////////////              }
////////////////////              for {
////////////////////                collector <- ZIO.service[LogCollector]
////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////                logs      <- collector.getLogs
////////////////////                _         <- collector.clearLogs
////////////////////                result <- fn(input).map { output =>
////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////////                }.catchAll { error =>
////////////////////                  ZIO.succeed(
////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////////                  )
////////////////////                }
////////////////////                // Update context only if the output isn’t Unit (preserves meaningful state)
////////////////////                newContext = if (flattenOutput(result.output) == ()) context else Some(result.output)
////////////////////                finalResult <- if (result.succeeded) {
////////////////////                  runSteps(rest, newContext, result.output, result :: acc)
////////////////////                } else {
////////////////////                  ZIO.succeed(
////////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////////                    )
////////////////////                  )
////////////////////                }
////////////////////              } yield finalResult
////////////////////            case None =>
////////////////////              for {
////////////////////                collector <- ZIO.service[LogCollector]
////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////                logs      <- collector.getLogs
////////////////////                _         <- collector.clearLogs
////////////////////                next <- runSteps(
////////////////////                  rest,
////////////////////                  context,
////////////////////                  previousOutput,
////////////////////                  StepResult(
////////////////////                    line,
////////////////////                    succeeded = false,
////////////////////                    error = Some("No step definition matches"),
////////////////////                    output = (),
////////////////////                    logs = logs
////////////////////                  ) :: acc
////////////////////                )
////////////////////              } yield next
////////////////////          }
////////////////////      }
////////////////////
////////////////////    val effect = runSteps(lines, None, (), Nil)
////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////////////  }
////////////////////
////////////////////  def runScenarios[R](
////////////////////                       steps: ZIOSteps[R],
////////////////////                       feature: Feature,
////////////////////                       parallelism: Int
////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////    val allExamples = feature.scenarios.flatMap(_.examples)
////////////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
////////////////////      feature.scenarios.map { scenario =>
////////////////////        val baseSteps = feature.background ++ scenario.steps
////////////////////        (baseSteps.mkString("\n"), scenario.metadata)
////////////////////      }
////////////////////    } else {
////////////////////      allExamples.flatMap { row =>
////////////////////        val exampleData = row.data
////////////////////        feature.scenarios.map { scenario =>
////////////////////          val baseSteps = feature.background ++ scenario.steps
////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
////////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////////////////              val placeholderName = placeholderMatch.group(1)
////////////////////              val placeholderType = placeholderMatch.group(2)
////////////////////              val rawValue = exampleData.getOrElse(
////////////////////                placeholderName,
////////////////////                throw new IllegalArgumentException(
////////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////////////////                )
////////////////////              )
////////////////////              val validatedValue = placeholderType.toLowerCase match {
////////////////////                case "string" => rawValue
////////////////////                case "int" =>
////////////////////                  try rawValue.toInt.toString
////////////////////                  catch {
////////////////////                    case _: NumberFormatException =>
////////////////////                      throw new IllegalArgumentException(
////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////////////////                      )
////////////////////                  }
////////////////////                case "double" =>
////////////////////                  try rawValue.toDouble.toString
////////////////////                  catch {
////////////////////                    case _: NumberFormatException =>
////////////////////                      throw new IllegalArgumentException(
////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////////////////                      )
////////////////////                  }
////////////////////                case unknown =>
////////////////////                  throw new IllegalArgumentException(
////////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////////////////                  )
////////////////////              }
////////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////////////////            }
////////////////////          }.mkString("\n")
////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText")
////////////////////          (parameterizedText, scenario.metadata)
////////////////////        }
////////////////////      }
////////////////////    }
////////////////////
////////////////////    ZIO
////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////          for {
////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////          } yield results
////////////////////        }
////////////////////      }
////////////////////      .withParallelism(parallelism)
////////////////////  }
////////////////////
////////////////////  def runScenarios[R](
////////////////////                       steps: ZIOSteps[R],
////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////                       parallelism: Int = 4
////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////    ZIO
////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////          for {
////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////            results <- run(steps, scenario, metadata)
////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////          } yield results
////////////////////        }
////////////////////      }
////////////////////      .withParallelism(parallelism)
////////////////////
////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////
////////////////////  private def parseParam(param: String): Any = {
////////////////////    val trimmed = param.trim
////////////////////    trimmed
////////////////////  }
////////////////////}
////////////////////
//////////////////////package zio.bdd.core
//////////////////////
//////////////////////import zio.*
//////////////////////
//////////////////////import scala.util.matching.Regex
//////////////////////import java.time.Instant
//////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////
//////////////////////case class StepResult(
//////////////////////                       step: String,
//////////////////////                       succeeded: Boolean,
//////////////////////                       error: Option[String],
//////////////////////                       output: Any,
//////////////////////                       logs: List[(String, Instant)]
//////////////////////                     )
//////////////////////
//////////////////////object ScenarioRunner {
//////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////    case () => ()
//////////////////////    case (a, b) =>
//////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////        case ((), ()) => ()
//////////////////////        case ((), b)  => b
//////////////////////        case (a, ())  => a
//////////////////////        case (a, b)   => (a, b)
//////////////////////      }
//////////////////////    case other => other
//////////////////////  }
//////////////////////
//////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////    params match {
//////////////////////      case Nil => flattenedPrev
//////////////////////      case head :: Nil =>
//////////////////////        flattenedPrev match {
//////////////////////          case ()     => parseParam(head)
//////////////////////          case single => (single, parseParam(head))
//////////////////////        }
//////////////////////      case many =>
//////////////////////        flattenedPrev match {
//////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////        }
//////////////////////    }
//////////////////////  }
//////////////////////
//////////////////////  def run[R](
//////////////////////              steps: ZIOSteps[R],
//////////////////////              scenario: String,
//////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////
//////////////////////    def runSteps(
//////////////////////                  remaining: List[String],
//////////////////////                  previousOutput: Any,
//////////////////////                  acc: List[StepResult]
//////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////      remaining match {
//////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////        case line :: rest =>
//////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////            case Some(stepDef) =>
//////////////////////              val pattern = stepDef.pattern
//////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////              val params  = extractParams(pattern, line)
//////////////////////              val input   = if (params.nonEmpty) combine((), params) else previousOutput
//////////////////////              for {
//////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////                logs      <- collector.getLogs
//////////////////////                _         <- collector.clearLogs
//////////////////////                result <- fn(input).map { output =>
//////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////////////                }.catchAll { error =>
//////////////////////                  ZIO.succeed(
//////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////////////                  )
//////////////////////                }
//////////////////////                finalResult <- if (result.succeeded) {
//////////////////////                  runSteps(rest, result.output, result :: acc)
//////////////////////                } else {
//////////////////////                  ZIO.succeed(
//////////////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////////////                    )
//////////////////////                  )
//////////////////////                }
//////////////////////              } yield finalResult
//////////////////////            case None =>
//////////////////////              for {
//////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////                logs      <- collector.getLogs
//////////////////////                _         <- collector.clearLogs
//////////////////////                next <- runSteps(
//////////////////////                  rest,
//////////////////////                  previousOutput,
//////////////////////                  StepResult(
//////////////////////                    line,
//////////////////////                    succeeded = false,
//////////////////////                    error = Some("No step definition matches"),
//////////////////////                    output = (),
//////////////////////                    logs = logs
//////////////////////                  ) :: acc
//////////////////////                )
//////////////////////              } yield next
//////////////////////          }
//////////////////////      }
//////////////////////
//////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////////////  }
//////////////////////
//////////////////////  def runScenarios[R](
//////////////////////                       steps: ZIOSteps[R],
//////////////////////                       feature: Feature,
//////////////////////                       parallelism: Int
//////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////    // Collect all examples from any scenario in the feature
//////////////////////    val allExamples = feature.scenarios.flatMap(_.examples)
//////////////////////    val scenariosWithMetadata = if (allExamples.isEmpty) {
//////////////////////      // No examples: run each scenario as-is
//////////////////////      feature.scenarios.map { scenario =>
//////////////////////        val baseSteps = feature.background ++ scenario.steps
//////////////////////        (baseSteps.mkString("\n"), scenario.metadata)
//////////////////////      }
//////////////////////    } else {
//////////////////////      // Apply examples to all scenarios
//////////////////////      allExamples.flatMap { row =>
//////////////////////        val exampleData = row.data
//////////////////////        feature.scenarios.map { scenario =>
//////////////////////          val baseSteps = feature.background ++ scenario.steps
//////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
//////////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////////////////////              val placeholderName = placeholderMatch.group(1)
//////////////////////              val placeholderType = placeholderMatch.group(2)
//////////////////////              val rawValue = exampleData.getOrElse(
//////////////////////                placeholderName,
//////////////////////                throw new IllegalArgumentException(
//////////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////////////////////                )
//////////////////////              )
//////////////////////              val validatedValue = placeholderType.toLowerCase match {
//////////////////////                case "string" => rawValue
//////////////////////                case "int" =>
//////////////////////                  try rawValue.toInt.toString
//////////////////////                  catch {
//////////////////////                    case _: NumberFormatException =>
//////////////////////                      throw new IllegalArgumentException(
//////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////////////////////                      )
//////////////////////                  }
//////////////////////                case "double" =>
//////////////////////                  try rawValue.toDouble.toString
//////////////////////                  catch {
//////////////////////                    case _: NumberFormatException =>
//////////////////////                      throw new IllegalArgumentException(
//////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////////////////////                      )
//////////////////////                  }
//////////////////////                case unknown =>
//////////////////////                  throw new IllegalArgumentException(
//////////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////////////////////                  )
//////////////////////              }
//////////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////////////////////            }
//////////////////////          }.mkString("\n")
//////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
//////////////////////          (parameterizedText, scenario.metadata)
//////////////////////        }
//////////////////////      }
//////////////////////    }
//////////////////////
//////////////////////    ZIO
//////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////          for {
//////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////          } yield results
//////////////////////        }
//////////////////////      }
//////////////////////      .withParallelism(parallelism)
//////////////////////  }
//////////////////////
//////////////////////  def runScenarios[R](
//////////////////////                       steps: ZIOSteps[R],
//////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////                       parallelism: Int = 4
//////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////    ZIO
//////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////          for {
//////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////            results <- run(steps, scenario, metadata)
//////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////          } yield results
//////////////////////        }
//////////////////////      }
//////////////////////      .withParallelism(parallelism)
//////////////////////
//////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////
//////////////////////  private def parseParam(param: String): Any = {
//////////////////////    val trimmed = param.trim
//////////////////////    trimmed // Return as String, let step definitions handle type validation
//////////////////////  }
//////////////////////}
//////////////////////
////////////////////////package zio.bdd.core
////////////////////////
////////////////////////import zio.*
////////////////////////
////////////////////////import scala.util.matching.Regex
////////////////////////import java.time.Instant
////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////
////////////////////////case class StepResult(
////////////////////////                       step: String,
////////////////////////                       succeeded: Boolean,
////////////////////////                       error: Option[String],
////////////////////////                       output: Any,
////////////////////////                       logs: List[(String, Instant)]
////////////////////////                     )
////////////////////////
////////////////////////object ScenarioRunner {
////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////    case () => ()
////////////////////////    case (a, b) =>
////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////        case ((), ()) => ()
////////////////////////        case ((), b)  => b
////////////////////////        case (a, ())  => a
////////////////////////        case (a, b)   => (a, b)
////////////////////////      }
////////////////////////    case other => other
////////////////////////  }
////////////////////////
////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////    params match {
////////////////////////      case Nil => flattenedPrev
////////////////////////      case head :: Nil =>
////////////////////////        flattenedPrev match {
////////////////////////          case ()     => parseParam(head)
////////////////////////          case single => (single, parseParam(head))
////////////////////////        }
////////////////////////      case many =>
////////////////////////        flattenedPrev match {
////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////        }
////////////////////////    }
////////////////////////  }
////////////////////////
////////////////////////  def run[R](
////////////////////////              steps: ZIOSteps[R],
////////////////////////              scenario: String,
////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////
////////////////////////    def runSteps(
////////////////////////                  remaining: List[String],
////////////////////////                  previousOutput: Any,
////////////////////////                  acc: List[StepResult]
////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////      remaining match {
////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////        case line :: rest =>
////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////            case Some(stepDef) =>
////////////////////////              val pattern = stepDef.pattern
////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////              val params  = extractParams(pattern, line)
////////////////////////              val input   = if (params.nonEmpty) combine((), params) else previousOutput
////////////////////////              for {
////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////                logs      <- collector.getLogs
////////////////////////                _         <- collector.clearLogs
////////////////////////                result <- fn(input).map { output =>
////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////////////                }.catchAll { error =>
////////////////////////                  ZIO.succeed(
////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////////////                  )
////////////////////////                }
////////////////////////                finalResult <- if (result.succeeded) {
////////////////////////                  runSteps(rest, result.output, result :: acc)
////////////////////////                } else {
////////////////////////                  ZIO.succeed(
////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////////////                    )
////////////////////////                  )
////////////////////////                }
////////////////////////              } yield finalResult
////////////////////////            case None =>
////////////////////////              for {
////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////                logs      <- collector.getLogs
////////////////////////                _         <- collector.clearLogs
////////////////////////                next <- runSteps(
////////////////////////                  rest,
////////////////////////                  previousOutput,
////////////////////////                  StepResult(
////////////////////////                    line,
////////////////////////                    succeeded = false,
////////////////////////                    error = Some("No step definition matches"),
////////////////////////                    output = (),
////////////////////////                    logs = logs
////////////////////////                  ) :: acc
////////////////////////                )
////////////////////////              } yield next
////////////////////////          }
////////////////////////      }
////////////////////////
////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////////////////  }
////////////////////////
////////////////////////  def runScenarios[R](
////////////////////////                       steps: ZIOSteps[R],
////////////////////////                       feature: Feature,
////////////////////////                       parallelism: Int
////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////      val baseSteps = feature.background ++ scenario.steps
////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
////////////////////////      } else {
////////////////////////        scenario.examples.map { row =>
////////////////////////          val exampleData = row.data // Map of column names to values
////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r // Match {name:String}, {age:Int}, etc.
////////////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////////////////////              val placeholderName = placeholderMatch.group(1) // e.g., "name", "age"
////////////////////////              val placeholderType = placeholderMatch.group(2) // e.g., "String", "Int"
////////////////////////              val rawValue = exampleData.getOrElse(
////////////////////////                placeholderName,
////////////////////////                throw new IllegalArgumentException(
////////////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////////////////////                )
////////////////////////              )
////////////////////////              val validatedValue = placeholderType.toLowerCase match {
////////////////////////                case "string" => rawValue
////////////////////////                case "int" =>
////////////////////////                  try rawValue.toInt.toString
////////////////////////                  catch {
////////////////////////                    case _: NumberFormatException =>
////////////////////////                      throw new IllegalArgumentException(
////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////////////////////                      )
////////////////////////                  }
////////////////////////                case "double" =>
////////////////////////                  try rawValue.toDouble.toString
////////////////////////                  catch {
////////////////////////                    case _: NumberFormatException =>
////////////////////////                      throw new IllegalArgumentException(
////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////////////////////                      )
////////////////////////                  }
////////////////////////                case unknown =>
////////////////////////                  throw new IllegalArgumentException(
////////////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////////////////////                  )
////////////////////////              }
////////////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////////////////////            }
////////////////////////          }.mkString("\n")
////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////        }
////////////////////////      }
////////////////////////    }
////////////////////////
////////////////////////    ZIO
////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////          for {
////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////          } yield results
////////////////////////        }
////////////////////////      }
////////////////////////      .withParallelism(parallelism)
////////////////////////  }
////////////////////////
////////////////////////  def runScenarios[R](
////////////////////////                       steps: ZIOSteps[R],
////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////                       parallelism: Int = 4
////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////    ZIO
////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////          for {
////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////          } yield results
////////////////////////        }
////////////////////////      }
////////////////////////      .withParallelism(parallelism)
////////////////////////
////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////
////////////////////////  private def parseParam(param: String): Any = {
////////////////////////    val trimmed = param.trim
////////////////////////    trimmed // Return as String, let step definitions handle type validation
////////////////////////  }
////////////////////////}
////////////////////////
////////////////////////
////////////////////////
//////////////////////////package zio.bdd.core
//////////////////////////
//////////////////////////import zio.*
//////////////////////////
//////////////////////////import scala.util.matching.Regex
//////////////////////////import java.time.Instant
//////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////
//////////////////////////case class StepResult(
//////////////////////////                       step: String,
//////////////////////////                       succeeded: Boolean,
//////////////////////////                       error: Option[String],
//////////////////////////                       output: Any,
//////////////////////////                       logs: List[(String, Instant)]
//////////////////////////                     )
//////////////////////////
//////////////////////////object ScenarioRunner {
//////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////    case () => ()
//////////////////////////    case (a, b) =>
//////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////        case ((), ()) => ()
//////////////////////////        case ((), b)  => b
//////////////////////////        case (a, ())  => a
//////////////////////////        case (a, b)   => (a, b)
//////////////////////////      }
//////////////////////////    case other => other
//////////////////////////  }
//////////////////////////
//////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////    params match {
//////////////////////////      case Nil => flattenedPrev
//////////////////////////      case head :: Nil =>
//////////////////////////        flattenedPrev match {
//////////////////////////          case ()     => parseParam(head)
//////////////////////////          case single => (single, parseParam(head))
//////////////////////////        }
//////////////////////////      case many =>
//////////////////////////        flattenedPrev match {
//////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////        }
//////////////////////////    }
//////////////////////////  }
//////////////////////////
//////////////////////////  def run[R](
//////////////////////////              steps: ZIOSteps[R],
//////////////////////////              scenario: String,
//////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////
//////////////////////////    def runSteps(
//////////////////////////                  remaining: List[String],
//////////////////////////                  previousOutput: Any,
//////////////////////////                  acc: List[StepResult]
//////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////      remaining match {
//////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////        case line :: rest =>
//////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////            case Some(stepDef) =>
//////////////////////////              val pattern = stepDef.pattern
//////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////              // Use params directly if present (e.g., from Examples), otherwise combine with previous output
//////////////////////////              val input   = if (params.nonEmpty) combine((), params) else previousOutput
//////////////////////////              for {
//////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////                logs      <- collector.getLogs
//////////////////////////                _         <- collector.clearLogs
//////////////////////////                result <- fn(input).map { output =>
//////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////////////////                }.catchAll { error =>
//////////////////////////                  ZIO.succeed(
//////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////////////////                  )
//////////////////////////                }
//////////////////////////                finalResult <- if (result.succeeded) {
//////////////////////////                  runSteps(rest, result.output, result :: acc)
//////////////////////////                } else {
//////////////////////////                  ZIO.succeed(
//////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////////////////                    )
//////////////////////////                  )
//////////////////////////                }
//////////////////////////              } yield finalResult
//////////////////////////            case None =>
//////////////////////////              for {
//////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////                logs      <- collector.getLogs
//////////////////////////                _         <- collector.clearLogs
//////////////////////////                next <- runSteps(
//////////////////////////                  rest,
//////////////////////////                  previousOutput,
//////////////////////////                  StepResult(
//////////////////////////                    line,
//////////////////////////                    succeeded = false,
//////////////////////////                    error = Some("No step definition matches"),
//////////////////////////                    output = (),
//////////////////////////                    logs = logs
//////////////////////////                  ) :: acc
//////////////////////////                )
//////////////////////////              } yield next
//////////////////////////          }
//////////////////////////      }
//////////////////////////
//////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////////////////  }
//////////////////////////
//////////////////////////  def runScenarios[R](
//////////////////////////                       steps: ZIOSteps[R],
//////////////////////////                       feature: Feature,
//////////////////////////                       parallelism: Int
//////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////      val baseSteps = feature.background ++ scenario.steps
//////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
//////////////////////////      } else {
//////////////////////////        scenario.examples.map { row =>
//////////////////////////          val exampleData = row.data // Map of column names to values
//////////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r // Match {name:String}, {age:Int}, etc.
//////////////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
//////////////////////////              val placeholderName = placeholderMatch.group(1) // e.g., "name", "age"
//////////////////////////              val placeholderType = placeholderMatch.group(2) // e.g., "String", "Int"
//////////////////////////              val rawValue = exampleData.getOrElse(
//////////////////////////                placeholderName,
//////////////////////////                throw new IllegalArgumentException(
//////////////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
//////////////////////////                )
//////////////////////////              )
//////////////////////////              val validatedValue = placeholderType.toLowerCase match {
//////////////////////////                case "string" => rawValue
//////////////////////////                case "int" =>
//////////////////////////                  try rawValue.toInt.toString
//////////////////////////                  catch {
//////////////////////////                    case _: NumberFormatException =>
//////////////////////////                      throw new IllegalArgumentException(
//////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
//////////////////////////                      )
//////////////////////////                  }
//////////////////////////                case "double" =>
//////////////////////////                  try rawValue.toDouble.toString
//////////////////////////                  catch {
//////////////////////////                    case _: NumberFormatException =>
//////////////////////////                      throw new IllegalArgumentException(
//////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
//////////////////////////                      )
//////////////////////////                  }
//////////////////////////                case unknown =>
//////////////////////////                  throw new IllegalArgumentException(
//////////////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
//////////////////////////                  )
//////////////////////////              }
//////////////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
//////////////////////////            }
//////////////////////////          }.mkString("\n")
//////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
//////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////        }
//////////////////////////      }
//////////////////////////    }
//////////////////////////
//////////////////////////    ZIO
//////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////          for {
//////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////          } yield results
//////////////////////////        }
//////////////////////////      }
//////////////////////////      .withParallelism(parallelism)
//////////////////////////  }
//////////////////////////
//////////////////////////  def runScenarios[R](
//////////////////////////                       steps: ZIOSteps[R],
//////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////                       parallelism: Int = 4
//////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////    ZIO
//////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////          for {
//////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////          } yield results
//////////////////////////        }
//////////////////////////      }
//////////////////////////      .withParallelism(parallelism)
//////////////////////////
//////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////
//////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////    val trimmed = param.trim
//////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////    } else {
//////////////////////////      trimmed
//////////////////////////    }
//////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////    else unquoted
//////////////////////////  }
//////////////////////////}
//////////////////////////
//////////////////////////trait LogCollector {
//////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////}
//////////////////////////
//////////////////////////object LogCollector {
//////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////    for {
//////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////    } yield new LogCollector {
//////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////    }
//////////////////////////  }
//////////////////////////}
//////////////////////////
////////////////////////////package zio.bdd.core
////////////////////////////
////////////////////////////import zio.*
////////////////////////////
////////////////////////////import scala.util.matching.Regex
////////////////////////////import java.time.Instant
////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////////
////////////////////////////case class StepResult(
////////////////////////////                       step: String,
////////////////////////////                       succeeded: Boolean,
////////////////////////////                       error: Option[String],
////////////////////////////                       output: Any,
////////////////////////////                       logs: List[(String, Instant)]
////////////////////////////                     )
////////////////////////////
////////////////////////////object ScenarioRunner {
////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////    case () => ()
////////////////////////////    case (a, b) =>
////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////        case ((), ()) => ()
////////////////////////////        case ((), b)  => b
////////////////////////////        case (a, ())  => a
////////////////////////////        case (a, b)   => (a, b)
////////////////////////////      }
////////////////////////////    case other => other
////////////////////////////  }
////////////////////////////
////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////    params match {
////////////////////////////      case Nil => flattenedPrev
////////////////////////////      case head :: Nil =>
////////////////////////////        flattenedPrev match {
////////////////////////////          case ()     => parseParam(head)
////////////////////////////          case single => (single, parseParam(head))
////////////////////////////        }
////////////////////////////      case many =>
////////////////////////////        flattenedPrev match {
////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////        }
////////////////////////////    }
////////////////////////////  }
////////////////////////////
////////////////////////////  def run[R](
////////////////////////////              steps: ZIOSteps[R],
////////////////////////////              scenario: String,
////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////
////////////////////////////    def runSteps(
////////////////////////////                  remaining: List[String],
////////////////////////////                  previousOutput: Any,
////////////////////////////                  acc: List[StepResult]
////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////      remaining match {
////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////        case line :: rest =>
////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////            case Some(stepDef) =>
////////////////////////////              val pattern = stepDef.pattern
////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////              for {
////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////                logs      <- collector.getLogs
////////////////////////////                _         <- collector.clearLogs
////////////////////////////                result <- fn(input).map { output =>
////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////////////////                }.catchAll { error =>
////////////////////////////                  ZIO.succeed(
////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////////////////                  )
////////////////////////////                }
////////////////////////////                finalResult <- if (result.succeeded) {
////////////////////////////                  runSteps(rest, result.output, result :: acc)
////////////////////////////                } else {
////////////////////////////                  ZIO.succeed(
////////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////////////////                    )
////////////////////////////                  )
////////////////////////////                }
////////////////////////////              } yield finalResult
////////////////////////////            case None =>
////////////////////////////              for {
////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////                logs      <- collector.getLogs
////////////////////////////                _         <- collector.clearLogs
////////////////////////////                next <- runSteps(
////////////////////////////                  rest,
////////////////////////////                  previousOutput,
////////////////////////////                  StepResult(
////////////////////////////                    line,
////////////////////////////                    succeeded = false,
////////////////////////////                    error = Some("No step definition matches"),
////////////////////////////                    output = (),
////////////////////////////                    logs = logs
////////////////////////////                  ) :: acc
////////////////////////////                )
////////////////////////////              } yield next
////////////////////////////          }
////////////////////////////      }
////////////////////////////
////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////////////////////  }
////////////////////////////
////////////////////////////  def runScenarios[R](
////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////                       feature: Feature,
////////////////////////////                       parallelism: Int
////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////////      val baseSteps = feature.background ++ scenario.steps
////////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
////////////////////////////      } else {
////////////////////////////        scenario.examples.map { row =>
////////////////////////////          val exampleData = row.data // Map of column names to values
////////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////////            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r // Match {name:String}, {age:Int}, etc.
////////////////////////////            placeholderPattern.findAllMatchIn(step).foldLeft(step) { (currentStep, placeholderMatch) =>
////////////////////////////              val placeholderName = placeholderMatch.group(1) // e.g., "name", "age"
////////////////////////////              val placeholderType = placeholderMatch.group(2) // e.g., "String", "Int"
////////////////////////////              val rawValue = exampleData.getOrElse(
////////////////////////////                placeholderName,
////////////////////////////                throw new IllegalArgumentException(
////////////////////////////                  s"Placeholder '{$placeholderName}' in step '$step' not found in Examples row: $exampleData"
////////////////////////////                )
////////////////////////////              )
////////////////////////////              val validatedValue = placeholderType.toLowerCase match {
////////////////////////////                case "string" => rawValue // Already a String
////////////////////////////                case "int" =>
////////////////////////////                  try rawValue.toInt.toString
////////////////////////////                  catch {
////////////////////////////                    case _: NumberFormatException =>
////////////////////////////                      throw new IllegalArgumentException(
////////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not an Int"
////////////////////////////                      )
////////////////////////////                  }
////////////////////////////                case "double" =>
////////////////////////////                  try rawValue.toDouble.toString
////////////////////////////                  catch {
////////////////////////////                    case _: NumberFormatException =>
////////////////////////////                      throw new IllegalArgumentException(
////////////////////////////                        s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '$step' is not a Double"
////////////////////////////                      )
////////////////////////////                  }
////////////////////////////                case unknown =>
////////////////////////////                  throw new IllegalArgumentException(
////////////////////////////                    s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '$step'"
////////////////////////////                  )
////////////////////////////              }
////////////////////////////              currentStep.replace(s"{$placeholderName:$placeholderType}", validatedValue)
////////////////////////////            }
////////////////////////////          }.mkString("\n")
////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
////////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////////        }
////////////////////////////      }
////////////////////////////    }
////////////////////////////
////////////////////////////    ZIO
////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////          for {
////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////////          } yield results
////////////////////////////        }
////////////////////////////      }
////////////////////////////      .withParallelism(parallelism)
////////////////////////////  }
////////////////////////////
////////////////////////////  def runScenarios[R](
////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////////                       parallelism: Int = 4
////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////    ZIO
////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////          for {
////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////          } yield results
////////////////////////////        }
////////////////////////////      }
////////////////////////////      .withParallelism(parallelism)
////////////////////////////
////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////
////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////    val trimmed = param.trim
////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////    } else {
////////////////////////////      trimmed
////////////////////////////    }
////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////    else unquoted
////////////////////////////  }
////////////////////////////}
////////////////////////////
////////////////////////////trait LogCollector {
////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////}
////////////////////////////
////////////////////////////object LogCollector {
////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////    for {
////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////    } yield new LogCollector {
////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////    }
////////////////////////////  }
////////////////////////////}
////////////////////////////
////////////////////////////
//////////////////////////////package zio.bdd.core
//////////////////////////////
//////////////////////////////import zio.*
//////////////////////////////
//////////////////////////////import scala.util.matching.Regex
//////////////////////////////import java.time.Instant
//////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////////
//////////////////////////////case class StepResult(
//////////////////////////////                       step: String,
//////////////////////////////                       succeeded: Boolean,
//////////////////////////////                       error: Option[String],
//////////////////////////////                       output: Any,
//////////////////////////////                       logs: List[(String, Instant)]
//////////////////////////////                     )
//////////////////////////////
//////////////////////////////object ScenarioRunner {
//////////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////////    case () => ()
//////////////////////////////    case (a, b) =>
//////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////////        case ((), ()) => ()
//////////////////////////////        case ((), b)  => b
//////////////////////////////        case (a, ())  => a
//////////////////////////////        case (a, b)   => (a, b)
//////////////////////////////      }
//////////////////////////////    case other => other
//////////////////////////////  }
//////////////////////////////
//////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////////    params match {
//////////////////////////////      case Nil => flattenedPrev
//////////////////////////////      case head :: Nil =>
//////////////////////////////        flattenedPrev match {
//////////////////////////////          case ()     => parseParam(head)
//////////////////////////////          case single => (single, parseParam(head))
//////////////////////////////        }
//////////////////////////////      case many =>
//////////////////////////////        flattenedPrev match {
//////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////////        }
//////////////////////////////    }
//////////////////////////////  }
//////////////////////////////
//////////////////////////////  def run[R](
//////////////////////////////              steps: ZIOSteps[R],
//////////////////////////////              scenario: String,
//////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////////
//////////////////////////////    def runSteps(
//////////////////////////////                  remaining: List[String],
//////////////////////////////                  previousOutput: Any,
//////////////////////////////                  acc: List[StepResult]
//////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////////      remaining match {
//////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////////        case line :: rest =>
//////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////////            case Some(stepDef) =>
//////////////////////////////              val pattern = stepDef.pattern
//////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////////              val input   = combine(previousOutput, params)
//////////////////////////////              for {
//////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////////                logs      <- collector.getLogs
//////////////////////////////                _         <- collector.clearLogs
//////////////////////////////                result <- fn(input).map { output =>
//////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////////////////////                }.catchAll { error =>
//////////////////////////////                  ZIO.succeed(
//////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////////////////////                  )
//////////////////////////////                }
//////////////////////////////                finalResult <- if (result.succeeded) {
//////////////////////////////                  runSteps(rest, result.output, result :: acc)
//////////////////////////////                } else {
//////////////////////////////                  ZIO.succeed(
//////////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////////////////////                    )
//////////////////////////////                  )
//////////////////////////////                }
//////////////////////////////              } yield finalResult
//////////////////////////////            case None =>
//////////////////////////////              for {
//////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////////                logs      <- collector.getLogs
//////////////////////////////                _         <- collector.clearLogs
//////////////////////////////                next <- runSteps(
//////////////////////////////                  rest,
//////////////////////////////                  previousOutput,
//////////////////////////////                  StepResult(
//////////////////////////////                    line,
//////////////////////////////                    succeeded = false,
//////////////////////////////                    error = Some("No step definition matches"),
//////////////////////////////                    output = (),
//////////////////////////////                    logs = logs
//////////////////////////////                  ) :: acc
//////////////////////////////                )
//////////////////////////////              } yield next
//////////////////////////////          }
//////////////////////////////      }
//////////////////////////////
//////////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////////////////////  }
//////////////////////////////
//////////////////////////////  def runScenarios[R](
//////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////                       feature: Feature,
//////////////////////////////                       parallelism: Int
//////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////      val baseSteps = feature.background ++ scenario.steps
//////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
//////////////////////////////      } else {
//////////////////////////////        scenario.examples.map { row =>
//////////////////////////////          val exampleValues = row.data.values.toList // Ordered list of values from Examples table
//////////////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////////////            // Find all {string} placeholders in the step
//////////////////////////////            val placeholderPattern = "\\{string\\}".r
//////////////////////////////            val placeholders = placeholderPattern.findAllIn(step).toList
//////////////////////////////            if (placeholders.isEmpty) step
//////////////////////////////            else {
//////////////////////////////              // Validate placeholder count matches example values
//////////////////////////////              if (placeholders.length > exampleValues.length) {
//////////////////////////////                throw new IllegalArgumentException(
//////////////////////////////                  s"Step '$step' has ${placeholders.length} placeholders but only ${exampleValues.length} example values in row: ${row.data}"
//////////////////////////////                )
//////////////////////////////              }
//////////////////////////////              // Replace each {string} with the corresponding value by position
//////////////////////////////              placeholders.foldLeft((step, exampleValues)) { case ((currentStep, remainingValues), _) =>
//////////////////////////////                remainingValues match {
//////////////////////////////                  case value :: rest => (currentStep.replaceFirst("\\{string\\}", value), rest)
//////////////////////////////                  case Nil           => (currentStep, Nil) // Shouldn't happen due to validation
//////////////////////////////                }
//////////////////////////////              }._1
//////////////////////////////            }
//////////////////////////////          }.mkString("\n")
//////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
//////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////        }
//////////////////////////////      }
//////////////////////////////    }
//////////////////////////////
//////////////////////////////    ZIO
//////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////          for {
//////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////          } yield results
//////////////////////////////        }
//////////////////////////////      }
//////////////////////////////      .withParallelism(parallelism)
//////////////////////////////  }
//////////////////////////////
//////////////////////////////  def runScenarios[R](
//////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////////                       parallelism: Int = 4
//////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////////    ZIO
//////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////          for {
//////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////////          } yield results
//////////////////////////////        }
//////////////////////////////      }
//////////////////////////////      .withParallelism(parallelism)
//////////////////////////////
//////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////////
//////////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////////    val trimmed = param.trim
//////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////////    } else {
//////////////////////////////      trimmed
//////////////////////////////    }
//////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////////    else unquoted
//////////////////////////////  }
//////////////////////////////}
//////////////////////////////
//////////////////////////////trait LogCollector {
//////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////////}
//////////////////////////////
//////////////////////////////object LogCollector {
//////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////////    for {
//////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////////    } yield new LogCollector {
//////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////////    }
//////////////////////////////  }
//////////////////////////////}
//////////////////////////////
////////////////////////////////package zio.bdd.core
////////////////////////////////
////////////////////////////////import zio.*
////////////////////////////////
////////////////////////////////import scala.util.matching.Regex
////////////////////////////////import java.time.Instant
////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////////////
////////////////////////////////case class StepResult(
////////////////////////////////                       step: String,
////////////////////////////////                       succeeded: Boolean,
////////////////////////////////                       error: Option[String],
////////////////////////////////                       output: Any,
////////////////////////////////                       logs: List[(String, Instant)]
////////////////////////////////                     )
////////////////////////////////
////////////////////////////////object ScenarioRunner {
////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////////    case () => ()
////////////////////////////////    case (a, b) =>
////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////////        case ((), ()) => ()
////////////////////////////////        case ((), b)  => b
////////////////////////////////        case (a, ())  => a
////////////////////////////////        case (a, b)   => (a, b)
////////////////////////////////      }
////////////////////////////////    case other => other
////////////////////////////////  }
////////////////////////////////
////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////////    params match {
////////////////////////////////      case Nil => flattenedPrev
////////////////////////////////      case head :: Nil =>
////////////////////////////////        flattenedPrev match {
////////////////////////////////          case ()     => parseParam(head)
////////////////////////////////          case single => (single, parseParam(head))
////////////////////////////////        }
////////////////////////////////      case many =>
////////////////////////////////        flattenedPrev match {
////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////////        }
////////////////////////////////    }
////////////////////////////////  }
////////////////////////////////
////////////////////////////////  def run[R](
////////////////////////////////              steps: ZIOSteps[R],
////////////////////////////////              scenario: String,
////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////////
////////////////////////////////    def runSteps(
////////////////////////////////                  remaining: List[String],
////////////////////////////////                  previousOutput: Any,
////////////////////////////////                  acc: List[StepResult]
////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////////      remaining match {
////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////////        case line :: rest =>
////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////////            case Some(stepDef) =>
////////////////////////////////              val pattern = stepDef.pattern
////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////////              for {
////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////                result <- fn(input).map { output =>
////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////////////////////                }.catchAll { error =>
////////////////////////////////                  ZIO.succeed(
////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////////////////////                  )
////////////////////////////////                }
////////////////////////////////                finalResult <- if (result.succeeded) {
////////////////////////////////                  runSteps(rest, result.output, result :: acc)
////////////////////////////////                } else {
////////////////////////////////                  ZIO.succeed(
////////////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////////////////////                    )
////////////////////////////////                  )
////////////////////////////////                }
////////////////////////////////              } yield finalResult
////////////////////////////////            case None =>
////////////////////////////////              for {
////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////                next <- runSteps(
////////////////////////////////                  rest,
////////////////////////////////                  previousOutput,
////////////////////////////////                  StepResult(
////////////////////////////////                    line,
////////////////////////////////                    succeeded = false,
////////////////////////////////                    error = Some("No step definition matches"),
////////////////////////////////                    output = (),
////////////////////////////////                    logs = logs
////////////////////////////////                  ) :: acc
////////////////////////////////                )
////////////////////////////////              } yield next
////////////////////////////////          }
////////////////////////////////      }
////////////////////////////////
////////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
////////////////////////////////  }
////////////////////////////////
////////////////////////////////  def runScenarios[R](
////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////                       feature: Feature,
////////////////////////////////                       parallelism: Int
////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
////////////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
////////////////////////////////      } else {
////////////////////////////////        scenario.examples.map { row =>
////////////////////////////////          val exampleValues = row.data.values.toList // Ordered list of values from Examples table
////////////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////////////            // Find all {string} placeholders in the step
////////////////////////////////            val placeholderPattern = "\\{string\\}".r
////////////////////////////////            val placeholders = placeholderPattern.findAllIn(step).toList
////////////////////////////////            if (placeholders.isEmpty) step
////////////////////////////////            else {
////////////////////////////////              // Replace each {string} with the corresponding value by position
////////////////////////////////              placeholders.foldLeft((step, exampleValues)) { case ((currentStep, remainingValues), _) =>
////////////////////////////////                remainingValues match {
////////////////////////////////                  case value :: rest => (currentStep.replaceFirst("\\{string\\}", value), rest)
////////////////////////////////                  case Nil           => (currentStep, Nil) // No more values, leave as is
////////////////////////////////                }
////////////////////////////////              }._1
////////////////////////////////            }
////////////////////////////////          }.mkString("\n")
////////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
////////////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////////////        }
////////////////////////////////      }
////////////////////////////////    }
////////////////////////////////
////////////////////////////////    ZIO
////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////          for {
////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////////////          } yield results
////////////////////////////////        }
////////////////////////////////      }
////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////  }
////////////////////////////////
////////////////////////////////  def runScenarios[R](
////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////////////                       parallelism: Int = 4
////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////////    ZIO
////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////          for {
////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////////          } yield results
////////////////////////////////        }
////////////////////////////////      }
////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////
////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////////
////////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////////    val trimmed = param.trim
////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////////    } else {
////////////////////////////////      trimmed
////////////////////////////////    }
////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////////    else unquoted
////////////////////////////////  }
////////////////////////////////}
////////////////////////////////
////////////////////////////////trait LogCollector {
////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////////}
////////////////////////////////
////////////////////////////////object LogCollector {
////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////////    for {
////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////////    } yield new LogCollector {
////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////////    }
////////////////////////////////  }
////////////////////////////////}
////////////////////////////////
//////////////////////////////////package zio.bdd.core
//////////////////////////////////
//////////////////////////////////import zio.*
//////////////////////////////////
//////////////////////////////////import scala.util.matching.Regex
//////////////////////////////////import java.time.Instant
//////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////////////
//////////////////////////////////case class StepResult(
//////////////////////////////////                       step: String,
//////////////////////////////////                       succeeded: Boolean,
//////////////////////////////////                       error: Option[String],
//////////////////////////////////                       output: Any,
//////////////////////////////////                       logs: List[(String, Instant)]
//////////////////////////////////                     )
//////////////////////////////////
//////////////////////////////////object ScenarioRunner {
//////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////////////    case () => ()
//////////////////////////////////    case (a, b) =>
//////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////////////        case ((), ()) => ()
//////////////////////////////////        case ((), b)  => b
//////////////////////////////////        case (a, ())  => a
//////////////////////////////////        case (a, b)   => (a, b)
//////////////////////////////////      }
//////////////////////////////////    case other => other
//////////////////////////////////  }
//////////////////////////////////
//////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////////////    params match {
//////////////////////////////////      case Nil => flattenedPrev
//////////////////////////////////      case head :: Nil =>
//////////////////////////////////        flattenedPrev match {
//////////////////////////////////          case ()     => parseParam(head)
//////////////////////////////////          case single => (single, parseParam(head))
//////////////////////////////////        }
//////////////////////////////////      case many =>
//////////////////////////////////        flattenedPrev match {
//////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////////////        }
//////////////////////////////////    }
//////////////////////////////////  }
//////////////////////////////////
//////////////////////////////////  def run[R](
//////////////////////////////////              steps: ZIOSteps[R],
//////////////////////////////////              scenario: String,
//////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////////////
//////////////////////////////////    def runSteps(
//////////////////////////////////                  remaining: List[String],
//////////////////////////////////                  previousOutput: Any,
//////////////////////////////////                  acc: List[StepResult]
//////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////////////      remaining match {
//////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////////////        case line :: rest =>
//////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////////////            case Some(stepDef) =>
//////////////////////////////////              val pattern = stepDef.pattern
//////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////////////              val input   = combine(previousOutput, params)
//////////////////////////////////              for {
//////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////                result <- fn(input).map { output =>
//////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
//////////////////////////////////                }.catchAll { error =>
//////////////////////////////////                  ZIO.succeed(
//////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
//////////////////////////////////                  )
//////////////////////////////////                }
//////////////////////////////////                finalResult <- if (result.succeeded) {
//////////////////////////////////                  runSteps(rest, result.output, result :: acc)
//////////////////////////////////                } else {
//////////////////////////////////                  ZIO.succeed(
//////////////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
//////////////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
//////////////////////////////////                    )
//////////////////////////////////                  )
//////////////////////////////////                }
//////////////////////////////////              } yield finalResult
//////////////////////////////////            case None =>
//////////////////////////////////              for {
//////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////                next <- runSteps(
//////////////////////////////////                  rest,
//////////////////////////////////                  previousOutput,
//////////////////////////////////                  StepResult(
//////////////////////////////////                    line,
//////////////////////////////////                    succeeded = false,
//////////////////////////////////                    error = Some("No step definition matches"),
//////////////////////////////////                    output = (),
//////////////////////////////////                    logs = logs
//////////////////////////////////                  ) :: acc
//////////////////////////////////                )
//////////////////////////////////              } yield next
//////////////////////////////////          }
//////////////////////////////////      }
//////////////////////////////////
//////////////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////////////    // Repeat the effect and concatenate results into a single list
//////////////////////////////////    ZIO.collectAll(List.fill(metadata.repeatCount)(effect)).map(_.flatten)
//////////////////////////////////  }
//////////////////////////////////
//////////////////////////////////  def runScenarios[R](
//////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////                       feature: Feature,
//////////////////////////////////                       parallelism: Int
//////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
//////////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
//////////////////////////////////      } else {
//////////////////////////////////        scenario.examples.map { row =>
//////////////////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////////////////            if (step.contains("name")) {
//////////////////////////////////              step.replace("{string}", row.data("name"))
//////////////////////////////////            } else if (step.contains("email")) {
//////////////////////////////////              step.replace("{string}", row.data("email"))
//////////////////////////////////            } else {
//////////////////////////////////              step
//////////////////////////////////            }
//////////////////////////////////          }.mkString("\n")
//////////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
//////////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////////        }
//////////////////////////////////      }
//////////////////////////////////    }
//////////////////////////////////
//////////////////////////////////    ZIO
//////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////          for {
//////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////////          } yield results
//////////////////////////////////        }
//////////////////////////////////      }
//////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////  }
//////////////////////////////////
//////////////////////////////////  def runScenarios[R](
//////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////////////                       parallelism: Int = 4
//////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////////////    ZIO
//////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////          for {
//////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////////////          } yield results
//////////////////////////////////        }
//////////////////////////////////      }
//////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////
//////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////////////
//////////////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////////////    val trimmed = param.trim
//////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////////////    } else {
//////////////////////////////////      trimmed
//////////////////////////////////    }
//////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////////////    else unquoted
//////////////////////////////////  }
//////////////////////////////////}
//////////////////////////////////
//////////////////////////////////trait LogCollector {
//////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////////////}
//////////////////////////////////
//////////////////////////////////object LogCollector {
//////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////////////    for {
//////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////////////    } yield new LogCollector {
//////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////////////    }
//////////////////////////////////  }
//////////////////////////////////}
//////////////////////////////////
////////////////////////////////////package zio.bdd.core
////////////////////////////////////
////////////////////////////////////import zio.*
////////////////////////////////////
////////////////////////////////////import scala.util.matching.Regex
////////////////////////////////////import java.time.Instant
////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////////////////
////////////////////////////////////case class StepResult(
////////////////////////////////////                       step: String,
////////////////////////////////////                       succeeded: Boolean,
////////////////////////////////////                       error: Option[String],
////////////////////////////////////                       output: Any,
////////////////////////////////////                       logs: List[(String, Instant)]
////////////////////////////////////                     )
////////////////////////////////////
////////////////////////////////////object ScenarioRunner {
////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////////////    case () => ()
////////////////////////////////////    case (a, b) =>
////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////////////        case ((), ()) => ()
////////////////////////////////////        case ((), b)  => b
////////////////////////////////////        case (a, ())  => a
////////////////////////////////////        case (a, b)   => (a, b)
////////////////////////////////////      }
////////////////////////////////////    case other => other
////////////////////////////////////  }
////////////////////////////////////
////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////////////    params match {
////////////////////////////////////      case Nil => flattenedPrev
////////////////////////////////////      case head :: Nil =>
////////////////////////////////////        flattenedPrev match {
////////////////////////////////////          case ()     => parseParam(head)
////////////////////////////////////          case single => (single, parseParam(head))
////////////////////////////////////        }
////////////////////////////////////      case many =>
////////////////////////////////////        flattenedPrev match {
////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////////////        }
////////////////////////////////////    }
////////////////////////////////////  }
////////////////////////////////////
////////////////////////////////////  def run[R](
////////////////////////////////////              steps: ZIOSteps[R],
////////////////////////////////////              scenario: String,
////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////////////
////////////////////////////////////    def runSteps(
////////////////////////////////////                  remaining: List[String],
////////////////////////////////////                  previousOutput: Any,
////////////////////////////////////                  acc: List[StepResult]
////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////////////      remaining match {
////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////////////        case line :: rest =>
////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////////////            case Some(stepDef) =>
////////////////////////////////////              val pattern = stepDef.pattern
////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////////////              for {
////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////                result <- fn(input).map { output =>
////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs)
////////////////////////////////////                }.catchAll { error =>
////////////////////////////////////                  ZIO.succeed(
////////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs)
////////////////////////////////////                  )
////////////////////////////////////                }
////////////////////////////////////                finalResult <- if (result.succeeded) {
////////////////////////////////////                  runSteps(rest, result.output, result :: acc)
////////////////////////////////////                } else {
////////////////////////////////////                  ZIO.succeed(
////////////////////////////////////                    (result :: acc).reverse ++ rest.map(line =>
////////////////////////////////////                      StepResult(line, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil)
////////////////////////////////////                    )
////////////////////////////////////                  )
////////////////////////////////////                }
////////////////////////////////////              } yield finalResult
////////////////////////////////////            case None =>
////////////////////////////////////              for {
////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////                next <- runSteps(
////////////////////////////////////                  rest,
////////////////////////////////////                  previousOutput,
////////////////////////////////////                  StepResult(
////////////////////////////////////                    line,
////////////////////////////////////                    succeeded = false,
////////////////////////////////////                    error = Some("No step definition matches"),
////////////////////////////////////                    output = (),
////////////////////////////////////                    logs = logs
////////////////////////////////////                  ) :: acc
////////////////////////////////////                )
////////////////////////////////////              } yield next
////////////////////////////////////          }
////////////////////////////////////      }
////////////////////////////////////
////////////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////////////
////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
////////////////////////////////////  }
////////////////////////////////////
////////////////////////////////////  def runScenarios[R](
////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////                       feature: Feature,
////////////////////////////////////                       parallelism: Int
////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
////////////////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
////////////////////////////////////      } else {
////////////////////////////////////        scenario.examples.map { row =>
////////////////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////////////////            if (step.contains("name")) {
////////////////////////////////////              step.replace("{string}", row.data("name"))
////////////////////////////////////            } else if (step.contains("email")) {
////////////////////////////////////              step.replace("{string}", row.data("email"))
////////////////////////////////////            } else {
////////////////////////////////////              step
////////////////////////////////////            }
////////////////////////////////////          }.mkString("\n")
////////////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
////////////////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////////////////        }
////////////////////////////////////      }
////////////////////////////////////    }
////////////////////////////////////
////////////////////////////////////    ZIO
////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////          for {
////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////////////////          } yield results
////////////////////////////////////        }
////////////////////////////////////      }
////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////  }
////////////////////////////////////
////////////////////////////////////  def runScenarios[R](
////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////////////////                       parallelism: Int = 4
////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////////////    ZIO
////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////          for {
////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////////////          } yield results
////////////////////////////////////        }
////////////////////////////////////      }
////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////
////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////////////
////////////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////////////    val trimmed = param.trim
////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////////////    } else {
////////////////////////////////////      trimmed
////////////////////////////////////    }
////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////////////    else unquoted
////////////////////////////////////  }
////////////////////////////////////}
////////////////////////////////////
////////////////////////////////////trait LogCollector {
////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////////////}
////////////////////////////////////
////////////////////////////////////object LogCollector {
////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////////////    for {
////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////////////    } yield new LogCollector {
////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////////////    }
////////////////////////////////////  }
////////////////////////////////////}
////////////////////////////////////
//////////////////////////////////////package zio.bdd.core
//////////////////////////////////////
//////////////////////////////////////import zio.*
//////////////////////////////////////
//////////////////////////////////////import scala.util.matching.Regex
//////////////////////////////////////import java.time.Instant
//////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////////////////
//////////////////////////////////////case class StepResult(
//////////////////////////////////////                       step: String,
//////////////////////////////////////                       succeeded: Boolean,
//////////////////////////////////////                       error: Option[String],
//////////////////////////////////////                       output: Any,
//////////////////////////////////////                       logs: List[(String, Instant)]
//////////////////////////////////////                     )
//////////////////////////////////////
//////////////////////////////////////object ScenarioRunner {
//////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////////////////    case () => ()
//////////////////////////////////////    case (a, b) =>
//////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////////////////        case ((), ()) => ()
//////////////////////////////////////        case ((), b)  => b
//////////////////////////////////////        case (a, ())  => a
//////////////////////////////////////        case (a, b)   => (a, b)
//////////////////////////////////////      }
//////////////////////////////////////    case other => other
//////////////////////////////////////  }
//////////////////////////////////////
//////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////////////////    params match {
//////////////////////////////////////      case Nil => flattenedPrev
//////////////////////////////////////      case head :: Nil =>
//////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////          case ()     => parseParam(head)
//////////////////////////////////////          case single => (single, parseParam(head))
//////////////////////////////////////        }
//////////////////////////////////////      case many =>
//////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////////////////        }
//////////////////////////////////////    }
//////////////////////////////////////  }
//////////////////////////////////////
//////////////////////////////////////  def run[R](
//////////////////////////////////////              steps: ZIOSteps[R],
//////////////////////////////////////              scenario: String,
//////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////////////////
//////////////////////////////////////    def runSteps(
//////////////////////////////////////                  remaining: List[String],
//////////////////////////////////////                  previousOutput: Any,
//////////////////////////////////////                  acc: List[StepResult]
//////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////////////////      remaining match {
//////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////////////////        case line :: rest =>
//////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////////////////            case Some(stepDef) =>
//////////////////////////////////////              val pattern = stepDef.pattern
//////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////////////////              val input   = combine(previousOutput, params)
//////////////////////////////////////              for {
//////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////                result <- fn(input).map { output =>
//////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
//////////////////////////////////////                }.catchAll { error =>
//////////////////////////////////////                  // On failure, mark this step and stop processing further steps
//////////////////////////////////////                  val failedResult = StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs) :: acc
//////////////////////////////////////                  ZIO.succeed(
//////////////////////////////////////                    rest.foldLeft(failedResult) { (currentAcc, unprocessedLine) =>
//////////////////////////////////////                      StepResult(unprocessedLine, succeeded = false, error = Some("Skipped due to prior failure"), output = (), logs = Nil) :: currentAcc
//////////////////////////////////////                    }.reverse
//////////////////////////////////////                  )
//////////////////////////////////////                }
//////////////////////////////////////                finalResult <- if (result.head.succeeded) runSteps(rest, result.head.output, result) else ZIO.succeed(result)
//////////////////////////////////////              } yield finalResult
//////////////////////////////////////            case None =>
//////////////////////////////////////              for {
//////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////                next <- runSteps(
//////////////////////////////////////                  rest,
//////////////////////////////////////                  previousOutput,
//////////////////////////////////////                  StepResult(
//////////////////////////////////////                    line,
//////////////////////////////////////                    succeeded = false,
//////////////////////////////////////                    error = Some("No step definition matches"),
//////////////////////////////////////                    output = (),
//////////////////////////////////////                    logs = logs
//////////////////////////////////////                  ) :: acc
//////////////////////////////////////                )
//////////////////////////////////////              } yield next
//////////////////////////////////////          }
//////////////////////////////////////      }
//////////////////////////////////////
//////////////////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////////////////
//////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
//////////////////////////////////////  }
//////////////////////////////////////
//////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////                       feature: Feature,
//////////////////////////////////////                       parallelism: Int
//////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
//////////////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
//////////////////////////////////////      } else {
//////////////////////////////////////        scenario.examples.map { row =>
//////////////////////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////////////////////            if (step.contains("name")) {
//////////////////////////////////////              step.replace("{string}", row.data("name"))
//////////////////////////////////////            } else if (step.contains("email")) {
//////////////////////////////////////              step.replace("{string}", row.data("email"))
//////////////////////////////////////            } else {
//////////////////////////////////////              step
//////////////////////////////////////            }
//////////////////////////////////////          }.mkString("\n")
//////////////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
//////////////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////////////        }
//////////////////////////////////////      }
//////////////////////////////////////    }
//////////////////////////////////////
//////////////////////////////////////    ZIO
//////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////          for {
//////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////////////          } yield results
//////////////////////////////////////        }
//////////////////////////////////////      }
//////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////  }
//////////////////////////////////////
//////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////////////////                       parallelism: Int = 4
//////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////////////////    ZIO
//////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////          for {
//////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////////////////          } yield results
//////////////////////////////////////        }
//////////////////////////////////////      }
//////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////
//////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////////////////
//////////////////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////////////////    val trimmed = param.trim
//////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////////////////    } else {
//////////////////////////////////////      trimmed
//////////////////////////////////////    }
//////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////////////////    else unquoted
//////////////////////////////////////  }
//////////////////////////////////////}
//////////////////////////////////////
//////////////////////////////////////trait LogCollector {
//////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////////////////}
//////////////////////////////////////
//////////////////////////////////////object LogCollector {
//////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////////////////    for {
//////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////////////////    } yield new LogCollector {
//////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////////////////    }
//////////////////////////////////////  }
//////////////////////////////////////}
//////////////////////////////////////
////////////////////////////////////////package zio.bdd.core
////////////////////////////////////////
////////////////////////////////////////import zio.*
////////////////////////////////////////
////////////////////////////////////////import scala.util.matching.Regex
////////////////////////////////////////import java.time.Instant
////////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////////////////////
////////////////////////////////////////case class StepResult(
////////////////////////////////////////                       step: String,
////////////////////////////////////////                       succeeded: Boolean,
////////////////////////////////////////                       error: Option[String],
////////////////////////////////////////                       output: Any,
////////////////////////////////////////                       logs: List[(String, Instant)]
////////////////////////////////////////                     )
////////////////////////////////////////
////////////////////////////////////////object ScenarioRunner {
////////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////////////////    case () => ()
////////////////////////////////////////    case (a, b) =>
////////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////////////////        case ((), ()) => ()
////////////////////////////////////////        case ((), b)  => b
////////////////////////////////////////        case (a, ())  => a
////////////////////////////////////////        case (a, b)   => (a, b)
////////////////////////////////////////      }
////////////////////////////////////////    case other => other
////////////////////////////////////////  }
////////////////////////////////////////
////////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////////////////    params match {
////////////////////////////////////////      case Nil => flattenedPrev
////////////////////////////////////////      case head :: Nil =>
////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////          case ()     => parseParam(head)
////////////////////////////////////////          case single => (single, parseParam(head))
////////////////////////////////////////        }
////////////////////////////////////////      case many =>
////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////////////////        }
////////////////////////////////////////    }
////////////////////////////////////////  }
////////////////////////////////////////
////////////////////////////////////////  def run[R](
////////////////////////////////////////              steps: ZIOSteps[R],
////////////////////////////////////////              scenario: String,
////////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////////////////
////////////////////////////////////////    def runSteps(
////////////////////////////////////////                  remaining: List[String],
////////////////////////////////////////                  previousOutput: Any,
////////////////////////////////////////                  acc: List[StepResult]
////////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////////////////      remaining match {
////////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////////////////        case line :: rest =>
////////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////////////////            case Some(stepDef) =>
////////////////////////////////////////              val pattern = stepDef.pattern
////////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////////////////              for {
////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////                result <- fn(input).map { output =>
////////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
////////////////////////////////////////                }.catchAll { error =>
////////////////////////////////////////                  ZIO.succeed(
////////////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs) :: acc
////////////////////////////////////////                  )
////////////////////////////////////////                }
////////////////////////////////////////                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
////////////////////////////////////////              } yield finalResult
////////////////////////////////////////            case None =>
////////////////////////////////////////              for {
////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////                next <- runSteps(
////////////////////////////////////////                  rest,
////////////////////////////////////////                  previousOutput,
////////////////////////////////////////                  StepResult(
////////////////////////////////////////                    line,
////////////////////////////////////////                    succeeded = false,
////////////////////////////////////////                    error = Some("No step definition matches"),
////////////////////////////////////////                    output = (),
////////////////////////////////////////                    logs = logs
////////////////////////////////////////                  ) :: acc
////////////////////////////////////////                )
////////////////////////////////////////              } yield next
////////////////////////////////////////          }
////////////////////////////////////////      }
////////////////////////////////////////
////////////////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////////////////
////////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
////////////////////////////////////////  }
////////////////////////////////////////
////////////////////////////////////////  def runScenarios[R](
////////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////////                       feature: Feature,
////////////////////////////////////////                       parallelism: Int
////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
////////////////////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
////////////////////////////////////////      } else {
////////////////////////////////////////        scenario.examples.map { row =>
////////////////////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////////////////////            // Map {string} placeholders to specific column values based on step context
////////////////////////////////////////            if (step.contains("name")) {
////////////////////////////////////////              step.replace("{string}", row.data("name"))
////////////////////////////////////////            } else if (step.contains("email")) {
////////////////////////////////////////              step.replace("{string}", row.data("email"))
////////////////////////////////////////            } else {
////////////////////////////////////////              step // No substitution if no clear context
////////////////////////////////////////            }
////////////////////////////////////////          }.mkString("\n")
////////////////////////////////////////          println(s"Parameterized text for ${row.data}: $parameterizedText") // Debug
////////////////////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////////////////////        }
////////////////////////////////////////      }
////////////////////////////////////////    }
////////////////////////////////////////
////////////////////////////////////////    ZIO
////////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////////          for {
////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////////////////////          } yield results
////////////////////////////////////////        }
////////////////////////////////////////      }
////////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////////  }
////////////////////////////////////////
////////////////////////////////////////  // Original overload for raw string scenarios
////////////////////////////////////////  def runScenarios[R](
////////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////////////////////                       parallelism: Int = 4
////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////////////////    ZIO
////////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////////          for {
////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////////////////          } yield results
////////////////////////////////////////        }
////////////////////////////////////////      }
////////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////////
////////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////////////////
////////////////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////////////////    val trimmed = param.trim
////////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////////////////    } else {
////////////////////////////////////////      trimmed
////////////////////////////////////////    }
////////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////////////////    else unquoted
////////////////////////////////////////  }
////////////////////////////////////////}
////////////////////////////////////////
////////////////////////////////////////trait LogCollector {
////////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////////////////}
////////////////////////////////////////
////////////////////////////////////////object LogCollector {
////////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////////////////    for {
////////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////////////////    } yield new LogCollector {
////////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////////////////    }
////////////////////////////////////////  }
////////////////////////////////////////}
////////////////////////////////////////
//////////////////////////////////////////package zio.bdd.core
//////////////////////////////////////////
//////////////////////////////////////////import zio.*
//////////////////////////////////////////
//////////////////////////////////////////import scala.util.matching.Regex
//////////////////////////////////////////import java.time.Instant
//////////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////////////////////
//////////////////////////////////////////case class StepResult(
//////////////////////////////////////////                       step: String,
//////////////////////////////////////////                       succeeded: Boolean,
//////////////////////////////////////////                       error: Option[String],
//////////////////////////////////////////                       output: Any,
//////////////////////////////////////////                       logs: List[(String, Instant)]
//////////////////////////////////////////                     )
//////////////////////////////////////////
//////////////////////////////////////////object ScenarioRunner {
//////////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////////////////////    case () => ()
//////////////////////////////////////////    case (a, b) =>
//////////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////////////////////        case ((), ()) => ()
//////////////////////////////////////////        case ((), b)  => b
//////////////////////////////////////////        case (a, ())  => a
//////////////////////////////////////////        case (a, b)   => (a, b)
//////////////////////////////////////////      }
//////////////////////////////////////////    case other => other
//////////////////////////////////////////  }
//////////////////////////////////////////
//////////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////////////////////    params match {
//////////////////////////////////////////      case Nil => flattenedPrev
//////////////////////////////////////////      case head :: Nil =>
//////////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////////          case ()     => parseParam(head)
//////////////////////////////////////////          case single => (single, parseParam(head))
//////////////////////////////////////////        }
//////////////////////////////////////////      case many =>
//////////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////////////////////        }
//////////////////////////////////////////    }
//////////////////////////////////////////  }
//////////////////////////////////////////
//////////////////////////////////////////  def run[R](
//////////////////////////////////////////              steps: ZIOSteps[R],
//////////////////////////////////////////              scenario: String,
//////////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////////////////////
//////////////////////////////////////////    def runSteps(
//////////////////////////////////////////                  remaining: List[String],
//////////////////////////////////////////                  previousOutput: Any,
//////////////////////////////////////////                  acc: List[StepResult]
//////////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////////////////////      remaining match {
//////////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////////////////////        case line :: rest =>
//////////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////////////////////            case Some(stepDef) =>
//////////////////////////////////////////              val pattern = stepDef.pattern
//////////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////////////////////              val input   = combine(previousOutput, params)
//////////////////////////////////////////              for {
//////////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////////                result <- fn(input).map { output =>
//////////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
//////////////////////////////////////////                }.catchAll { error =>
//////////////////////////////////////////                  ZIO.succeed(
//////////////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs) :: acc
//////////////////////////////////////////                  )
//////////////////////////////////////////                }
//////////////////////////////////////////                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
//////////////////////////////////////////              } yield finalResult
//////////////////////////////////////////            case None =>
//////////////////////////////////////////              for {
//////////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////////                next <- runSteps(
//////////////////////////////////////////                  rest,
//////////////////////////////////////////                  previousOutput,
//////////////////////////////////////////                  StepResult(
//////////////////////////////////////////                    line,
//////////////////////////////////////////                    succeeded = false,
//////////////////////////////////////////                    error = Some("No step definition matches"),
//////////////////////////////////////////                    output = (),
//////////////////////////////////////////                    logs = logs
//////////////////////////////////////////                  ) :: acc
//////////////////////////////////////////                )
//////////////////////////////////////////              } yield next
//////////////////////////////////////////          }
//////////////////////////////////////////      }
//////////////////////////////////////////
//////////////////////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////////////////////
//////////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
//////////////////////////////////////////  }
//////////////////////////////////////////
//////////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////////                       feature: Feature,
//////////////////////////////////////////                       parallelism: Int
//////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////////////////      val baseSteps = feature.background ++ scenario.steps
//////////////////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////////////////        List((baseSteps.mkString("\n"), scenario.metadata))
//////////////////////////////////////////      } else {
//////////////////////////////////////////        scenario.examples.map { row =>
//////////////////////////////////////////          val values = row.data.values.toList // Ordered list of values (e.g., ["Alice", "alice@example.com"])
////////////////////////////////////////////          val parameterizedText = baseSteps.map { step =>
////////////////////////////////////////////            // Replace all {string} occurrences positionally with example values
////////////////////////////////////////////            var currentStep = step
////////////////////////////////////////////            values.zipWithIndex.foldLeft(currentStep) { case (acc, (value, index)) =>
////////////////////////////////////////////              acc.replaceFirst("\\{string\\}", value)
////////////////////////////////////////////            }
////////////////////////////////////////////          }.mkString("\n")
//////////////////////////////////////////
//////////////////////////////////////////          val parameterizedText = baseSteps.map { step =>
//////////////////////////////////////////            val result = values.zipWithIndex.foldLeft(step) { case (acc, (value, index)) =>
//////////////////////////////////////////              acc.replaceFirst("\\{string\\}", value)
//////////////////////////////////////////            }
//////////////////////////////////////////            println(s"Parameterized step: $result") // Debug
//////////////////////////////////////////            result
//////////////////////////////////////////          }.mkString("\n")
//////////////////////////////////////////
//////////////////////////////////////////
//////////////////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////////////////        }
//////////////////////////////////////////      }
//////////////////////////////////////////    }
//////////////////////////////////////////
//////////////////////////////////////////    ZIO
//////////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////////          for {
//////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////////////////          } yield results
//////////////////////////////////////////        }
//////////////////////////////////////////      }
//////////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////////  }
//////////////////////////////////////////
//////////////////////////////////////////  // Original overload for raw string scenarios
//////////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////////////////////                       parallelism: Int = 4
//////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////////////////////    ZIO
//////////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////////          for {
//////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////////////////////          } yield results
//////////////////////////////////////////        }
//////////////////////////////////////////      }
//////////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////////
//////////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////////////////////
//////////////////////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////////////////////    val trimmed = param.trim
//////////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////////////////////    } else {
//////////////////////////////////////////      trimmed
//////////////////////////////////////////    }
//////////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////////////////////    else unquoted
//////////////////////////////////////////  }
//////////////////////////////////////////}
//////////////////////////////////////////
//////////////////////////////////////////trait LogCollector {
//////////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////////////////////}
//////////////////////////////////////////
//////////////////////////////////////////object LogCollector {
//////////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////////////////////    for {
//////////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////////////////////    } yield new LogCollector {
//////////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////////////////////    }
//////////////////////////////////////////  }
//////////////////////////////////////////}
//////////////////////////////////////////
////////////////////////////////////////////package zio.bdd.core
////////////////////////////////////////////
////////////////////////////////////////////import zio.*
////////////////////////////////////////////
////////////////////////////////////////////import scala.util.matching.Regex
////////////////////////////////////////////import java.time.Instant
////////////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
////////////////////////////////////////////
////////////////////////////////////////////case class StepResult(
////////////////////////////////////////////                       step: String,
////////////////////////////////////////////                       succeeded: Boolean,
////////////////////////////////////////////                       error: Option[String],
////////////////////////////////////////////                       output: Any,
////////////////////////////////////////////                       logs: List[(String, Instant)]
////////////////////////////////////////////                     )
////////////////////////////////////////////
////////////////////////////////////////////object ScenarioRunner {
////////////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////////////////////    case () => ()
////////////////////////////////////////////    case (a, b) =>
////////////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////////////////////        case ((), ()) => ()
////////////////////////////////////////////        case ((), b)  => b
////////////////////////////////////////////        case (a, ())  => a
////////////////////////////////////////////        case (a, b)   => (a, b)
////////////////////////////////////////////      }
////////////////////////////////////////////    case other => other
////////////////////////////////////////////  }
////////////////////////////////////////////
////////////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////////////////////    params match {
////////////////////////////////////////////      case Nil => flattenedPrev
////////////////////////////////////////////      case head :: Nil =>
////////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////////          case ()     => parseParam(head)
////////////////////////////////////////////          case single => (single, parseParam(head))
////////////////////////////////////////////        }
////////////////////////////////////////////      case many =>
////////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////////////////////        }
////////////////////////////////////////////    }
////////////////////////////////////////////  }
////////////////////////////////////////////
////////////////////////////////////////////  def run[R](
////////////////////////////////////////////              steps: ZIOSteps[R],
////////////////////////////////////////////              scenario: String,
////////////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////////////////////
////////////////////////////////////////////    def runSteps(
////////////////////////////////////////////                  remaining: List[String],
////////////////////////////////////////////                  previousOutput: Any,
////////////////////////////////////////////                  acc: List[StepResult]
////////////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////////////////////      remaining match {
////////////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////////////////////        case line :: rest =>
////////////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////////////////////            case Some(stepDef) =>
////////////////////////////////////////////              val pattern = stepDef.pattern
////////////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////////////////////              for {
////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////////                result <- fn(input).map { output =>
////////////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
////////////////////////////////////////////                }.catchAll { error =>
////////////////////////////////////////////                  ZIO.succeed(
////////////////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs) :: acc
////////////////////////////////////////////                  )
////////////////////////////////////////////                }
////////////////////////////////////////////                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
////////////////////////////////////////////              } yield finalResult
////////////////////////////////////////////            case None =>
////////////////////////////////////////////              for {
////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////////                next <- runSteps(
////////////////////////////////////////////                  rest,
////////////////////////////////////////////                  previousOutput,
////////////////////////////////////////////                  StepResult(
////////////////////////////////////////////                    line,
////////////////////////////////////////////                    succeeded = false,
////////////////////////////////////////////                    error = Some("No step definition matches"),
////////////////////////////////////////////                    output = (),
////////////////////////////////////////////                    logs = logs
////////////////////////////////////////////                  ) :: acc
////////////////////////////////////////////                )
////////////////////////////////////////////              } yield next
////////////////////////////////////////////          }
////////////////////////////////////////////      }
////////////////////////////////////////////
////////////////////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////////////////////
////////////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
////////////////////////////////////////////  }
////////////////////////////////////////////
////////////////////////////////////////////  // Overload for parsed Gherkin Feature, no default parallelism
////////////////////////////////////////////  def runScenarios[R](
////////////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////////////                       feature: Feature,
////////////////////////////////////////////                       parallelism: Int
////////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
////////////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
////////////////////////////////////////////      val baseText = (feature.background ++ scenario.steps).mkString("\n")
////////////////////////////////////////////      if (scenario.examples.isEmpty) {
////////////////////////////////////////////        List((baseText, scenario.metadata))
////////////////////////////////////////////      } else {
////////////////////////////////////////////        scenario.examples.map { row =>
////////////////////////////////////////////          val parameterizedText = (feature.background ++ scenario.steps)
////////////////////////////////////////////            .map { s =>
////////////////////////////////////////////              // Replace {string} placeholders with example values based on column names
////////////////////////////////////////////              row.data.foldLeft(s) { (acc, kv) =>
////////////////////////////////////////////                // Match {key} in the step text
////////////////////////////////////////////                val placeholder = s"{${kv._1}}"
////////////////////////////////////////////                acc.replace(placeholder, kv._2)
////////////////////////////////////////////              }
////////////////////////////////////////////            }
////////////////////////////////////////////            .mkString("\n")
////////////////////////////////////////////          (parameterizedText, scenario.metadata)
////////////////////////////////////////////        }
////////////////////////////////////////////      }
////////////////////////////////////////////    }
////////////////////////////////////////////
////////////////////////////////////////////    ZIO
////////////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
////////////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////////////          for {
////////////////////////////////////////////            _ <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
////////////////////////////////////////////            results <- run(steps, scenarioText, metadata)
////////////////////////////////////////////            _ <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
////////////////////////////////////////////          } yield results
////////////////////////////////////////////        }
////////////////////////////////////////////      }
////////////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////////////  }
//////////////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////////////                       feature: Feature,
//////////////////////////////////////////////                       parallelism: Int
//////////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////////////////////      val baseText = (feature.background ++ scenario.steps).mkString("\n")
//////////////////////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////////////////////        List((baseText, scenario.metadata))
//////////////////////////////////////////////      } else {
//////////////////////////////////////////////        scenario.examples.map { row =>
//////////////////////////////////////////////          val parameterizedText = (feature.background ++ scenario.steps)
//////////////////////////////////////////////            .map(s => row.data.foldLeft(s)((acc, kv) => acc.replace(s"<${kv._1}>", kv._2)))
//////////////////////////////////////////////            .mkString("\n")
//////////////////////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////////////////////        }
//////////////////////////////////////////////      }
//////////////////////////////////////////////    }
//////////////////////////////////////////////
//////////////////////////////////////////////    ZIO
//////////////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////////////          for {
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////////////////////          } yield results
//////////////////////////////////////////////        }
//////////////////////////////////////////////      }
//////////////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////////////  }
////////////////////////////////////////////
////////////////////////////////////////////  // Original overload for raw string scenarios, keeping default parallelism
////////////////////////////////////////////  def runScenarios[R](
////////////////////////////////////////////                       steps: ZIOSteps[R],
////////////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
////////////////////////////////////////////                       parallelism: Int = 4
////////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////////////////////    ZIO
////////////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////////////          for {
////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////////////////////          } yield results
////////////////////////////////////////////        }
////////////////////////////////////////////      }
////////////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////////////
////////////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////////////////////
////////////////////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////////////////////    val trimmed = param.trim
////////////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////////////////////    } else {
////////////////////////////////////////////      trimmed
////////////////////////////////////////////    }
////////////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////////////////////    else unquoted
////////////////////////////////////////////  }
////////////////////////////////////////////}
////////////////////////////////////////////
////////////////////////////////////////////trait LogCollector {
////////////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////////////////////}
////////////////////////////////////////////
////////////////////////////////////////////object LogCollector {
////////////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////////////////////    for {
////////////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////////////////////    } yield new LogCollector {
////////////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////////////////////    }
////////////////////////////////////////////  }
////////////////////////////////////////////}
////////////////////////////////////////////
//////////////////////////////////////////////package zio.bdd.core
//////////////////////////////////////////////
//////////////////////////////////////////////import zio.*
//////////////////////////////////////////////
//////////////////////////////////////////////import scala.util.matching.Regex
//////////////////////////////////////////////import java.time.Instant
//////////////////////////////////////////////import zio.bdd.gherkin.{Feature, ScenarioMetadata, Scenario as GherkinScenario}
//////////////////////////////////////////////
//////////////////////////////////////////////case class StepResult(
//////////////////////////////////////////////                       step: String,
//////////////////////////////////////////////                       succeeded: Boolean,
//////////////////////////////////////////////                       error: Option[String],
//////////////////////////////////////////////                       output: Any,
//////////////////////////////////////////////                       logs: List[(String, Instant)]
//////////////////////////////////////////////                     )
//////////////////////////////////////////////
//////////////////////////////////////////////object ScenarioRunner {
//////////////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
//////////////////////////////////////////////    case () => ()
//////////////////////////////////////////////    case (a, b) =>
//////////////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
//////////////////////////////////////////////        case ((), ()) => ()
//////////////////////////////////////////////        case ((), b)  => b
//////////////////////////////////////////////        case (a, ())  => a
//////////////////////////////////////////////        case (a, b)   => (a, b)
//////////////////////////////////////////////      }
//////////////////////////////////////////////    case other => other
//////////////////////////////////////////////  }
//////////////////////////////////////////////
//////////////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
//////////////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
//////////////////////////////////////////////    params match {
//////////////////////////////////////////////      case Nil => flattenedPrev
//////////////////////////////////////////////      case head :: Nil =>
//////////////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////////////          case ()     => parseParam(head)
//////////////////////////////////////////////          case single => (single, parseParam(head))
//////////////////////////////////////////////        }
//////////////////////////////////////////////      case many =>
//////////////////////////////////////////////        flattenedPrev match {
//////////////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
//////////////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
//////////////////////////////////////////////        }
//////////////////////////////////////////////    }
//////////////////////////////////////////////  }
//////////////////////////////////////////////
//////////////////////////////////////////////  def run[R](
//////////////////////////////////////////////              steps: ZIOSteps[R],
//////////////////////////////////////////////              scenario: String,
//////////////////////////////////////////////              metadata: ScenarioMetadata = ScenarioMetadata()
//////////////////////////////////////////////            ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
//////////////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
//////////////////////////////////////////////
//////////////////////////////////////////////    def runSteps(
//////////////////////////////////////////////                  remaining: List[String],
//////////////////////////////////////////////                  previousOutput: Any,
//////////////////////////////////////////////                  acc: List[StepResult]
//////////////////////////////////////////////                ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
//////////////////////////////////////////////      remaining match {
//////////////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
//////////////////////////////////////////////        case line :: rest =>
//////////////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
//////////////////////////////////////////////            case Some(stepDef) =>
//////////////////////////////////////////////              val pattern = stepDef.pattern
//////////////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
//////////////////////////////////////////////              val params  = extractParams(pattern, line)
//////////////////////////////////////////////              val input   = combine(previousOutput, params)
//////////////////////////////////////////////              for {
//////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
//////////////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////////////                result <- fn(input).map { output =>
//////////////////////////////////////////////                  StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
//////////////////////////////////////////////                }.catchAll { error =>
//////////////////////////////////////////////                  ZIO.succeed(
//////////////////////////////////////////////                    StepResult(line, succeeded = false, error = Some(error.getMessage), output = (), logs = logs) :: acc
//////////////////////////////////////////////                  )
//////////////////////////////////////////////                }
//////////////////////////////////////////////                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
//////////////////////////////////////////////              } yield finalResult
//////////////////////////////////////////////            case None =>
//////////////////////////////////////////////              for {
//////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
//////////////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
//////////////////////////////////////////////                logs      <- collector.getLogs
//////////////////////////////////////////////                _         <- collector.clearLogs
//////////////////////////////////////////////                next <- runSteps(
//////////////////////////////////////////////                  rest,
//////////////////////////////////////////////                  previousOutput,
//////////////////////////////////////////////                  StepResult(
//////////////////////////////////////////////                    line,
//////////////////////////////////////////////                    succeeded = false,
//////////////////////////////////////////////                    error = Some("No step definition matches"),
//////////////////////////////////////////////                    output = (),
//////////////////////////////////////////////                    logs = logs
//////////////////////////////////////////////                  ) :: acc
//////////////////////////////////////////////                )
//////////////////////////////////////////////              } yield next
//////////////////////////////////////////////          }
//////////////////////////////////////////////      }
//////////////////////////////////////////////
//////////////////////////////////////////////    val effect = runSteps(lines, (), Nil)
//////////////////////////////////////////////
//////////////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
//////////////////////////////////////////////  }
//////////////////////////////////////////////
//////////////////////////////////////////////  // Updated to handle parsed Gherkin scenarios
//////////////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////////////                       feature: Feature,
//////////////////////////////////////////////                       parallelism: Int = 4
//////////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] = {
//////////////////////////////////////////////    // Convert Gherkin scenarios to executable strings with examples
//////////////////////////////////////////////    val scenariosWithMetadata = feature.scenarios.flatMap { scenario =>
//////////////////////////////////////////////      val baseText = (feature.background ++ scenario.steps).mkString("\n")
//////////////////////////////////////////////      if (scenario.examples.isEmpty) {
//////////////////////////////////////////////        List((baseText, scenario.metadata))
//////////////////////////////////////////////      } else {
//////////////////////////////////////////////        scenario.examples.map { row =>
//////////////////////////////////////////////          val parameterizedText = (feature.background ++ scenario.steps)
//////////////////////////////////////////////            .map(s => row.data.foldLeft(s)((acc, kv) => acc.replace(s"<${kv._1}>", kv._2)))
//////////////////////////////////////////////            .mkString("\n")
//////////////////////////////////////////////          (parameterizedText, scenario.metadata)
//////////////////////////////////////////////        }
//////////////////////////////////////////////      }
//////////////////////////////////////////////    }
//////////////////////////////////////////////
//////////////////////////////////////////////    ZIO
//////////////////////////////////////////////      .foreachPar(scenariosWithMetadata) { case (scenarioText, metadata) =>
//////////////////////////////////////////////        val scenarioId = scenarioText.hashCode.toString
//////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////////////          for {
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed")))
//////////////////////////////////////////////            results <- run(steps, scenarioText, metadata)
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenarioText.split("\n").headOption.getOrElse("Unnamed"), results))
//////////////////////////////////////////////          } yield results
//////////////////////////////////////////////        }
//////////////////////////////////////////////      }
//////////////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////////////  }
//////////////////////////////////////////////
//////////////////////////////////////////////  // Keep the original for raw string scenarios
//////////////////////////////////////////////  def runScenarios[R](
//////////////////////////////////////////////                       steps: ZIOSteps[R],
//////////////////////////////////////////////                       scenarios: List[(String, ScenarioMetadata)],
//////////////////////////////////////////////                       parallelism: Int = 4
//////////////////////////////////////////////                     ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
//////////////////////////////////////////////    ZIO
//////////////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
//////////////////////////////////////////////        val scenarioId = scenario.hashCode.toString
//////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
//////////////////////////////////////////////          for {
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
//////////////////////////////////////////////            results <- run(steps, scenario, metadata)
//////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
//////////////////////////////////////////////          } yield results
//////////////////////////////////////////////        }
//////////////////////////////////////////////      }
//////////////////////////////////////////////      .withParallelism(parallelism)
//////////////////////////////////////////////
//////////////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
//////////////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
//////////////////////////////////////////////
//////////////////////////////////////////////  private def parseParam(param: String): Any = {
//////////////////////////////////////////////    val trimmed = param.trim
//////////////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
//////////////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
//////////////////////////////////////////////    } else {
//////////////////////////////////////////////      trimmed
//////////////////////////////////////////////    }
//////////////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
//////////////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
//////////////////////////////////////////////    else unquoted
//////////////////////////////////////////////  }
//////////////////////////////////////////////}
//////////////////////////////////////////////
//////////////////////////////////////////////trait LogCollector {
//////////////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
//////////////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
//////////////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
//////////////////////////////////////////////}
//////////////////////////////////////////////
//////////////////////////////////////////////object LogCollector {
//////////////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
//////////////////////////////////////////////    for {
//////////////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
//////////////////////////////////////////////    } yield new LogCollector {
//////////////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
//////////////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
//////////////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
//////////////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
//////////////////////////////////////////////    }
//////////////////////////////////////////////  }
//////////////////////////////////////////////}
//////////////////////////////////////////////
////////////////////////////////////////////////package zio.bdd.core
////////////////////////////////////////////////
////////////////////////////////////////////////import zio.*
////////////////////////////////////////////////import scala.util.matching.Regex
////////////////////////////////////////////////import java.time.Instant
////////////////////////////////////////////////
////////////////////////////////////////////////case class StepResult(
////////////////////////////////////////////////  step: String,
////////////////////////////////////////////////  succeeded: Boolean,
////////////////////////////////////////////////  error: Option[String],
////////////////////////////////////////////////  output: Any,
////////////////////////////////////////////////  logs: List[(String, Instant)]
////////////////////////////////////////////////)
////////////////////////////////////////////////
////////////////////////////////////////////////object ScenarioRunner {
////////////////////////////////////////////////  private def flattenOutput(value: Any): Any = value match {
////////////////////////////////////////////////    case () => ()
////////////////////////////////////////////////    case (a, b) =>
////////////////////////////////////////////////      (flattenOutput(a), flattenOutput(b)) match {
////////////////////////////////////////////////        case ((), ()) => ()
////////////////////////////////////////////////        case ((), b)  => b
////////////////////////////////////////////////        case (a, ())  => a
////////////////////////////////////////////////        case (a, b)   => (a, b)
////////////////////////////////////////////////      }
////////////////////////////////////////////////    case other => other
////////////////////////////////////////////////  }
////////////////////////////////////////////////
////////////////////////////////////////////////  private def combine(prev: Any, params: List[String]): Any = {
////////////////////////////////////////////////    val flattenedPrev = flattenOutput(prev)
////////////////////////////////////////////////    params match {
////////////////////////////////////////////////      case Nil => flattenedPrev
////////////////////////////////////////////////      case head :: Nil =>
////////////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////////////          case ()     => parseParam(head)
////////////////////////////////////////////////          case single => (single, parseParam(head))
////////////////////////////////////////////////        }
////////////////////////////////////////////////      case many =>
////////////////////////////////////////////////        flattenedPrev match {
////////////////////////////////////////////////          case ()     => Tuple.fromArray(many.map(parseParam).toArray)
////////////////////////////////////////////////          case single => Tuple.fromArray((single :: many.map(parseParam)).toArray)
////////////////////////////////////////////////        }
////////////////////////////////////////////////    }
////////////////////////////////////////////////  }
////////////////////////////////////////////////
////////////////////////////////////////////////  def run[R](
////////////////////////////////////////////////    steps: ZIOSteps[R],
////////////////////////////////////////////////    scenario: String,
////////////////////////////////////////////////    metadata: ScenarioMetadata = ScenarioMetadata()
////////////////////////////////////////////////  ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
////////////////////////////////////////////////    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList
////////////////////////////////////////////////
////////////////////////////////////////////////    def runSteps(
////////////////////////////////////////////////      remaining: List[String],
////////////////////////////////////////////////      previousOutput: Any,
////////////////////////////////////////////////      acc: List[StepResult]
////////////////////////////////////////////////    ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
////////////////////////////////////////////////      remaining match {
////////////////////////////////////////////////        case Nil => ZIO.succeed(acc.reverse)
////////////////////////////////////////////////        case line :: rest =>
////////////////////////////////////////////////          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
////////////////////////////////////////////////            case Some(stepDef) =>
////////////////////////////////////////////////              val pattern = stepDef.pattern
////////////////////////////////////////////////              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
////////////////////////////////////////////////              val params  = extractParams(pattern, line)
////////////////////////////////////////////////              val input   = combine(previousOutput, params)
////////////////////////////////////////////////              for {
////////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////////////                _         <- collector.log(s"Executing: $line with input: $input")
////////////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////////////                result <- fn(input).map { output =>
////////////////////////////////////////////////                            StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
////////////////////////////////////////////////                          }
////////////////////////////////////////////////                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
////////////////////////////////////////////////              } yield finalResult
////////////////////////////////////////////////            case None =>
////////////////////////////////////////////////              for {
////////////////////////////////////////////////                collector <- ZIO.service[LogCollector]
////////////////////////////////////////////////                _         <- collector.log(s"No step definition matches: $line")
////////////////////////////////////////////////                logs      <- collector.getLogs
////////////////////////////////////////////////                _         <- collector.clearLogs
////////////////////////////////////////////////                next <- runSteps(
////////////////////////////////////////////////                          rest,
////////////////////////////////////////////////                          previousOutput,
////////////////////////////////////////////////                          StepResult(
////////////////////////////////////////////////                            line,
////////////////////////////////////////////////                            succeeded = false,
////////////////////////////////////////////////                            error = Some("No step definition matches"),
////////////////////////////////////////////////                            output = (),
////////////////////////////////////////////////                            logs = logs
////////////////////////////////////////////////                          ) :: acc
////////////////////////////////////////////////                        )
////////////////////////////////////////////////              } yield next
////////////////////////////////////////////////          }
////////////////////////////////////////////////      }
////////////////////////////////////////////////
////////////////////////////////////////////////    val effect = runSteps(lines, (), Nil)
////////////////////////////////////////////////
////////////////////////////////////////////////    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
////////////////////////////////////////////////  }
////////////////////////////////////////////////
////////////////////////////////////////////////  def runScenarios[R](
////////////////////////////////////////////////    steps: ZIOSteps[R],
////////////////////////////////////////////////    scenarios: List[(String, ScenarioMetadata)],
////////////////////////////////////////////////    parallelism: Int = 4
////////////////////////////////////////////////  ): ZIO[R with LogCollector with Reporter, Throwable, List[List[StepResult]]] =
////////////////////////////////////////////////    ZIO
////////////////////////////////////////////////      .foreachPar(scenarios) { case (scenario, metadata) =>
////////////////////////////////////////////////        val scenarioId = scenario.hashCode.toString
////////////////////////////////////////////////        ZIO.logAnnotate("scenarioId", scenarioId) {
////////////////////////////////////////////////          for {
////////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
////////////////////////////////////////////////            results <- run(steps, scenario, metadata)
////////////////////////////////////////////////            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
////////////////////////////////////////////////          } yield results
////////////////////////////////////////////////        }
////////////////////////////////////////////////      }
////////////////////////////////////////////////      .withParallelism(parallelism)
////////////////////////////////////////////////
////////////////////////////////////////////////  private def extractParams(pattern: Regex, line: String): List[String] =
////////////////////////////////////////////////    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)
////////////////////////////////////////////////
////////////////////////////////////////////////  private def parseParam(param: String): Any = {
////////////////////////////////////////////////    val trimmed = param.trim
////////////////////////////////////////////////    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
////////////////////////////////////////////////      trimmed.substring(1, trimmed.length - 1)
////////////////////////////////////////////////    } else {
////////////////////////////////////////////////      trimmed
////////////////////////////////////////////////    }
////////////////////////////////////////////////    if (unquoted.matches("\\d+")) unquoted.toInt
////////////////////////////////////////////////    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
////////////////////////////////////////////////    else unquoted
////////////////////////////////////////////////  }
////////////////////////////////////////////////}
////////////////////////////////////////////////
////////////////////////////////////////////////trait LogCollector {
////////////////////////////////////////////////  def log(message: String): ZIO[Any, Nothing, Unit]
////////////////////////////////////////////////  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
////////////////////////////////////////////////  def clearLogs: ZIO[Any, Nothing, Unit]
////////////////////////////////////////////////}
////////////////////////////////////////////////
////////////////////////////////////////////////object LogCollector {
////////////////////////////////////////////////  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
////////////////////////////////////////////////    for {
////////////////////////////////////////////////      ref <- Ref.make[List[(String, Instant)]](Nil)
////////////////////////////////////////////////    } yield new LogCollector {
////////////////////////////////////////////////      def log(message: String): ZIO[Any, Nothing, Unit] =
////////////////////////////////////////////////        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
////////////////////////////////////////////////      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
////////////////////////////////////////////////      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
////////////////////////////////////////////////    }
////////////////////////////////////////////////  }
////////////////////////////////////////////////}
