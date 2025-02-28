package zio.bdd.runtime

import zio.*
import zio.bdd.dsl.*
import scala.util.matching.Regex

object ScenarioRunner {
  final case class StepResult(step: String, succeeded: Boolean, error: Option[String], output: Any)

  def run[R](steps: ZIOSteps[R], scenario: String): ZIO[R, Nothing, List[StepResult]] = {
    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList

    def runSteps(
      remaining: List[String],
      previousOutput: Any,
      acc: List[StepResult]
    ): ZIO[R, Nothing, List[StepResult]] =
      remaining match {
        case Nil => ZIO.succeed(acc.reverse)
        case line :: rest =>
          steps.getSteps.find(_.pattern.findFirstIn(line).isDefined) match {
            case Some(stepDef) =>
              val pattern = stepDef.pattern
              val fn      = stepDef.asInstanceOf[ZIOSteps[R]#StepDef[Any, Any]].fn
              val params  = extractParams(pattern, line)
              val input   = combine(previousOutput, params)
              for {
                _ <- ZIO.log(s"Executing: $line with input: $input")
                result <- fn(input).map { output =>
                            StepResult(line, succeeded = true, error = None, output = output) :: acc
                          }.catchAll { error =>
                            ZIO.succeed(
                              StepResult(line, succeeded = false, error = Some(error.getMessage), output = ()) :: acc
                            )
                          }
                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
              } yield finalResult
            case None =>
              runSteps(
                rest,
                previousOutput,
                StepResult(line, succeeded = false, error = Some("No step definition matches"), output = ()) :: acc
              )
          }
      }

    runSteps(lines, (), Nil).map { results =>
      printReport(results)
      results
    }
  }

  private def printReport(results: List[StepResult]): Unit = {
    println("\n=== Scenario Execution Report ===")
    results.foreach { result =>
      val status   = if (result.succeeded) "PASSED" else "FAILED"
      val errorMsg = result.error.map(msg => s" - Error: $msg").getOrElse("")
      println(s"[$status] ${result.step}$errorMsg")
    }
    val passed = results.count(_.succeeded)
    val failed = results.length - passed
    println(s"\nSummary: $passed passed, $failed failed, ${results.length} total")
    println("===============================\n")
  }

  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def combine(prev: Any, params: List[String]): Any = params match {
    case Nil => prev // Pass previous output directly if no params
    case head :: Nil =>
      prev match {
        case () => parseParam(head)         // Use param if no previous output
        case _  => (prev, parseParam(head)) // Combine if both exist
      }
    case many =>
      prev match {
        case () => Tuple.fromArray(many.map(parseParam).toArray)
        case _  => (prev, Tuple.fromArray(many.map(parseParam).toArray))
      }
  }

  private def parseParam(param: String): Any = {
    val trimmed = param.trim
    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
      trimmed.substring(1, trimmed.length - 1) // Strip quotes
    } else {
      trimmed
    }
    if (unquoted.matches("\\d+")) unquoted.toInt
    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
    else unquoted
  }
}
