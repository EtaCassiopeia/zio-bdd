package zio.bdd.core

import zio.*
import scala.util.matching.Regex
import java.time.Instant

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
  ): ZIO[R with LogCollector, Throwable, List[StepResult]] = {
    val lines = scenario.split("\n").map(_.trim).filter(_.nonEmpty).toList

    def runSteps(
      remaining: List[String],
      previousOutput: Any,
      acc: List[StepResult]
    ): ZIO[R with LogCollector, Throwable, List[StepResult]] =
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
                collector <- ZIO.service[LogCollector]
                _         <- collector.log(s"Executing: $line with input: $input")
                logs      <- collector.getLogs
                _         <- collector.clearLogs
                result <- fn(input).map { output =>
                            StepResult(line, succeeded = true, error = None, output = output, logs = logs) :: acc
                          }
                finalResult <- runSteps(rest, result.headOption.map(_.output).getOrElse(()), result)
              } yield finalResult
            case None =>
              for {
                collector <- ZIO.service[LogCollector]
                _         <- collector.log(s"No step definition matches: $line")
                logs      <- collector.getLogs
                _         <- collector.clearLogs
                next <- runSteps(
                          rest,
                          previousOutput,
                          StepResult(
                            line,
                            succeeded = false,
                            error = Some("No step definition matches"),
                            output = (),
                            logs = logs
                          ) :: acc
                        )
              } yield next
          }
      }

    val effect = runSteps(lines, (), Nil)

    effect.repeatN(metadata.repeatCount - 1).retryN(metadata.retryCount)
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
            _       <- ZIO.service[Reporter].flatMap(_.startScenario(scenario))
            results <- run(steps, scenario, metadata)
            _       <- ZIO.service[Reporter].flatMap(_.endScenario(scenario, results))
          } yield results
        }
      }
      .withParallelism(parallelism)

  private def extractParams(pattern: Regex, line: String): List[String] =
    pattern.findFirstMatchIn(line).map(_.subgroups).getOrElse(Nil)

  private def parseParam(param: String): Any = {
    val trimmed = param.trim
    val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
      trimmed.substring(1, trimmed.length - 1)
    } else {
      trimmed
    }
    if (unquoted.matches("\\d+")) unquoted.toInt
    else if (unquoted.matches("\\d+\\.\\d+")) unquoted.toDouble
    else unquoted
  }
}

trait LogCollector {
  def log(message: String): ZIO[Any, Nothing, Unit]
  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
  def clearLogs: ZIO[Any, Nothing, Unit]
}

object LogCollector {
  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
    for {
      ref <- Ref.make[List[(String, Instant)]](Nil)
    } yield new LogCollector {
      def log(message: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
    }
  }
}
