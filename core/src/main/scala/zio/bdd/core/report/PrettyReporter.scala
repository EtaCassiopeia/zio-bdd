package zio.bdd.core.report

import zio.*
import zio.bdd.core.{CollectedLogs, LogCollector, StepResult}

case class PrettyReporter(
  resultsRef: Ref[Map[String, (List[List[StepResult]], Int)]] // feature -> (results, ignoredCount)
) extends Reporter {
  // Reuse ConsoleReporter's ANSI colors
  private val LightGreen  = "\u001b[92m" // Bright green for passed
  private val LightRed    = "\u001b[91m" // Bright red for failed
  private val LightBlue   = "\u001b[94m" // Bright blue for features and steps
  private val LightYellow = "\u001b[93m" // Bright yellow for scenarios and logs
  private val LightGray   = "\u001b[90m" // Gray for ignored scenarios
  private val Reset       = "\u001b[0m"

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    resultsRef.update(_.updated(feature, (Nil, 0)))

  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] =
    for {
      _          <- resultsRef.update(_.updated(feature, (results, ignoredCount)))
      allResults <- resultsRef.get
      _ <- ZIO.when(allResults.forall(_._2._1.nonEmpty)) { // All features have results
             for {
               _ <- printReport(allResults)
               _ <- resultsRef.set(Map.empty) // Clear after printing
               _ <- ZIO.serviceWithZIO[LogCollector](_.clearLogs)
             } yield ()
           }
    } yield ()

  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = ZIO.unit

  override def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit

  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit

  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    resultsRef.update { map =>
      map.map { case (feature, (results, ignoredCount)) =>
        (feature, (results, ignoredCount + 1))
      }
    }

  private def printReport(
    results: Map[String, (List[List[StepResult]], Int)]
  ): ZIO[LogCollector, Nothing, Unit] =
    ZIO.foreachDiscard(results.toList.sortBy(_._1)) { case (feature, (scenarioResults, ignoredCount)) =>
      for {
        _ <- Console.printLine(s"${LightBlue}* Feature: $feature${Reset}").orDie
        _ <- ZIO.foreachDiscard(scenarioResults.zipWithIndex) { case (results, idx) =>
               val scenarioId = s"$feature-scenario-$idx"
               for {
                 logs <- LogCollector.getScenarioLogs(scenarioId)
                 _    <- printScenario(s"Scenario $idx", results, logs)
               } yield ()
             }
        passed = scenarioResults.flatten.count(_.succeeded)
        failed = scenarioResults.flatten.length - passed
        _ <-
          Console
            .printLine(
              s"${LightBlue}* Finished Feature: $feature - ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}, ${LightGray}$ignoredCount ignored${Reset}"
            )
            .orDie
      } yield ()
    }

  private def printScenario(
    scenario: String,
    results: List[StepResult],
    logs: CollectedLogs
  ): ZIO[Any, Nothing, Unit] =
    for {
      _ <- Console.printLine(s"${LightYellow}  ◉ $scenario${Reset}").orDie
      _ <- ZIO.foreachDiscard(results) { result =>
             val status   = if (result.succeeded) s"${LightGreen}PASSED${Reset}" else s"${LightRed}FAILED${Reset}"
             val errorMsg = result.error.map(t => s" - ${LightRed}Error: ${t.getMessage}${Reset}").getOrElse("")
             val stackTrace =
               result.error
                 .map(t => s"${LightRed}Stack trace:\n${t.getStackTrace.mkString("\n")}${Reset}")
                 .getOrElse("")
             val stepLogs = logs.toStepResultLogs.filter { case (_, time) =>
               time.isAfter(result.startTime) && time.isBefore(result.startTime.plusNanos(result.duration.toNanos))
             }
             val logOutput = if (stepLogs.nonEmpty) {
               stepLogs.map { case (msg, time) => s"${LightYellow}      ╰─ [$time] $msg${Reset}" }.mkString("\n")
             } else ""
             // Extract just the file name from the full path
             val fileName = result.file.getOrElse("unknown").split("[/\\\\]").last
             val timing =
               s" (start: ${result.startTime}, duration: ${result.duration.toMillis}ms, file: $fileName:${result.line
                   .getOrElse(-1)})"

             Console
               .printLine(
                 s"${LightBlue}    ├─◑ [$status] ${result.step}$errorMsg$timing${Reset}" +
                   (if (logOutput.nonEmpty || stackTrace.nonEmpty) s"\n$logOutput$stackTrace" else "")
               )
               .orDie
           }
      passed = results.count(_.succeeded)
      failed = results.length - passed
      _ <- Console
             .printLine(
               s"${LightYellow}  ◉ Results: ${LightGreen}$passed passed${Reset}, ${LightRed}$failed failed${Reset}"
             )
             .orDie
    } yield ()
}

object PrettyReporter {
  def live: ZLayer[Any, Nothing, Reporter] =
    ZLayer.fromZIO(
      Ref.make(Map.empty[String, (List[List[StepResult]], Int)]).map(PrettyReporter(_))
    )
}
