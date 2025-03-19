package zio.bdd.core.report

import zio.*
import zio.bdd.core.{CollectedLogs, InternalLogLevel, LogCollector, StepResult, TestError}

case class PrettyReporter(
  resultsRef: Ref[Map[String, (List[(String, List[StepResult])], Int)]],
  currentScenarios: Ref[Map[String, List[String]]]
) extends Reporter {
  private val LightGreen    = "\u001b[92m" // Passed, Info
  private val LightRed      = "\u001b[91m" // Failed, Error
  private val LightBlue     = "\u001b[94m" // Features/Steps
  private val LightYellow   = "\u001b[93m" // Scenarios
  private val LightGray     = "\u001b[90m" // Ignored
  private val BrightCyan    = "\u001b[96m" // Debug
  private val BrightMagenta = "\u001b[95m" // Unused here, but kept for consistency
  private val Reset         = "\u001b[0m"

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] =
    for {
      _ <- resultsRef.update(_.updated(feature, (Nil, 0)))
      _ <- currentScenarios.update(_.updated(feature, Nil))
      _ <- ZIO.logInfo(s"Starting feature: $feature")
    } yield ()

  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] =
    for {
      scenarioNames <- currentScenarios.get.map(_.getOrElse(feature, Nil))
      pairedResults  = scenarioNames.reverse.zipAll(results, "Unknown Scenario", Nil)
      _             <- resultsRef.update(_.updated(feature, (pairedResults, ignoredCount)))
      allResults    <- resultsRef.get
      _ <- ZIO.when(allResults.forall(_._2._1.nonEmpty)) {
             for {
               _ <- printReport(allResults)
               _ <- resultsRef.set(Map.empty)
               _ <- currentScenarios.set(Map.empty)
               _ <- ZIO.serviceWithZIO[LogCollector](_.clearLogs)
             } yield ()
           }
    } yield ()

  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    for {
      map           <- resultsRef.get
      currentFeature = map.keys.lastOption.getOrElse("Unknown Feature")
      _ <- currentScenarios.update { scenarios =>
             scenarios.updated(currentFeature, scenario :: scenarios.getOrElse(currentFeature, Nil))
           }
      scenarioId = s"$currentFeature-$scenario".hashCode.toString
      _         <- ZIO.logAnnotate("scenarioId", scenarioId)(ZIO.logDebug(s"Starting scenario: $scenario"))
    } yield ()

  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] =
    ZIO.logDebug(s"Ending scenario: $scenario with ${results.length} steps")

  override def startStep(step: String): ZIO[Any, Nothing, Unit] =
    ZIO.unit

  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] =
    ZIO.unit

  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit] =
    resultsRef.update { map =>
      map.map { case (feature, (results, ignoredCount)) =>
        (feature, (results, ignoredCount + 1))
      }
    }

  override def generateFinalReport(
    features: List[zio.bdd.gherkin.Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String] =
    for {
      logCollector <- ZIO.service[LogCollector]
      stats = ReportStats(
                features = features.length,
                scenarios = results.length,
                failedScenarios = results.count(_.exists(!_.succeeded)),
                ignoredScenarios = ignoredCount
              )
      report = buildFinalReport(stats)
      _     <- logCollector.logFeature("report", report, InternalLogLevel.Info)
      _     <- Console.printLine(report).orDie
    } yield report

  private def buildFinalReport(stats: ReportStats): String = {
    val featurePassed   = stats.features // All features "pass" if executed, no failure concept here
    val featureFailed   = 0
    val featureIgnored  = 0              // No feature-level ignore in your setup
    val scenarioPassed  = stats.scenarios - stats.failedScenarios - stats.ignoredScenarios
    val scenarioFailed  = stats.failedScenarios
    val scenarioIgnored = stats.ignoredScenarios

    s"""
       |
       |${LightBlue}Finished Features:${Reset} ${LightGreen}$featurePassed passed${Reset}, ${LightRed}$featureFailed failed${Reset}, ${LightGray}$featureIgnored ignored${Reset}
       |${LightBlue}Finished Scenarios:${Reset} ${LightGreen}$scenarioPassed passed${Reset}, ${LightRed}$scenarioFailed failed${Reset}, ${LightGray}$scenarioIgnored ignored${Reset}
       |
       |""".stripMargin.trim
  }

  private def printReport(
    results: Map[String, (List[(String, List[StepResult])], Int)]
  ): ZIO[LogCollector, Nothing, Unit] =
    ZIO.foreachDiscard(results.toList.sortBy(_._1)) { case (feature, (scenarioResults, ignoredCount)) =>
      for {
        _ <- Console.printLine(s"${LightBlue}* Feature: $feature${Reset}").orDie
        _ <- ZIO.foreachDiscard(scenarioResults) { case (scenarioName, results) =>
               val scenarioId = s"$feature-$scenarioName".hashCode.toString
               for {
                 logs <- ZIO.serviceWithZIO[LogCollector](_.getScenarioLogs(scenarioId))
                 _    <- printScenario(scenarioName, results, logs)
               } yield ()
             }
        passed = scenarioResults.flatMap(_._2).count(_.succeeded)
        failed = scenarioResults.flatMap(_._2).length - passed
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
             val errorMsg = result.error.map(e => s" - ${LightRed}Error: ${e.message}${Reset}").getOrElse("")
             val traceMsg = result.error
               .flatMap(_.trace)
               .map { t =>
                 Trace.unapply(t) match {
                   case Some((location, file, line)) => s"${LightRed}Trace: $location ($file:$line)${Reset}"
                   case None                         => s"${LightRed}Trace unavailable${Reset}"
                 }
               }
               .getOrElse("")
             val stepId = result.step.hashCode.toString
             val stepLogs = logs.entries
               .filter(_.stepId == stepId)
               .sortBy(_.timestamp)
             val logOutput = if (stepLogs.nonEmpty) {
               stepLogs.map { entry =>
                 val color = entry.level match {
                   case InternalLogLevel.Info  => LightGreen
                   case InternalLogLevel.Debug => BrightCyan
                   case InternalLogLevel.Error => LightRed
                   case _                      => LightYellow
                 }
                 // s"${color}    ├─ [${entry.timestamp}] [${entry.level}] ${entry.message}${Reset}"
                 s"${color}    ├─ ${entry.message}${Reset}"
               }.mkString("\n")
             } else ""
             val fileName = result.file.getOrElse("unknown").split("[/\\\\]").last
             val timing =
               s" (start: ${result.startTime}, duration: ${result.duration.toMillis}ms, file: $fileName:${result.line
                   .getOrElse(-1)})"
             Console
               .printLine(
                 s"${LightBlue}    ├─◑ [$status] ${result.step}$errorMsg$timing${Reset}" +
                   (if (logOutput.nonEmpty || traceMsg.nonEmpty)
                      s"\n$logOutput${if (traceMsg.nonEmpty && logOutput.nonEmpty) "\n" else ""}$traceMsg"
                    else "")
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

  private case class ReportStats(
    features: Int,
    scenarios: Int,
    failedScenarios: Int,
    ignoredScenarios: Int
  )
}

object PrettyReporter {
  def live: ZLayer[Any, Nothing, Reporter] =
    ZLayer.fromZIO(
      for {
        resultsRef   <- Ref.make(Map.empty[String, (List[(String, List[StepResult])], Int)])
        scenariosRef <- Ref.make(Map.empty[String, List[String]])
      } yield PrettyReporter(resultsRef, scenariosRef)
    )
}
