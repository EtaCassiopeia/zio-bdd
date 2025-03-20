package zio.bdd.core.report

import zio.*
import zio.bdd.core.{InternalLogLevel, LogCollector, StepResult}
import zio.bdd.gherkin.{Feature, StepType}

case class PrettyReporter() extends Reporter {
  private val LightGreen  = "\u001b[92m" // Passed, Info
  private val LightRed    = "\u001b[91m" // Failed, Error
  private val LightBlue   = "\u001b[94m" // Features/Steps
  private val LightYellow = "\u001b[93m" // Scenarios
  private val LightGray   = "\u001b[90m" // Ignored/Warning/Debug
  private val Reset       = "\u001b[0m"

  override def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
  override def endFeature(
    feature: String,
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  override def startScenario(scenario: String): ZIO[Any, Nothing, Unit]                                   = ZIO.unit
  override def endScenario(scenario: String, results: List[StepResult]): ZIO[LogCollector, Nothing, Unit] = ZIO.unit
  override def startStep(step: String): ZIO[Any, Nothing, Unit]                                           = ZIO.unit
  override def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit]                         = ZIO.unit
  override def reportIgnoredScenario(scenario: String): ZIO[Any, Nothing, Unit]                           = ZIO.unit

  override def generateFinalReport(
    features: List[Feature],
    results: List[List[StepResult]],
    ignoredCount: Int
  ): ZIO[LogCollector, Nothing, String] =
    for {
      logCollector <- ZIO.service[LogCollector]
      allSteps      = results.flatten
      stats = ReportStats(
                features = features.length,
                scenarios = features.flatMap(_.scenarios).length, // Total scenarios including ignored
                failedScenarios = results.count(_.exists(!_.succeeded)),
                ignoredScenarios = ignoredCount + features
                  .flatMap(_.scenarios)
                  .count(s => s.metadata.isIgnored || !s.tags.contains("positive")),
                steps = allSteps.length,
                failedSteps = allSteps.count(!_.succeeded)
              )
      report <- buildFinalReport(stats, features, logCollector, results)
      _      <- logCollector.logFeature("report", report, InternalLogLevel.Info)
      _      <- Console.printLine(report).orDie
    } yield report

  private def buildFinalReport(
    stats: ReportStats,
    features: List[Feature],
    logCollector: LogCollector,
    results: List[List[StepResult]]
  ): ZIO[Any, Nothing, String] = {
    val featurePassed = stats.features - features.count(f =>
      results.exists(_.exists(r => !r.succeeded && r.featureId.contains(f.name.hashCode.toString)))
    )
    val featureFailed  = stats.features - featurePassed - features.count(_.scenarios.isEmpty)
    val scenarioPassed = results.count(_.forall(_.succeeded)) // Count scenarios where all steps succeeded
    val stepPassed     = stats.steps - stats.failedSteps

    ZIO.succeed {
      val featureDetails = features.map { feature =>
        val featureId      = feature.name.hashCode.toString
        val hasNoScenarios = feature.scenarios.isEmpty
        val featureStatus =
          if (hasNoScenarios) s"${LightGray}[IGNORED]${Reset}"
          else if (results.exists(_.exists(r => !r.succeeded && r.featureId.contains(featureId))))
            s"${LightRed}[FAILED]${Reset}"
          else s"${LightGreen}[PASSED]${Reset}"
        val featureLine = s"* $featureStatus Feature: ${feature.name}"

        val scenarioLines = feature.scenarios.map { scenario =>
          val scenarioId = s"${feature.name}-${scenario.name}".hashCode.toString
          val steps      = results.find(_.headOption.exists(_.scenarioId.contains(scenarioId))).getOrElse(Nil)
          val isIgnored  = scenario.metadata.isIgnored || !scenario.tags.contains("positive")
          val scenarioStatus =
            if (isIgnored || steps.isEmpty) s"${LightGray}[IGNORED]${Reset}"
            else if (steps.exists(!_.succeeded)) s"${LightRed}[FAILED]${Reset}"
            else s"${LightGreen}[PASSED]${Reset}"
          val scenarioLine = s"  ${LightYellow}◉${Reset} $scenarioStatus ${scenario.name}"

          val stepLines = steps
            .zip(scenario.steps)
            .map { case (step, gherkinStep) =>
              val stepId = step.stepId.getOrElse(step.step.hashCode.toString)
              val stepType = gherkinStep.stepType match {
                case StepType.GivenStep => "Given"
                case StepType.WhenStep  => "When"
                case StepType.ThenStep  => "Then"
                case StepType.AndStep   => "And"
              }
              val stepStatus = if (step.succeeded) s"${LightGreen}[PASSED]${Reset}" else s"${LightRed}[FAILED]${Reset}"
              val timing = s"(start: ${step.startTime}, duration: ${step.duration.toMillis}ms${step.file
                  .map(f => s", file: $f:${step.line.getOrElse(-1)}")
                  .getOrElse("")})"
              val stepLine = s"    ${LightBlue}├─◑${Reset} $stepStatus $stepType ${step.step} $timing"

              val stepLogs = Unsafe.unsafe { implicit u =>
                Runtime.default.unsafe.run(logCollector.getLogs(scenarioId, stepId)).getOrThrowFiberFailure()
              }.entries
                .sortBy(_.timestamp)
                .map { entry =>
                  val color = entry.level match {
                    case InternalLogLevel.Info    => LightGreen
                    case InternalLogLevel.Debug   => LightGray
                    case InternalLogLevel.Warning => LightYellow
                    case InternalLogLevel.Error   => LightRed
                    case InternalLogLevel.Fatal   => LightRed
                  }
                  s"      ${LightGray}├─${Reset} $color${entry.level}: ${entry.message}$Reset"
                }
                .mkString("\n")

              val errorLines = step.error.map { e =>
                val traceLines = e.trace.flatMap { t =>
                  Trace.unapply(t).map { case (loc, file, line) =>
                    s"      ${LightGray}├─${Reset} ${LightRed}Trace: $loc ($file:$line)$Reset"
                  }
                }.getOrElse("")
                val causeLines = e.cause.map { c =>
                  val stack = c.getStackTrace
                    .take(5)
                    .map(st => s"        ${LightGray}│${Reset} $LightRed$st$Reset")
                    .mkString("\n")
                  s"      ${LightGray}├─${Reset} ${LightRed}Cause: ${c.getMessage}\n$stack$Reset"
                }.getOrElse("")
                s"      ${LightGray}├─${Reset} ${LightRed}Error: ${e.message}$Reset" + (if (
                                                                                          traceLines.nonEmpty || causeLines.nonEmpty
                                                                                        )
                                                                                          s"\n$traceLines" + (if (
                                                                                                                causeLines.nonEmpty
                                                                                                              ) s"\n$causeLines"
                                                                                                              else "")
                                                                                        else "")
              }.getOrElse("")

              s"$stepLine" + (if (stepLogs.nonEmpty || errorLines.nonEmpty)
                                s"\n$stepLogs" + (if (errorLines.nonEmpty && stepLogs.nonEmpty) "\n"
                                                  else "") + errorLines
                              else "")
            }
            .mkString("\n")

          val passed = steps.count(_.succeeded)
          val failed = steps.length - passed
          val scenarioSummary =
            if (!isIgnored && steps.nonEmpty)
              s"  ${LightYellow}◉${Reset} Results: $LightGreen$passed passed$Reset, $LightRed$failed failed$Reset"
            else ""

          s"$scenarioLine" + (if (stepLines.nonEmpty) s"\n$stepLines" else "") + (if (scenarioSummary.nonEmpty)
                                                                                    s"\n$scenarioSummary"
                                                                                  else "")
        }.mkString("\n")

        val finishedFeature = if (hasNoScenarios) {
          s"* ${LightGray}Finished Feature: ${feature.name} - 0 passed, 0 failed, 1 ignored$Reset"
        } else {
          val passed  = results.flatten.count(_.succeeded)
          val failed  = results.flatten.length - passed
          val ignored = feature.scenarios.count(s => s.metadata.isIgnored || !s.tags.contains("positive"))
          s"* ${LightBlue}Finished Feature: ${feature.name} - $LightGreen$passed passed$Reset, $LightRed$failed failed$Reset, $LightGray$ignored ignored$Reset"
        }

        s"$featureLine" + (if (scenarioLines.nonEmpty) s"\n$scenarioLines" else "") + s"\n$finishedFeature"
      }.mkString("\n")

      s"""
         |$featureDetails
         |
         |${LightBlue}Finished Features:${Reset} ${LightGreen}$featurePassed passed${Reset}, ${LightRed}$featureFailed failed${Reset}, ${LightGray}${stats.features - featurePassed - featureFailed} ignored${Reset}
         |${LightYellow}Finished Scenarios:${Reset} ${LightGreen}$scenarioPassed passed${Reset}, ${LightRed}${stats.failedScenarios} failed${Reset}, ${LightGray}${stats.ignoredScenarios} ignored${Reset}
         |${LightBlue}Finished Steps:${Reset} ${LightGreen}$stepPassed passed${Reset}, ${LightRed}${stats.failedSteps} failed${Reset}
         |""".stripMargin.trim
    }
  }

  private case class ReportStats(
    features: Int,
    scenarios: Int,
    failedScenarios: Int,
    ignoredScenarios: Int,
    steps: Int,
    failedSteps: Int
  )
}

object PrettyReporter {
  def live: ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(PrettyReporter())
}
