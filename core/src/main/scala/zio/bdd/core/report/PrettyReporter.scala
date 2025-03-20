package zio.bdd.core.report

import zio.*
import zio.bdd.core.{InternalLogLevel, LogCollector, StepResult}
import zio.bdd.gherkin.{Feature, StepType}

class PrettyReporter extends Reporter {
  private val LightGreen  = "\u001b[92m" // Passed (bright green)
  private val LightRed    = "\u001b[91m" // Failed, Error (bright red)
  private val LightBlue   = "\u001b[94m" // Features/Steps (bright blue)
  private val LightYellow = "\u001b[93m" // Scenarios (bright yellow)
  private val LightGray   = "\u001b[90m" // Ignored (bright gray)
  private val Reset       = "\u001b[0m"

  // Pale colors for logs
  private val PaleGreen  = "\u001b[38;2;144;238;144m" // Info (pale green)
  private val PaleCyan   = "\u001b[38;2;173;216;230m" // Debug (pale cyan)
  private val PaleYellow = "\u001b[38;2;245;245;220m" // Warning (pale beige/yellow)
  private val PaleRed    = "\u001b[38;2;255;182;193m" // Error, Fatal (pale red/pink)

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
      report       <- buildFinalReport(features, logCollector, results)
      _            <- logCollector.logFeature("report", report, InternalLogLevel.Info)
      _            <- Console.printLine(report).orDie
    } yield report

  private def buildFinalReport(
    features: List[Feature],
    logCollector: LogCollector,
    results: List[List[StepResult]]
  ): ZIO[Any, Nothing, String] = {
    val allSteps = results.flatten

    // Extract executed feature and scenario IDs
    val executedFeatureIds  = allSteps.flatMap(_.featureId).toSet
    val executedScenarioIds = allSteps.flatMap(_.scenarioId).toSet

    val totalFeatures   = features.length
    val ignoredFeatures = features.count(f => !executedFeatureIds.contains(f.name))
    val failedFeatures = features.count(f =>
      results.exists(r => r.headOption.exists(_.featureId.contains(f.name)) && r.exists(!_.succeeded))
    )
    val totalScenarios  = features.map(_.scenarios.length).sum
    val failedScenarios = results.count(_.exists(!_.succeeded))
    val ignoredScenarios = totalScenarios - features
      .filter(feature => executedFeatureIds.contains(feature.name))
      .flatMap(feature => feature.scenarios.map(scenario => (feature.name, scenario)))
      .count { case (featureName, scenario) =>
        executedScenarioIds.contains(s"$featureName-${scenario.name}".hashCode.toString)
      }

    val stats = ReportStats(
      features = totalFeatures,
      failedFeatures = failedFeatures,
      ignoredFeatures = ignoredFeatures,
      scenarios = totalScenarios,
      failedScenarios = failedScenarios,
      ignoredScenarios = ignoredScenarios
    )

    ZIO.succeed {
      val featureDetails = features.map { feature =>
        val featureId         = feature.name
        val featureSteps      = results.filter(_.headOption.exists(_.featureId.contains(featureId)))
        val executedScenarios = featureSteps.map(_.head.scenarioId.getOrElse("unknown")).toSet
        val passedScenarios   = featureSteps.count(_.forall(_.succeeded))
        val failedScenarios   = featureSteps.count(_.exists(!_.succeeded))
        val ignoredScenarios =
          feature.scenarios.count(s => !executedScenarios.contains(s"${feature.name}-${s.name}".hashCode.toString))
        val featureStatus =
          if (failedScenarios > 0) s"${LightRed}[FAILED]${Reset}"
          else if (passedScenarios > 0) s"${LightGreen}[PASSED]${Reset}"
          else s"${LightGray}[IGNORED]${Reset}"
        val featureLine = s"* $featureStatus ${LightBlue}Feature: ${feature.name}"

        val scenarioLines = feature.scenarios.map { scenario =>
          val scenarioId = s"${feature.name}-${scenario.name}".hashCode.toString
          val steps      = featureSteps.find(_.headOption.exists(_.scenarioId.contains(scenarioId))).getOrElse(Nil)
          val isIgnored  = scenario.metadata.isIgnored || steps.isEmpty
          val scenarioStatus =
            if (isIgnored) s"${LightGray}[IGNORED]${Reset}"
            else if (steps.exists(!_.succeeded)) s"${LightRed}[FAILED]${Reset}"
            else s"${LightGreen}[PASSED]${Reset}"
          val scenarioLine = s"  ${LightYellow}◉ $scenarioStatus ${LightYellow}${scenario.name}"

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
                    case InternalLogLevel.Info    => PaleGreen
                    case InternalLogLevel.Debug   => PaleCyan
                    case InternalLogLevel.Warning => PaleYellow
                    case InternalLogLevel.Error   => PaleRed
                    case InternalLogLevel.Fatal   => PaleRed
                  }
                  s"      ${LightGray}├─${Reset} $color ${entry.message}$Reset"
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
                                s"\n$stepLogs" + (if (errorLines.nonEmpty && stepLogs.nonEmpty) s"\n$errorLines"
                                                  else if (errorLines.nonEmpty) s"\n$errorLines"
                                                  else "")
                              else "")
            }
            .mkString("\n")

          val scenarioSummary = if (!isIgnored && steps.nonEmpty) {
            val passed = steps.count(_.succeeded)
            val failed = steps.length - passed
            s"  ${LightYellow}◉${Reset} Results: $LightGreen$passed passed$Reset, $LightRed$failed failed$Reset"
          } else ""

          s"$scenarioLine" + (if (stepLines.nonEmpty) s"\n$stepLines" else "") + (if (scenarioSummary.nonEmpty)
                                                                                    s"\n$scenarioSummary"
                                                                                  else "")
        }.mkString("\n")

        val finishedFeature = if (feature.scenarios.isEmpty) {
          s"* ${LightGray}Feature: ${feature.name} - 0 passed, 0 failed, ${feature.scenarios.length} ignored$Reset"
        } else {
          s"* ${LightBlue}Feature: ${feature.name} - $LightGreen$passedScenarios passed$Reset, $LightRed$failedScenarios failed$Reset, $LightGray$ignoredScenarios ignored$Reset"
        }

        s"$featureLine" + (if (scenarioLines.nonEmpty) s"\n$scenarioLines" else "") + s"\n$finishedFeature"
      }.mkString("\n")

      s"""
         |$featureDetails
         |
         |${LightBlue}Features:${Reset} ${LightGreen}${stats.features - stats.failedFeatures - stats.ignoredFeatures} passed${Reset}, ${LightRed}${stats.failedFeatures} failed${Reset}, ${LightGray}${stats.ignoredFeatures} ignored${Reset}
         |${LightYellow}Scenarios:${Reset} ${LightGreen}${stats.scenarios - stats.failedScenarios - stats.ignoredScenarios} passed${Reset}, ${LightRed}${stats.failedScenarios} failed${Reset}, ${LightGray}${stats.ignoredScenarios} ignored${Reset}
         |""".stripMargin.trim
    }
  }

  private case class ReportStats(
    features: Int,
    failedFeatures: Int,
    ignoredFeatures: Int,
    scenarios: Int,
    failedScenarios: Int,
    ignoredScenarios: Int
  )
}

object PrettyReporter {
  def live: ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(new PrettyReporter())
}
