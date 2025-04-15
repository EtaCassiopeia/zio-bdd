package zio.bdd.core.report

import zio.*
import zio.bdd.core.*

import java.time.format.DateTimeFormatter

case class PrettyReporter() extends Reporter {
  // ANSI color codes
  private val LightGreen       = "\u001b[92m" // Passed (bright green)
  private val LightRed         = "\u001b[91m" // Failed, Error (bright red)
  private val LightBlue        = "\u001b[94m" // Features/Steps (bright blue)
  private val LightYellow      = "\u001b[93m" // Scenarios (bright yellow)
  private val LightGray        = "\u001b[90m" // Ignored (bright gray)
  private val LightBlueSummary = "\u001b[96m" // Summary (bright cyan)
  private val LightBlueFinal   = "\u001b[96m" // Final summary (bright cyan)
  private val Reset            = "\u001b[0m"

  // Pale colors for logs
  private val PaleGreen  = "\u001b[38;2;144;238;144m" // Info (pale green)
  private val PaleCyan   = "\u001b[38;2;173;216;230m" // Debug (pale cyan)
  private val PaleYellow = "\u001b[38;2;245;245;220m" // Warning (pale beige/yellow)
  private val PaleRed    = "\u001b[38;2;255;182;193m" // Error, Fatal (pale red/pink)

  private val Branch     = "├─ "
  private val LastBranch = "╰─ "
  private val Vertical   = "│  "
  private val Indent     = "   "

  private def printFeature(
    feature: FeatureResult,
    logCollector: LogCollector,
    isLast: Boolean
  ): ZIO[Any, Nothing, Unit] = {
    val (status, featureColor) =
      if (feature.isIgnored) ("IGNORED", LightGray)
      else if (feature.isPassed) ("PASSED", LightGreen)
      else ("FAILED", LightRed)
    val fileInfo = feature.feature.file.map(f => s" (${shortenFilePath(f)})").getOrElse("")
    val prefix   = if (isLast) LastBranch else Branch
    ZIO.succeed(println(s"$featureColor$prefix◉ Feature: ${feature.feature.name}$fileInfo - $status$Reset")) *>
      ZIO.foreachDiscard(feature.scenarioResults.zipWithIndex) { case (scenario, index) =>
        val isLastScenario = index == feature.scenarioResults.size - 1
        printScenario(scenario, logCollector, isLastScenario, if (isLast) Indent else Vertical, feature.isIgnored)
      } *>
      printFeatureSummary(feature, if (isLast) Indent else Vertical)
  }

  private def printScenario(
    scenarioResult: ScenarioResult,
    logCollector: LogCollector,
    isLast: Boolean,
    indent: String,
    featureIgnored: Boolean // Added parameter to pass feature's ignore status
  ): ZIO[Any, Nothing, Unit] = {
    val effectiveIgnored = featureIgnored || scenarioResult.isIgnored // Combine feature and scenario ignore status
    val (status, scenarioColor) =
      if (effectiveIgnored) ("IGNORED", LightGray)
      else if (scenarioResult.isPassed) ("PASSED", LightYellow)
      else ("FAILED", LightRed)
    val lineInfo      = scenarioResult.scenario.line.map(l => s":$l").getOrElse("")
    val prefix        = if (isLast) LastBranch else Branch
    val scenarioIdStr = scenarioResult.scenario.id.toString
    ZIO.succeed(
      println(s"$indent$scenarioColor$prefix◑ Scenario: ${scenarioResult.scenario.name}$lineInfo - $status$Reset")
    ) *>
      (if (effectiveIgnored) ZIO.unit // Skip steps if feature or scenario is ignored
       else {
         ZIO.foreachDiscard(scenarioResult.stepResults.zipWithIndex) { case (step, index) =>
           val isLastStep = index == scenarioResult.stepResults.size - 1
           printStep(scenarioIdStr, step, logCollector, isLastStep, indent + (if (isLast) Indent else Vertical))
         }
       })
  }

  private def printStep(
    scenarioId: String,
    stepResult: StepResult,
    logCollector: LogCollector,
    isLast: Boolean,
    indent: String
  ): ZIO[Any, Nothing, Unit] = {
    val stepColor = if (stepResult.isPassed) LightBlue else LightRed
    val stepLine  = stepResult.step.line.map(l => s":$l").getOrElse("")
    val prefix    = if (isLast) LastBranch else Branch
    val stepIdStr = stepResult.step.id.toString
    val subIndent = if (isLast) indent + Indent else indent + Vertical
    for {
      logs <- logCollector.getLogs(scenarioId, stepIdStr)
      _ <-
        ZIO.succeed(
          println(
            s"$indent$stepColor$prefix• Step: ${stepResult.step.stepType} - ${stepResult.step.pattern}$stepLine$Reset"
          )
        )
      _ <- ZIO.unless(stepResult.isPassed) {
             stepResult.outcome match {
               case Left(cause) =>
                 val causeLines = cause.prettyPrint.split("\n")
                 ZIO.foreachDiscard(causeLines) { line =>
                   ZIO.succeed(println(s"$subIndent$line"))
                 }
               case Right(_) => ZIO.unit // This case is unlikely since isPassed is false
             }
           }
      _ <- ZIO.foreachDiscard(logs.entries) { log =>
             val logColor = log.level match {
               case InternalLogLevel.Debug   => PaleCyan
               case InternalLogLevel.Info    => PaleGreen
               case InternalLogLevel.Warning => PaleYellow
               case InternalLogLevel.Error   => PaleRed
               case InternalLogLevel.Fatal   => PaleRed
             }
             val logPrefix = s"[${log.level}] [${DateTimeFormatter.ISO_INSTANT.format(log.timestamp)}] "
             val logLines  = log.message.split("\n")
             ZIO.foreachDiscard(logLines.zipWithIndex) { case (line, idx) =>
               val coloredLine = if (idx == 0) s"$logPrefix$line" else line
               ZIO.succeed(println(s"$subIndent$logColor$coloredLine$Reset"))
             }
           }
    } yield ()
  }

  private def printFeatureSummary(feature: FeatureResult, indent: String): ZIO[Any, Nothing, Unit] = {
    val (passed, failed, ignored) = if (feature.isIgnored) {
      // If the feature is ignored, all scenarios are ignored
      (0, 0, feature.scenarioResults.size)
    } else {
      // Otherwise, count based on individual scenario statuses
      val passed  = feature.scenarioResults.count(s => !s.isIgnored && s.isPassed)
      val failed  = feature.scenarioResults.count(s => !s.isIgnored && !s.isPassed)
      val ignored = feature.scenarioResults.count(_.isIgnored)
      (passed, failed, ignored)
    }
    ZIO.succeed(println(s"$indent$LightBlueSummary: Passed: $passed, Failed: $failed, Ignored: $ignored$Reset"))
  }

  private def shortenFilePath(path: String): String = {
    val maxLength = 40
    if (path.length <= maxLength) path
    else "..." + path.takeRight(maxLength - 3)
  }

  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    for {
      logCollector <- ZIO.service[LogCollector]
      _ <- ZIO.foreachDiscard(results.zipWithIndex) { case (feature, index) =>
             val isLastFeature = index == results.size - 1
             printFeature(feature, logCollector, isLastFeature)
           }
      totalPassed = results.foldLeft(0) { (acc, feature) =>
                      if (feature.isIgnored) acc // Ignored features contribute 0 to passed
                      else acc + feature.scenarioResults.count(s => !s.isIgnored && s.isPassed)
                    }
      totalFailed = results.foldLeft(0) { (acc, feature) =>
                      if (feature.isIgnored) acc // Ignored features contribute 0 to failed
                      else acc + feature.scenarioResults.count(s => !s.isIgnored && !s.isPassed)
                    }
      totalIgnored = results.foldLeft(0) { (acc, feature) =>
                       if (feature.isIgnored) acc + feature.scenarioResults.size // All scenarios ignored
                       else acc + feature.scenarioResults.count(_.isIgnored)
                     }
      _ <-
        ZIO.succeed(
          println(s"$LightBlueFinal Summary: Passed: $totalPassed, Failed: $totalFailed, Ignored: $totalIgnored$Reset")
        )
    } yield ()
}
