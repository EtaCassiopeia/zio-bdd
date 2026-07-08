package zio.bdd.core.report

import zio.*
import zio.bdd.core.{FeatureResult, LogCollector, ScenarioResult, StepResult}
import zio.bdd.gherkin.{Feature, Scenario, Step}
import zio.stream.ZStream

/**
 * Events emitted during test execution for streaming reporters.
 *
 * The event stream enables:
 *   - Live progress output (print a line as each scenario completes)
 *   - Streaming JUnit XML (write test cases as they complete, not after all
 *     tests finish)
 *   - CI service integrations (TeamCity, GitHub Actions) that consume streaming
 *     events
 *   - Memory-efficient reporting (no need to accumulate all results before
 *     printing)
 */
sealed trait TestEvent

object TestEvent:
  /** Emitted once before any feature starts. */
  case class SuiteStarted(featureCount: Int, dryRun: Boolean) extends TestEvent

  /** Emitted when a feature begins execution. */
  case class FeatureStarted(feature: Feature) extends TestEvent

  /** Emitted when a scenario begins execution. */
  case class ScenarioStarted(scenario: Scenario, featureName: String) extends TestEvent

  /** Emitted after each step completes. */
  case class StepFinished(step: Step, result: StepResult, featureName: String, scenarioName: String) extends TestEvent

  /** Emitted after a scenario completes (pass/fail/ignore/pending). */
  case class ScenarioFinished(result: ScenarioResult, featureName: String) extends TestEvent

  /** Emitted after a feature completes (all scenarios done). */
  case class FeatureFinished(result: FeatureResult) extends TestEvent

  /** Emitted once after all features complete. */
  case class SuiteFinished(results: List[FeatureResult]) extends TestEvent

/**
 * A reporter that consumes a stream of `TestEvent` values.
 *
 * Compared to the batch `Reporter` (which receives all results at once), a
 * `StreamingReporter` can:
 *   - Print progress in real-time
 *   - Write results incrementally (no memory accumulation)
 *   - Be fan-outed to multiple subscribers via `ZHub`
 */
trait StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit]

/**
 * Adapter: wrap a batch `Reporter` as a `StreamingReporter`. Accumulates all
 * events, then calls `reporter.report` on `SuiteFinished`.
 */
final class BatchReporterAdapter(reporter: Reporter) extends StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit] =
    events.runForeach {
      case TestEvent.SuiteFinished(results) => reporter.report(results)
      case _                                => ZIO.unit
    }

/**
 * A streaming reporter that prints live progress to the console. One line per
 * scenario as it completes.
 */
object LiveProgressReporter extends StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit] =
    events.runForeach {
      case TestEvent.SuiteStarted(n, dryRun) =>
        val mode = if (dryRun) " (dry-run)" else ""
        Console.printLine(s"Running $n feature(s)$mode...").orDie

      case TestEvent.ScenarioFinished(result, featureName) =>
        // Same precedence as JUnitXMLFormatter/PrettyReporter: ignored first, then XFAIL/XPASS
        // (which must precede isPassed — an expected failure has isPassed == true), then pass/fail.
        val (icon, note) =
          if (result.isIgnored) ("○", "")
          else if (result.isExpectedFailure) ("⊗", " (expected failure)")
          else if (result.isUnexpectedlyPassing) ("✗", " (unexpectedly passed — remove @expectedFailure)")
          else if (result.isPassed) ("✓", "")
          else ("✗", "")
        val attempts = if (result.attempts > 1) s" (${result.attempts} attempts)" else ""
        val name     = s"$featureName / ${result.scenario.name}"
        Console.printLine(s"  $icon $name$note$attempts [${result.duration}ms]").orDie

      case TestEvent.SuiteFinished(results) =>
        val scenarios = results.flatMap(_.scenarioResults)
        val total     = scenarios.length
        val passed    = scenarios.count(_.isPassed)
        val failed    = scenarios.count(_.hasFailure)
        val ignored   = scenarios.count(_.isIgnored)
        val xfail     = scenarios.count(_.isExpectedFailure)
        val xpass     = scenarios.count(_.isUnexpectedlyPassing)
        val extra     = Seq(Option.when(xfail > 0)(s"$xfail xfail"), Option.when(xpass > 0)(s"$xpass xpass")).flatten
        val suffix    = if (extra.nonEmpty) extra.mkString(" (", ", ", ")") else ""
        Console.printLine(s"\nDone. $passed/$total passed, $failed failed, $ignored ignored$suffix").orDie

      case _ => ZIO.unit
    }
