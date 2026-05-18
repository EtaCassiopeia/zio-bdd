package zio.bdd.core.report

import zio.*
import zio.bdd.core.*

import java.time.format.DateTimeFormatter

// ─────────────────────────────────────────────────────────────────────────────
// Styled text algebra
//
// Rather than threading ANSI escape strings through functions, we model output
// as a pure algebraic tree of `Doc` nodes.  Each node carries a `Style` that
// is interpreted by a `DocRenderer`.  Pure construction is separated from IO.
// ─────────────────────────────────────────────────────────────────────────────

// ── Theme detection ───────────────────────────────────────────────────────────

/**
 * Output theme.
 *
 *   - `Dark` — bright ANSI codes, optimised for dark terminal backgrounds
 *     (default)
 *   - `Light` — darker ANSI codes, optimised for light terminal backgrounds
 *   - `Plain` — no colour codes; used when NO_COLOR / TERM=dumb is set
 */
sealed trait Theme
object Theme:
  case object Dark  extends Theme
  case object Light extends Theme
  case object Plain extends Theme

  /**
   * Auto-detect the appropriate theme from the process environment.
   *
   * Priority:
   *   1. `NO_COLOR` set (any value) → Plain (https://no-color.org) 2.
   *      `TERM=dumb` → Plain 3. `COLORFGBG` present → parse background index;
   *      bg ≥ 8 → Light 4. Fallback → Dark (most common modern terminal
   *      default)
   */
  def detect(): Theme =
    val env = java.lang.System.getenv
    if (env.containsKey("NO_COLOR")) Plain
    else if (Option(env.get("TERM")).contains("dumb")) Plain
    else
      Option(env.get("COLORFGBG")) match
        case Some(v) =>
          val bg = v.split(";").last.toIntOption.getOrElse(0)
          if (bg >= 8) Light else Dark
        case None => Dark

// ── Color model ───────────────────────────────────────────────────────────────

/**
 * A semantic colour with separate ANSI codes for dark and light terminal
 * themes.
 *
 * `darkCode` — escape suffix for dark terminal backgrounds (bright variants
 * 90–97) `lightCode` — escape suffix for light terminal backgrounds (normal
 * variants 30–37)
 *
 * The renderer calls `ansiFor(theme)` once per leaf.
 */
sealed abstract class Color(val darkCode: String, val lightCode: String):
  def ansiFor(theme: Theme): String = theme match
    case Theme.Dark  => darkCode
    case Theme.Light => lightCode
    case Theme.Plain => ""

object Color:
  // ── Result / status colors ────────────────────────────────────────────────
  case object Green  extends Color("[92m", "[32m")             // passed feature
  case object Red    extends Color("[91m", "[31m")             // failed / error
  case object Blue   extends Color("[94m", "[34m")             // step (passed)
  case object Yellow extends Color("[93m", "[33m")             // scenario (passed)
  case object Gray   extends Color("[90m", "[37m")             // ignored / skipped
  case object Orange extends Color("[38;5;214m", "[38;5;166m") // pending
  case object Cyan   extends Color("[96m", "[36m")             // summary / headings

  // ── Muted log entry palette ────────────────────────────────────────────────
  // Dark:  pale 24-bit tints — visible without competing with tree nodes
  // Light: normal ANSI — pale 24-bit tints are near-invisible on white
  case object PaleGreen  extends Color("[38;2;144;238;144m", "[32m")
  case object PaleCyan   extends Color("[38;2;173;216;230m", "[36m")
  case object PaleYellow extends Color("[38;2;200;200;100m", "[33m")
  case object PaleRed    extends Color("[38;2;255;182;193m", "[31m")

// ── Style ─────────────────────────────────────────────────────────────────────

/**
 * Style attached to a `Doc` leaf: a foreground colour with an optional icon
 * prefix.
 */
final case class Style(color: Color, icon: String = "")

// ── StatusColor typeclass ─────────────────────────────────────────────────────

/**
 * Typeclass that maps a value of type `A` to the `Style` used to render it.
 */
trait StatusColor[A]:
  def style(a: A): Style

object StatusColor:

  def apply[A](using sc: StatusColor[A]): StatusColor[A] = sc

  def from[A](f: A => Style): StatusColor[A] = a => f(a)

  given StatusColor[StepStatus] = from {
    case StepStatus.Passed         => Style(Color.Blue, "•")   // •
    case StepStatus.Failed(_)      => Style(Color.Red, "✗")    // ✗
    case StepStatus.TimedOut(_, _) => Style(Color.Red, "⏱")    // ⏱
    case StepStatus.Skipped        => Style(Color.Gray, "○")   // ○
    case StepStatus.Pending(_)     => Style(Color.Orange, "◌") // ◌
  }

  given StatusColor[ScenarioResult] = from { sc =>
    if (sc.isIgnored) Style(Color.Gray, "◑")                           // ◑
    else if (sc.hasPending && !sc.hasFailure) Style(Color.Orange, "◑") // ◑
    else if (sc.isPassed) Style(Color.Yellow, "✓")                     // ✓
    else Style(Color.Red, "✗")                                         // ✗
  }

  given StatusColor[FeatureResult] = from { f =>
    if (f.isIgnored) Style(Color.Gray, "◉")      // ◉
    else if (f.isPassed) Style(Color.Green, "◉") // ◉
    else Style(Color.Red, "◉")                   // ◉
  }

  given StatusColor[InternalLogLevel] = from {
    case InternalLogLevel.Debug   => Style(Color.PaleCyan, "")
    case InternalLogLevel.Info    => Style(Color.PaleGreen, "")
    case InternalLogLevel.Warning => Style(Color.PaleYellow, "")
    case InternalLogLevel.Error   => Style(Color.PaleRed, "")
    case InternalLogLevel.Fatal   => Style(Color.PaleRed, "")
  }

// ── Doc tree (pure, testable) ─────────────────────────────────────────────────

/**
 * An algebraic document tree.
 *
 * `Doc.Leaf` — a single styled line of text `Doc.Branch` — a styled header line
 * with zero or more children (indented) `Doc.Many` — a flat sequence of Docs
 * (no additional indentation)
 *
 * This tree is completely pure and can be inspected in unit tests without
 * rendering to the terminal.
 */
sealed trait Doc

object Doc:
  final case class Leaf(text: String, style: Style)                           extends Doc
  final case class Branch(header: Leaf, children: List[Doc], isLast: Boolean) extends Doc
  final case class Many(docs: List[Doc])                                      extends Doc

  def empty: Doc                = Many(Nil)
  def sequence(docs: Doc*): Doc = Many(docs.toList)
  def plain(text: String): Doc  = Leaf(text, Style(Color.Cyan))

// ── Tree-branch glyphs ────────────────────────────────────────────────────────

object Glyph:
  val Branch     = "├─ " // ├─
  val LastBranch = "╰─ " // ╰─
  val Vertical   = "│  " // │
  val Indent     = "   "

  def prefix(isLast: Boolean): String      = if (isLast) LastBranch else Branch
  def childIndent(isLast: Boolean): String = if (isLast) Indent else Vertical

// ── DocRenderer typeclass ─────────────────────────────────────────────────────

trait DocRenderer[F[_]]:
  def render(doc: Doc, indent: String = ""): F[Unit]

// ── Render model ──────────────────────────────────────────────────────────────

final case class RenderedLine(text: String)

final case class DocBuffer(lines: List[RenderedLine]):
  def toPlainText: String = lines.map(_.text).mkString("\n")

// ── Theme-aware ANSI renderer ─────────────────────────────────────────────────

/**
 * Renders a `Doc` tree to the console using `theme`-selected ANSI colour codes.
 *
 * When `theme == Theme.Plain` no ANSI codes are emitted — same as the old
 * `PlainRenderer`. Callers that previously used `AnsiRenderer` (the singleton)
 * should switch to `AnsiRenderer()` (auto-detect) or one of the named
 * constructors.
 */
final class AnsiRenderer(val theme: Theme) extends DocRenderer[UIO]:
  private val Reset = "[0m"

  private def ansi(color: Color): String = "" + color.ansiFor(theme)

  private def renderLeaf(leaf: Doc.Leaf, prefix: String, indent: String): UIO[Unit] =
    val line =
      if (theme == Theme.Plain)
        s"$indent$prefix${leaf.style.icon} ${leaf.text}"
      else
        s"$indent${ansi(leaf.style.color)}$prefix${leaf.style.icon} ${leaf.text}$Reset"
    Console.printLine(line).orDie

  def render(doc: Doc, indent: String = ""): UIO[Unit] = doc match
    case leaf: Doc.Leaf =>
      renderLeaf(leaf, "", indent)
    case Doc.Branch(header, children, isLast) =>
      val prefix   = Glyph.prefix(isLast)
      val childInd = indent + Glyph.childIndent(isLast)
      renderLeaf(header, prefix, indent) *>
        ZIO.foreachDiscard(children)(render(_, childInd))
    case Doc.Many(docs) =>
      ZIO.foreachDiscard(docs)(render(_, indent))

object AnsiRenderer:
  /**
   * Construct using the auto-detected theme (reads `NO_COLOR`, `COLORFGBG`,
   * etc.).
   */
  def apply(): AnsiRenderer = new AnsiRenderer(Theme.detect())
  def dark: AnsiRenderer    = new AnsiRenderer(Theme.Dark)
  def light: AnsiRenderer   = new AnsiRenderer(Theme.Light)
  def plain: AnsiRenderer   = new AnsiRenderer(Theme.Plain)

/** Backward-compatible alias. */
val PlainRenderer: AnsiRenderer = AnsiRenderer.plain

// ── Buffer renderer ───────────────────────────────────────────────────────────

class BufferRenderer(ref: Ref[List[RenderedLine]]) extends DocRenderer[[A] =>> UIO[A]]:

  def render(doc: Doc, indent: String = ""): UIO[Unit] = doc match
    case leaf: Doc.Leaf =>
      ref.update(_ :+ RenderedLine(s"$indent${leaf.style.icon} ${leaf.text}"))
    case Doc.Branch(header, children, isLast) =>
      val prefix   = Glyph.prefix(isLast)
      val childInd = indent + Glyph.childIndent(isLast)
      ref.update(_ :+ RenderedLine(s"$indent$prefix${header.style.icon} ${header.text}")) *>
        ZIO.foreachDiscard(children)(render(_, childInd))
    case Doc.Many(docs) =>
      ZIO.foreachDiscard(docs)(render(_, indent))

  def collected: UIO[DocBuffer] = ref.get.map(DocBuffer.apply)

object BufferRenderer:
  def make: UIO[BufferRenderer] = Ref.make(List.empty[RenderedLine]).map(new BufferRenderer(_))

// ── Summary statistics (pure) ─────────────────────────────────────────────────

final case class ScenarioCounts(passed: Int, failed: Int, ignored: Int, pending: Int):
  def total: Int = passed + failed + ignored + pending

object ScenarioCounts:
  val zero: ScenarioCounts = ScenarioCounts(0, 0, 0, 0)

  def fromFeature(f: FeatureResult): ScenarioCounts =
    if (f.isIgnored) ScenarioCounts(0, 0, f.scenarioResults.size, 0)
    else
      f.scenarioResults.foldLeft(zero) { (acc, sc) =>
        if (sc.isIgnored) acc.copy(ignored = acc.ignored + 1)
        else if (sc.hasPending && !sc.hasFailure) acc.copy(pending = acc.pending + 1)
        else if (sc.isPassed) acc.copy(passed = acc.passed + 1)
        else acc.copy(failed = acc.failed + 1)
      }

  def aggregate(features: List[FeatureResult]): ScenarioCounts =
    features.foldLeft(zero)((acc, f) => acc + fromFeature(f))

  extension (a: ScenarioCounts)
    def +(b: ScenarioCounts): ScenarioCounts =
      ScenarioCounts(a.passed + b.passed, a.failed + b.failed, a.ignored + b.ignored, a.pending + b.pending)

    def toSummaryString: String =
      s"Passed: ${a.passed}  Failed: ${a.failed}  Ignored: ${a.ignored}  Pending: ${a.pending}"

// ── Doc builders (pure) ───────────────────────────────────────────────────────

object DocBuilder:

  private val timeFmt = DateTimeFormatter.ISO_INSTANT

  private def durStr(ms: Long): String = if (ms > 0) s" [${ms}ms]" else ""

  private def fileSuffix(path: Option[String]): String =
    path.map(p => s" (${if (p.length > 40) "..." + p.takeRight(37) else p})").getOrElse("")

  private def statusLabel(status: StepStatus): String = status match
    case StepStatus.Passed         => ""
    case StepStatus.Failed(_)      => " [FAILED]"
    case StepStatus.TimedOut(d, _) => s" [TIMEOUT after ${d.toSeconds}s]"
    case StepStatus.Skipped        => " [SKIPPED]"
    case StepStatus.Pending(msg)   => s" [PENDING: $msg]"

  def stepLeaf(sr: StepResult): Doc.Leaf =
    val st  = sr.status
    val sty = summon[StatusColor[StepStatus]].style(st)
    val kw  = sr.step.stepType.toString.replace("Step", "")
    val loc = sr.step.line.map(l => s":$l").getOrElse("")
    Doc.Leaf(
      text = s"$kw ${sr.step.pattern}$loc${statusLabel(st)}${durStr(sr.duration)}",
      style = sty
    )

  def causeLeaves(cause: zio.Cause[Throwable]): List[Doc.Leaf] =
    cause.squash match
      case mae: zio.bdd.core.MultipleAssertionError =>
        mae.failures.map(msg => Doc.Leaf(s"  - $msg", Style(Color.Red)))
      case _ =>
        cause.prettyPrint.split("\n").map(line => Doc.Leaf(line, Style(Color.Red))).toList

  def logLeaves(logs: CollectedLogs): List[Doc.Leaf] =
    logs.entries.flatMap { entry =>
      val sty    = summon[StatusColor[InternalLogLevel]].style(entry.level)
      val prefix = s"[${entry.level}] [${timeFmt.format(entry.timestamp)}] "
      entry.message.split("\n").zipWithIndex.map { case (line, idx) =>
        Doc.Leaf(text = if (idx == 0) s"$prefix$line" else line, style = sty)
      }
    }

  def stepBranch(sr: StepResult, isLast: Boolean, logs: CollectedLogs): Doc =
    val header = stepLeaf(sr)
    val causeDoc = sr.status match
      case StepStatus.Failed(cause)      => causeLeaves(cause)
      case StepStatus.TimedOut(_, cause) => causeLeaves(cause)
      case _                             => Nil
    val logDoc   = logLeaves(logs)
    val children = (causeDoc ++ logDoc).map(l => l: Doc)
    Doc.Branch(header, children, isLast)

  def featureSummary(f: FeatureResult): Doc.Leaf =
    Doc.Leaf(ScenarioCounts.fromFeature(f).toSummaryString, Style(Color.Cyan))

  def globalSummary(results: List[FeatureResult]): Doc.Leaf =
    Doc.Leaf(s"Summary: ${ScenarioCounts.aggregate(results).toSummaryString}", Style(Color.Cyan))

  def scenarioBranch(
    sc: ScenarioResult,
    isLast: Boolean,
    featureIgnored: Boolean,
    logs: String => CollectedLogs
  ): Doc =
    val effectiveIgnored = featureIgnored || sc.isIgnored
    val sty              = summon[StatusColor[ScenarioResult]].style(sc)
    val loc              = sc.scenario.line.map(l => s":$l").getOrElse("")
    val header = Doc.Leaf(
      text = s"Scenario: ${sc.scenario.name}$loc - ${statusText(sc)}${durStr(sc.duration)}",
      style = sty
    )
    val stepDocs =
      if (effectiveIgnored) Nil
      else
        sc.stepResults.zipWithIndex.map { case (sr, i) =>
          stepBranch(sr, isLast = i == sc.stepResults.size - 1, logs(sr.step.id.toString))
        }
    Doc.Branch(header, stepDocs, isLast)

  def featureBranch(
    f: FeatureResult,
    isLast: Boolean,
    logs: (String, String) => CollectedLogs
  ): Doc =
    val sty = summon[StatusColor[FeatureResult]].style(f)
    val header = Doc.Leaf(
      text = s"Feature: ${f.feature.name}${fileSuffix(f.feature.file)} - ${statusText(f)}${durStr(f.duration)}",
      style = sty
    )
    val scenarioDocs = f.scenarioResults.zipWithIndex.map { case (sc, i) =>
      val scStepLogs: String => CollectedLogs = stepId => logs(sc.scenario.id.toString, stepId)
      scenarioBranch(sc, isLast = i == f.scenarioResults.size - 1, f.isIgnored, scStepLogs)
    }
    Doc.Branch(header, scenarioDocs :+ featureSummary(f), isLast)

  private def statusText(sc: ScenarioResult): String =
    if (sc.isIgnored) "IGNORED"
    else if (sc.hasPending && !sc.hasFailure) "PENDING"
    else if (sc.isPassed) "PASSED"
    else "FAILED"

  private def statusText(f: FeatureResult): String =
    if (f.isIgnored) "IGNORED"
    else if (f.isPassed) "PASSED"
    else "FAILED"

// ── PrettyReporter ────────────────────────────────────────────────────────────

/**
 * Renders BDD results to the terminal.
 *
 * Architecture:
 *   1. Fetch all step logs up-front 2. Build a pure `Doc` tree via `DocBuilder`
 *      3. Render the tree via the injected `DocRenderer`
 *
 * The `DocRenderer` is injected so tests can use `BufferRenderer` and
 * production code can use `AnsiRenderer` (auto-detects theme) or
 * `PlainRenderer`.
 */
final class PrettyReporter(renderer: DocRenderer[UIO]) extends Reporter:

  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    for {
      lc <- ZIO.service[LogCollector]
      docs <- ZIO.foreach(results.zipWithIndex) { case (f, i) =>
                buildFeatureDoc(f, isLast = i == results.size - 1, lc)
              }
      _ <- ZIO.foreachDiscard(docs)(renderer.render(_))
      _ <- renderer.render(DocBuilder.globalSummary(results))
    } yield ()

  private def buildFeatureDoc(f: FeatureResult, isLast: Boolean, lc: LogCollector): UIO[Doc] =
    for {
      logMap <- ZIO
                  .foreach(f.scenarioResults) { sc =>
                    ZIO
                      .foreach(sc.stepResults) { sr =>
                        lc.getLogs(sc.scenario.id.toString, sr.step.id.toString).map { logs =>
                          (sc.scenario.id.toString, sr.step.id.toString) -> logs
                        }
                      }
                      .map(_.toMap)
                  }
                  .map(_.flatten.toMap)
    } yield DocBuilder.featureBranch(
      f,
      isLast,
      logs = (scenarioId, stepId) => logMap.getOrElse((scenarioId, stepId), CollectedLogs())
    )

object PrettyReporter:

  /** Auto-detect theme from the environment (NO_COLOR, COLORFGBG, etc.). */
  def live: ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(new PrettyReporter(AnsiRenderer()))

  /** Force plain-text output (no ANSI codes). */
  def plain: ZLayer[Any, Nothing, Reporter] =
    ZLayer.succeed(new PrettyReporter(PlainRenderer))

  /** In-memory reporter for testing. */
  def buffered: UIO[(PrettyReporter, BufferRenderer)] =
    BufferRenderer.make.map(buf => (new PrettyReporter(buf), buf))
