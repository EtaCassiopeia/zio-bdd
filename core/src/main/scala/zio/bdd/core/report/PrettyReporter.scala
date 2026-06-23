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
  //
  // Each entry: (darkTerminalCode, lightTerminalCode)
  // Light codes are chosen for legibility on white backgrounds (WCAG AA contrast).
  //
  //   Feature  → dark forest green  / deep green
  //   Scenario → sky blue           / dark navy blue
  //   Step     → soft mint          / dark teal green
  //   Failed   → bright red         / dark crimson
  //   Pending  → amber orange       / dark burnt orange
  //   Skipped  → dim gray           / dark charcoal (not ANSI white which vanishes on white bg)
  //   Headings → bright cyan        / dark teal
  //
  case object Green  extends Color("[38;5;35m", "[38;5;22m")   // forest green  / deep green
  case object Blue   extends Color("[38;5;75m", "[38;5;19m")   // sky blue      / dark navy
  case object Mint   extends Color("[38;5;121m", "[38;5;29m")  // soft mint     / dark teal
  case object Red    extends Color("[91m", "[38;5;160m")       // bright red    / dark crimson
  case object Gray   extends Color("[90m", "[38;5;240m")       // dim gray      / dark charcoal
  case object Orange extends Color("[38;5;214m", "[38;5;130m") // amber orange  / dark burnt orange
  case object Cyan   extends Color("[96m", "[38;5;30m")        // bright cyan   / dark teal

  // Aliases so any code outside StatusColor that used the old names compiles.
  val Yellow: Color = Blue
  val Teal: Color   = Blue

  // ── Muted log entry palette ────────────────────────────────────────────────
  // Dark:  faded enough not to compete with tree nodes
  // Light: darker than the tree colours so they're readable on white,
  //        but still clearly subordinate (muted hue, not bold)
  //
  //   Debug   → steel gray    / dark slate gray
  //   Info    → dusty teal    / dark slate teal
  //   Warning → muted gold    / dark olive/brown
  //   Error   → muted rose    / dark rose/maroon
  //
  case object LogDebug   extends Color("[38;5;242m", "[38;5;238m")         // steel gray    / dark slate
  case object LogInfo    extends Color("[38;2;100;180;160m", "[38;5;66m")  // dusty teal    / dark slate teal
  case object LogWarning extends Color("[38;2;200;170;80m", "[38;5;94m")   // muted gold    / dark olive
  case object LogError   extends Color("[38;2;210;100;100m", "[38;5;124m") // muted rose    / dark maroon

  // Keep Pale* as aliases for any code that references them directly
  val PaleGreen: Color  = LogInfo
  val PaleCyan: Color   = LogDebug
  val PaleYellow: Color = LogWarning
  val PaleRed: Color    = LogError

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
    case StepStatus.Passed         => Style(Color.Mint, "•") // soft mint  — leaf-level pass
    case StepStatus.Failed(_)      => Style(Color.Red, "✗")
    case StepStatus.TimedOut(_, _) => Style(Color.Red, "⏱")
    case StepStatus.Skipped        => Style(Color.Gray, "○")
    case StepStatus.Pending(_)     => Style(Color.Orange, "◌")
  }

  given StatusColor[ScenarioResult] = from { sc =>
    if (sc.isIgnored) Style(Color.Gray, "◑")
    else if (sc.hasPending && !sc.hasFailure) Style(Color.Orange, "◑")
    else if (sc.isPassed) Style(Color.Blue, "✓") // sky blue — scenario-level pass
    else Style(Color.Red, "✗")
  }

  given StatusColor[FeatureResult] = from { f =>
    if (f.isIgnored) Style(Color.Gray, "◉")
    else if (f.isPassed) Style(Color.Green, "◉") // bright green — feature-level pass
    else Style(Color.Red, "◉")
  }

  given StatusColor[InternalLogLevel] = from {
    case InternalLogLevel.Debug   => Style(Color.LogDebug, "")    // steel gray  — least noise
    case InternalLogLevel.Info    => Style(Color.LogInfo, "")     // dusty teal  — readable, calm
    case InternalLogLevel.Warning => Style(Color.LogWarning, "⚠") // muted gold  — caution, noticeable
    case InternalLogLevel.Error   => Style(Color.LogError, "✖")   // muted rose  — error, softer than tree red
    case InternalLogLevel.Fatal   => Style(Color.Red, "✖")        // full red    — fatal demands attention
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
        // Glyph (├─ ╰─) — no colour, matches the plain │ in the indent string
        s"$indent$prefix${ansi(leaf.style.color)}${leaf.style.icon} ${leaf.text}$Reset"
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
    val kw  = sr.step.stepType.toString.replace("Step", "")
    val loc = sr.step.line.map(l => s":$l").getOrElse("")
    val pat = sr.step.pattern
    // Property synthetic steps get distinct styling instead of the normal step colour.
    if (pat.startsWith("[counterexample]"))
      Doc.Leaf(
        text = s"$pat${statusLabel(st)}${durStr(sr.duration)}",
        style = Style(Color.Orange, icon = "⚡")
      )
    else if (pat.startsWith("[property]"))
      Doc.Leaf(
        text = s"$pat${durStr(sr.duration)}",
        style = Style(Color.Cyan, icon = "⟳")
      )
    else
      Doc.Leaf(
        text = s"$kw $pat$loc${statusLabel(st)}${durStr(sr.duration)}",
        style = summon[StatusColor[StepStatus]].style(st)
      )

  /** Render a `[counterexample]` pattern as a compact table. */
  private def counterexampleTable(pattern: String): List[Doc.Leaf] =
    // Pattern: "[counterexample] col1=val1 (gen1), col2=val2 (gen2)"
    val body = pattern.stripPrefix("[counterexample]").trim
    if (body.isEmpty) Nil
    else
      val entries = body.split(",").map(_.trim).toList
      val colWidth = entries.map { e =>
        e.indexOf('=') match { case -1 => 0; case i => i }
      }.maxOption.getOrElse(0)
      val sep = "  " + "─" * (colWidth + 2 + 30)
      val rows = entries.map { e =>
        e.indexOf('=') match
          case -1 => Doc.Leaf(s"  │ $e", Style(Color.Orange))
          case i =>
            val col  = e.take(i).padTo(colWidth, ' ')
            val rest = e.drop(i + 1)
            Doc.Leaf(s"  │ $col │ $rest", Style(Color.Orange))
      }
      Doc.Leaf(sep, Style(Color.Orange)) ::
        rows ++
        List(Doc.Leaf(sep, Style(Color.Orange)))

  def causeLeaves(cause: zio.Cause[Throwable]): List[Doc.Leaf] =
    cause.squash match
      case mae: zio.bdd.core.MultipleAssertionError =>
        mae.failures.map(msg => Doc.Leaf(s"  - $msg", Style(Color.Red)))
      case e: AssertionError =>
        // User assertion: show only the message — no stack frames needed.
        List(Doc.Leaf(e.getMessage, Style(Color.Red)))
      case e if e.getClass.getName == "zio.bdd.core.property.PropertyFalsifiedException" =>
        // Property counterexample summary: show only the message, no internal frames.
        List(Doc.Leaf(e.getMessage, Style(Color.Red)))
      case e =>
        // Other exceptions: show message then user-relevant frames only
        // (strip zio.bdd.core.*, zio.*, scala.runtime.* internals).
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val frames = Option(e.getStackTrace)
          .getOrElse(Array.empty[StackTraceElement])
          .filterNot { f =>
            val c = f.getClassName
            c.startsWith("zio.bdd.") || c.startsWith("zio.") ||
            c.startsWith("scala.runtime.") || c.startsWith("java.")
          }
          .take(5)
          .map(f => s"  at ${f.getClassName}.${f.getMethodName}(${f.getFileName}:${f.getLineNumber})")
        val msgLeaf     = Doc.Leaf(msg, Style(Color.Red))
        val frameLeaves = frames.map(l => Doc.Leaf(l, Style(Color.Red))).toList
        msgLeaf :: frameLeaves

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
    val causeDoc: List[Doc.Leaf] =
      if (sr.step.pattern.startsWith("[counterexample]"))
        // Replace the raw exception message with the table view.
        counterexampleTable(sr.step.pattern)
      else
        sr.status match
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
