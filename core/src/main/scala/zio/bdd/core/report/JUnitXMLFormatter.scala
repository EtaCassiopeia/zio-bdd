package zio.bdd.core.report

import zio.*
import zio.bdd.core.*
import zio.bdd.core.property.PropertyFalsifiedException
import zio.bdd.gherkin.{Feature, Step, StepType}

import java.time.Instant
import java.time.format.DateTimeFormatter
import scala.xml.{Elem, NodeSeq, PrettyPrinter, Text}

/**
 * Produces JUnit XML enriched with Gherkin structure:
 *
 * <testsuite name="Provision" tests="23" failures="2" skipped="1" time="4.210"
 * file="features/components/Provision.feature" tags="@provision"> <properties/>
 * <testcase name="Successful Provision (Happy Path)" classname="Provision"
 * time="0.831" file="features/components/Provision.feature" line="4"
 * tags="@provision1"> <steps> <step keyword="Given" name="a valid provision
 * body" status="passed" time="0.012"/> <step keyword="When" name="a provision
 * request is sent" status="passed" time="0.801"/> <step keyword="Then"
 * name="the ledger returns a 201 status code" status="failed" time="0.018"
 * message="Expected 201, got 203"/> </steps> <failure message="Expected 201,
 * got 203" type="AssertionError">…stack…</failure> </testcase> </testsuite>
 *
 * Follows the extended Cucumber-JVM / pytest-bdd JUnit XML convention so that
 * CI tools (Jenkins, GitHub Actions) can display step-level detail alongside
 * the standard pass/fail counts.
 */
object JUnitXMLFormatter {

  // ── Model ─────────────────────────────────────────────────────────────────

  /**
   * Mirrors [[StepStatus]] but carries the rendered message for XML emission.
   */
  enum StepOutcome:
    case Passed
    case Failed(message: String, stackTrace: Option[String])
    case TimedOut(message: String)
    case Skipped
    case Pending(reason: String)

  object StepOutcome:
    def from(result: StepResult): StepOutcome = result.status match
      case StepStatus.Passed     => Passed
      case StepStatus.Skipped    => Skipped
      case StepStatus.Pending(r) => Pending(r)
      case StepStatus.TimedOut(d, cause) =>
        TimedOut(s"Step timed out after ${d.toSeconds}s")
      case StepStatus.Failed(cause) =>
        val e   = cause.squash
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val trace = e match
          case _: AssertionError             => None // message is self-explanatory; no trace needed
          case _: PropertyFalsifiedException => None
          case _ =>
            Option(e.getStackTrace).map { frames =>
              val userFrames = frames.filterNot { f =>
                val c = f.getClassName
                c.startsWith("zio.bdd.") || c.startsWith("zio.") ||
                c.startsWith("scala.runtime.") || c.startsWith("java.")
              }
                .take(10)
              userFrames.mkString("\n\tat ", "\n\tat ", "")
            }
        Failed(msg, trace)

    def xmlStatus(o: StepOutcome): String = o match
      case Passed       => "passed"
      case Skipped      => "skipped"
      case Pending(_)   => "pending"
      case TimedOut(_)  => "timed_out"
      case Failed(_, _) => "failed"

  case class StepRecord(
    keyword: String,
    name: String,
    outcome: StepOutcome,
    durationSecs: Double
  )

  object StepRecord:
    def keyword(t: StepType): String = t match
      case StepType.GivenStep => "Given"
      case StepType.WhenStep  => "When"
      case StepType.ThenStep  => "Then"
      case StepType.AndStep   => "And"
      case StepType.ButStep   => "But"

    def from(step: Step, result: StepResult): StepRecord =
      StepRecord(keyword(step.stepType), step.pattern, StepOutcome.from(result), result.duration / 1000.0)

  case class TestCaseRecord(
    name: String,
    classname: String,
    tags: List[String],
    file: Option[String],
    line: Option[Int],
    steps: List[StepRecord],
    logs: CollectedLogs,
    timestamp: Instant,
    durationSecs: Double,
    outcome: ScenarioOutcome,
    // How many times the scenario ran (retry aspects, #225). 1 unless a retry aspect applied;
    // carried into JUnit XML so aggregation systems can tell "passed after N attempts" from a
    // first-try pass.
    attempts: Int = 1
  )

  enum ScenarioOutcome:
    case Passed
    case Failed(message: String, stackTrace: Option[String])
    case Skipped
    case Pending(reason: String)
    // An `@expectedFailure` scenario whose body failed as expected (#232). Rendered as <skipped>
    // (build stays green) but distinguishable from a plain pass — CI dashboards can surface it.
    case ExpectedFailure(reason: String)

  object ScenarioOutcome:
    def from(r: ScenarioResult): ScenarioOutcome =
      // XPASS/XFAIL must be checked before `isPassed`: an expected failure has isPassed == true
      // (the inversion), so it would otherwise be indistinguishable from a genuine pass.
      if (r.isIgnored) Skipped
      else if (r.isUnexpectedlyPassing) Failed("expected to fail but passed — remove @expectedFailure", None)
      else if (r.isExpectedFailure) ExpectedFailure(r.error.map(_.getMessage).getOrElse("body failed as expected"))
      else if (r.isPassed) Passed
      else if (r.hasPending && !r.hasFailure)
        Pending(
          r.stepResults.collectFirst { case s if s.isPending => s.status.asInstanceOf[StepStatus.Pending].reason }
            .getOrElse("TODO")
        )
      else
        val msg = r.error.map(_.getMessage).getOrElse("Scenario failed")
        val trace =
          r.error.map(t => Option(t.getStackTrace).map(_.mkString("\n\tat ", "\n\tat ", "")).getOrElse("")).orElse(None)
        Failed(msg, trace)

  case class TestSuiteRecord(
    name: String,
    tags: List[String],
    file: Option[String],
    cases: List[TestCaseRecord],
    timestamp: Instant,
    durationSecs: Double
  ):
    def total: Int            = cases.length
    def failures: Int         = cases.count(_.outcome.isInstanceOf[ScenarioOutcome.Failed])
    def skipped: Int          = cases.count(_.outcome == ScenarioOutcome.Skipped)
    def pending: Int          = cases.count(_.outcome.isInstanceOf[ScenarioOutcome.Pending])
    def expectedFailures: Int = cases.count(_.outcome.isInstanceOf[ScenarioOutcome.ExpectedFailure])
    def passed: Int           = cases.count(_.outcome == ScenarioOutcome.Passed)

  sealed trait Format
  object Format:
    case object JUnit4 extends Format
    case object JUnit5 extends Format

  // ── Assembly ──────────────────────────────────────────────────────────────

  def buildSuite(
    feature: Feature,
    results: List[ScenarioResult],
    logs: Map[String, CollectedLogs],
    timestamp: Instant,
    suiteClassname: String = ""
  ): TestSuiteRecord =
    // classname on testcase = suite simple name if provided, else feature name
    // This matches the Jenkins convention where classname groups scenarios under one class
    val classname = if (suiteClassname.nonEmpty) suiteClassname else feature.name
    val cases = results.map { r =>
      val stepRecords = r.scenario.steps.zip(r.stepResults).map((s, sr) => StepRecord.from(s, sr))
      TestCaseRecord(
        name = r.scenario.name,
        classname = classname,
        tags = r.scenario.tags,
        file = r.scenario.file,
        line = r.scenario.line,
        steps = stepRecords,
        logs = logs.getOrElse(r.scenario.id.toString, CollectedLogs()),
        timestamp = timestamp,
        durationSecs = r.duration / 1000.0,
        outcome = ScenarioOutcome.from(r),
        attempts = r.attempts
      )
    }
    TestSuiteRecord(
      name = feature.name,
      tags = feature.tags,
      file = feature.file,
      cases = cases,
      timestamp = timestamp,
      durationSecs = results.map(_.duration).sum / 1000.0
    )

  // ── XML rendering ─────────────────────────────────────────────────────────

  private val iso = DateTimeFormatter.ISO_INSTANT

  def generateXML(suite: TestSuiteRecord, format: Format = Format.JUnit5): String =
    // A PrettyPrinter holds a mutable StringBuilder and is not thread-safe, so use a
    // fresh instance per call rather than a shared one.
    new PrettyPrinter(120, 2).format(suiteElem(suite, format))

  private def suiteElem(suite: TestSuiteRecord, format: Format): Elem =
    val attrs = Seq(
      "name"      -> suite.name,
      "tests"     -> suite.total.toString,
      "failures"  -> suite.failures.toString,
      "skipped"   -> (suite.skipped + suite.pending + suite.expectedFailures).toString,
      "time"      -> f"${suite.durationSecs}%.3f",
      "timestamp" -> iso.format(suite.timestamp)
    ) ++
      suite.file.map("file" -> _).toSeq ++
      (if (suite.tags.nonEmpty) Some("tags" -> suite.tags.map("@" + _).mkString(" ")) else None).toSeq ++
      (if (format == Format.JUnit4) Seq("errors" -> "0") else Nil)

    buildElem("testsuite", attrs, suite.cases.flatMap(caseElems(_, format)))

  private def caseElems(tc: TestCaseRecord, format: Format): NodeSeq =
    val baseAttrs = Seq(
      "name"      -> tc.name,
      "classname" -> tc.classname,
      "time"      -> f"${tc.durationSecs}%.3f",
      "timestamp" -> iso.format(tc.timestamp)
    ) ++
      tc.file.map("file" -> _).toSeq ++
      tc.line.map("line" -> _.toString).toSeq ++
      (if (tc.attempts > 1) Some("attempts" -> tc.attempts.toString) else None).toSeq ++
      (if (tc.tags.nonEmpty) Some("tags" -> tc.tags.map("@" + _).mkString(" ")) else None).toSeq ++
      (if (format == Format.JUnit4) Seq("assertions" -> tc.steps.count(_.outcome == StepOutcome.Passed).toString)
       else Nil)

    val children: NodeSeq =
      stepsElem(tc.steps) ++
        outcomeElem(tc.outcome) ++
        logsElems(tc.logs)

    buildElem("testcase", baseAttrs, children)

  /**
   * <steps> block — one <step> per Gherkin step with keyword, name, status,
   * time.
   */
  private def stepsElem(steps: List[StepRecord]): NodeSeq =
    if (steps.isEmpty) NodeSeq.Empty
    else
      val stepElems = steps.map { s =>
        val attrs = Seq(
          "keyword" -> s.keyword,
          "name"    -> s.name,
          "status"  -> StepOutcome.xmlStatus(s.outcome),
          "time"    -> f"${s.durationSecs}%.3f"
        )
        val inner: NodeSeq = s.outcome match
          case StepOutcome.Failed(msg, _) => <message>{msg}</message>
          case StepOutcome.Pending(r)     => <message>{r}</message>
          case StepOutcome.TimedOut(msg)  => <message>{msg}</message>
          case _                          => NodeSeq.Empty
        buildElem("step", attrs, inner)
      }
      <steps>{stepElems}</steps>

  /** <failure> / <skipped> / <pending> element from the scenario outcome. */
  private def outcomeElem(o: ScenarioOutcome): NodeSeq = o match
    case ScenarioOutcome.Passed  => NodeSeq.Empty
    case ScenarioOutcome.Skipped => <skipped/>
    case ScenarioOutcome.Pending(r) =>
      <skipped message={s"Pending: $r"}/>
    case ScenarioOutcome.ExpectedFailure(reason) =>
      <skipped message={s"expected failure: $reason"}/>
    case ScenarioOutcome.Failed(msg, trace) =>
      buildElem(
        "failure",
        Seq("message" -> msg, "type" -> "AssertionError"),
        trace.fold(NodeSeq.Empty: NodeSeq)(t => Text(t))
      )

  private def logsElems(logs: CollectedLogs): NodeSeq =
    val (out, err)       = logs.entries.partition(_.source == zio.bdd.core.LogSource.Stdout)
    val outStr           = out.map(e => s"[${e.level}] ${e.message}").mkString("\n")
    val errStr           = err.map(e => s"[${e.level}] ${e.message}").mkString("\n")
    val outElem: NodeSeq = if (outStr.nonEmpty) <system-out>{outStr}</system-out> else NodeSeq.Empty
    val errElem: NodeSeq = if (errStr.nonEmpty) <system-err>{errStr}</system-err> else NodeSeq.Empty
    outElem ++ errElem

  /**
   * Build an Elem with dynamic attributes — scala.xml requires this pattern for
   * attribute lists.
   */
  private def buildElem(label: String, attrs: Seq[(String, String)], children: NodeSeq): Elem =
    attrs.foldLeft(<x>{children}</x>.copy(label = label)) { case (elem, (k, v)) =>
      elem % scala.xml.Attribute(None, k, Text(v), scala.xml.Null)
    }

  // ── File I/O ──────────────────────────────────────────────────────────────

  def writeToFile(suite: TestSuiteRecord, filePath: String, format: Format = Format.JUnit5)(implicit
    trace: Trace
  ): ZIO[Any, Throwable, Unit] =
    ZIO.attemptBlocking {
      val xml = generateXML(suite, format)
      scala.util.Using.resource(new java.io.PrintWriter(filePath))(_.print(xml))
    }
}
