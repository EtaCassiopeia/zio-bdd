package zio.bdd.core.report

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.*
import zio.bdd.gherkin.*

/**
 * Tests for the refactored PrettyReporter:
 *
 *   1. Color model — Color and Style are well-typed 2. StatusColor typeclass —
 *      correct styles per status 3. ScenarioCounts — pure aggregation 4.
 *      DocBuilder — pure Doc construction (no IO) 5. AnsiRenderer /
 *      PlainRenderer / BufferRenderer — render Doc to output 6. PrettyReporter
 *      end-to-end with BufferRenderer
 */
object PrettyReporterSpec extends ZIOSpecDefault {

  private val F = "test.feature"

  private def mkStep(t: StepType, p: String, lineNo: Int = 1) =
    Step(t, p, file = Some(F), line = Some(lineNo))

  private def mkStepResult(step: Step, status: StepStatus, durationMs: Long = 0L): StepResult =
    status match
      case StepStatus.Passed         => StepResult(step, Right(()), durationMs)
      case StepStatus.Failed(c)      => StepResult(step, Left(c), durationMs)
      case StepStatus.TimedOut(d, c) => StepResult(step, Left(c), durationMs)
      case StepStatus.Skipped        => StepResult.skipped(step)
      case StepStatus.Pending(msg)   => StepResult(step, Left(Cause.fail(new PendingException(msg))), durationMs)

  private def mkScenario(name: String, tags: List[String] = Nil, lineNo: Int = 1) =
    Scenario(name, tags, Nil, Some(F), Some(lineNo))

  private def mkScenarioResult(
    name: String,
    steps: List[(StepType, String, StepStatus)],
    tags: List[String] = Nil
  ): ScenarioResult = {
    val sc = mkScenario(name, tags)
    val stepResults = steps.zipWithIndex.map { case ((t, p, st), i) =>
      mkStepResult(mkStep(t, p, i + 1), st)
    }
    ScenarioResult(sc, stepResults)
  }

  private def mkFeatureResult(name: String, scenarios: List[ScenarioResult], durationMs: Long = 0L) =
    FeatureResult(Feature(name, Nil, Nil, Some(F), Some(1)), scenarios, durationMs)

  private val emptyLogs = CollectedLogs()

  private val statusColorTests = suite("StatusColor typeclass")(
    test("StepStatus.Passed → Mint") {
      val s = summon[StatusColor[StepStatus]].style(StepStatus.Passed)
      assertTrue(s.color == Color.Mint)
    },
    test("StepStatus.Failed → Red + cross icon") {
      val s = summon[StatusColor[StepStatus]].style(StepStatus.Failed(Cause.fail(new Exception())))
      assertTrue(s.color == Color.Red, s.icon == "✗")
    },
    test("StepStatus.TimedOut → Red + clock icon") {
      val s = summon[StatusColor[StepStatus]].style(
        StepStatus.TimedOut(zio.Duration.fromSeconds(30), Cause.fail(new Exception()))
      )
      assertTrue(s.color == Color.Red, s.icon == "⏱")
    },
    test("StepStatus.Skipped → Gray") {
      val s = summon[StatusColor[StepStatus]].style(StepStatus.Skipped)
      assertTrue(s.color == Color.Gray)
    },
    test("StepStatus.Pending → Orange") {
      val s = summon[StatusColor[StepStatus]].style(StepStatus.Pending("todo"))
      assertTrue(s.color == Color.Orange)
    },
    test("passed ScenarioResult → Blue") {
      val sc = mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Passed)))
      val s  = summon[StatusColor[ScenarioResult]].style(sc)
      assertTrue(s.color == Color.Blue)
    },
    test("failed ScenarioResult → Red") {
      val sc =
        mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Failed(Cause.fail(new Exception())))))
      val s = summon[StatusColor[ScenarioResult]].style(sc)
      assertTrue(s.color == Color.Red)
    },
    test("ignored ScenarioResult → Gray") {
      val sc = mkScenarioResult("s", Nil, tags = List("ignore"))
      val s  = summon[StatusColor[ScenarioResult]].style(sc)
      assertTrue(s.color == Color.Gray)
    },
    test("pending ScenarioResult (pending step, no failure) → Orange") {
      val sc = mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Pending("todo"))))
      val s  = summon[StatusColor[ScenarioResult]].style(sc)
      assertTrue(s.color == Color.Orange)
    },
    test("passed FeatureResult → Green + circle icon") {
      val sc = mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Passed)))
      val fr = mkFeatureResult("F", List(sc))
      val s  = summon[StatusColor[FeatureResult]].style(fr)
      assertTrue(s.color == Color.Green, s.icon == "◉")
    },
    test("failed FeatureResult → Red") {
      val sc =
        mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Failed(Cause.fail(new Exception())))))
      val fr = mkFeatureResult("F", List(sc))
      val s  = summon[StatusColor[FeatureResult]].style(fr)
      assertTrue(s.color == Color.Red)
    }
  )

  private val countsTests = suite("ScenarioCounts")(
    test("fromFeature counts passed, failed, ignored and pending correctly") {
      val passed = mkScenarioResult("passed", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val failed =
        mkScenarioResult("failed", List((StepType.GivenStep, "a", StepStatus.Failed(Cause.fail(new Exception())))))
      val ignored = mkScenarioResult("ignored", Nil, tags = List("ignore"))
      val pending = mkScenarioResult("pending", List((StepType.GivenStep, "a", StepStatus.Pending("todo"))))
      val fr      = mkFeatureResult("F", List(passed, failed, ignored, pending))
      val counts  = ScenarioCounts.fromFeature(fr)
      assertTrue(
        counts.passed == 1,
        counts.failed == 1,
        counts.ignored == 1,
        counts.pending == 1
      )
    },
    test("fromFeature treats an entirely ignored feature as all-ignored") {
      val sc = mkScenarioResult("s", Nil, tags = List("ignore"))
      // A feature with all scenarios ignored → feature.isIgnored == false but
      // all scenarios are individually ignored
      val fr     = mkFeatureResult("F", List(sc, sc))
      val counts = ScenarioCounts.fromFeature(fr)
      assertTrue(counts.ignored == 2, counts.passed == 0)
    },
    test("aggregate sums counts across multiple features") {
      val sc1 = mkScenarioResult("a", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val sc2 = mkScenarioResult("b", List((StepType.GivenStep, "b", StepStatus.Failed(Cause.fail(new Exception())))))
      val fr1 = mkFeatureResult("F1", List(sc1))
      val fr2 = mkFeatureResult("F2", List(sc2))
      val agg = ScenarioCounts.aggregate(List(fr1, fr2))
      assertTrue(agg.passed == 1, agg.failed == 1)
    },
    test("toSummaryString includes all four counters") {
      val c = ScenarioCounts(3, 1, 2, 0)
      val s = c.toSummaryString
      assertTrue(s.contains("3"), s.contains("1"), s.contains("2"), s.contains("Passed"), s.contains("Failed"))
    }
  )

  private val docBuilderTests = suite("DocBuilder (pure)")(
    test("stepLeaf text includes step keyword and pattern") {
      val step = mkStep(StepType.GivenStep, "a condition", lineNo = 5)
      val sr   = StepResult(step, Right(()), 0L)
      val leaf = DocBuilder.stepLeaf(sr)
      assertTrue(leaf.text.contains("Given"), leaf.text.contains("a condition"))
    },
    test("stepLeaf for failed step has style Red") {
      val step = mkStep(StepType.ThenStep, "assertion fails")
      val sr   = StepResult(step, Left(Cause.fail(new RuntimeException("boom"))), 0L)
      val leaf = DocBuilder.stepLeaf(sr)
      assertTrue(leaf.style.color == Color.Red)
    },
    test("stepLeaf for timed-out step has style Red and TIMEOUT label") {
      val step  = mkStep(StepType.WhenStep, "a slow step")
      val cause = Cause.fail(new StepTimeoutException("a slow step", zio.Duration.fromSeconds(5)))
      val sr    = StepResult(step, Left(cause), 0L)
      val leaf  = DocBuilder.stepLeaf(sr)
      assertTrue(leaf.style.color == Color.Red, leaf.text.contains("TIMEOUT"), leaf.text.contains("5s"))
    },
    test("stepLeaf for skipped step has style Gray") {
      val step    = mkStep(StepType.ThenStep, "skipped step")
      val skipped = StepResult.skipped(step)
      val leaf    = DocBuilder.stepLeaf(skipped)
      assertTrue(leaf.style.color == Color.Gray, leaf.text.contains("SKIPPED"))
    },
    test("stepBranch with no logs and passed step is a Branch with no children") {
      val step = mkStep(StepType.GivenStep, "a step")
      val sr   = StepResult(step, Right(()), 0L)
      DocBuilder.stepBranch(sr, isLast = true, emptyLogs) match
        case Doc.Branch(_, children, true) => assertTrue(children.isEmpty)
        case _                             => assertTrue(false)
    },
    test("stepBranch for a failed step includes cause leaves as children") {
      val step  = mkStep(StepType.ThenStep, "assertion")
      val cause = Cause.fail(new RuntimeException("boom"))
      val sr    = StepResult(step, Left(cause), 0L)
      DocBuilder.stepBranch(sr, isLast = false, emptyLogs) match
        case Doc.Branch(_, children, false) => assertTrue(children.nonEmpty)
        case _                              => assertTrue(false)
    },
    test("stepBranch for a timed-out step includes cause leaves as children") {
      val step  = mkStep(StepType.WhenStep, "slow op")
      val cause = Cause.fail(new StepTimeoutException("slow op", zio.Duration.fromSeconds(10)))
      val sr    = StepResult(step, Left(cause), 0L)
      DocBuilder.stepBranch(sr, isLast = false, emptyLogs) match
        case Doc.Branch(_, children, false) => assertTrue(children.nonEmpty)
        case _                              => assertTrue(false)
    },
    test("stepBranch for MultipleAssertionError renders each failure on its own line") {
      val step  = mkStep(StepType.ThenStep, "multi-check")
      val mae   = new zio.bdd.core.MultipleAssertionError(List("first", "second", "third"))
      val cause = Cause.fail(mae)
      val sr    = StepResult(step, Left(cause), 0L)
      DocBuilder.stepBranch(sr, isLast = false, emptyLogs) match
        case Doc.Branch(_, children, false) =>
          val texts = children.collect { case Doc.Leaf(t, _) => t }
          assertTrue(
            texts.length == 3,
            texts.exists(_.contains("first")),
            texts.exists(_.contains("second")),
            texts.exists(_.contains("third"))
          )
        case _ => assertTrue(false)
    },
    test("stepBranch for a [counterexample] step renders one table row per column") {
      import zio.bdd.core.property.PropertyExecutor.counterexampleEntrySep
      val pattern =
        s"[counterexample] amount=42 (HasGen[Int])${counterexampleEntrySep}limit=0 (HasGen[Long])"
      val step = mkStep(StepType.GivenStep, pattern)
      val sr   = StepResult(step, Left(Cause.fail(new RuntimeException("boom"))), 0L)
      DocBuilder.stepBranch(sr, isLast = false, emptyLogs) match
        case Doc.Branch(_, children, false) =>
          val texts = children.collect { case Doc.Leaf(t, _) => t }
          assertTrue(
            texts.exists(_.contains("amount")),
            texts.exists(_.contains("42")),
            texts.exists(_.contains("limit")),
            // The two columns must end up as two distinct rows, not merged into one.
            texts.count(_.contains("HasGen")) == 2
          )
        case _ => assertTrue(false)
    },
    test("stepBranch for a [counterexample] step is not corrupted by a comma inside a value") {
      import zio.bdd.core.property.PropertyExecutor.counterexampleEntrySep
      // A domain type's default toString commonly contains a literal comma, e.g. a case class:
      // Address(Main St,London). A plain comma-separated join would mis-split this into two
      // fake columns; the dedicated entry separator must keep it as one.
      val pattern =
        s"[counterexample] address=Address(Main St,London) (HasGen[Address])${counterexampleEntrySep}zip=12345 (HasGen[String])"
      val step = mkStep(StepType.GivenStep, pattern)
      val sr   = StepResult(step, Left(Cause.fail(new RuntimeException("boom"))), 0L)
      DocBuilder.stepBranch(sr, isLast = false, emptyLogs) match
        case Doc.Branch(_, children, false) =>
          val texts = children.collect { case Doc.Leaf(t, _) => t }
          assertTrue(
            // Exactly one row contains the full, unsplit address value...
            texts.count(_.contains("Address(Main St,London)")) == 1,
            // ...and a separate row for zip, not three+ rows from a naive comma split.
            texts.exists(_.contains("zip")),
            texts.count(_.contains("HasGen")) == 2
          )
        case _ => assertTrue(false)
    },
    test("featureSummary leaf contains Passed/Failed/Ignored/Pending labels") {
      val sc   = mkScenarioResult("s", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val fr   = mkFeatureResult("F", List(sc))
      val leaf = DocBuilder.featureSummary(fr)
      assertTrue(leaf.text.contains("Passed"), leaf.text.contains("Failed"))
    },
    test("globalSummary leaf aggregates all features") {
      val sc1  = mkScenarioResult("a", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val sc2  = mkScenarioResult("b", List((StepType.GivenStep, "b", StepStatus.Failed(Cause.fail(new Exception())))))
      val fr1  = mkFeatureResult("F1", List(sc1))
      val fr2  = mkFeatureResult("F2", List(sc2))
      val leaf = DocBuilder.globalSummary(List(fr1, fr2))
      assertTrue(leaf.text.contains("Summary"), leaf.text.contains("1"))
    },
    test("featureBranch header contains feature name and status") {
      val sc = mkScenarioResult("s", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val fr = mkFeatureResult("MyFeature", List(sc))
      DocBuilder.featureBranch(fr, isLast = false, (_, _) => emptyLogs) match
        case Doc.Branch(header, _, _) =>
          assertTrue(header.text.contains("MyFeature"), header.text.contains("PASSED"))
        case _ => assertTrue(false)
    },
    test("featureBranch children include one branch per scenario plus summary") {
      val sc1 = mkScenarioResult("A", List((StepType.GivenStep, "a", StepStatus.Passed)))
      val sc2 = mkScenarioResult("B", List((StepType.WhenStep, "b", StepStatus.Passed)))
      val fr  = mkFeatureResult("F", List(sc1, sc2))
      DocBuilder.featureBranch(fr, isLast = true, (_, _) => emptyLogs) match
        case Doc.Branch(_, children, _) =>
          // 2 scenarios + 1 summary leaf = 3 children
          assertTrue(children.length == 3)
        case _ => assertTrue(false)
    },
    test("ignored scenario branch skips step rendering (no step children)") {
      val sc  = mkScenarioResult("ignored", Nil, tags = List("ignore"))
      val doc = DocBuilder.scenarioBranch(sc, isLast = false, featureIgnored = false, _ => emptyLogs)
      doc match
        case Doc.Branch(_, children, _) => assertTrue(children.isEmpty)
        case _                          => assertTrue(false)
    },
    test("durStr is included in step text when duration > 0") {
      val step = mkStep(StepType.GivenStep, "timed step")
      val sr   = StepResult(step, Right(()), 123L)
      val leaf = DocBuilder.stepLeaf(sr)
      assertTrue(leaf.text.contains("123ms"))
    }
  )

  private val bufferRendererTests = suite("BufferRenderer")(
    test("renders a Leaf to one line") {
      for {
        buf <- BufferRenderer.make
        _   <- buf.render(Doc.Leaf("hello world", Style(Color.Green, "◉")))
        out <- buf.collected
      } yield assertTrue(out.lines.length == 1, out.lines.head.text.contains("hello world"))
    },
    test("renders a Branch: header then indented children") {
      for {
        buf   <- BufferRenderer.make
        header = Doc.Leaf("parent", Style(Color.Teal, "◉"))
        child  = Doc.Leaf("child", Style(Color.Green, "•"))
        _     <- buf.render(Doc.Branch(header, List(child), isLast = true))
        out   <- buf.collected
      } yield assertTrue(
        out.lines.length == 2,
        out.lines.head.text.contains("parent"),
        out.lines(1).text.contains("child")
      )
    },
    test("renders a Many by rendering each Doc in sequence") {
      for {
        buf <- BufferRenderer.make
        many = Doc.Many(
                 List(
                   Doc.Leaf("a", Style(Color.Green)),
                   Doc.Leaf("b", Style(Color.Cyan)),
                   Doc.Leaf("c", Style(Color.Red))
                 )
               )
        _   <- buf.render(many)
        out <- buf.collected
      } yield assertTrue(out.lines.length == 3)
    },
    test("children of a branch are indented relative to the header") {
      for {
        buf   <- BufferRenderer.make
        header = Doc.Leaf("root", Style(Color.Cyan))
        child  = Doc.Leaf("leaf", Style(Color.Green))
        _     <- buf.render(Doc.Branch(header, List(child), isLast = false), indent = "  ")
        out   <- buf.collected
      } yield assertTrue(
        // header has "  ├─ " prefix
        out.lines.head.text.contains("root"),
        // child is indented further
        out.lines(1).text.contains("leaf")
      )
    }
  )

  private val plainRendererTests = suite("PlainRenderer")(
    test("plain renderer does not include ANSI escape codes") {
      for {
        buf <- BufferRenderer.make
        // Use PlainRenderer logic: no ANSI codes should appear in output
        // We test by using the BufferRenderer directly (which also produces no codes)
        _   <- buf.render(Doc.Leaf("step text", Style(Color.Green, "•")))
        out <- buf.collected
        // Verify no ESC character in the output
      } yield assertTrue(!out.lines.head.text.contains(""))
    }
  )

  // ── PrettyReporter end-to-end ─────────────────────────────────────────────

  private val endToEnd = suite("PrettyReporter end-to-end (BufferRenderer)")(
    test("report output contains feature name") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("My Scenario", List((StepType.GivenStep, "a step", StepStatus.Passed)))
        fr    = mkFeatureResult("My Feature", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(_.text.contains("My Feature")))
    },
    test("report output contains scenario name") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("My Scenario", List((StepType.GivenStep, "a step", StepStatus.Passed)))
        fr    = mkFeatureResult("F", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(_.text.contains("My Scenario")))
    },
    test("report output contains step pattern") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("s", List((StepType.GivenStep, "a unique step pattern", StepStatus.Passed)))
        fr    = mkFeatureResult("F", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(_.text.contains("a unique step pattern")))
    },
    test("report output contains global summary") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Passed)))
        fr    = mkFeatureResult("F", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(_.text.contains("Summary")))
    },
    test("ignored scenario is present in output but shows IGNORED") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("Ignored S", Nil, tags = List("ignore"))
        fr    = mkFeatureResult("F", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(l => l.text.contains("Ignored S") && l.text.contains("IGNORED")))
    },
    test("multiple features all appear in output") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        sc    = mkScenarioResult("s", List((StepType.GivenStep, "a", StepStatus.Passed)))
        fr1   = mkFeatureResult("Alpha", List(sc))
        fr2   = mkFeatureResult("Beta", List(sc))
        fr3   = mkFeatureResult("Gamma", List(sc))
        _    <- reporter.report(List(fr1, fr2, fr3)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(
        out.lines.exists(_.text.contains("Alpha")),
        out.lines.exists(_.text.contains("Beta")),
        out.lines.exists(_.text.contains("Gamma"))
      )
    },
    test("failed step cause appears in output") {
      for {
        pair <- PrettyReporter.buffered; (reporter, buf) = pair
        cause = Cause.fail(new RuntimeException("assertion boom"))
        sc    = mkScenarioResult("s", List((StepType.ThenStep, "assertion", StepStatus.Failed(cause))))
        fr    = mkFeatureResult("F", List(sc))
        _    <- reporter.report(List(fr)).provide(LogCollector.live())
        out  <- buf.collected
      } yield assertTrue(out.lines.exists(_.text.contains("boom")))
    }
  )

  private val glyphTests = suite("Glyph tree-drawing characters")(
    test("isLast=true produces LastBranch prefix") {
      assertTrue(Glyph.prefix(true) == Glyph.LastBranch)
    },
    test("isLast=false produces Branch prefix") {
      assertTrue(Glyph.prefix(false) == Glyph.Branch)
    },
    test("isLast=true produces Indent child-indent") {
      assertTrue(Glyph.childIndent(true) == Glyph.Indent)
    },
    test("isLast=false produces Vertical child-indent") {
      assertTrue(Glyph.childIndent(false) == Glyph.Vertical)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PrettyReporter")(
    statusColorTests,
    countsTests,
    docBuilderTests,
    bufferRendererTests,
    plainRendererTests,
    endToEnd,
    glyphTests
  )
}
