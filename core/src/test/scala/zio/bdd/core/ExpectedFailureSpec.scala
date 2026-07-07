package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.gherkin.*
import zio.bdd.core.step.{ZIOSteps, DefaultTypedExtractor}
import zio.bdd.core.report.*
import zio.schema.{DeriveSchema, Schema}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Gate for issue #232 — the `@expectedFailure` / `@failing` scenario tag, which
 * inverts a scenario's effective outcome.
 */
object ExpectedFailureSpec extends ZIOSpecDefault {

  case class St(x: Int = 0)
  given Schema[St] = DeriveSchema.gen[St]

  /**
   * `When` step passes or fails per `pass`; counts executions across attempts.
   */
  class OutcomeSteps(pass: Boolean, val counter: AtomicInteger, val aspects: Map[String, ScenarioAspect] = Map.empty)
      extends ZIOSteps[Any, St]
      with DefaultTypedExtractor {
    override def scenarioAspects: Map[String, ScenarioAspect] = aspects
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the action runs") {
      ZIO.succeed(counter.incrementAndGet()) *> (if (pass) ZIO.unit else ZIO.fail(new RuntimeException("boom")))
    }
    Then("it is done")(ZIO.unit)
  }

  /** A `beforeScenario` hook that dies — an infrastructure/setup failure. */
  class FailingSetupSteps extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    beforeScenario((_: ScenarioMetadata) => ZIO.die(new RuntimeException("infra down")))
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the action runs")(ZIO.unit)
    Then("it is done")(ZIO.unit)
  }

  /** A step marked pending (unimplemented). */
  class PendingSteps extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the action runs")(pending("not implemented"))
    Then("it is done")(ZIO.unit)
  }

  private def run(content: String, dryRun: Boolean = false)(using steps: ZIOSteps[Any, St]) =
    GherkinParser.parseFeature(content, "xf.feature").flatMap { feature =>
      FeatureExecutor.executeFeatures[Any, St](List(feature), steps.getSteps, steps, dryRun = dryRun)
    }

  private def scenario(tag: String): String =
    s"""Feature: XF
       |  $tag
       |  Scenario: known bug
       |    Given a system
       |    When the action runs
       |    Then it is done
       |""".stripMargin

  private def scResult(content: String)(using steps: ZIOSteps[Any, St]): ZIO[Any, Throwable, ScenarioResult] =
    run(content).map(_.head.scenarioResults.head)

  // ── Parsing (AC1) ───────────────────────────────────────────────────────────
  private val parsing = suite("ScenarioAspect parsing (AC1)")(
    test("parses @expectedFailure and @failing, case-insensitively") {
      assertTrue(
        ScenarioAspect.fromTag("expectedFailure").contains(ScenarioAspect.ExpectedFailure),
        ScenarioAspect.fromTag("@failing").contains(ScenarioAspect.ExpectedFailure),
        ScenarioAspect.fromTag("EXPECTEDFAILURE").contains(ScenarioAspect.ExpectedFailure),
        ScenarioAspect.fromTag("smoke").isEmpty
      )
    }
  )

  // ── Outcome inversion (AC2, AC3) ────────────────────────────────────────────
  private val inversion = suite("outcome inversion (AC2, AC3)")(
    test("a failing body under @expectedFailure is an expected (passing) failure") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, new AtomicInteger(0))
      scResult(scenario("@expectedFailure")).map { sc =>
        assertTrue(sc.isPassed, sc.isExpectedFailure, !sc.hasFailure, !sc.isUnexpectedlyPassing)
      }
    },
    test("a passing body under @expectedFailure is an unexpectedly-passing failure") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = true, new AtomicInteger(0))
      scResult(scenario("@expectedFailure")).map { sc =>
        assertTrue(!sc.isPassed, sc.isUnexpectedlyPassing, sc.hasFailure, !sc.isExpectedFailure)
      }
    },
    test("@failing alias behaves the same") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, new AtomicInteger(0))
      scResult(scenario("@failing")).map(sc => assertTrue(sc.isPassed, sc.isExpectedFailure))
    },
    test("the inversion reaches the build gate: XFAIL → feature passes, XPASS → feature fails") {
      for
        xf <- run(scenario("@expectedFailure"))(using new OutcomeSteps(pass = false, new AtomicInteger(0)))
        xp <- run(scenario("@expectedFailure"))(using new OutcomeSteps(pass = true, new AtomicInteger(0)))
      yield assertTrue(xf.head.isPassed, xf.head.isComplete, !xp.head.isPassed, !xp.head.isComplete)
    }
  )

  // ── Code-side + interactions (AC4) ──────────────────────────────────────────
  private val interactions = suite("code-side + interactions (AC4)")(
    test("code-side scenarioAspects ExpectedFailure applies without a tag") {
      given steps: ZIOSteps[Any, St] =
        new OutcomeSteps(
          pass = false,
          new AtomicInteger(0),
          aspects = Map("known bug" -> ScenarioAspect.ExpectedFailure)
        )
      scResult(scenario("# none")).map(sc => assertTrue(sc.isPassed, sc.isExpectedFailure))
    },
    test("@ignore wins over @expectedFailure — the scenario is not run") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, counter)
      scResult(scenario("@ignore @expectedFailure")).map(sc => assertTrue(sc.isIgnored, counter.get() == 0))
    },
    test("@expectedFailure takes precedence over a retry tag — the body runs once") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, counter)
      scResult(scenario("@expectedFailure @retry(3)")).map { sc =>
        assertTrue(sc.isExpectedFailure, sc.attempts == 1, counter.get() == 1)
      }
    },
    test("@expectedFailure takes precedence over @flaky too") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, counter)
      scResult(scenario("@expectedFailure @flaky(3)")).map(sc => assertTrue(sc.isExpectedFailure, sc.attempts == 1))
    },
    test("a SETUP failure under @expectedFailure is a real failure, not masked green") {
      given steps: ZIOSteps[Any, St] = new FailingSetupSteps
      scResult(scenario("@expectedFailure")).map { sc =>
        assertTrue(!sc.isPassed, sc.hasFailure, !sc.isExpectedFailure, !sc.isUnexpectedlyPassing)
      }
    },
    test("--dry-run does not turn @expectedFailure into an unexpectedly-passing failure") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, new AtomicInteger(0))
      run(scenario("@expectedFailure"), dryRun = true).map { fr =>
        val sc = fr.head.scenarioResults.head
        assertTrue(sc.isPassed, !sc.isUnexpectedlyPassing, fr.head.isPassed)
      }
    },
    test("a PENDING step under @expectedFailure stays PENDING, not absorbed as XFAIL") {
      given steps: ZIOSteps[Any, St] = new PendingSteps
      scResult(scenario("@expectedFailure")).map { sc =>
        assertTrue(sc.hasPending, !sc.isExpectedFailure, !sc.hasFailure, sc.isComplete, !sc.isUnexpectedlyPassing)
      }
    }
  )

  // ── Reporter states (AC5) ───────────────────────────────────────────────────
  private val reporter = suite("reporter surfaces the states (AC5)")(
    test("an expected failure renders as XFAIL, not red or a plain pass") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = false, new AtomicInteger(0))
      scResult(scenario("@expectedFailure")).map { sc =>
        val header = DocBuilder.scenarioBranch(sc, isLast = true, featureIgnored = false, _ => CollectedLogs()) match
          case Doc.Branch(h, _, _) => h.text
          case _                   => ""
        val color = summon[StatusColor[ScenarioResult]].style(sc).color
        assertTrue(header.toUpperCase.contains("XFAIL"), color != Color.Red)
      }
    },
    test("an unexpectedly-passing scenario renders as a red failure") {
      given steps: ZIOSteps[Any, St] = new OutcomeSteps(pass = true, new AtomicInteger(0))
      scResult(scenario("@expectedFailure")).map { sc =>
        val header = DocBuilder.scenarioBranch(sc, isLast = true, featureIgnored = false, _ => CollectedLogs()) match
          case Doc.Branch(h, _, _) => h.text
          case _                   => ""
        val color = summon[StatusColor[ScenarioResult]].style(sc).color
        assertTrue(header.toUpperCase.contains("UNEXPECTEDLY"), color == Color.Red)
      }
    }
  )

  def spec = suite("ExpectedFailureSpec")(parsing, inversion, interactions, reporter)
}
