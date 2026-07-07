package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.gherkin.*
import zio.bdd.core.step.{ZIOSteps, DefaultTypedExtractor}
import zio.schema.{DeriveSchema, Schema}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Gate for issue #225 — scenario-level retry tags (@retry / @flaky / @nonFlaky)
 * and the code-side `scenarioAspects` override.
 */
object RetryTagsSpec extends ZIOSpecDefault {

  case class St(x: Int = 0)
  given Schema[St] = DeriveSchema.gen[St]

  /**
   * The `When` step fails on its first `failFirst` executions (counted across
   * retry attempts via the shared counter), then succeeds.
   */
  class FlakySteps(failFirst: Int, val counter: AtomicInteger, val aspects: Map[String, ScenarioAspect] = Map.empty)
      extends ZIOSteps[Any, St]
      with DefaultTypedExtractor {
    override def scenarioAspects: Map[String, ScenarioAspect] = aspects
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the flaky action runs") {
      ZIO.succeed(counter.incrementAndGet()).flatMap { n =>
        if (n <= failFirst) ZIO.fail(new RuntimeException(s"flaky failure #$n")) else ZIO.unit
      }
    }
    Then("it is done")(ZIO.unit)
  }

  /**
   * The `When` step fails on exactly the `failOn`-th execution, passes
   * otherwise.
   */
  class FailOnNthSteps(failOn: Int, val counter: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the action runs") {
      ZIO.succeed(counter.incrementAndGet()).flatMap { n =>
        if (n == failOn) ZIO.fail(new RuntimeException(s"failure on attempt $n")) else ZIO.unit
      }
    }
    Then("it is done")(ZIO.unit)
  }

  /**
   * Does NOT reset state in `Given`, and fails the first `failFirst` attempts.
   * A step asserts the per-scenario state starts at its default each attempt —
   * so a leaked FiberRef is detectable.
   */
  class StatefulFlakySteps(failFirst: Int, val counter: AtomicInteger)
      extends ZIOSteps[Any, St]
      with DefaultTypedExtractor {
    // Deliberately does NOT reset state — a fresh attempt should already start at St(x = 0) via the
    // per-attempt FiberRef, so the `s.x != 0` check below catches a leak rather than being masked.
    Given("a system")(ZIO.unit)
    When("the stateful flaky action runs") {
      ScenarioContext.get.flatMap { s =>
        if (s.x != 0) ZIO.fail(new RuntimeException(s"state leaked across attempts: x=${s.x}"))
        else
          ScenarioContext.update(_.copy(x = 1)) *> ZIO.succeed(counter.incrementAndGet()).flatMap { n =>
            if (n <= failFirst) ZIO.fail(new RuntimeException(s"flaky failure #$n")) else ZIO.unit
          }
      }
    }
    Then("it is done")(ZIO.unit)
  }

  /**
   * `beforeScenario` hook fails (dies) every attempt → recorded as the
   * scenario's setupError.
   */
  class FailingHookSteps(val counter: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    beforeScenario { (_: ScenarioMetadata) =>
      ZIO.succeed(counter.incrementAndGet()) *> ZIO.die(new RuntimeException("setup boom"))
    }
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the flaky action runs")(ZIO.unit)
    Then("it is done")(ZIO.unit)
  }

  /**
   * The `When` step raises an interrupt-only `Cause` (the shape produced when a
   * step is cancelled), without globally interrupting the run fiber — so the
   * executor's handling of `Cause.Interrupt` is exercised deterministically.
   */
  class InterruptRaisingSteps(val attempts: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    Given("a system")(ScenarioContext.update(_ => St()))
    When("the interrupted action runs") {
      ZIO.succeed(attempts.incrementAndGet()) *> ZIO.failCause(Cause.interrupt(zio.FiberId.None))
    }
    Then("it is done")(ZIO.unit)
  }

  /** A `beforeScenario` hook that raises an interrupt-only cause. */
  class InterruptBeforeScenarioSteps(val hookRuns: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    beforeScenario { (_: ScenarioMetadata) =>
      ZIO.succeed(hookRuns.incrementAndGet()) *> ZIO.failCause(Cause.interrupt(zio.FiberId.None))
    }
    Given("a system")(ScenarioContext.update(_ => St()))
    When("a step")(ZIO.unit)
    Then("it is done")(ZIO.unit)
  }

  /**
   * A `beforeStep` hook that raises an interrupt-only cause on the first step.
   */
  class InterruptBeforeStepSteps(val hookRuns: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    beforeStep { (_: StepMetadata) =>
      ZIO.succeed(hookRuns.incrementAndGet()) *> ZIO.failCause(Cause.interrupt(zio.FiberId.None))
    }
    Given("a system")(ScenarioContext.update(_ => St()))
    When("a step")(ZIO.unit)
    Then("it is done")(ZIO.unit)
  }

  /**
   * An `afterStep` hook that raises an interrupt-only cause after the first
   * (passing) step.
   */
  class InterruptAfterStepSteps(val hookRuns: AtomicInteger) extends ZIOSteps[Any, St] with DefaultTypedExtractor {
    afterStep { (_: StepMetadata) =>
      ZIO.succeed(hookRuns.incrementAndGet()) *> ZIO.failCause(Cause.interrupt(zio.FiberId.None))
    }
    Given("a system")(ScenarioContext.update(_ => St()))
    When("a step")(ZIO.unit)
    Then("it is done")(ZIO.unit)
  }

  private val F = "retry.feature"

  private def run(content: String)(using steps: ZIOSteps[Any, St]) =
    GherkinParser.parseFeature(content, F).flatMap { feature =>
      FeatureExecutor.executeFeatures[Any, St](List(feature), steps.getSteps, steps)
    }

  private def scenario(tag: String, when: String = "the flaky action runs"): String =
    s"""Feature: Retry
       |  $tag
       |  Scenario: flaky one
       |    Given a system
       |    When $when
       |    Then it is done
       |""".stripMargin

  // ── Tag parsing (AC1) ──────────────────────────────────────────────────────
  private val parsing = suite("ScenarioAspect.fromTags (AC1)")(
    test("parses retry / flaky / nonFlaky with an integer argument") {
      assertTrue(
        ScenarioAspect.fromTags(List("retry(3)")).contains(ScenarioAspect.Retry(3)),
        ScenarioAspect.fromTags(List("flaky(5)")).contains(ScenarioAspect.Flaky(5)),
        ScenarioAspect.fromTags(List("nonFlaky(2)")).contains(ScenarioAspect.NonFlaky(2))
      )
    },
    test("tolerates a leading @ and is case-insensitive on the name") {
      assertTrue(
        ScenarioAspect.fromTags(List("@retry(4)")).contains(ScenarioAspect.Retry(4)),
        ScenarioAspect.fromTags(List("NonFlaky(3)")).contains(ScenarioAspect.NonFlaky(3))
      )
    },
    test("returns None for unrelated tags and the first match wins") {
      assertTrue(
        ScenarioAspect.fromTags(List("smoke", "wip")).isEmpty,
        ScenarioAspect.fromTags(List("retry(2)", "flaky(9)")).contains(ScenarioAspect.Retry(2))
      )
    },
    test("ignores malformed arguments and clamps a zero count to 1") {
      assertTrue(
        ScenarioAspect.fromTag("retry(abc)").isEmpty,
        ScenarioAspect.fromTag("retry()").isEmpty,
        ScenarioAspect.fromTag("retry").isEmpty,
        ScenarioAspect.fromTag("retry(0)").contains(ScenarioAspect.Retry(1))
      )
    }
  )

  // ── Semantics (AC2) ─────────────────────────────────────────────────────────
  private val semantics = suite("retry semantics (AC2)")(
    test("@retry(n) re-runs on failure and passes on the first success") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 2, counter) {}
      run(scenario("@retry(3)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 3, counter.get() == 3)
      }
    },
    test("@retry(n) gives up after n attempts and reports failure") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 99, counter) {}
      run(scenario("@retry(2)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(!sc.isPassed, sc.attempts == 2, counter.get() == 2)
      }
    },
    test("@flaky(n) passes if any of the n runs succeeds") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 1, counter) {}
      run(scenario("@flaky(3)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 2)
      }
    },
    test("@nonFlaky(n) runs all n when they all pass") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 0, counter) {}
      run(scenario("@nonFlaky(3)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 3, counter.get() == 3)
      }
    },
    test("@nonFlaky(n) fails fast on the first failing run") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FailOnNthSteps(failOn = 2, counter) {}
      run(scenario("@nonFlaky(5)", when = "the action runs")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(!sc.isPassed, sc.attempts == 2, counter.get() == 2)
      }
    }
  )

  // ── Code-side override (AC3) + interactions (AC4) ───────────────────────────
  private val overrideAndInteractions = suite("code-side override + interactions (AC3, AC4)")(
    test("scenarioAspects code override applies when there is no tag") {
      val counter = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] =
        new FlakySteps(failFirst = 2, counter, aspects = Map("flaky one" -> ScenarioAspect.Retry(3))) {}
      run(scenario("# no tag")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 3)
      }
    },
    test("@ignore wins over @retry — an ignored scenario is not retried") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 99, counter) {}
      run(scenario("@ignore @retry(3)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isIgnored, counter.get() == 0)
      }
    },
    test("a scenario without any aspect runs exactly once (attempts == 1)") {
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FlakySteps(failFirst = 0, counter) {}
      run(scenario("# none")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 1, counter.get() == 1)
      }
    },
    test("a scenario tag takes precedence over the scenarioAspects map") {
      // Map says NonFlaky(1) (run once → would fail); tag says @retry(3) (would pass on attempt 3).
      val counter = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] =
        new FlakySteps(failFirst = 2, counter, aspects = Map("flaky one" -> ScenarioAspect.NonFlaky(1))) {}
      run(scenario("@retry(3)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 3) // tag won
      }
    },
    test("each attempt gets fresh per-scenario state (no FiberRef leak)") {
      // The step fails once then passes; it also asserts state.x == 0 at the start of every attempt.
      val counter                    = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new StatefulFlakySteps(failFirst = 1, counter) {}
      run(scenario("@retry(3)", when = "the stateful flaky action runs")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(sc.isPassed, sc.attempts == 2) // passed on attempt 2 without a leak failure
      }
    },
    test("a failing beforeScenario hook is retried and runs the hook each attempt") {
      val hookRuns                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new FailingHookSteps(hookRuns) {}
      run(scenario("@retry(2)")).map { r =>
        val sc = r.head.scenarioResults.head
        assertTrue(!sc.isPassed, sc.attempts == 2, hookRuns.get() == 2)
      }
    }
  )

  // ── Interruption (issue #230) ───────────────────────────────────────────────
  private val interruption = suite("interruption vs failure (#230)")(
    test("an interrupted step propagates interruption instead of completing as a failed scenario (AC1)") {
      val attempts                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new InterruptRaisingSteps(attempts)
      run(scenario("# no tag", when = "the interrupted action runs")).exit.map { exit =>
        // Unfixed: the interrupt is recorded as a failed step, so the run completes (Success).
        assertTrue(exit.isInterrupted, attempts.get() == 1)
      }
    },
    test("@retry(n) does NOT re-run an interrupted scenario — cancellation is honored (AC2)") {
      val attempts                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new InterruptRaisingSteps(attempts)
      run(scenario("@retry(3)", when = "the interrupted action runs")).exit.map { exit =>
        // Unfixed: interrupt → failed attempt → retried 3×. Fixed: propagates on the first attempt.
        assertTrue(exit.isInterrupted, attempts.get() == 1)
      }
    },
    test("an interrupted beforeScenario hook propagates and is not retried") {
      val hookRuns                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new InterruptBeforeScenarioSteps(hookRuns)
      run(scenario("@retry(3)", when = "a step")).exit.map { exit =>
        assertTrue(exit.isInterrupted, hookRuns.get() == 1)
      }
    },
    test("an interrupted beforeStep hook propagates and is not retried") {
      val hookRuns                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new InterruptBeforeStepSteps(hookRuns)
      run(scenario("@retry(3)", when = "a step")).exit.map { exit =>
        assertTrue(exit.isInterrupted, hookRuns.get() == 1)
      }
    },
    test("an interrupted afterStep hook propagates and is not retried") {
      val hookRuns                   = new AtomicInteger(0)
      given steps: ZIOSteps[Any, St] = new InterruptAfterStepSteps(hookRuns)
      run(scenario("@retry(3)", when = "a step")).exit.map { exit =>
        assertTrue(exit.isInterrupted, hookRuns.get() == 1)
      }
    }
  )

  def spec = suite("RetryTagsSpec")(parsing, semantics, overrideAndInteractions, interruption)
}
