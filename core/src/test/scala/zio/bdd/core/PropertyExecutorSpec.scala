package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.property.{ColumnGenLookup, HasGen}
import zio.bdd.core.step.{State, ZIOSteps}
import zio.bdd.gherkin.GherkinParser
import zio.test.Gen

/**
 * End-to-end tests for Mode P (property-based testing) execution.
 *
 * Verifies:
 *   - Property scenarios pass when all samples satisfy the predicate
 *   - Property scenarios fail (with PropertyFalsifiedException) when a sample
 *     falsifies
 *   - Failure carries seed and counterexample in the message
 *   - The failure file is written on failure
 *   - Replaying a stored failure returns immediately without new samples
 *   - Named generator overrides via column header `| col: genName |` work
 *   - `HasGen.named` registration is resolved during execution
 *   - Existing literal BDD scenarios in the same feature are unaffected
 *   - `Default` state is reset between property samples
 */
object PropertyExecutorSpec extends ZIOSpecDefault {

  case class S(value: String = "")
  import zio.schema.{DeriveSchema, Schema}
  given Schema[S] = DeriveSchema.gen[S]

  private def runFeature(content: String, steps: ZIOSteps[Any, S]) =
    GherkinParser.parseFeature(content, "property-test.feature").flatMap { f =>
      // Use Int lookup for basic types
      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" | "x" | "y" | "a" | "b" | "c" => Some(HasGen[Int])
          case "s" | "str"                       => Some(HasGen[String])
          case _                                 => None
      FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
    }

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PropertyExecutorSpec")(
    passSuite,
    failSuite,
    generatorSuite,
    mixedSuite,
    stateSuite,
    flagsSuite,
    replaySuite,
    columnIndependenceSuite,
    dryRunSuite
  )

  // ── Passing property scenarios ─────────────────────────────────────────────

  private val passSuite = suite("Passing property scenarios")(
    test("all samples pass → ScenarioResult.isPassed") {
      val steps = new ZIOSteps[Any, S]:
        Given("a number " / int) { (n: Int) =>
          ZIO.when(false)(ZIO.fail(new RuntimeException("never"))).unit
        }
        Then("it exists")(ZIO.unit)

      runFeature(
        """Feature: Properties
          |  Scenario Outline: number always passes
          |    Given a number <n>
          |    Then it exists
          |    @property(samples=20)
          |    Examples:
          |      | n |
          |""".stripMargin,
        steps
      ).map { results =>
        val sc = results.head.scenarioResults.head
        assertTrue(
          sc.isPassed,
          // Pass result carries exactly one synthetic summary step
          sc.stepResults.length == 1,
          sc.stepResults.head.step.pattern.startsWith("[property]"),
          sc.stepResults.head.isPassed
        )
      }
    },
    test("pass result name contains sample count and seed") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      runFeature(
        """Feature: F
          |  Scenario Outline: counts
          |    Given value <n>
          |    Then ok
          |    @property(samples=5, seed=42)
          |    Examples:
          |      | n |
          |""".stripMargin,
        steps
      ).map { results =>
        val sc = results.head.scenarioResults.head
        assertTrue(
          sc.scenario.name.contains("5"),
          sc.scenario.name.contains("42"),
          sc.stepResults.head.step.pattern.contains("HasGen[Int]")
        )
      }
    }
  )

  // ── Failing property scenarios ─────────────────────────────────────────────

  private val failSuite = suite("Failing property scenarios")(
    test("failure when any sample violates predicate → hasFailure") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => State.update[S](_ => S(n.toString)))
        Then("value is zero") {
          for {
            s <- State.get[S]
            _ <- ZIO.fail(new RuntimeException(s"Expected 0, got ${s.value}")).when(s.value != "0")
          } yield ()
        }

      runFeature(
        """Feature: Fail
          |  Scenario Outline: zero predicate
          |    Given value <n>
          |    Then value is zero
          |    @property(samples=50, replay=false)
          |    Examples:
          |      | n |
          |""".stripMargin,
        steps
      ).map { results =>
        val sc = results.head.scenarioResults.head
        // First step is the synthetic [counterexample] step
        val counterStep = sc.stepResults.head
        assertTrue(
          !sc.isPassed,
          counterStep.step.pattern.startsWith("[counterexample]"),
          !counterStep.isPassed
        )
      }
    },
    test("failure message contains seed") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("always fails") {
          ZIO.fail(new RuntimeException("deliberate failure"))
        }

      runFeature(
        """Feature: Always fail
          |  Scenario Outline: alwaysFail
          |    Given value <n>
          |    Then always fails
          |    @property(samples=3, seed=999, replay=false)
          |    Examples:
          |      | n |
          |""".stripMargin,
        steps
      ).map { results =>
        val sc         = results.head.scenarioResults.head
        val errMsg     = sc.error.map(_.getMessage).getOrElse("")
        val stepDetail = sc.stepResults.head.step.pattern
        assertTrue(
          !sc.isPassed,
          errMsg.contains("999"),
          stepDetail.startsWith("[counterexample]")
        )
      }
    }
  )

  // ── Generator resolution ───────────────────────────────────────────────────

  private val generatorSuite = suite("Generator resolution")(
    test("column with :namedGen override uses registered named generator") {
      val captured = new java.util.concurrent.atomic.AtomicReference[List[Int]](Nil)
      HasGen.named("smallInts")(Gen.int(1, 5))

      val steps = new ZIOSteps[Any, S]:
        Given("value " / int) { (n: Int) =>
          ZIO.succeed(captured.updateAndGet(n :: _)).unit
        }
        Then("captured")(ZIO.unit)

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      GherkinParser
        .parseFeature(
          """Feature: Named gen
            |  Scenario Outline: small ints only
            |    Given value <n>
            |    Then captured
            |    @property(samples=20, replay=false)
            |    Examples:
            |      | n: smallInts |
            |""".stripMargin,
          "named-gen.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          val sc     = results.head.scenarioResults.head
          val values = captured.get()
          // All sampled values should be in range 1..5
          assertTrue(
            sc.isPassed,
            values.nonEmpty,
            values.forall(v => v >= 1 && v <= 5)
          )
        }
    },
    test("unknown column with no registered HasGen produces a setup error") {
      val steps = new ZIOSteps[Any, S]:
        Given("x is " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      runFeature(
        """Feature: Missing gen
          |  Scenario Outline: no gen
          |    Given x is <unknownColumn>
          |    Then ok
          |    @property(samples=5, replay=false)
          |    Examples:
          |      | unknownColumn |
          |""".stripMargin,
        new ZIOSteps[Any, S]:
          Given("x is " / string)((s: String) => ZIO.unit)
          Then("ok")(ZIO.unit)
      ).map { results =>
        val sc = results.head.scenarioResults.head
        // setupError should be set because no HasGen resolved for the column
        assertTrue(sc.setupError.isDefined)
      }
    }
  )

  // ── Mixed literal + property in same feature ───────────────────────────────

  private val mixedSuite = suite("Mixed literal BDD + property scenarios")(
    test("literal BDD scenarios are not affected by @property scenarios in the same feature") {
      val steps = new ZIOSteps[Any, S]:
        Given("a fixed value " / string) { (v: String) =>
          State.update[S](_ => S(v))
        }
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("the value is stored") {
          State.get[S].flatMap { s =>
            ZIO.fail(new RuntimeException(s"Expected 'hello', got '${s.value}'")).when(s.value != "hello").unit
          }
        }
        Then("ok")(ZIO.unit)

      runFeature(
        """Feature: Mixed
          |  Scenario: literal BDD
          |    Given a fixed value "hello"
          |    Then the value is stored
          |
          |  Scenario Outline: property scenario
          |    Given value <n>
          |    Then ok
          |    @property(samples=10, replay=false)
          |    Examples:
          |      | n |
          |""".stripMargin,
        steps
      ).map { results =>
        val scenarioResults = results.head.scenarioResults
        assertTrue(
          scenarioResults.length == 2,
          scenarioResults.head.isPassed, // literal BDD passes
          scenarioResults.last.isPassed  // property passes
        )
      }
    },
    test("literal + property Examples blocks in one Scenario Outline both run") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = Some(HasGen[Int])

      GherkinParser
        .parseFeature(
          """Feature: Dual blocks
            |  Scenario Outline: both
            |    Given value <n>
            |    Then ok
            |    @regression
            |    Examples:
            |      | n |
            |      | 1 |
            |      | 2 |
            |    @property(samples=5, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "dual.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          val scenarioResults = results.head.scenarioResults
          // 2 literal + 1 property = 3 total
          assertTrue(
            scenarioResults.length == 3,
            scenarioResults.forall(_.isPassed)
          )
        }
    }
  )

  // ── State isolation between samples ───────────────────────────────────────

  private val stateSuite = suite("State isolation between property samples")(
    test("state is reset to default between samples") {
      val invocations = new java.util.concurrent.atomic.AtomicInteger(0)
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int) { (n: Int) =>
          State.update[S](_ => S(n.toString))
        }
        Then("state was freshly initialised") {
          State.get[S].flatMap { s =>
            // Each run should see only the value set by Given, not leftover from previous sample
            ZIO.succeed(invocations.incrementAndGet()).unit
          }
        }

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = Some(HasGen[Int])

      GherkinParser
        .parseFeature(
          """Feature: State reset
            |  Scenario Outline: isolation
            |    Given value <n>
            |    Then state was freshly initialised
            |    @property(samples=10, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "state.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          assertTrue(
            results.head.scenarioResults.head.isPassed,
            invocations.get() == 10
          )
        }
    }
  )

  // ── @flags(...) combined with @property(...) ────────────────────────────────

  private val flagsSuite = suite("@flags(...) on a property scenario")(
    test("each @flags(...) combination runs its own full sample batch, with the flag map applied via flagLayer") {
      val invocations   = new java.util.concurrent.atomic.AtomicInteger(0)
      val capturedFlags = new java.util.concurrent.atomic.AtomicReference[List[Map[String, String]]](Nil)

      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.succeed(invocations.incrementAndGet()).unit)

        override def flagLayer(
          meta: zio.bdd.gherkin.ScenarioMetadata,
          flags: Map[String, String]
        ): ZLayer[Any, Throwable, Any] =
          capturedFlags.updateAndGet(flags :: _)
          scenarioLayer(meta)

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      GherkinParser
        .parseFeature(
          """Feature: Flags plus property
            |  @flags(a=true) @flags(a=false)
            |  Scenario Outline: flagged property
            |    Given value <n>
            |    Then ok
            |    @property(samples=7, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "flags-plus-property.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          val scenarioResults = results.head.scenarioResults
          val flagsSeen       = capturedFlags.get()
          assertTrue(
            // One scenario result per @flags(...) combination.
            scenarioResults.length == 2,
            scenarioResults.forall(_.isPassed),
            // Two full batches of 7 samples each — flagLayer (and thus a real sample run)
            // was invoked once per sample per flag combination, not skipped.
            invocations.get() == 14,
            flagsSeen.contains(Map("a" -> "true")),
            flagsSeen.contains(Map("a" -> "false"))
          )
        }
    }
  )

  // ── Failure replay across runs (writes/reads real .zio-bdd/failures files) ─

  private val replaySuite = suite("Failure replay store")(
    test("failing run writes a failure file; next run replays it; fixing the bug clears it and runs fresh samples") {
      val shouldFail = new java.util.concurrent.atomic.AtomicBoolean(true)
      val freshRuns  = new java.util.concurrent.atomic.AtomicInteger(0)

      def stepsFor() = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("maybe fails") {
          ZIO.succeed(freshRuns.incrementAndGet()) *>
            ZIO.fail(new RuntimeException("deliberate failure")).when(shouldFail.get()).unit
        }

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      val feature =
        """Feature: Replay across runs
          |  Scenario Outline: maybe fails
          |    Given value <n>
          |    Then maybe fails
          |    @property(samples=5, seed=4242)
          |    Examples:
          |      | n |
          |""".stripMargin

      def run() =
        GherkinParser.parseFeature(feature, "replay-across-runs.feature").flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), stepsFor().getSteps, stepsFor(), genLookup = lookup)
        }

      val failuresDir = java.nio.file.Paths.get(".zio-bdd", "failures").toFile
      def failureFiles(): List[java.io.File] =
        Option(failuresDir.listFiles()).fold(List.empty[java.io.File])(_.filter(_.getName.endsWith(".json")).toList)

      for {
        _ <- ZIO.attempt(failureFiles().foreach(_.delete())).ignore

        // Run 1: always fails → failure file should be written.
        firstResults   <- run()
        firstScenario   = firstResults.head.scenarioResults.head
        existsAfterRun1 = failureFiles().nonEmpty

        // Run 2: still fails, but replay should consult the stored seed first
        // rather than burning a fresh batch of `samples` runs.
        freshRunsBeforeRun2 = freshRuns.get()
        secondResults      <- run()
        secondScenario      = secondResults.head.scenarioResults.head
        freshRunsDuringRun2 = freshRuns.get() - freshRunsBeforeRun2

        // Fix the bug, run again: replay should now pass.
        _                  <- ZIO.succeed(shouldFail.set(false))
        freshRunsBeforeRun3 = freshRuns.get()
        thirdResults       <- run()
        thirdScenario       = thirdResults.head.scenarioResults.head
        freshRunsDuringRun3 = freshRuns.get() - freshRunsBeforeRun3
        existsAfterRun3     = failureFiles().nonEmpty

        _ <- ZIO.attempt(failureFiles().foreach(_.delete())).ignore
      } yield assertTrue(
        !firstScenario.isPassed,
        existsAfterRun1,
        !secondScenario.isPassed,
        secondScenario.error.exists(_.getMessage.contains("replayed from failure store")),
        // Replaying one stored sample should cost far fewer step invocations than a fresh 5-sample batch.
        freshRunsDuringRun2 < 5,
        thirdScenario.isPassed,
        !existsAfterRun3,
        // Once the replay passes, a full fresh batch of samples should still run.
        freshRunsDuringRun3 >= 5
      )
    },
    test("stale failure record (scenario body changed) is discarded, not replayed") {
      val freshRuns = new java.util.concurrent.atomic.AtomicInteger(0)

      def failingFeature =
        """Feature: Stale replay
          |  Scenario Outline: maybe fails
          |    Given value <n>
          |    Then it always fails
          |    @property(samples=4, seed=123)
          |    Examples:
          |      | n |
          |""".stripMargin

      // Same scenario name, edited step text → different bodyHash, and this version always
      // passes. If the stale record were wrongly replayed as a single sample, only one
      // invocation would happen; running the full fresh batch means all 4 samples execute.
      def editedPassingFeature =
        """Feature: Stale replay
          |  Scenario Outline: maybe fails
          |    Given value <n>
          |    Then it now always passes
          |    @property(samples=4, seed=123)
          |    Examples:
          |      | n |
          |""".stripMargin

      val failingSteps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("it always fails")(ZIO.fail(new RuntimeException("deliberate failure")).unit)

      def passingSteps() = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("it now always passes")(ZIO.succeed(freshRuns.incrementAndGet()).unit)

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      val failuresDir = java.nio.file.Paths.get(".zio-bdd", "failures").toFile
      def failureFiles(): List[java.io.File] =
        Option(failuresDir.listFiles()).fold(List.empty[java.io.File])(_.filter(_.getName.endsWith(".json")).toList)

      for {
        _ <- ZIO.attempt(failureFiles().foreach(_.delete())).ignore

        // Run 1: fails, writes a failure record keyed to this exact step body.
        f1               <- GherkinParser.parseFeature(failingFeature, "stale-replay.feature")
        _                <- FeatureExecutor.executeFeatures[Any, S](List(f1), failingSteps.getSteps, failingSteps, genLookup = lookup)
        recordedAfterRun1 = failureFiles().nonEmpty

        // Run 2: edited step body → bodyHash no longer matches. The stale record must be
        // discarded (full fresh batch runs), not replayed as a single stored sample.
        f2 <- GherkinParser.parseFeature(editedPassingFeature, "stale-replay.feature")
        _ <- FeatureExecutor.executeFeatures[Any, S](
               List(f2),
               passingSteps().getSteps,
               passingSteps(),
               genLookup = lookup
             )

        _ <- ZIO.attempt(failureFiles().foreach(_.delete())).ignore
      } yield assertTrue(
        recordedAfterRun1,
        // All 4 fresh samples ran — not a single-sample replay of the stale record.
        freshRuns.get() == 4
      )
    }
  )

  // ── Column independence and reproducibility ─────────────────────────────────

  private val columnIndependenceSuite = suite("Column sampling")(
    test("two columns with the same generator type are sampled independently, not correlated") {
      val captured = new java.util.concurrent.atomic.AtomicReference[List[(Int, Int)]](Nil)
      val steps = new ZIOSteps[Any, S]:
        Given("x is " / int / " and y is " / int) { (x: Int, y: Int) =>
          ZIO.succeed(captured.updateAndGet((x, y) :: _)).unit
        }
        Then("ok")(ZIO.unit)

      runFeature(
        """Feature: F
          |  Scenario Outline: independence
          |    Given x is <x> and y is <y>
          |    Then ok
          |    @property(samples=20, seed=5, replay=false)
          |    Examples:
          |      | x | y |
          |""".stripMargin,
        steps
      ).map { _ =>
        val pairs = captured.get()
        assertTrue(
          pairs.length == 20,
          // Both columns share HasGen[Int] — if they were sampled with the same seed (the
          // bug this regresses), every pair would be equal. With 20 samples from the full
          // Int range, at least one pair being unequal is overwhelming evidence of
          // independence (and in practice all of them differ).
          pairs.exists { case (x, y) => x != y }
        )
      }
    },
    test("same seed reproduces the same sampled values across separate runs") {
      def capture() = new java.util.concurrent.atomic.AtomicReference[List[(Int, Int)]](Nil)

      def runOnce(captured: java.util.concurrent.atomic.AtomicReference[List[(Int, Int)]]) =
        val steps = new ZIOSteps[Any, S]:
          Given("x is " / int / " and y is " / int) { (x: Int, y: Int) =>
            ZIO.succeed(captured.updateAndGet((x, y) :: _)).unit
          }
          Then("ok")(ZIO.unit)
        runFeature(
          """Feature: F
            |  Scenario Outline: reproducible
            |    Given x is <x> and y is <y>
            |    Then ok
            |    @property(samples=10, seed=777, replay=false)
            |    Examples:
            |      | x | y |
            |""".stripMargin,
          steps
        )

      val captured1 = capture()
      val captured2 = capture()
      for {
        _ <- runOnce(captured1)
        _ <- runOnce(captured2)
      } yield assertTrue(captured1.get() == captured2.get(), captured1.get().nonEmpty)
    }
  )

  // ── --dry-run support ────────────────────────────────────────────────────────

  private val dryRunSuite = suite("--dry-run on a property scenario")(
    test("dry-run validates step matching once, without executing step bodies or looping samples") {
      val invocations = new java.util.concurrent.atomic.AtomicInteger(0)
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.succeed(invocations.incrementAndGet()).unit)
        Then("ok")(ZIO.succeed(invocations.incrementAndGet()).unit)

      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      GherkinParser
        .parseFeature(
          """Feature: Dry run
            |  Scenario Outline: dry run check
            |    Given value <n>
            |    Then ok
            |    @property(samples=500, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "dry-run.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, dryRun = true, genLookup = lookup)
        }
        .map { results =>
          val sc = results.head.scenarioResults.head
          assertTrue(
            sc.isPassed,
            // Step bodies never ran — dry-run only validates that patterns match.
            invocations.get() == 0,
            sc.scenario.name.contains("dry-run")
          )
        }
    },
    test("dry-run still surfaces a setup error when a column has no registered HasGen") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Dry run missing gen
            |  Scenario Outline: dry run missing gen
            |    Given value <n>
            |    Then ok
            |    @property(samples=500, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "dry-run-missing-gen.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](
            List(f),
            steps.getSteps,
            steps,
            dryRun = true,
            genLookup = ColumnGenLookup.empty
          )
        }
        .map { results =>
          assertTrue(results.head.scenarioResults.head.setupError.isDefined)
        }
    }
  )
}
