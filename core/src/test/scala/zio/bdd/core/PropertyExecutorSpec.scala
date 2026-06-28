package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.property.{ColumnGenLookup, HasGen, PropertyFailureStore}
import zio.bdd.core.step.{State, TypedExtractor, ZIOSteps}
import zio.bdd.gherkin.GherkinParser
import zio.test.Gen

import java.nio.file.{Files, Path}

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

  private def deleteRecursively(dir: Path): UIO[Unit] =
    ZIO.attempt {
      import scala.jdk.CollectionConverters.*
      if (Files.exists(dir)) {
        val walk = Files.walk(dir)
        try walk.iterator().asScala.toList.sortBy(_.toString).reverse.foreach(Files.deleteIfExists(_))
        finally walk.close()
      }
    }.orDie

  // Redirect the property failure store to a fresh, scoped temp directory so the replay
  // tests never read, write, or bulk-delete the process-global `.zio-bdd/failures` dir —
  // which other tests touch concurrently (#138). `use` receives that directory.
  private def withIsolatedStore[R, E, A](use: Path => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO
      .acquireRelease(ZIO.attempt(Files.createTempDirectory("zio-bdd-failures-exec")).orDie)(deleteRecursively)
      .flatMap(dir => PropertyFailureStore.withBaseDir(dir)(use(dir)))

  private def failureFiles(dir: Path): List[java.io.File] =
    Option(dir.toFile.listFiles()).fold(List.empty[java.io.File])(_.filter(_.getName.endsWith(".json")).toList)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PropertyExecutorSpec")(
    passSuite,
    failSuite,
    generatorSuite,
    autoResolutionSuite,
    setupErrorVsFalsificationSuite,
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
      // A genuinely unregistered custom type (not int/string) — those auto-resolve via the
      // built-in HasGen now, so this must use a type nothing has registered a HasGen for.
      final case class Unresolvable(raw: String)
      val unresolvableExtractor: TypedExtractor[Unresolvable] =
        TypedExtractor.make[Unresolvable]("(.+)") { (_, groups, idx) =>
          groups.lift(idx).map(s => (Unresolvable(s), idx + 1)).toRight("expected a value")
        }

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
          Given("x is " / unresolvableExtractor)((u: Unresolvable) => ZIO.unit)
          Then("ok")(ZIO.unit)
      ).map { results =>
        val sc = results.head.scenarioResults.head
        // setupError should be set because no HasGen resolved for the column
        assertTrue(sc.setupError.isDefined)
      }
    }
  )

  // ── Automatic type-based resolution (#99) ───────────────────────────────────

  private val autoResolutionSuite = suite("Automatic type-based resolution")(
    test("a built-in-typed column resolves automatically with no columnGenLookup entry") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Auto resolve built-in
            |  Scenario Outline: auto int
            |    Given value <amount>
            |    Then ok
            |    @property(samples=10, replay=false)
            |    Examples:
            |      | amount |
            |""".stripMargin,
          "auto-builtin.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map(results => assertTrue(results.head.scenarioResults.head.isPassed))
    },
    test("a custom registered type resolves automatically via HasGen.registerType, with no columnGenLookup entry") {
      // toString must round-trip through the extractor's pattern, since sampled values are
      // substituted into step text via toString (see PropertyExecutor.sampleGenToString) —
      // same constraint the Title example in example/SimpleSpec.scala relies on.
      final case class Score(value: Int) {
        override def toString: String = value.toString
      }
      val scoreExtractor: TypedExtractor[Score] =
        TypedExtractor.make[Score]("(-?\\d+)") { (_, groups, idx) =>
          groups.lift(idx).flatMap(_.toIntOption).map(n => (Score(n), idx + 1)).toRight("expected an int")
        }
      given HasGen[Score] with
        def gen            = Gen.int(0, 100).map(Score.apply)
        override def label = "HasGen[Score]"
      HasGen.registerType(HasGen[Score])

      val steps = new ZIOSteps[Any, S]:
        Given("score " / scoreExtractor)((s: Score) => ZIO.unit)
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Auto resolve custom type
            |  Scenario Outline: auto score
            |    Given score <s>
            |    Then ok
            |    @property(samples=10, replay=false)
            |    Examples:
            |      | s |
            |""".stripMargin,
          "auto-custom.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map(results => assertTrue(results.head.scenarioResults.head.isPassed))
    },
    test("columnGenLookup's explicit entry still wins over automatic resolution") {
      val captured = new java.util.concurrent.atomic.AtomicReference[List[Int]](Nil)
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int) { (n: Int) =>
          ZIO.succeed(captured.updateAndGet(n :: _)).unit
        }
        Then("ok")(ZIO.unit)

      val narrowGen = new HasGen[Int]:
        def gen            = Gen.int(1, 5)
        override def label = "HasGen[Int] (narrow override)"
      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "amount" => Some(narrowGen)
          case _        => None

      GherkinParser
        .parseFeature(
          """Feature: Override wins
            |  Scenario Outline: override wins
            |    Given value <amount>
            |    Then ok
            |    @property(samples=20, replay=false)
            |    Examples:
            |      | amount |
            |""".stripMargin,
          "override-wins.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          val sc     = results.head.scenarioResults.head
          val values = captured.get()
          assertTrue(sc.isPassed, values.nonEmpty, values.forall(v => v >= 1 && v <= 5))
        }
    },
    test("a named header override still wins over automatic resolution") {
      val captured = new java.util.concurrent.atomic.AtomicReference[List[Int]](Nil)
      HasGen.named("tinyInts")(Gen.int(1, 3))

      val steps = new ZIOSteps[Any, S]:
        Given("value " / int) { (n: Int) =>
          ZIO.succeed(captured.updateAndGet(n :: _)).unit
        }
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Named override wins
            |  Scenario Outline: named override wins
            |    Given value <amount>
            |    Then ok
            |    @property(samples=20, replay=false)
            |    Examples:
            |      | amount: tinyInts |
            |""".stripMargin,
          "named-override-wins.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map { results =>
          val sc     = results.head.scenarioResults.head
          val values = captured.get()
          assertTrue(sc.isPassed, values.nonEmpty, values.forall(v => v >= 1 && v <= 3))
        }
    },
    test("the same column inferred as two different types across steps produces a setup error naming both types") {
      val steps = new ZIOSteps[Any, S]:
        Given("amount is " / int)((n: Int) => ZIO.unit)
        Given("moved " / string)((s: String) => ZIO.unit)
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Ambiguous column type
            |  Scenario Outline: ambiguous
            |    Given amount is <amount>
            |    Given moved <amount>
            |    Then ok
            |    @property(samples=5, replay=false)
            |    Examples:
            |      | amount |
            |""".stripMargin,
          "ambiguous.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map { results =>
          val sc      = results.head.scenarioResults.head
          val message = sc.error.map(_.getMessage).getOrElse("")
          assertTrue(
            sc.setupError.isDefined,
            message.contains("amount"),
            message.contains("Int"),
            message.contains("String")
          )
        }
    },
    test("a typo'd named-generator header override produces a setup error, not a silent fallback") {
      // Regression: `explicit(col)` used to do
      // `scenario.columnGens.get(col).flatMap(HasGen.resolve).orElse(lookup.byColumn(col))` —
      // an unregistered name fell through to columnGenLookup/automatic resolution instead of
      // erroring, so a typo in `| n: smallInts |` silently used a *different* generator with
      // no indication the override was ignored.
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      // A lookup that WOULD resolve "n" if the named override were (wrongly) skipped —
      // proves the typo doesn't silently fall through to this path either.
      val lookup = new ColumnGenLookup:
        def byColumn(col: String): Option[HasGen[?]] = col match
          case "n" => Some(HasGen[Int])
          case _   => None

      GherkinParser
        .parseFeature(
          """Feature: Typo'd named override
            |  Scenario Outline: typo
            |    Given value <n>
            |    Then ok
            |    @property(samples=5, replay=false)
            |    Examples:
            |      | n: definitelyNotRegistered |
            |""".stripMargin,
          "typo-named-gen.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = lookup)
        }
        .map { results =>
          val sc      = results.head.scenarioResults.head
          val message = sc.error.map(_.getMessage).getOrElse("")
          assertTrue(
            sc.setupError.isDefined,
            message.contains("n"),
            message.contains("definitelyNotRegistered")
          )
        }
    }
  )

  // ── Setup errors and pending steps must not be misreported as falsifications ────

  private val setupErrorVsFalsificationSuite = suite(
    "Setup errors and pending steps are distinguished from genuine falsifications"
  )(
    test("a beforeScenario hook failure surfaces as a setup error, not a [counterexample]") {
      // Regression: runOneSample used to collapse every non-passed ScenarioResult — including
      // one whose `setupError` is set because `beforeScenarioHook` failed — into "this sample
      // falsified the property", persisting it to PropertyFailureStore and rendering it as
      // "[counterexample] n=... (HasGen[Int])". A broken environment hook isn't evidence the
      // property itself is false.
      val steps = new ZIOSteps[Any, S]:
        beforeScenario(_ => ZIO.die(new RuntimeException("environment is down")))
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(ZIO.unit)

      GherkinParser
        .parseFeature(
          """Feature: Broken setup
            |  Scenario Outline: setup fails
            |    Given value <n>
            |    Then ok
            |    @property(samples=5, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "setup-fails.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map { results =>
          val sc = results.head.scenarioResults.head
          // beforeScenario's ScenarioHook is URIO — it can only fail via a defect (`die`), which
          // ends up in setupError's Cause as a Die, not a Fail. `ScenarioResult.error`'s
          // `failureOption` only unwraps typed Fail causes, so check the Cause directly here.
          val causeText = sc.setupError.map(_.prettyPrint).getOrElse("")
          assertTrue(
            sc.setupError.isDefined,
            causeText.contains("environment is down"),
            !causeText.contains("counterexample"),
            !causeText.contains("Falsified")
          )
        }
    },
    test("a pending step surfaces as pending, not a [counterexample], and is not persisted to the failure store") {
      val steps = new ZIOSteps[Any, S]:
        Given("value " / int)((n: Int) => ZIO.unit)
        Then("ok")(pending("not implemented yet"))

      GherkinParser
        .parseFeature(
          """Feature: Pending step
            |  Scenario Outline: not implemented
            |    Given value <n>
            |    Then ok
            |    @property(samples=5, replay=false)
            |    Examples:
            |      | n |
            |""".stripMargin,
          "pending-step.feature"
        )
        .flatMap { f =>
          FeatureExecutor.executeFeatures[Any, S](List(f), steps.getSteps, steps, genLookup = ColumnGenLookup.empty)
        }
        .map { results =>
          val sc = results.head.scenarioResults.head
          assertTrue(
            sc.hasPending,
            sc.setupError.isEmpty,
            !sc.stepResults.exists(_.step.pattern.contains("[counterexample]"))
          )
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

      withIsolatedStore { dir =>
        for {
          // Run 1: always fails → failure file should be written.
          firstResults   <- run()
          firstScenario   = firstResults.head.scenarioResults.head
          existsAfterRun1 = failureFiles(dir).nonEmpty

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
          existsAfterRun3     = failureFiles(dir).nonEmpty
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
      }
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

      withIsolatedStore { dir =>
        for {
          // Run 1: fails, writes a failure record keyed to this exact step body.
          f1 <- GherkinParser.parseFeature(failingFeature, "stale-replay.feature")
          _ <-
            FeatureExecutor.executeFeatures[Any, S](List(f1), failingSteps.getSteps, failingSteps, genLookup = lookup)
          recordedAfterRun1 = failureFiles(dir).nonEmpty

          // Run 2: edited step body → bodyHash no longer matches. The stale record must be
          // discarded (full fresh batch runs), not replayed as a single stored sample.
          f2 <- GherkinParser.parseFeature(editedPassingFeature, "stale-replay.feature")
          _ <- FeatureExecutor.executeFeatures[Any, S](
                 List(f2),
                 passingSteps().getSteps,
                 passingSteps(),
                 genLookup = lookup
               )
          // The discard path actively clears the stale record. If the read had instead been
          // silently swallowed to None, run 1's file would still be here — so this also fails
          // a "4 fresh runs for the wrong reason" false green.
          existsAfterRun2 = failureFiles(dir).nonEmpty
        } yield assertTrue(
          recordedAfterRun1,
          !existsAfterRun2,
          // All 4 fresh samples ran — not a single-sample replay of the stale record.
          freshRuns.get() == 4
        )
      }
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
      // A genuinely unregistered custom type — int/string now auto-resolve via the built-in
      // HasGen, so this must use a type nothing has registered a HasGen for.
      final case class Unresolvable(raw: String)
      val unresolvableExtractor: TypedExtractor[Unresolvable] =
        TypedExtractor.make[Unresolvable]("(.+)") { (_, groups, idx) =>
          groups.lift(idx).map(s => (Unresolvable(s), idx + 1)).toRight("expected a value")
        }
      val steps = new ZIOSteps[Any, S]:
        Given("value " / unresolvableExtractor)((u: Unresolvable) => ZIO.unit)
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
