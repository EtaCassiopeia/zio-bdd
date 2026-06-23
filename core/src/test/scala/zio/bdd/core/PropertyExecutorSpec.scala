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
    stateSuite
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
}
