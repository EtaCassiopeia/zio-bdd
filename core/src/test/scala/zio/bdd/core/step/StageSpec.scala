package zio.bdd.core.step

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.{Default, FeatureExecutor}
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for Stage — the per-scenario typed staging store.
 */
object StageSpec extends ZIOSpecDefault {

  // A simple domain type to stage
  final case class Payload(value: String)

  // ── Pure put/get operations ──────────────────────────────────────────────

  private val putGet = suite("Stage.put / Stage.get")(
    test("put then get returns the stored value") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("hello"))
        result <- Stage.get[Payload]
      } yield assertTrue(result == Payload("hello"))
    },
    test("get on missing type returns StagingError.NotFound") {
      for {
        _      <- Stage.reset
        result <- Stage.get[Payload].either
      } yield assert(result)(isLeft(isSubtype[StagingError.NotFound](anything)))
    },
    test("second put overwrites the first") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("first"))
        _      <- Stage.put(Payload("second"))
        result <- Stage.get[Payload]
      } yield assertTrue(result == Payload("second"))
    }
  )

  // ── getOrElse ────────────────────────────────────────────────────────────

  private val getOrElseOps = suite("Stage.getOrElse")(
    test("returns default when not staged") {
      for {
        _      <- Stage.reset
        result <- Stage.getOrElse(Payload("default"))
      } yield assertTrue(result == Payload("default"))
    },
    test("returns staged value when present") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("staged"))
        result <- Stage.getOrElse(Payload("default"))
      } yield assertTrue(result == Payload("staged"))
    }
  )

  // ── getOption ────────────────────────────────────────────────────────────

  private val getOptionOps = suite("Stage.getOption")(
    test("returns None when not staged") {
      for {
        _      <- Stage.reset
        result <- Stage.getOption[Payload]
      } yield assertTrue(result.isEmpty)
    },
    test("returns Some when staged") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("present"))
        result <- Stage.getOption[Payload]
      } yield assertTrue(result.contains(Payload("present")))
    }
  )

  // ── modify ───────────────────────────────────────────────────────────────

  private val modifyOps = suite("Stage.modify")(
    test("modify updates the staged value") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("original"))
        _      <- Stage.modify[Payload](p => p.copy(value = p.value.toUpperCase))
        result <- Stage.get[Payload]
      } yield assertTrue(result == Payload("ORIGINAL"))
    },
    test("modify is a no-op when value is not staged") {
      for {
        _      <- Stage.reset
        _      <- Stage.modify[Payload](p => p.copy(value = "should not appear"))
        result <- Stage.getOption[Payload]
      } yield assertTrue(result.isEmpty)
    }
  )

  // ── remove ───────────────────────────────────────────────────────────────

  private val removeOps = suite("Stage.remove")(
    test("remove removes the staged value") {
      for {
        _      <- Stage.reset
        _      <- Stage.put(Payload("to-remove"))
        _      <- Stage.remove[Payload]
        result <- Stage.getOption[Payload]
      } yield assertTrue(result.isEmpty)
    },
    test("remove on absent key is a no-op") {
      for {
        _ <- Stage.reset
        _ <- Stage.remove[Payload]
      } yield assertCompletes
    }
  )

  // ── Reset between scenarios ───────────────────────────────────────────────
  //
  // ScenarioExecutor calls Stage.reset at the start of each scenario.
  // We verify this by running two sequential scenarios: the first stages a
  // value, and the second checks that it is no longer present.

  private val resetBetweenScenarios = suite("Stage is reset between scenarios")(
    test("staged value from first scenario is absent in second scenario") {
      case class S(v: Int = 0)
      given Schema[S] = DeriveSchema.gen[S]

      // Track what the second scenario sees
      var secondScenarioSawValue: Option[Payload] = None

      val stepsImpl = new ZIOSteps[Any, S] {
        Given("stage a payload") {
          Stage.put(Payload("from-first-scenario"))
        }
        Then("stage is empty") {
          Stage.getOption[Payload].flatMap { opt =>
            ZIO.succeed { secondScenarioSawValue = opt }
          }
        }
      }

      val featureText =
        """Feature: Stage isolation
          |  Scenario: First scenario stages a value
          |    Given stage a payload
          |  Scenario: Second scenario checks stage is clean
          |    Then stage is empty
          |""".stripMargin

      for {
        feature <- GherkinParser.parseFeature(featureText, "stage.feature")
        _       <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
      } yield assertTrue(secondScenarioSawValue.isEmpty)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Stage")(
    putGet,
    getOrElseOps,
    getOptionOps,
    modifyOps,
    removeOps,
    resetBetweenScenarios
  )
}
