package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for FeatureContext — the per-feature typed staging store.
 */
object FeatureContextSpec extends ZIOSpecDefault {

  final case class FeatureToken(value: String)

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  // ── Pure put/get operations ──────────────────────────────────────────────

  private val putGet = suite("FeatureContext.put / FeatureContext.get")(
    test("put then get returns the stored value") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("hello"))
        result <- FeatureContext.get[FeatureToken]
      } yield assertTrue(result == FeatureToken("hello"))
    },
    test("get on missing type returns FeatureContextError.NotFound") {
      for {
        _      <- FeatureContext.reset
        result <- FeatureContext.get[FeatureToken].either
      } yield assert(result)(isLeft(isSubtype[FeatureContextError.NotFound](anything)))
    },
    test("second put overwrites the first") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("first"))
        _      <- FeatureContext.put(FeatureToken("second"))
        result <- FeatureContext.get[FeatureToken]
      } yield assertTrue(result == FeatureToken("second"))
    }
  )

  // ── getOrElse ────────────────────────────────────────────────────────────

  private val getOrElseOps = suite("FeatureContext.getOrElse")(
    test("returns default when not stored") {
      for {
        _      <- FeatureContext.reset
        result <- FeatureContext.getOrElse(FeatureToken("default"))
      } yield assertTrue(result == FeatureToken("default"))
    },
    test("returns stored value when present") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("stored"))
        result <- FeatureContext.getOrElse(FeatureToken("default"))
      } yield assertTrue(result == FeatureToken("stored"))
    }
  )

  // ── getOption ────────────────────────────────────────────────────────────

  private val getOptionOps = suite("FeatureContext.getOption")(
    test("returns None when not stored") {
      for {
        _      <- FeatureContext.reset
        result <- FeatureContext.getOption[FeatureToken]
      } yield assertTrue(result.isEmpty)
    },
    test("returns Some when stored") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("present"))
        result <- FeatureContext.getOption[FeatureToken]
      } yield assertTrue(result.contains(FeatureToken("present")))
    }
  )

  // ── modify ───────────────────────────────────────────────────────────────

  private val modifyOps = suite("FeatureContext.modify")(
    test("modify updates the stored value") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("lower"))
        _      <- FeatureContext.modify[FeatureToken](t => t.copy(value = t.value.toUpperCase))
        result <- FeatureContext.get[FeatureToken]
      } yield assertTrue(result == FeatureToken("LOWER"))
    },
    test("modify is a no-op when value is not stored") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.modify[FeatureToken](t => t.copy(value = "should not appear"))
        result <- FeatureContext.getOption[FeatureToken]
      } yield assertTrue(result.isEmpty)
    }
  )

  // ── set / remove ─────────────────────────────────────────────────────────

  private val setRemoveOps = suite("FeatureContext.set / remove")(
    test("set stores a value") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.set(FeatureToken("via-set"))
        result <- FeatureContext.get[FeatureToken]
      } yield assertTrue(result == FeatureToken("via-set"))
    },
    test("remove removes the stored value") {
      for {
        _      <- FeatureContext.reset
        _      <- FeatureContext.put(FeatureToken("to-remove"))
        _      <- FeatureContext.remove[FeatureToken]
        result <- FeatureContext.getOption[FeatureToken]
      } yield assertTrue(result.isEmpty)
    }
  )

  // ── Values persist across scenarios within same feature ───────────────────
  //
  // The first scenario stores a FeatureToken; the second scenario reads it.
  // If FeatureContext persists across scenarios, the second scenario will see it.

  private val persistsAcrossScenarios = suite("FeatureContext persists across scenarios within a feature")(
    test("value stored in first scenario is visible in second scenario") {
      var secondScenarioSawValue: Option[FeatureToken] = None

      val stepsImpl = new ZIOSteps[Any, S] {
        Given("store a feature token") {
          FeatureContext.put(FeatureToken("feature-shared"))
        }
        Then("the feature token is visible") {
          FeatureContext.getOption[FeatureToken].flatMap { opt =>
            ZIO.succeed { secondScenarioSawValue = opt }
          }
        }
      }

      val featureText =
        """Feature: FeatureContext persistence
          |  Scenario: First scenario stores a token
          |    Given store a feature token
          |  Scenario: Second scenario reads the token
          |    Then the feature token is visible
          |""".stripMargin

      for {
        feature <- GherkinParser.parseFeature(featureText, "feature-context.feature")
        _       <- FeatureExecutor.executeFeatures[Any, S](List(feature), stepsImpl.getSteps, stepsImpl)
      } yield assertTrue(secondScenarioSawValue.contains(FeatureToken("feature-shared")))
    }
  )

  // ── Values are reset between features ────────────────────────────────────
  //
  // FeatureExecutor calls FeatureContext.reset at the start of each feature.
  // We run two sequential features: the first stores a value, the second checks
  // that the value is no longer present.

  private val resetBetweenFeatures = suite("FeatureContext is reset between features")(
    test("value from first feature is absent in second feature") {
      var secondFeatureSawValue: Option[FeatureToken] = None

      val stepsImpl = new ZIOSteps[Any, S] {
        Given("store a feature token in feature context") {
          FeatureContext.put(FeatureToken("from-first-feature"))
        }
        Then("feature context is empty") {
          FeatureContext.getOption[FeatureToken].flatMap { opt =>
            ZIO.succeed { secondFeatureSawValue = opt }
          }
        }
        Given("a no-op step") {
          ZIO.unit
        }
      }

      val feature1Text =
        """Feature: First feature stores context
          |  Scenario: Store token
          |    Given store a feature token in feature context
          |""".stripMargin

      val feature2Text =
        """Feature: Second feature checks context is clean
          |  Scenario: Check context empty
          |    Then feature context is empty
          |""".stripMargin

      for {
        f1 <- GherkinParser.parseFeature(feature1Text, "f1.feature")
        f2 <- GherkinParser.parseFeature(feature2Text, "f2.feature")
        _  <- FeatureExecutor.executeFeatures[Any, S](List(f1, f2), stepsImpl.getSteps, stepsImpl)
      } yield assertTrue(secondFeatureSawValue.isEmpty)
    }
  )

  // ── Concurrent writes from parallel scenarios accumulate ──────────────────
  //
  // Under scenarioParallelism > 1 the scenarios run on true parallel fibers that
  // share one feature store. Each scenario increments the same counter; with the
  // old FiberRef (last-writer-wins fork/join) the siblings clobber each other and
  // the counter ends at 1. A synchronized cell must accumulate all N updates.

  final case class Counter(n: Int)

  private val concurrentAccumulation =
    suite("FeatureContext accumulates concurrent writes across parallel scenarios")(
      test("N parallel scenarios each incrementing the shared counter reach N (no lost updates)") {
        val n        = 8
        val observed = new java.util.concurrent.atomic.AtomicInteger(-1)

        val stepsImpl = new ZIOSteps[Any, S] {
          beforeFeature(FeatureContext.put(Counter(0)))
          afterFeature(
            FeatureContext.getOption[Counter].flatMap(o => ZIO.succeed(observed.set(o.map(_.n).getOrElse(-1))))
          )
          Given("increment the feature counter") {
            FeatureContext.modify[Counter](c => Counter(c.n + 1))
          }
        }

        val scenarios =
          (1 to n).map(i => s"  Scenario: increment $i\n    Given increment the feature counter\n").mkString
        val featureText = s"Feature: concurrent counter\n$scenarios"

        for {
          feature <- GherkinParser.parseFeature(featureText, "fc-concurrent.feature")
          _ <- FeatureExecutor.executeFeatures[Any, S](
                 List(feature),
                 stepsImpl.getSteps,
                 stepsImpl,
                 scenarioParallelism = n
               )
        } yield assertTrue(observed.get() == n)
      } @@ TestAspect.withLiveClock
    )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("FeatureContext")(
    putGet,
    getOrElseOps,
    getOptionOps,
    modifyOps,
    setRemoveOps,
    persistsAcrossScenarios,
    resetBetweenFeatures,
    concurrentAccumulation
    // Run sequentially: the direct-API tests operate on the shared out-of-feature
    // default store (production always runs inside a per-feature scope via
    // freshScope, so it never shares this cell — only these tests do).
  ) @@ TestAspect.sequential
}
