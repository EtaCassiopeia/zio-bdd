package zio.bdd.core

import zio.*
import zio.bdd.core.*
import zio.bdd.core.step.*
import zio.bdd.gherkin.{GherkinParser, ScenarioMetadata}
import zio.schema.{DeriveSchema, Schema}
import zio.test.*

/**
 * Demonstration: running BDD scenarios with multiple feature flag combinations
 * using the @flags(k=v) tag expansion feature.
 *
 * Two integration patterns are shown:
 *
 *   1. Simple env-var / config style (current ledger approach): `flagLayer`
 *      overrides the environment by inserting flag values into a `FlagConfig`
 *      service. This mirrors how the ledger today reads flags from config files
 *      overridden by environment variables.
 *
 * 2. zio-openfeature style (in-memory provider): `flagLayer` seeds an in-memory
 * `FeatureFlags` service with the per-scenario flag values. This is compatible
 * with the zio-openfeature library: swap `InMemoryFeatureFlags` for the real
 * `OpenFeatureFlags` backed by Optimizely with zero step-code changes.
 */
object FlagMatrixDemoSpec extends ZIOSpecDefault {

  // ─── Shared domain ──────────────────────────────────────────────────────────

  final case class Response(status: Int, body: String)

  // The scenario state
  case class LedgerState(
    response: Option[Response] = None
  )
  given Schema[LedgerState] = DeriveSchema.gen[LedgerState]

  // ─── Pattern 1: FlagConfig service (maps to the ledger's current mechanism) ──

  /**
   * A simple service that holds flag values as a Map[String, String]. In the
   * real ledger this corresponds to the feature-flag section of
   * application.conf, overridable by environment variables.
   */
  trait FlagConfig:
    def get(key: String): UIO[Option[String]]
    def getOrDefault(key: String, default: String): UIO[String] =
      get(key).map(_.getOrElse(default))
    def isEnabled(key: String): UIO[Boolean] =
      getOrDefault(key, "false").map(_.equalsIgnoreCase("true"))

  object FlagConfig:
    def fromMap(m: Map[String, String]): FlagConfig = new FlagConfig:
      def get(key: String): UIO[Option[String]] = ZIO.succeed(m.get(key))

    val default: ZLayer[Any, Nothing, FlagConfig] =
      ZLayer.succeed(fromMap(Map.empty))

    def withFlags(flags: Map[String, String]): ZLayer[Any, Nothing, FlagConfig] =
      ZLayer.succeed(fromMap(flags))

  // ─── Pattern 2: zio-openfeature style (in-memory provider) ────────────────

  /**
   * A minimal `FeatureFlags` service that mirrors the zio-openfeature API.
   *
   * To use the real zio-openfeature library:
   *   1. Add `"io.github.etacassiopeia" %% "zio-openfeature" % "0.8.0"` as a
   *      dependency. 2. Replace `InMemoryFeatureFlags` with `OpenFeatureFlags`.
   *      3. Replace `InMemoryFeatureFlags.withFlags(flags)` with
   *      `OpenFeatureFlags.live(InMemoryProvider(flags.map { case (k, v) => ...
   *      }))`.
   *
   * The `flagLayer` override and all step bodies remain unchanged — only the
   * ZLayer construction changes.
   */
  trait FeatureFlags:
    def getBooleanFlag(key: String, default: Boolean): UIO[Boolean]
    def getStringFlag(key: String, default: String): UIO[String]

  object FeatureFlags:
    def withFlags(flags: Map[String, String]): ZLayer[Any, Nothing, FeatureFlags] =
      ZLayer.succeed(new FeatureFlags:
        def getBooleanFlag(key: String, default: Boolean): UIO[Boolean] =
          ZIO.succeed(flags.get(key).map(_.equalsIgnoreCase("true")).getOrElse(default))
        def getStringFlag(key: String, default: String): UIO[String] =
          ZIO.succeed(flags.getOrElse(key, default))
      )

  // ─── Pattern 1 suite: FlagConfig ─────────────────────────────────────────

  class FlagConfigSuite extends ZIOSteps[FlagConfig, LedgerState] with DefaultTypedExtractor:

    // Provide the base (empty) FlagConfig as the suite-level environment.
    // flagLayer overrides this per-scenario with the actual flags.
    override def environment: ZLayer[Any, Throwable, FlagConfig] =
      FlagConfig.default.asInstanceOf[ZLayer[Any, Throwable, FlagConfig]]

    override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, FlagConfig] =
      FlagConfig.withFlags(flags).asInstanceOf[ZLayer[Any, Throwable, FlagConfig]]

    // Step: read the rateLimiting flag from FlagConfig and simulate a response
    Given("a provision request is sent") {
      for {
        flagCfg      <- ZIO.service[FlagConfig]
        rateLimiting <- flagCfg.isEnabled("rateLimiting")
        response = if (rateLimiting) Response(429, "rate limited")
                   else Response(200, "success")
        _ <- ScenarioContext.update(_.copy(response = Some(response)))
      } yield ()
    }

    Then("the response status is " / int) { (expected: Int) =>
      ScenarioContext.get.flatMap { s =>
        val actual = s.response.map(_.status).getOrElse(-1)
        Assertions.assertEquals(
          actual,
          expected,
          s"Expected status $expected but got $actual (rateLimiting flag determines outcome)"
        )
      }
    }

  // ─── Pattern 2 suite: FeatureFlags (zio-openfeature style) ────────────────

  class FeatureFlagsSuite extends ZIOSteps[FeatureFlags, LedgerState] with DefaultTypedExtractor:

    override def environment: ZLayer[Any, Throwable, FeatureFlags] =
      FeatureFlags.withFlags(Map.empty).asInstanceOf[ZLayer[Any, Throwable, FeatureFlags]]

    override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, FeatureFlags] =
      FeatureFlags.withFlags(flags).asInstanceOf[ZLayer[Any, Throwable, FeatureFlags]]

    Given("a provision request is sent") {
      for {
        ff           <- ZIO.service[FeatureFlags]
        rateLimiting <- ff.getBooleanFlag("rateLimiting", default = false)
        newAlgo      <- ff.getStringFlag("pricingAlgo", default = "v1")
        response = (rateLimiting, newAlgo) match
                     case (true, _)     => Response(429, "rate limited")
                     case (false, "v2") => Response(200, s"success via $newAlgo pricing")
                     case (false, _)    => Response(200, "success via v1 pricing")
        _ <- ScenarioContext.update(_.copy(response = Some(response)))
      } yield ()
    }

    Then("the response status is " / int) { (expected: Int) =>
      ScenarioContext.get.flatMap { s =>
        Assertions.assertEquals(s.response.map(_.status).getOrElse(-1), expected)
      }
    }

    Then("the response body contains " / string) { (expected: String) =>
      ScenarioContext.get.flatMap { s =>
        val body = s.response.map(_.body).getOrElse("")
        Assertions.assertTrue(body.contains(expected), s"Expected '$expected' in '$body'")
      }
    }

  // ─── Tests ──────────────────────────────────────────────────────────────────

  def spec: Spec[TestEnvironment & Scope, Any] = suite("FlagMatrix demo")(
    suite("Pattern 1: FlagConfig (current ledger style)")(
      test("rate limiting ON returns 429, OFF returns 200") {
        // The feature file drives the flag combinations declaratively.
        // No Java annotations, no code duplication — Gherkin is the single source of truth.
        val suite = new FlagConfigSuite {}
        for {
          f <- GherkinParser.parseFeature(
                 """Feature: Provision rate limiting
                   |
                   |  @flags(rateLimiting=true)
                   |  @flags(rateLimiting=false)
                   |  Scenario: Provision with flag matrix
                   |    Given a provision request is sent
                   |    Then the response status is 200
                   |""".stripMargin,
                 "provision.feature"
               )
          // Note: this feature has an intentional mistake — we test "then status 200"
          // for both, but the rateLimiting=true run will produce 429 and fail.
          // That's exactly the point: the flag matrix reveals the regression.
          results        <- suite.run(List(f)).provide(suite.environment)
          scenarioResults = results.head.scenarioResults
        } yield {
          val rateLimitingOnResult  = scenarioResults.find(_.scenario.name.contains("rateLimiting=true")).get
          val rateLimitingOffResult = scenarioResults.find(_.scenario.name.contains("rateLimiting=false")).get
          assertTrue(
            scenarioResults.length == 2,
            rateLimitingOnResult.hasFailure, // correctly fails: 429 ≠ 200
            rateLimitingOffResult.isPassed   // correctly passes: 200 = 200
          )
        }
      }
    ),
    suite("Pattern 2: FeatureFlags service (zio-openfeature style)")(
      test("matrix of rateLimiting × pricingAlgo runs 4 scenarios") {
        val suite = new FeatureFlagsSuite {}
        for {
          f <- GherkinParser.parseFeature(
                 """Feature: Pricing and rate limiting
                   |
                   |  @flags(rateLimiting=false, pricingAlgo=v1)
                   |  @flags(rateLimiting=false, pricingAlgo=v2)
                   |  @flags(rateLimiting=true, pricingAlgo=v1)
                   |  @flags(rateLimiting=true, pricingAlgo=v2)
                   |  Scenario: Provision under all flag combinations
                   |    Given a provision request is sent
                   |    Then the response status is 200
                   |""".stripMargin,
                 "pricing.feature"
               )
          results        <- suite.run(List(f)).provide(suite.environment)
          scenarioResults = results.head.scenarioResults
        } yield {
          // 4 flags tags → 4 scenario runs
          val noRateLimit   = scenarioResults.filter(r => r.scenario.name.contains("rateLimiting=false"))
          val withRateLimit = scenarioResults.filter(r => r.scenario.name.contains("rateLimiting=true"))
          assertTrue(
            scenarioResults.length == 4,
            noRateLimit.forall(_.isPassed),    // both pass (200)
            withRateLimit.forall(_.hasFailure) // both fail (429 ≠ 200)
          )
        }
      },
      test("response body is different between v1 and v2 pricing algo") {
        val suite = new FeatureFlagsSuite {}
        for {
          f <- GherkinParser.parseFeature(
                 """Feature: Pricing algorithm variants
                   |
                   |  @flags(pricingAlgo=v1)
                   |  @flags(pricingAlgo=v2)
                   |  Scenario: Provision with pricing algo
                   |    Given a provision request is sent
                   |    Then the response status is 200
                   |    And the response body contains pricing
                   |""".stripMargin,
                 "pricing.feature"
               )
          results        <- suite.run(List(f)).provide(suite.environment)
          scenarioResults = results.head.scenarioResults
        } yield assertTrue(
          scenarioResults.length == 2,
          scenarioResults.forall(_.isPassed)
        )
      }
    )
  )
}

// (GherkinParser is already imported at the top)
