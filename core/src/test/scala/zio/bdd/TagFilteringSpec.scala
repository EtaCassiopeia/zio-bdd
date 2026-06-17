package zio.bdd

import zio.test.*
import zio.bdd.gherkin.{Feature, Scenario, Step, StepType}

/**
 * Unit tests for ZIOBDDTask.filterFeatures tag filtering behaviour.
 *
 * Key invariants exercised:
 *   - Scenario-level include/exclude tags work as before
 *   - Feature-level tags are inherited by scenarios for filtering
 *   - Exclude takes precedence over include
 *   - When all scenarios in a feature are ignored the feature itself is ignored
 *   - Empty include/exclude sets leave everything intact
 *   - scenarioNameFilter is independent of tag inheritance
 */
object TagFilteringSpec extends ZIOSpecDefault {

  private def mkScenario(name: String, tags: String*): Scenario =
    Scenario(name, tags.toList, List(Step(StepType.GivenStep, "a step")), Some("test.feature"), Some(1))

  private def mkFeature(name: String, featureTags: List[String], scenarios: Scenario*): Feature =
    Feature(name, featureTags, scenarios.toList, Some("test.feature"), Some(1))

  private def filter(config: BDDTestConfig)(features: Feature*): List[Feature] =
    ZIOBDDTask.filterFeatures(features.toList, config)

  private def withInclude(tags: String*): BDDTestConfig =
    BDDTestConfig(includeTags = tags.toSet)

  private def withExclude(tags: String*): BDDTestConfig =
    BDDTestConfig(excludeTags = tags.toSet)

  // ── No-op baseline ─────────────────────────────────────────────────────────

  private val noopSuite = suite("no-op when no filters are set")(
    test("scenarios without any tags pass through unchanged") {
      val features = filter(BDDTestConfig())(
        mkFeature("F", Nil, mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.scenarios.forall(!_.isIgnored))
    },
    test("scenarios with tags pass through when no filter is configured") {
      val features = filter(BDDTestConfig())(
        mkFeature("F", List("smoke"), mkScenario("s1", "smoke"), mkScenario("s2"))
      )
      assertTrue(features.head.scenarios.forall(!_.isIgnored))
    }
  )

  // ── Include-tag filtering ──────────────────────────────────────────────────

  private val includeSuite = suite("include-tag filtering")(
    test("scenario with matching tag is kept") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", Nil, mkScenario("s1", "smoke"), mkScenario("s2"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    },
    test("feature-level tag is inherited: scenario runs when feature carries the include tag") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", List("smoke"), mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.scenarios.forall(!_.isIgnored))
    },
    test("feature-level tag inheritance: scenario tag also satisfies include") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", List("billing"), mkScenario("tagged", "smoke"), mkScenario("untagged"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    },
    test("scenario without any matching tag (scenario or feature) is ignored") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", List("billing"), mkScenario("s1"), mkScenario("s2", "regression"))
      )
      assertTrue(features.head.scenarios.forall(_.isIgnored))
    },
    test("when all scenarios are ignored the feature itself is marked ignored") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", Nil, mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.isIgnored)
    },
    test("feature is NOT marked ignored when at least one scenario survives") {
      val features = filter(withInclude("smoke"))(
        mkFeature("F", Nil, mkScenario("s1", "smoke"), mkScenario("s2"))
      )
      assertTrue(!features.head.isIgnored)
    }
  )

  // ── Exclude-tag filtering ──────────────────────────────────────────────────

  private val excludeSuite = suite("exclude-tag filtering")(
    test("scenario with excluded tag is ignored") {
      val features = filter(withExclude("slow"))(
        mkFeature("F", Nil, mkScenario("fast"), mkScenario("slow-test", "slow"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    },
    test("feature-level exclude tag ignores all scenarios") {
      val features = filter(withExclude("slow"))(
        mkFeature("F", List("slow"), mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.scenarios.forall(_.isIgnored))
    },
    test("feature-level exclude tag causes feature itself to be ignored") {
      val features = filter(withExclude("slow"))(
        mkFeature("F", List("slow"), mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.isIgnored)
    },
    test("scenario-level exclude tag ignores only that scenario") {
      val features = filter(withExclude("slow"))(
        mkFeature("F", Nil, mkScenario("fast"), mkScenario("slow-one", "slow"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    }
  )

  // ── Exclude takes precedence over include ──────────────────────────────────

  private val precedenceSuite = suite("exclude takes precedence over include")(
    test("excluded scenario is ignored even when it carries an include tag") {
      val config = BDDTestConfig(includeTags = Set("smoke"), excludeTags = Set("slow"))
      val features = filter(config)(
        mkFeature("F", Nil, mkScenario("both", "smoke", "slow"))
      )
      assertTrue(features.head.scenarios.head.isIgnored)
    },
    test("feature-level exclude overrides feature-level include") {
      val config = BDDTestConfig(includeTags = Set("smoke"), excludeTags = Set("slow"))
      val features = filter(config)(
        mkFeature("F", List("smoke", "slow"), mkScenario("s1"), mkScenario("s2"))
      )
      assertTrue(features.head.scenarios.forall(_.isIgnored))
    }
  )

  // ── Scenario-name filter ───────────────────────────────────────────────────

  private val nameSuite = suite("scenario-name filter")(
    test("exact name match keeps scenario") {
      val config = BDDTestConfig(scenarioNameFilter = Some("Login"))
      val features = filter(config)(
        mkFeature("F", Nil, mkScenario("Login"), mkScenario("Register"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    },
    test("glob wildcard filter works case-insensitively") {
      val config = BDDTestConfig(scenarioNameFilter = Some("Log*"))
      val features = filter(config)(
        mkFeature("F", Nil, mkScenario("Logout"), mkScenario("login flow"), mkScenario("Register"))
      )
      val results = features.head.scenarios
      assertTrue(!results(0).isIgnored, !results(1).isIgnored, results(2).isIgnored)
    },
    test("name filter is independent of tag inheritance") {
      val config = BDDTestConfig(includeTags = Set("smoke"), scenarioNameFilter = Some("Pay*"))
      val features = filter(config)(
        mkFeature("F", List("smoke"), mkScenario("Payment"), mkScenario("Refund"))
      )
      val results = features.head.scenarios
      // "Payment" matches name and inherits smoke tag → not ignored
      // "Refund" fails name filter → ignored even though it inherits smoke
      assertTrue(!results(0).isIgnored, results(1).isIgnored)
    }
  )

  // ── Multiple features ──────────────────────────────────────────────────────

  private val multiFeatureSuite = suite("multiple features")(
    test("each feature is filtered independently") {
      val config = BDDTestConfig(includeTags = Set("smoke"))
      val features = filter(config)(
        mkFeature("F1", List("smoke"), mkScenario("s1")),
        mkFeature("F2", Nil, mkScenario("s2"))
      )
      assertTrue(!features(0).scenarios.head.isIgnored, features(1).scenarios.head.isIgnored)
    },
    test("feature without matching scenarios is marked ignored, others are not") {
      val config = BDDTestConfig(includeTags = Set("smoke"))
      val features = filter(config)(
        mkFeature("F1", List("smoke"), mkScenario("s1")),
        mkFeature("F2", Nil, mkScenario("s2"))
      )
      assertTrue(!features(0).isIgnored, features(1).isIgnored)
    }
  )

  def spec: Spec[Any, Nothing] = suite("TagFiltering")(
    noopSuite,
    includeSuite,
    excludeSuite,
    precedenceSuite,
    nameSuite,
    multiFeatureSuite
  )
}
