package zio.bdd

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.*
import zio.bdd.core.report.Reporter

/**
 * Tests for the sbt test-interface entry point:
 *   - ZIOBDDFingerprint annotationName and isModule
 *   - BDDTestConfig tag/name/parallelism parsing
 *   - CLI wins over annotation for overlapping fields
 *   - CompositeReporter assembled from multiple --reporter flags
 *   - filterFeatures: feature-level tag inheritance, all-ignored feature
 *     propagation
 */
object ZIOBDDFrameworkSpec extends ZIOSpecDefault {

  import zio.bdd.gherkin.{Feature, Scenario}

  // ── Fingerprint ──────────────────────────────────────────────────────────────

  private val fingerprintSuite = suite("ZIOBDDFingerprint")(
    test("annotationName is 'zio.bdd.core.Suite'") {
      val fp = new ZIOBDDFingerprint
      assertTrue(fp.annotationName() == "zio.bdd.core.Suite")
    },
    test("isModule is true") {
      val fp = new ZIOBDDFingerprint
      assertTrue(fp.isModule)
    }
  )

  // ── filterFeatures (static, unit-testable) ────────────────────────────────────

  private def mkFeature(name: String, tags: List[String], scenarios: List[(String, List[String])]): Feature = {
    val scs = scenarios.map { case (scName, scTags) =>
      Scenario(scName, scTags, Nil, Some("test.feature"), Some(1))
    }
    Feature(name, tags, scs, Some("test.feature"), Some(1))
  }

  private val filterSuite = suite("filterFeatures")(
    test("scenario matching includeTags is not ignored") {
      val feature   = mkFeature("F", Nil, List(("s1", List("smoke")), ("s2", List("regression"))))
      val config    = BDDTestConfig(includeTags = Set("smoke"))
      val result    = ZIOBDDTask.filterFeatures(List(feature), config)
      val scenarios = result.head.scenarios
      assertTrue(
        !scenarios(0).isIgnored,
        scenarios(1).isIgnored
      )
    },
    test("scenario with excludeTag is ignored") {
      val feature   = mkFeature("F", Nil, List(("s1", List("slow")), ("s2", List("smoke"))))
      val config    = BDDTestConfig(excludeTags = Set("slow"))
      val result    = ZIOBDDTask.filterFeatures(List(feature), config)
      val scenarios = result.head.scenarios
      assertTrue(
        scenarios(0).isIgnored,
        !scenarios(1).isIgnored
      )
    },
    test("feature-level @smoke tag makes all scenarios match include 'smoke'") {
      val feature   = mkFeature("F", List("smoke"), List(("s1", Nil), ("s2", Nil)))
      val config    = BDDTestConfig(includeTags = Set("smoke"))
      val result    = ZIOBDDTask.filterFeatures(List(feature), config)
      val scenarios = result.head.scenarios
      assertTrue(!scenarios(0).isIgnored, !scenarios(1).isIgnored)
    },
    test("feature whose all scenarios are ignored gets @ignore tag") {
      val feature = mkFeature("F", Nil, List(("s1", List("slow")), ("s2", List("slow"))))
      val config  = BDDTestConfig(excludeTags = Set("slow"))
      val result  = ZIOBDDTask.filterFeatures(List(feature), config)
      assertTrue(result.head.isIgnored)
    },
    test("feature with at least one non-ignored scenario is NOT marked @ignore") {
      val feature = mkFeature("F", Nil, List(("s1", List("smoke")), ("s2", List("slow"))))
      val config  = BDDTestConfig(includeTags = Set("smoke"))
      val result  = ZIOBDDTask.filterFeatures(List(feature), config)
      assertTrue(!result.head.isIgnored)
    },
    test("excludeTags takes precedence over includeTags for same tag") {
      val feature = mkFeature("F", Nil, List(("s1", List("smoke", "wip"))))
      val config  = BDDTestConfig(includeTags = Set("smoke"), excludeTags = Set("wip"))
      val result  = ZIOBDDTask.filterFeatures(List(feature), config)
      assertTrue(result.head.scenarios.head.isIgnored)
    },
    test("scenarioNameFilter with glob '*' matches by pattern") {
      val feature = mkFeature(
        "F",
        Nil,
        List(
          ("Happy path works", Nil),
          ("Sad path fails", Nil),
          ("Happy-day scenario", Nil)
        )
      )
      val config    = BDDTestConfig(scenarioNameFilter = Some("Happy*"))
      val result    = ZIOBDDTask.filterFeatures(List(feature), config)
      val scenarios = result.head.scenarios
      assertTrue(
        !scenarios(0).isIgnored, // "Happy path works" matches
        scenarios(1).isIgnored,  // "Sad path fails" does not
        !scenarios(2).isIgnored  // "Happy-day scenario" matches
      )
    },
    test("scenarioNameFilter is case-insensitive") {
      val feature = mkFeature("F", Nil, List(("HAPPY path", Nil), ("sad path", Nil)))
      val config  = BDDTestConfig(scenarioNameFilter = Some("happy*"))
      val result  = ZIOBDDTask.filterFeatures(List(feature), config)
      assertTrue(!result.head.scenarios(0).isIgnored, result.head.scenarios(1).isIgnored)
    },
    test("no filters applied: all scenarios remain non-ignored") {
      val feature = mkFeature("F", Nil, List(("s1", Nil), ("s2", List("smoke"))))
      val config  = BDDTestConfig()
      val result  = ZIOBDDTask.filterFeatures(List(feature), config)
      assertTrue(result.head.scenarios.forall(!_.isIgnored))
    },
    test("empty feature list returns empty list") {
      val result = ZIOBDDTask.filterFeatures(Nil, BDDTestConfig(includeTags = Set("smoke")))
      assertTrue(result.isEmpty)
    },
    test("feature with no scenarios returns unchanged") {
      val feature = mkFeature("F", Nil, Nil)
      val result  = ZIOBDDTask.filterFeatures(List(feature), BDDTestConfig(includeTags = Set("smoke")))
      assertTrue(result.head.scenarios.isEmpty, !result.head.isIgnored)
    }
  )

  // ── CompositeReporter ──────────────────────────────────────────────────────

  private val compositeReporterSuite = suite("CompositeReporter")(
    test("CompositeReporter delegates to all contained reporters") {
      val count1 = new java.util.concurrent.atomic.AtomicInteger(0)
      val count2 = new java.util.concurrent.atomic.AtomicInteger(0)

      val r1: Reporter = results => ZIO.succeed(count1.incrementAndGet()).unit
      val r2: Reporter = results => ZIO.succeed(count2.incrementAndGet()).unit

      val composite = CompositeReporter(List(r1, r2))
      for {
        _ <- composite.report(Nil).provide(LogCollector.live())
      } yield assertTrue(count1.get() == 1, count2.get() == 1)
    },
    test("CompositeReporter passes the same results to all reporters") {
      val captured1 = new java.util.concurrent.CopyOnWriteArrayList[Int]()
      val captured2 = new java.util.concurrent.CopyOnWriteArrayList[Int]()

      val feature = Feature("F", Nil, Nil)
      val fr      = FeatureResult(feature, Nil)

      val r1: Reporter = results => ZIO.succeed(captured1.add(results.length)).unit
      val r2: Reporter = results => ZIO.succeed(captured2.add(results.length)).unit

      val composite = CompositeReporter(List(r1, r2))
      for {
        _ <- composite.report(List(fr)).provide(LogCollector.live())
      } yield {
        import scala.jdk.CollectionConverters.*
        assertTrue(
          captured1.asScala.toList == List(1),
          captured2.asScala.toList == List(1)
        )
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("ZIOBDDFrameworkSpec")(
    fingerprintSuite,
    filterSuite,
    compositeReporterSuite
  )
}
