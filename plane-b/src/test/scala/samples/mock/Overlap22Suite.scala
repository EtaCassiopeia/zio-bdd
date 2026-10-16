package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

/** Which overlapping definition fired, and the args it captured. */
final case class OverlapState(matched: String = "", arg1: String = "", arg2: String = "")
object OverlapState:
  given Schema[OverlapState] = DeriveSchema.gen[OverlapState]

/**
 * 22 — overlapping step resolution (zio-bdd#186). Registers two definitions
 * that share the "order <x>" prefix; the greedy unquoted `{string}` fallback
 * makes the short one a full-match candidate for the long line too, so
 * resolution must pick the most-specific (most-literal) definition rather than
 * the fewest-extractor one. A pure step-matching test — no backend — so it runs
 * identically on every leg and asserts the #186 fix directly (a regression
 * would mis-bind or go Ambiguous).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/22-overlapping-steps.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Overlap22Suite extends ZIOSteps[Any, OverlapState]:

  When("order " / string) { (x: String) =>
    ScenarioContext.update(_.copy(matched = "short", arg1 = x))
  }

  When("order " / string / " rush " / string) { (x: String, y: String) =>
    ScenarioContext.update(_.copy(matched = "long", arg1 = x, arg2 = y))
  }

  Then("the matched step was " / string) { (m: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.matched, m, "which overlapping step bound"))
  }

  Then("the first captured arg was " / string) { (a: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.arg1, a, "first captured arg"))
  }

  Then("the second captured arg was " / string) { (b: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.arg2, b, "second captured arg"))
  }

  override def environment: ZLayer[Any, Throwable, Any] = ZLayer.empty
