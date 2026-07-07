package zio.bdd

import zio.test.*
import zio.bdd.core.Default
import zio.bdd.core.step.ZIOSteps

/**
 * Gate for issue #216 — env-aware / suite-overridable parallelism.
 *
 * Precedence (highest first):
 *   1. suite override (ZIOSteps.scenarioParallelism / featureParallelism) 2.
 *      --scenario-parallelism / --parallelism CLI flag 3.
 *      ZIO_BDD_SCENARIO_PARALLELISM / ZIO_BDD_FEATURE_PARALLELISM env var 4.
 *      \@Suite annotation value
 */
object ParallelismResolutionSpec extends ZIOSpecDefault {

  private given Default[Unit] = Default.fromValue(())

  // Minimal suite that keeps the default (None) hooks.
  object DefaultSteps extends ZIOSteps[Any, Unit]

  // Suite pinning both knobs at runtime (Option A).
  object PinnedSteps extends ZIOSteps[Any, Unit] {
    override def scenarioParallelism: Option[Int] = Some(1)
    override def featureParallelism: Option[Int]  = Some(0)
  }

  private val envParsing = suite("parseEnvParallelism")(
    test("None env → None") {
      assertTrue(ZIOBDDTask.parseEnvParallelism(None).isEmpty)
    },
    test("empty string → None") {
      assertTrue(ZIOBDDTask.parseEnvParallelism(Some("")).isEmpty)
    },
    test("'auto' → Some(0)") {
      assertTrue(ZIOBDDTask.parseEnvParallelism(Some("auto")).contains(0))
    },
    test("numeric → Some(n)") {
      assertTrue(ZIOBDDTask.parseEnvParallelism(Some("4")).contains(4))
    },
    test("non-numeric garbage → None") {
      assertTrue(ZIOBDDTask.parseEnvParallelism(Some("banana")).isEmpty)
    }
  )

  private val scenarioPrecedence = suite("resolveScenarioParallelism (CLI sentinel 0)")(
    test("CLI value (non-zero) wins over env and annotation") {
      assertTrue(ZIOBDDTask.resolveScenarioParallelism(cli = 3, env = Some(2), anno = 1) == 3)
    },
    test("env applies when CLI unset (0), overriding annotation") {
      assertTrue(ZIOBDDTask.resolveScenarioParallelism(cli = 0, env = Some(2), anno = 1) == 2)
    },
    test("annotation preserved when CLI unset and no env (AC4)") {
      assertTrue(ZIOBDDTask.resolveScenarioParallelism(cli = 0, env = None, anno = 1) == 1)
    },
    test("annotation auto (0) preserved when nothing else set") {
      assertTrue(ZIOBDDTask.resolveScenarioParallelism(cli = 0, env = None, anno = 0) == 0)
    }
  )

  private val featurePrecedence = suite("resolveFeatureParallelism (CLI sentinel 1)")(
    test("CLI value (non-default) wins over env and annotation") {
      assertTrue(ZIOBDDTask.resolveFeatureParallelism(cli = 3, env = Some(2), anno = 4) == 3)
    },
    test("env applies when CLI unset (1), overriding annotation") {
      assertTrue(ZIOBDDTask.resolveFeatureParallelism(cli = 1, env = Some(2), anno = 4) == 2)
    },
    test("annotation preserved when CLI unset and no env") {
      assertTrue(ZIOBDDTask.resolveFeatureParallelism(cli = 1, env = None, anno = 4) == 4)
    }
  )

  private val suiteOverride = suite("effectiveParallelism (Option A — suite override)")(
    test("suite override wins over the resolved config value") {
      assertTrue(ZIOBDDTask.effectiveParallelism(Some(1), configValue = 8) == 1)
    },
    test("None defers to the resolved config value") {
      assertTrue(ZIOBDDTask.effectiveParallelism(None, configValue = 8) == 8)
    },
    test("override of 0 (auto) passes through") {
      assertTrue(ZIOBDDTask.effectiveParallelism(Some(0), configValue = 8) == 0)
    }
  )

  private val traitHooks = suite("ZIOSteps parallelism hooks")(
    test("default hooks are None (AC1, AC6)") {
      assertTrue(
        DefaultSteps.scenarioParallelism.isEmpty,
        DefaultSteps.featureParallelism.isEmpty
      )
    },
    test("overrides expose the pinned values") {
      assertTrue(
        PinnedSteps.scenarioParallelism.contains(1),
        PinnedSteps.featureParallelism.contains(0)
      )
    }
  )

  def spec = suite("ParallelismResolutionSpec")(
    envParsing,
    scenarioPrecedence,
    featurePrecedence,
    suiteOverride,
    traitHooks
  )
}
