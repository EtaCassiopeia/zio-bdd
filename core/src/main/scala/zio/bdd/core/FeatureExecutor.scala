package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.property.{ColumnGenLookup, PropertyExecutor}
import zio.bdd.core.step.{StepDef, StepRegistry, ZIOSteps}
import zio.bdd.gherkin.{Feature, FlagsTag, Scenario, ScenarioMetadata}

object FeatureExecutor {

  // Wall-clock duration measurement — independent of the ZIO Clock (and TestClock).
  private val nowNanos: UIO[Long] = ZIO.succeed(java.lang.System.nanoTime())

  /**
   * Resolves the `scenarioParallelism` auto sentinel (0) to the actual number
   * of available processors, falling back to 2 if the JVM reports an
   * unreasonable value. `Suite.scenarioParallelism()` documents `0` as "auto",
   * and this is the single place that contract is honored, regardless of entry
   * point (sbt test framework, CLI, or direct API use).
   */
  private def resolveParallelism(n: Int): Int =
    if (n > 0) n
    else
      java.lang.Runtime.getRuntime.availableProcessors() match {
        case p if p > 0 => p
        case _          => 2
      }

  def executeFeature[R: Tag, S: Tag: Default](
    feature: Feature,
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S],
    scenarioParallelism: Int = 1,
    dryRun: Boolean = false,
    genLookup: ColumnGenLookup = ColumnGenLookup.empty
  ): ZIO[R, Nothing, FeatureResult] =
    if (feature.isIgnored) {
      val ignoredScenarios = feature.scenarios.map { scenario =>
        ScenarioResult(scenario = scenario, stepResults = List.empty, setupError = None)
      }
      ZIO.succeed(FeatureResult(feature, ignoredScenarios))
    } else {
      ZIO.scoped {
        ZIO.logAnnotate("featureId", feature.id.toString) {
          for {
            startNanos <- nowNanos
            // Per-feature staging scoped to the feature and auto-restored on scope close.
            _ <- FeatureContext.ref.locallyScoped(Map.empty)
            featureResult <- (for {
                               _       <- suite.beforeFeatureHook
                               expanded = expandFlagScenarios(feature.scenarios)
                               results <- runScenarios(expanded, steps, suite, scenarioParallelism, dryRun, genLookup)
                             } yield FeatureResult(feature, results))
                               // afterFeature teardown always runs, even on failure or interruption.
                               .ensuring(suite.afterFeatureHook)
            endNanos <- nowNanos
            duration  = (endNanos - startNanos) / 1_000_000L
          } yield featureResult.copy(duration = duration)
        }
      }
        .provideSomeLayer[R](StepRegistry.layer[R, S](steps))
    }

  /**
   * Expand `@flags(k=v, ...)` tags into multiple (Scenario, flagValues) pairs.
   *
   * Scenarios with no `@flags(...)` tags produce exactly one pair with an empty
   * map. Scenarios with N `@flags(...)` tags produce N pairs — one per tag —
   * with the scenario name suffixed by the flag values for traceability in
   * reports.
   *
   * Also handles the Outline × Flags matrix: if the scenario was already
   * expanded from an Outline, each expanded scenario is independently
   * flag-expanded.
   */
  private def expandFlagScenarios(scenarios: List[Scenario]): List[(Scenario, Map[String, String])] =
    scenarios.flatMap { scenario =>
      val flagMaps = FlagsTag.extractAll(scenario.tags)
      if (flagMaps.isEmpty)
        List((scenario, Map.empty))
      else
        flagMaps.map { flags =>
          val label   = flags.toList.sortBy(_._1).map { case (k, v) => s"$k=$v" }.mkString(", ")
          val renamed = scenario.copy(name = s"${scenario.name} [$label]")
          (renamed, flags)
        }
    }

  private def runScenarios[R: Tag, S: Tag: Default](
    expanded: List[(Scenario, Map[String, String])],
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S],
    scenarioParallelism: Int,
    dryRun: Boolean,
    genLookup: ColumnGenLookup
  ): ZIO[R & StepRegistry[R, S], Nothing, List[ScenarioResult]] = {
    def runOne(scenario: Scenario, flags: Map[String, String]): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
      ZIO.logAnnotate("scenarioId", scenario.id.toString) {
        scenario.propertyConfig match
          case Some(_) =>
            // Property scenario — dispatch to PropertyExecutor, bypass flag expansion
            PropertyExecutor.run[R, S](scenario, suite, genLookup)
          case None =>
            val meta = ScenarioMetadata.from(scenario, flags)
            val layer =
              if (flags.isEmpty) suite.scenarioLayer(meta)
              else suite.flagLayer(meta, flags)
            ScenarioExecutor
              .executeScenario[R, S](scenario, suite, dryRun, flags, suite.stepTimeout)
              .provideSomeLayer[R & StepRegistry[R, S]](layer.orDie)
      }

    val resolved = resolveParallelism(scenarioParallelism)
    if (resolved <= 1)
      ZIO.foreach(expanded) { case (sc, flags) => runOne(sc, flags) }
    else
      ZIO.foreachExec(expanded)(ExecutionStrategy.ParallelN(resolved)) { case (sc, flags) =>
        runOne(sc, flags)
      }
  }

  def executeFeatures[R: Tag, S: Tag: Default](
    features: List[Feature],
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S],
    featureParallelism: Int = 1,
    scenarioParallelism: Int = 1,
    dryRun: Boolean = false,
    genLookup: ColumnGenLookup = ColumnGenLookup.empty
  ): ZIO[R, Nothing, List[FeatureResult]] =
    for {
      _ <- suite.beforeAllHook
      results <- if (featureParallelism <= 1)
                   ZIO.foreach(features)(executeFeature(_, steps, suite, scenarioParallelism, dryRun, genLookup))
                 else
                   ZIO.foreachExec(features)(ExecutionStrategy.ParallelN(featureParallelism.max(1)))(
                     executeFeature(_, steps, suite, scenarioParallelism, dryRun, genLookup)
                   )
      _ <- suite.afterAllHook
    } yield results
}
