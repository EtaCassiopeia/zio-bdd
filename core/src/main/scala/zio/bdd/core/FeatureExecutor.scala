package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepDef, StepRegistry, ZIOSteps}
import zio.bdd.gherkin.{Feature, FlagsTag, Scenario, ScenarioMetadata}

object FeatureExecutor {

  def executeFeature[R: Tag, S: Tag: Default](
    feature: Feature,
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S],
    scenarioParallelism: Int = 1,
    dryRun: Boolean = false
  ): ZIO[R, Nothing, FeatureResult] =
    if (feature.isIgnored) {
      val ignoredScenarios = feature.scenarios.map { scenario =>
        ScenarioResult(scenario = scenario, stepResults = List.empty, setupError = None)
      }
      ZIO.succeed(FeatureResult(feature, ignoredScenarios))
    } else {
      ZIO
        .logAnnotate("featureId", feature.id.toString) {
          for {
            startNanos <- Clock.nanoTime
            _          <- FeatureContext.reset // clear any leaked feature-scoped values from previous feature
            _          <- suite.beforeFeatureHook
            // Expand @flags(...) tags before running scenarios
            expanded = expandFlagScenarios(feature.scenarios)
            featureResult <- runScenarios(expanded, steps, suite, scenarioParallelism, dryRun)
                               .map(scenarioResults => FeatureResult(feature, scenarioResults))
            _        <- suite.afterFeatureHook
            endNanos <- Clock.nanoTime
            duration  = (endNanos - startNanos) / 1_000_000L
          } yield featureResult.copy(duration = duration)
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
    dryRun: Boolean
  ): ZIO[R & StepRegistry[R, S], Nothing, List[ScenarioResult]] = {
    def runOne(scenario: Scenario, flags: Map[String, String]): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
      ZIO.logAnnotate("scenarioId", scenario.id.toString) {
        val meta = ScenarioMetadata.from(scenario, flags)
        // Use flagLayer when flags are present, scenarioLayer otherwise (backward-compatible)
        val layer =
          if (flags.isEmpty) suite.scenarioLayer(meta)
          else suite.flagLayer(meta, flags)
        ScenarioExecutor
          .executeScenario[R, S](scenario, suite, dryRun, flags, suite.stepTimeout)
          .provideSomeLayer[R & StepRegistry[R, S]](
            layer.orDie.asInstanceOf[ZLayer[Any, Nothing, R]]
          )
      }

    if (scenarioParallelism <= 1)
      ZIO.foreach(expanded) { case (sc, flags) => runOne(sc, flags) }
    else
      ZIO.foreachExec(expanded)(ExecutionStrategy.ParallelN(scenarioParallelism.max(1))) { case (sc, flags) =>
        runOne(sc, flags)
      }
  }

  def executeFeatures[R: Tag, S: Tag: Default](
    features: List[Feature],
    steps: List[StepDef[R, S]],
    suite: ZIOSteps[R, S],
    featureParallelism: Int = 1,
    scenarioParallelism: Int = 1,
    dryRun: Boolean = false
  ): ZIO[R, Nothing, List[FeatureResult]] =
    for {
      _ <- suite.beforeAllHook
      results <- if (featureParallelism <= 1)
                   ZIO.foreach(features)(executeFeature(_, steps, suite, scenarioParallelism, dryRun))
                 else
                   ZIO.foreachExec(features)(ExecutionStrategy.ParallelN(featureParallelism.max(1)))(
                     executeFeature(_, steps, suite, scenarioParallelism, dryRun)
                   )
      _ <- suite.afterAllHook
    } yield results
}
