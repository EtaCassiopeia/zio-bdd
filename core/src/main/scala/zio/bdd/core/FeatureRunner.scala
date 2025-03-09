package zio.bdd.core

import zio.*
import zio.bdd.core.report.Reporter
import zio.bdd.gherkin.Feature

import java.time.Instant

object FeatureRunner {
  def runFeatures[R](
    steps: ZIOSteps[R],
    features: List[Feature],
    parallelism: Int
  ): ZIO[R & LogCollector & Reporter, Throwable, List[StepResult]] =
    ZIO
      .foreachPar(features) { feature =>
        ScenarioRunner.runScenarios(steps, feature, parallelism).map(_.flatten).catchAll { e =>
          ZIO.succeed(
            List(StepResult("Feature failed", succeeded = false, Some(e), (), Nil, Duration.Zero, Instant.now()))
          )
        }
      }
      .map(_.flatten)
      .withParallelism(parallelism)
}
