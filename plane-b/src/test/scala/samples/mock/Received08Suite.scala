package samples.mock

import zio.ZLayer
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/** 08 — request verification via received(): count, method/path, and body. */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/08-received-verification.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Received08Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a stub matching any request returns 200") {
    stageRule(anyRequest.respondWith(ok.text("ok")))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
