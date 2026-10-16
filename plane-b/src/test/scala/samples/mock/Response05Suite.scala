package samples.mock

import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/** 05 — response fidelity: base64 body, multi-value headers (#162), latency. */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/05-response-fidelity.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Response05Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a stub GET " / string / " returns 200 base64 " / string) { (path: String, b64: String) =>
    stageRule(get(path).respondWith(ok.base64(b64)))
  }

  Given(
    "a stub GET " / string / " returns 200 with header " / string / " values " / string / " and " / string
  ) { (path: String, name: String, v1: String, v2: String) =>
    stageRule(get(path).respondWith(ok.withHeader(name, v1).withHeader(name, v2)))
  }

  Given("a stub GET " / string / " returns 200 text " / string / " after " / int / " millis") {
    (path: String, body: String, ms: Int) =>
      stageRule(get(path).respondWith(ok.text(body).withLatency(ms.millis)))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
