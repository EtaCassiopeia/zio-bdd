package samples.mock

import zio.ZLayer
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 01 — basic request/response matching: method + exact path, status codes, and
 * the Text/Json/Empty response bodies, exercised end-to-end through
 * `SutClient`.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/01-basic-matching.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Basic01Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a stub " / string / " " / string / " returns " / int / " text " / string) {
    (m: String, path: String, code: Int, body: String) =>
      methodOf(m).flatMap(method => stageRule(request(method, path).respondWith(status(code).text(body))))
  }

  Given("a stub " / string / " " / string / " returns " / int / " json " / string) {
    (m: String, path: String, code: Int, body: String) =>
      methodOf(m).flatMap(method => stageRule(request(method, path).respondWith(status(code).json(body))))
  }

  Given("a stub " / string / " " / string / " returns " / int) { (m: String, path: String, code: Int) =>
    methodOf(m).flatMap(method => stageRule(request(method, path).respondWith(status(code))))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
