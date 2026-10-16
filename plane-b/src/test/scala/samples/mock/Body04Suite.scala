package samples.mock

import zio.ZLayer
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/** 04 — request body matching: Equals, Contains, Matches, JsonPath, XPath. */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/04-body-matching.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Body04Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given(
    "a stub POST " / string / " requiring body equal to " / string / " returns text " / string
  ) { (path: String, b: String, body: String) =>
    stageRule(post(path).where(bodyEquals(b)).respondWith(ok.text(body)))
  }

  Given(
    "a stub POST " / string / " requiring body containing " / string / " returns text " / string
  ) { (path: String, b: String, body: String) =>
    stageRule(post(path).where(bodyContains(b)).respondWith(ok.text(body)))
  }

  Given(
    "a stub POST " / string / " requiring jsonpath " / string / " equal to " / string / " returns text " / string
  ) { (path: String, jp: String, v: String, body: String) =>
    stageRule(post(path).where(jsonPath(jp).equalTo(v)).respondWith(ok.text(body)))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
