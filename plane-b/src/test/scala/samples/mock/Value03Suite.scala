package samples.mock

import zio.ZLayer
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/** 03 — header & query matching: Equals, Contains, Matches. */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/03-value-matching.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Value03Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given(
    "a stub GET " / string / " requiring header " / string / " equal to " / string / " returns text " / string
  ) { (path: String, h: String, v: String, body: String) =>
    stageRule(get(path).where(header(h).equalTo(v)).respondWith(ok.text(body)))
  }

  Given(
    "a stub GET " / string / " requiring header " / string / " containing " / string / " returns text " / string
  ) { (path: String, h: String, v: String, body: String) =>
    stageRule(get(path).where(header(h).contains(v)).respondWith(ok.text(body)))
  }

  Given(
    "a stub GET " / string / " requiring query " / string / " matching " / string / " returns text " / string
  ) { (path: String, q: String, re: String, body: String) =>
    stageRule(get(path).where(query(q).matches(re)).respondWith(ok.text(body)))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
