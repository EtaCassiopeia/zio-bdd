package samples.mock

import zio.ZLayer
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/** 02 — path matching: Exact, Regex, Template, and Any. */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/02-path-matching.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Path02Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a stub with exact path " / string / " returns text " / string) { (path: String, body: String) =>
    stageRule(get(path).respondWith(ok.text(body)))
  }

  Given("a stub with regex path " / string / " returns text " / string) { (pattern: String, body: String) =>
    stageRule(get("/ignored").withPath(PathMatch.Regex(pattern)).respondWith(ok.text(body)))
  }

  Given("a stub with template path " / string / " returns text " / string) { (tmpl: String, body: String) =>
    stageRule(get("/ignored").withPath(PathMatch.Template(tmpl)).respondWith(ok.text(body)))
  }

  Given("a stub matching any path returns text " / string) { (body: String) =>
    stageRule(anyRequest.withPath(PathMatch.Any).respondWith(ok.text(body)))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
