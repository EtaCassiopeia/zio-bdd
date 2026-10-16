package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.gherkin.ScenarioMetadata
import zio.bdd.mock.*
import zio.bdd.mock.fixtures.{MockFixture, MockFixtures}
import samples.mock.support.{Backends, Catalog, MockState}

/**
 * 10 — scenario-tier mock fixtures: a `@mock(name)` tag deploys the named
 * catalog entry fresh per scenario (share-nothing, auto-teardown in the fixture
 * scope) via `MockFixtures.scenario`. The environment carries both the
 * `MockControl` and the resulting `MockFixture` (the provisioned spaces).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/10-fixtures-tags.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Fixtures10Suite extends ZIOSteps[MockControl & MockFixture, MockState]:

  override def scenarioLayer(
    meta: ScenarioMetadata
  ): ZLayer[Any, Throwable, MockControl & MockFixture] =
    Backends.mockControl >+> MockFixtures.scenario(meta, Catalog.entries)

  When("the client sends " / string / " " / string) { (m: String, path: String) =>
    for
      method <- ZIO
                  .fromOption(Method.values.find(_.toString.equalsIgnoreCase(m)))
                  .orElseFail(new IllegalArgumentException(s"bad method $m"))
      fx <- ZIO.service[MockFixture]
      space <- ZIO
                 .fromOption(fx.spaces.headOption)
                 .orElseFail(new IllegalStateException("fixture provisioned no space"))
      resp <- SutClient.make(space).send(method, path)
      _    <- Stage.put(resp)
    yield ()
  }

  Then("the response body is " / string) { (expected: String) =>
    Stage
      .get[SutResponse]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(r => Assertions.assertEquals(r.body, expected, "response body"))
  }

  Then("the fixture provisioned " / int / " spaces") { (n: Int) =>
    ZIO
      .service[MockFixture]
      .flatMap(fx => Assertions.assertEquals(fx.spaces.size, n, "fixture spaces"))
  }
