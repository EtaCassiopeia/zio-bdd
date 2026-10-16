package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 21 — stress & edge cases meant to catch bugs in the adapters and the engine:
 * unicode + large + special-character bodies, root/encoded paths, multi-param
 * query matching, and rapid space churn (provision→serve→destroy in a loop).
 * Portable (core port) so it runs on WireMock, Rift container, and embedded
 * FFI.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/21-stress-edge-cases.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Stress21Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a stub GET " / string / " returns text " / string) { (path: String, body: String) =>
    stageRule(get(path).respondWith(ok.text(body)))
  }

  Given("a stub GET " / string / " returns a body of " / int / " chars") { (path: String, n: Int) =>
    stageRule(get(path).respondWith(ok.text("x" * n)))
  }

  // Distinct lead-in ("a two-query stub …") so it can't be shadowed by the 2-arg
  // "a stub GET … returns text …" step (see FINDINGS F3).
  Given(
    "a two-query stub at " / string / " needing " / string / " = " / string / " and " / string / " = " / string / " serving " / string
  ) { (path: String, q1: String, v1: String, q2: String, v2: String, body: String) =>
    stageRule(
      get(path).where(query(q1).equalTo(v1), query(q2).equalTo(v2)).respondWith(ok.text(body))
    )
  }

  Then("the response body has length " / int) { (n: Int) =>
    lastResponse.flatMap(r => Assertions.assertEquals(r.body.length, n, "response body length"))
  }

  When("churn " / int / " spaces serving GET " / string) { (n: Int, path: String) =>
    ZIO
      .foreach(1 to n) { i =>
        for
          spaces <- ctl(_.provision(mock(get(path).respondWith(ok.text(s"space-$i"))).source))
          space <- ZIO
                     .fromOption(spaces.headOption)
                     .orElseFail(new IllegalStateException("no space"))
          resp <- SutClient.make(space).send(Method.Get, path)
          _ <- ZIO
                 .fail(new AssertionError(s"churn $i: got '${resp.body}'"))
                 .unless(resp.body == s"space-$i")
          _ <- ctl(_.destroy(space))
        yield ()
      }
      .as(())
      .flatMap(_ => Stage.put(ChurnOk(true)))
  }

  Then("every churned space served correctly") {
    Stage
      .get[ChurnOk]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(c => Assertions.assertTrue(c.ok, "churn failed"))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class ChurnOk(ok: Boolean)
