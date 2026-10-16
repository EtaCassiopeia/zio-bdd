package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 07 — lifecycle & isolation: independent spaces, per-space destroy (never a
 * global reset), and space-local `received()`.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/07-lifecycle-isolation.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Isolation07Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def spaces: IO[Nothing, Map[String, MockSpace]] =
    Stage.get[NamedSpaces].map(_.byName).orElseSucceed(Map.empty)

  private def spaceNamed(name: String): IO[Throwable, MockSpace] =
    spaces.flatMap(m => ZIO.fromOption(m.get(name)).orElseFail(new IllegalStateException(s"no space '$name'")))

  Given("a space " / string / " serving GET " / string / " text " / string) {
    (name: String, path: String, body: String) =>
      for
        list  <- ctl(_.provision(mock(get(path).respondWith(ok.text(body))).source))
        space <- ZIO.fromOption(list.headOption).orElseFail(new IllegalStateException("no space"))
        cur   <- spaces
        _     <- Stage.put(NamedSpaces(cur.updated(name, space)))
      yield ()
  }

  When("GET " / string / " is sent to space " / string) { (path: String, name: String) =>
    for
      space <- spaceNamed(name)
      resp  <- SutClient.make(space).send(Method.Get, path)
      _     <- Stage.put(resp)
    yield ()
  }

  When("space " / string / " is destroyed") { (name: String) =>
    spaceNamed(name).flatMap(space => ctl(_.destroy(space)))
  }

  Then("space " / string / " recorded " / int / " requests") { (name: String, n: Int) =>
    for
      space <- spaceNamed(name)
      reqs  <- ctl(_.received(space))
      _     <- Assertions.assertEquals(reqs.size, n, s"space '$name' recorded count")
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class NamedSpaces(byName: Map[String, MockSpace])
