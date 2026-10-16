package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 09 — MockSource variants. Dsl is portable (both backends). Raw sources
 * (Json/Resource/File/Dir) carry backend wire text: Rift parses them via
 * `provision`, while WireMock requires `provisionNative` (see 17) — so the raw
 * scenarios are tagged @rift.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/09-mock-sources.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Sources09Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def provisionSource(source: MockSource): ZIO[MockControl, Throwable, List[MockSpace]] =
    ctl(_.provision(source)).tap {
      case space :: _ => Stage.put(space)
      case Nil        => ZIO.unit
    }

  Given("a Dsl source serving GET " / string / " text " / string) { (path: String, body: String) =>
    provisionSource(mock(get(path).respondWith(ok.text(body))).source).unit
  }

  Given("a resource source " / string) { (res: String) =>
    provisionSource(MockSource.Resource(res)).unit
  }

  Given("a file source " / string) { (path: String) =>
    provisionSource(MockSource.File(path)).unit
  }

  Given("a directory source " / string) { (path: String) =>
    for
      spaces <- provisionSource(MockSource.Dir(path))
      _      <- Stage.put(NamedSpaces(spaces.map(s => s.baseUri -> s).toMap))
    yield ()
  }

  Then("the directory produced " / int / " spaces") { (n: Int) =>
    Stage
      .get[NamedSpaces]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(ns => Assertions.assertEquals(ns.byName.size, n, "spaces from directory"))
  }

  Then("each directory space serves its own path") {
    for
      ns <- Stage.get[NamedSpaces].mapError(e => new IllegalStateException(e.message))
      bodies <- ZIO.foreach(ns.byName.values.toList) { space =>
                  SutClient.make(space).send(Method.Get, "/a").map(_.body) <*>
                    SutClient.make(space).send(Method.Get, "/b").map(_.body)
                }
      served = bodies.flatMap { case (a, b) => List(a, b) }.filter(_.nonEmpty).toSet
      _     <- Assertions.assertTrue(served == Set("space-a", "space-b"), s"served=$served")
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
