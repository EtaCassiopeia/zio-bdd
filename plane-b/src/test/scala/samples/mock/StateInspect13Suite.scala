package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 13 — state inspection (capability StateInspection): read a scenario's current
 * state and force a transition without driving requests.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/13-state-inspection.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object StateInspect13Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def si = ZIO
    .serviceWithZIO[MockControl](_.stateInspection)
    .mapError(u => new RuntimeException(u.message))

  Given("the backend supports state inspection") {
    requiring(Capability.StateInspection)
  }

  Given("a two-state scenario " / string / " for GET " / string) { (name: String, path: String) =>
    for
      space <- activeSpace
      sc <- ZIO
              .serviceWithZIO[MockControl](_.scenarios)
              .mapError(u => new RuntimeException(u.message))
      fsm = scenario(name)
              .startingAt("Started")
              .when("Started", get(path))
              .respond(ok.text("first"))
              .goTo("Next")
              .when("Next", get(path))
              .respond(ok.text("second"))
              .goTo("Started")
              .build
      _ <- sc.define(space, fsm).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  When("the state of " / string / " is forced to " / string) { (name: String, to: String) =>
    for
      space <- activeSpace
      s     <- si
      _     <- s.setState(space, name, ScenarioState(to)).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  Then("the current state of " / string / " is " / string) { (name: String, expected: String) =>
    for
      space <- activeSpace
      s     <- si
      cur   <- s.currentState(space, name).mapError(e => new RuntimeException(s"$e"))
      _     <- Assertions.assertEquals(cur.value, expected, s"current state of '$name'")
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
