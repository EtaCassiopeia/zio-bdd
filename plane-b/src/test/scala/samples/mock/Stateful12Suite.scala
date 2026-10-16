package samples.mock

import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 12 — stateful scenarios (capability StatefulScenarios): a single-token FSM
 * that fails twice then succeeds, and reset() returning it to the start.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/12-stateful-fsm.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Stateful12Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def defineRetry(path: String) =
    scenario("retry")
      .startingAt("Started")
      .when("Started", get(path))
      .respond(status(503))
      .goTo("Attempt1")
      .when("Attempt1", get(path))
      .respond(status(503))
      .goTo("Attempt2")
      .when("Attempt2", get(path))
      .respond(ok.text("ok"))
      .goTo("Done")
      .build

  Given("the backend supports stateful scenarios") {
    requiring(Capability.StatefulScenarios)
  }

  Given("a fail-twice-then-succeed scenario for GET " / string) { (path: String) =>
    for
      space <- activeSpace
      sc <- ZIO
              .serviceWithZIO[MockControl](_.scenarios)
              .mapError(u => new RuntimeException(u.message))
      _ <- sc.define(space, defineRetry(path)).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  When("the scenario " / string / " is reset") { (name: String) =>
    for
      space <- activeSpace
      sc <- ZIO
              .serviceWithZIO[MockControl](_.scenarios)
              .mapError(u => new RuntimeException(u.message))
      _ <- sc.reset(space, name).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
