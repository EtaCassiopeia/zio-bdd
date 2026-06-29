package zio.bdd.mock

import zio.*

/**
 * Minimal capability-instance stubs for tests that only need an advertised
 * accessor to return *some* valid instance (the negotiation tests don't drive
 * the capability's behaviour — they assert advertised ⇒ Right / un-advertised ⇒
 * Unsupported).
 */
object StubCaps:
  val scenarios: StatefulScenarios = new StatefulScenarios:
    def define(space: MockSpace, scenario: ScenarioDef): IO[MockError, Unit] = ZIO.unit
    def reset(space: MockSpace, name: String): IO[MockError, Unit]           = ZIO.unit

  val stateInspection: StateInspection = new StateInspection:
    def currentState(space: MockSpace, name: String): IO[MockError, ScenarioState]       = ZIO.succeed(ScenarioState.Started)
    def setState(space: MockSpace, name: String, to: ScenarioState): IO[MockError, Unit] = ZIO.unit
