package zio.bdd.mock

import zio.bdd.mock.dsl.*
import zio.test.*

/**
 * The stateful-scenario model + fluent DSL (#129): the builder is pure
 * convenience, so its output must be structurally identical to the same
 * [[ScenarioDef]] written by hand (the same contract the rest of the DSL
 * keeps).
 */
object StatefulDslSpec extends ZIOSpecDefault:

  def spec = suite("StatefulScenarios model + DSL")(
    test("ScenarioState.Started is the default initial state") {
      assertTrue(
        ScenarioState.Started.value == "Started",
        ScenarioDef("s", rules = Nil).initial == ScenarioState.Started
      )
    },
    test("the fluent builder yields a ScenarioDef structurally identical to the hand-written one") {
      val built =
        scenario("invoice")
          .startingAt("Created")
          .when("Created", get("/invoice"))
          .respond(ok.text("created"))
          .goTo("Paid")
          .when("Paid", get("/invoice"))
          .respond(ok.text("paid"))
          .stay
          .build

      val handWritten =
        ScenarioDef(
          "invoice",
          List(
            StatefulRule(
              ScenarioState("Created"),
              RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/invoice")),
              ResponseDef(status = 200, body = Body.Text("created")),
              Some(ScenarioState("Paid"))
            ),
            StatefulRule(
              ScenarioState("Paid"),
              RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/invoice")),
              ResponseDef(status = 200, body = Body.Text("paid")),
              None // .stay
            )
          ),
          ScenarioState("Created") // initial (.startingAt)
        )

      assertTrue(built == handWritten)
    },
    test("goTo sets thenState; stay leaves it None; the default starting state is Started") {
      val d = scenario("s").when("Started", get("/x")).respond(ok).stay.build
      val g = scenario("s").when("Started", get("/x")).respond(ok).goTo("Done").build
      assertTrue(
        d.initial == ScenarioState.Started,
        d.rules.head.whenState == ScenarioState.Started,
        d.rules.head.thenState.isEmpty,
        g.rules.head.thenState.contains(ScenarioState("Done"))
      )
    }
  )
