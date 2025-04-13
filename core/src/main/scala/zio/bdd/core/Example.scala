package zio.bdd.core

import zio.*
import zio.bdd.core.step.*
import zio.bdd.gherkin.*
import zio.schema.{DeriveSchema, Schema}

case class User(name: String, age: Int)

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen
}

trait Example extends ZIOSteps[Any, List[User]] {

  Given("the following users" / table[User] / " with role " / string) { (users: List[User], role: String) =>
    for {
      _ <- ZIO.debug(s"Role: $role, Users: $users")
      _ <- State.update[List[User]](_ => users)
    } yield ()
  }

  When("the system starts") {
    ZIO.debug("System starting")
  }

  Then("the user count is " / int) { (count: Int) =>
    for {
      users <- State.get[List[User]]
      _     <- ZIO.debug(s"Expected count: $count, Actual: ${users.length}")
      _     <- Assertions.assertEquals(users.length, count, s"Expected $count users, but found ${users.length}")
    } yield ()
  }
}

object ExampleApp extends ZIOAppDefault {

  val scenario = Scenario(
    name = "Example Scenario",
    steps = List(
      Step(
        stepType = StepType.GivenStep,
        pattern = "the following users with role \"admin\"",
        dataTable = Some(
          DataTable(
            headers = List("name", "age"),
            rows = List(DataTableRow(List("Alice", "30")), DataTableRow(List("Bob", "25")))
          )
        )
      ),
      Step(
        stepType = StepType.WhenStep,
        pattern = "the system starts",
        dataTable = None
      ),
      Step(
        stepType = StepType.ThenStep,
        pattern = "the user count is 2",
        dataTable = None
      )
    ),
    tags = Nil,
    examples = Nil,
    file = None,
    line = None
  )

  def runScenario[R: Tag, S: Tag](
    scenario: Scenario,
    initialState: S,
    steps: List[StepDef[R, S]]
  ): RIO[R with StepRegistry[R, S] with State[S], Unit] =
    ScenarioExecutor.executeScenario[R, S](scenario, initialState)

  def run = {
    val initialState: List[User] = Nil
    val example                  = new Example {}
    val steps                    = example.getSteps
    ZIO.scoped {
      for {
        fiberRef <- FiberRef.make(initialState)
        program   = runScenario[Any, List[User]](scenario, initialState, steps)
        _ <- program.provide(
               StepRegistry.layer[Any, List[User]](steps),
               State.layer[List[User]](fiberRef)
             )
      } yield ()
    }
  }
}
