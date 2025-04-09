package zio.bdd.core.step

import zio.*
import zio.bdd.core.step.*
import zio.bdd.core.step.StepPatternBuilder.stringToStepPatternBuilder
import zio.schema.Schema
import zio.schema.{DeriveSchema, Schema}
import zio.bdd.gherkin.{DataTable, DataTableRow, Scenario, ScenarioMetadata, Step, StepType}
import zio.*
import zio.bdd.core.step.*

case class User(name: String, age: Int)

object User {
  implicit val schema: Schema[User] =  DeriveSchema.gen
}

object Example {

  Given("the following users" / TypedExtractor.table[List[User]] / " with role " / TypedExtractor.string) {
    case (users: List[User], role: String) =>
      for {
        _ <- ZIO.debug(s"Role: $role, Users: $users")
        _ <- State.update[List[User]](_ => users)
      } yield ()
  }

  When("the system starts") { (_: Unit) =>
    ZIO.debug("System starting")
  }

  Then("the user count is " / TypedExtractor.int) { (count: Int) =>
    for {
      users <- State.get[List[User]]
      _     <- ZIO.debug(s"Expected count: $count, Actual: ${users.length}")
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
        dataTable = Some(DataTable(
          headers = List("name", "age"),
          rows = List(DataTableRow(List("Alice", "30")), DataTableRow(List("Bob", "25")))
        ))
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
    metadata = ScenarioMetadata(),
    file = None,
    line = None
  )

  def runScenario[R: Tag, S: Tag](scenario: Scenario, initialState: S): RIO[R with StepRegistry[R, S], Unit] =
    ScenarioExecutor.executeScenario[R, S](scenario, initialState)

  def run = {
    val initialState: List[User] = Nil
    val program = runScenario[Any, List[User]](scenario, initialState)
    program.provide(
      StepRegistry.layer[Any, List[User]] // Provide StepRegistry
    )
  }
}