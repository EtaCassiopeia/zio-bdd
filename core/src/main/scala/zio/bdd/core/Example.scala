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

  def report(result: FeatureResult): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.debug(s"Feature: ${result.feature.name} (${if (result.isPassed) "PASSED" else "FAILED"})")
      _ <- ZIO.foreachDiscard(result.scenarioResults) { scenarioResult =>
             ZIO.debug(
               s"  Scenario: ${scenarioResult.scenario.name} (${if (scenarioResult.isPassed) "PASSED" else "FAILED"})"
             ) *>
               ZIO.foreach(scenarioResult.stepResults) { stepResult =>
                 stepResult.outcome match {
                   case Right(_) =>
                     ZIO.debug(s"    Step: ${stepResult.step} - PASSED")
                   case Left(cause) =>
                     val location = s"${stepResult.step.file.getOrElse("unknown")}:${stepResult.step.line.getOrElse(0)}"
                     ZIO.debug(s"    Step: ${stepResult.step} - FAILED at $location\n      Cause: ${cause.prettyPrint}")
                 }
               }
           }
    } yield ()

  val feature = Feature(
    name = "User Management",
    scenarios = List(
      Scenario(
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
            ),
            file = Some("example.feature"),
            line = Some(5)
          ),
          Step(
            stepType = StepType.WhenStep,
            pattern = "the system starts",
            dataTable = None,
            file = Some("example.feature"),
            line = Some(10)
          ),
          Step(
            stepType = StepType.ThenStep,
            pattern = "the user count is 2",
            dataTable = None,
            file = Some("example.feature"),
            line = Some(15)
          )
        ),
        tags = Nil,
        file = Some("example.feature"),
        line = Some(3)
      )
    ),
    file = Some("example.feature"),
    line = Some(1)
  )

  def run = {
    val initialState: List[User] = Nil
    val example                  = new Example {}
    val steps                    = example.getSteps
    val program                  = FeatureExecutor.executeFeature[Any, List[User]](feature, initialState, steps)
    program.tap { result =>
      report(result)
    }
  }
}
