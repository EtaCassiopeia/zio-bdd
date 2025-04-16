import zio.*
import zio.bdd.core.report.PrettyReporter
import zio.bdd.core.step.*
import zio.bdd.core.*
import zio.bdd.gherkin.*
import zio.schema.{DeriveSchema, Schema}

case class User(name: String, age: Int)

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen
}

trait Example extends ZIOSteps[Any, List[User]] {

  Given("the following users" / table[User] / " with role " / string) { (users: List[User], role: String) =>
    for {
      _ <- ZIO.logInfo(s"Role: $role, Users: $users")
      _ <- ScenarioContext.update(_ => users)
    } yield ()
  }

  When("the system starts") {
    ZIO.logInfo("System starting")
  }

  Then("the user count is " / int) { (count: Int) =>
    for {
      users <- State.get
      _     <- ZIO.logInfo(s"Expected count: $count, Actual: ${users.length}")
      _     <- Assertions.assertEquals(users.length, count, s"Expected $count users, but found ${users.length}")
    } yield ()
  }

  beforeFeature {
    ZIO.debug("Starting feature execution")
  }

  beforeScenario {
    State.get.flatMap(state => ZIO.debug(s"Starting scenario execution with state: $state"))
  }
}

object ExampleApp extends ZIOAppDefault {

  val reporter = PrettyReporter()

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

  private val logConfig = LogLevelConfig(InternalLogLevel.Debug)

  def run = {
    val initialState: List[User] = Nil
    val example                  = new Example {}
    val steps                    = example.getSteps
    val program                  = FeatureExecutor.executeFeature[Any, List[User]](feature, initialState, steps, example)
    program.tap { result =>
      reporter.report(List(result))
    }
  }.provide(LogCollector.live(logConfig))
}
