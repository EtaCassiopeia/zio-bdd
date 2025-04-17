package zio.bdd.example

import zio.*
import zio.bdd.core.Assertions.assertTrue
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.example.Config
import zio.schema.{DeriveSchema, Schema}

case class ScenarioContext(userName: String, greeting: String)

object ScenarioContext {
  implicit val schema: Schema[ScenarioContext] = DeriveSchema.gen[ScenarioContext]
}

@Suite(
  featureDir = "example/src/test/resources/features",
  reporters = Array("pretty", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive"), // Pre-filter to only run @positive scenarios
  logLevel = "debug"
)
object SimpleSpec extends ZIOSteps[GreetingService, ScenarioContext] {
  Given("a user named " / string) { (name: String) =>
    ScenarioContext.update(_.copy(userName = name))
  }

  When("the user is greeted") {
    for {
        ctx <- ScenarioContext.get
        _   <- ZIO.logInfo(s"Greeting user ${ctx.userName}")
        - <- ScenarioContext.update(_.copy(greeting = s"Hello, ${ctx.userName}!"))
    } yield ()
  }

  Then("the greeting should be " / string) { (expectedGreeting: String) =>
    ScenarioContext.get.map(_.greeting).map(actualGreeting => assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', but got '$actualGreeting'"))
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}
