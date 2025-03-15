package zio.bdd.example

import zio.*
import zio.bdd.core.Assertions.assertTrue
import zio.bdd.core.{Suite, ZIOSteps}
import zio.bdd.example.Config

@Suite(
  featureDir = "example/src/test/resources/features",
  reporters = Array("console", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive") // Pre-filter to only run @positive scenarios
)
object SimpleSpec extends ZIOSteps.Default[GreetingService] {
  Given[String, String]("a user named {string}") { name =>
    ZIO.succeed(name)
  }

  When[String, String]("the user is greeted") { name =>
    ZIO.serviceWithZIO[GreetingService](_.greet(name))
  }

  Then[(String, String), Unit]("the greeting should be {string}") { case (actualGreeting: String, expectedGreeting: String) =>
    ZIO.succeed(assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', but got '$actualGreeting'"))
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}
