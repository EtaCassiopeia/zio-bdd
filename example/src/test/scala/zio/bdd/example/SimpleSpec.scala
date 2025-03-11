package zio.bdd.example

import zio.*
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

  Then[String, Unit]("the greeting should be {string}") { expectedGreeting =>
    for {
      name <- ZIO.succeed(expectedGreeting.split(", ").last.dropRight(1)) // Extract name from "Hello, Name!"
      actualGreeting <- ZIO.serviceWithZIO[GreetingService](_.greet(name))
      _ <- ZIO.succeed(assert(actualGreeting == expectedGreeting))
    } yield ()
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}
