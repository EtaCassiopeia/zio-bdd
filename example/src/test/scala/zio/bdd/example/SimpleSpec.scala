package zio.bdd.example

import zio.*
import zio.bdd.core.{Suite, ZIOSteps}
import zio.bdd.example.Config

@Suite(featureDir = "example/src/test/resources/features", reporters = Array("console", "junitxml"), parallelism = 1)
object SimpleSpec extends ZIOSteps.Default[GreetingService] {
  Given[String, String]("a user named {string}") { name =>
    ZIO.succeed(name)
  }

  When[String, String]("the user is greeted") { name =>
    ZIO.serviceWithZIO[GreetingService](_.greet(name))
  }

  Then[String, Unit]("the greeting should be {string}") { expectedGreeting =>
    for {
      actualGreeting <- ZIO.serviceWithZIO[GreetingService](_.greet("World"))
      _              <- ZIO.succeed(assert(actualGreeting == expectedGreeting))
    } yield ()
  }

  // Provide the environment layer using ZLayer composition
  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}
