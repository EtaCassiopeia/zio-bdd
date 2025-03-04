package zio.bdd.example

import zio.*

case class Config(greetingPrefix: String)
trait GreetingService {
  def greet(name: String): ZIO[Any, Nothing, String]
}

object GreetingService {
  val live: ZLayer[Config, Nothing, GreetingService] = ZLayer.fromFunction { (config: Config) =>
    new GreetingService {
      override def greet(name: String): ZIO[Any, Nothing, String] =
        ZIO.succeed(s"${config.greetingPrefix}, $name!")
    }
  }
}

object SimpleZIOApp extends ZIOAppDefault {
  val program: ZIO[GreetingService, Nothing, Unit] = for {
    greeting <- ZIO.serviceWithZIO[GreetingService](_.greet("World"))
    _        <- ZIO.debug(greeting)
  } yield ()

  override def run: ZIO[Any, Any, Any] =
    program.provide(GreetingService.live, ZLayer.succeed(Config("Hello")))
}
