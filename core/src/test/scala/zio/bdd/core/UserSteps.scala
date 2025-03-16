package zio.bdd.core

import zio.*
import zio.bdd.core.Assertions.assertTrue

object UserSteps extends ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
  case class User(name: String, email: String)

  Given("a user exists with name {name:String}") { (name: String) =>
    for {
      userRepo <- ZIO.service[UserRepo]
      user     <- userRepo.createUser(name)
      _        <- ZIO.logInfo(s"Creating user with name: $name")
    } yield user
  }

  When("the user requests a password reset") { (user: User) =>
    for {
      emailService <- ZIO.service[EmailService]
      _            <- emailService.sendResetEmail(user.email)
    } yield ()
  }

  Then("an email should be sent to {email:String}") { (email: String) =>
    for {
      emailService <- ZIO.service[EmailService]
      sentEmails   <- emailService.getSentEmails
      _            <- ZIO.succeed(assertTrue(sentEmails.contains(email), s"Email $email not found in $sentEmails"))
    } yield ()
  }

  override def beforeFeature: ZIO[UserRepo & EmailService & LogCollector, Throwable, Unit] =
    ZIO.logInfo("Preparing feature: setting up user system")

  override def beforeScenario(scenarioId: String): ZIO[UserRepo & EmailService & LogCollector, Throwable, Unit] =
    ZIO.logInfo(s"Starting scenario with ID: $scenarioId")

  override def beforeStep(scenarioId: String): ZIO[UserRepo & EmailService & LogCollector, Throwable, Unit] =
    ZIO.logInfo(s"Before step in scenario $scenarioId")
}

trait UserRepo {
  def createUser(name: String): ZIO[Any, Nothing, UserSteps.User]
}

trait EmailService {
  def sendResetEmail(email: String): ZIO[Any, Nothing, Unit]
  def getSentEmails: ZIO[Any, Nothing, List[String]]
}
