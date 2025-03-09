package zio.bdd.core

import zio.*

trait UserRepo {
  def createUser(name: String): ZIO[Any, Throwable, User]
}

trait EmailService {
  def sendResetEmail(email: String): ZIO[Any, Throwable, Unit]
  def getSentEmails: ZIO[Any, Throwable, List[String]]
}

case class User(name: String, email: String)

object UserSteps extends ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
  Given("a user exists with name {name:String}") { (name: String) =>
    for {
      _    <- ZIO.serviceWithZIO[LogCollector](_.logStdout(s"Creating user with name: $name"))
      repo <- ZIO.service[UserRepo]
      user <- repo.createUser(name)
    } yield user
  }

  When("the user requests a password reset") { (user: User) =>
    for {
      _        <- ZIO.serviceWithZIO[LogCollector](_.logStdout(s"Requesting reset for user: ${user.name}"))
      emailSvc <- ZIO.service[EmailService]
      _        <- emailSvc.sendResetEmail(user.email)
    } yield ()
  }

  And("the user requests a password reset") { (user: User) =>
    for {
      _        <- ZIO.serviceWithZIO[LogCollector](_.logStdout(s"Requesting reset for user: ${user.name}"))
      emailSvc <- ZIO.service[EmailService]
      _        <- emailSvc.sendResetEmail(user.email)
    } yield ()
  }

  And("the reset email is logged") { (prev: Any) =>
    for {
      _ <- ZIO.serviceWithZIO[LogCollector](_.logStdout(s"Logging reset email for previous output: $prev"))
    } yield ("Logged", 42)
  }

  Then("an email should be sent to {email:String}") { (input: Any) =>
    ZIO
      .fromEither(input match {
        case e: String if e.contains("@") => Right(e)
        case _                            => Left(new Exception("Invalid input for Then step: expected a valid email address"))
      })
      .flatMap { expectedEmail =>
        for {
          _          <- ZIO.serviceWithZIO[LogCollector](_.logStdout(s"Checking emails for: $expectedEmail"))
          emailSvc   <- ZIO.service[EmailService]
          sentEmails <- emailSvc.getSentEmails
          _ <-
            Assertions.assertTrue(sentEmails.contains(expectedEmail), s"Email $expectedEmail not found in $sentEmails")
        } yield ()
      }
  }
}
