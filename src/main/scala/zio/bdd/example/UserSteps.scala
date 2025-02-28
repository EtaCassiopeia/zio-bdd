package zio.bdd.example

import zio.*
import zio.bdd.dsl.*

trait UserRepo {
  def createUser(name: String): ZIO[Any, Throwable, User]
}

trait EmailService {
  def sendResetEmail(email: String): ZIO[Any, Throwable, Unit]
  def getSentEmails: ZIO[Any, Throwable, List[String]]
}

case class User(name: String, email: String)

object UserSteps extends ZIOSteps.Default[UserRepo & EmailService] {
  Given("a user exists with name {string}") { (name: String) =>
    for {
      repo <- ZIO.service[UserRepo]
      user <- repo.createUser(name)
    } yield user
  }

  When("the user requests a password reset") { (user: User) =>
    for {
      _        <- ZIO.debug(s"User ${user.name} requested a password reset")
      emailSvc <- ZIO.service[EmailService]
      _        <- emailSvc.sendResetEmail(user.email)
    } yield ()
  }

  Then("an email should be sent to {string}") { (expectedEmail: String) =>
    for {
      emailSvc   <- ZIO.service[EmailService]
      sentEmails <- emailSvc.getSentEmails
      _          <- ZIO.debug(s"Sent emails: $sentEmails")
      _          <- ZIO.debug(s"Expected email: $expectedEmail: ${sentEmails.contains(expectedEmail)}")
      _ <- ZIO.attempt {
             assert(sentEmails.contains(expectedEmail))
           }
    } yield ()
  }
}
