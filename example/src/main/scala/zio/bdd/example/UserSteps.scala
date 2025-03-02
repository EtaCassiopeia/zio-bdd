//package zio.bdd.example
//
//import zio.*
//import zio.bdd.core.*
//
//trait UserRepo {
//  def createUser(name: String): ZIO[Any, Throwable, User]
//}
//
//trait EmailService {
//  def sendResetEmail(email: String): ZIO[Any, Throwable, Unit]
//  def getSentEmails: ZIO[Any, Throwable, List[String]]
//}
//
//case class User(name: String, email: String)
//
//object UserSteps extends ZIOSteps.Default[UserRepo & EmailService with LogCollector] {
//  Given("a user exists with name {string}") { (name: String) =>
//    for {
//      _    <- ZIO.serviceWithZIO[LogCollector](_.log(s"Creating user with name: $name"))
//      repo <- ZIO.service[UserRepo]
//      user <- repo.createUser(name)
//    } yield user
//  }
//
//  When("the user requests a password reset") { (user: User) =>
//    for {
//      _        <- ZIO.serviceWithZIO[LogCollector](_.log(s"Requesting reset for user: ${user.name}"))
//      emailSvc <- ZIO.service[EmailService]
//      _        <- emailSvc.sendResetEmail(user.email)
//    } yield ()
//  }
//
//  And("the reset email is logged") { (prev: Any) =>
//    for {
//      _ <- ZIO.serviceWithZIO[LogCollector](_.log(s"Logging reset email for previous output: $prev"))
//    } yield ("Logged", 42) // Example multi-output
//  }
//
//  Then("an email should be sent to {string}") { (input: Any) =>
//    val (expectedEmail: String) = input match {
//      case e: String      => (e)
//      case (_, e: String) => (e)
//      case _              => throw new Exception("Invalid input for Then step")
//    }
//    for {
//      _          <- ZIO.serviceWithZIO[LogCollector](_.log(s"Checking emails for: $expectedEmail"))
//      emailSvc   <- ZIO.service[EmailService]
//      sentEmails <- emailSvc.getSentEmails
//      _          <- Assertions.assertTrue(sentEmails.contains(expectedEmail), s"Email $expectedEmail not found in $sentEmails")
//    } yield ()
//  }
//}
