package zio.bdd.example

import zio.*
import zio.bdd.runtime.*

object Main extends ZIOAppDefault {
  val scenario = """
    Given a user exists with name "Alice"
    When the user requests a password reset
    Then an email should be sent to "alice@example.com"
  """

  val failingScenario = """
    Given a user exists with name "Bob"
    When the user requests a password reset
    Then an email should be sent to "wrong@example.com"
  """

  val env = ZLayer.succeed(new UserRepo {
    def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase)) // Normalize to lowercase
  }) ++ ZLayer.succeed(new EmailService {
    private var emails: List[String]  = Nil
    def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
    def getSentEmails                 = ZIO.succeed(emails)
  })

  def run =
    ScenarioRunner
      .run(UserSteps, failingScenario) // scenario or failingScenario
      .provideLayer(env ++ ZLayer.succeed(ZIO.logLevel(LogLevel.Info)))
      .map { results =>
        val exitCode = if (results.exists(_.succeeded == false)) ExitCode.failure else ExitCode.success
        exitCode
      }
}
