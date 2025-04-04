package zio.bdd.core.report

import zio.*
import zio.bdd.core.*
import zio.bdd.gherkin.*
import zio.stream.{ZPipeline, ZSink, ZStream}
import zio.test.*

object JUnitXMLReporterTest extends ZIOSpecDefault {

  val testResultPath = "target/test-results"

  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
    ZLayer.succeed(new UserRepo {
      def createUser(name: String) = ZIO.succeed(UserSteps.User(name, s"$name@example.com".toLowerCase))
    }) ++
      ZLayer.fromZIO(
        Ref.make(List.empty[String]).map { emailsRef =>
          new EmailService {
            def sendResetEmail(email: String) = emailsRef.update(email :: _)
            def getSentEmails                 = emailsRef.get
          }
        }
      ) ++
      LogCollector.live ++
      JUnitXMLReporter.live(JUnitReporterConfig(outputDir = testResultPath, format = JUnitXMLFormatter.Format.JUnit5))

  def spec: Spec[TestEnvironment & Scope, Any] = suite("ScenarioRunner")(
    test("JUnitXMLReporter generates correct JUnit 5 XML report") {
      val content =
        """
          |Feature: JUnit Reporting
          |  Scenario: Simple user reset
          |    Given a user exists with name {name:String}
          |    When the user requests a password reset
          |    Then an email should be sent to {email:String}
          |  Examples:
          |    | name   | email             |
          |    | JUnit  | junit@example.com |
        """.stripMargin
      for {
        feature     <- GherkinParser.parseFeature(content)
        _           <- ZIO.logInfo(s"Feature: ${feature}")
        reporter    <- ZIO.service[Reporter]
        results     <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
        reportFiber <- reporter.generateFinalReport(List(feature), results, 0)
        _           <- ZIO.logInfo("Reading XML file")
        xmlContent <- ZStream
                        .fromFileName(s"$testResultPath/JUnit_Reporting.xml")
                        .via(ZPipeline.utf8Decode)
                        .run(ZSink.mkString)
        _ <- ZIO.debug(s"Generated XML: $xmlContent")
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name JUnit",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "an email should be sent to junit@example.com",
        xmlContent.contains("""<testsuite name="JUnit Reporting" tests="1" failures="0""""),
        xmlContent.contains("""<testcase name="Scenario: Simple user reset""""),
        xmlContent.contains("<system-out>") && xmlContent.contains("Creating user with name: JUnit"),
        !xmlContent.contains("<system-err>")
      )
    }
  ).provideLayer(testEnv)
}
