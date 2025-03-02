package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.core.*
import zio.bdd.gherkin.*

object ScenarioRunnerTest extends ZIOSpecDefault {
  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
    ZLayer.succeed(new UserRepo {
      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
    }) ++
      ZLayer.fromZIO(
        Ref.make(List.empty[String]).map { emailsRef =>
          new EmailService {
            def sendResetEmail(email: String) = emailsRef.update(email :: _)

            def getSentEmails = emailsRef.get
          }
        }
      ) ++
      LogCollector.live ++
      ZLayer.succeed(ConsoleReporter)

//  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
//    ZLayer.succeed(new UserRepo {
//      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//    }) ++
//      ZLayer.fromZIO(
//        Ref.make(List.empty[String]).map { emailsRef =>
//          new EmailService {
//            def sendResetEmail(email: String) = emailsRef.update(email :: _)
//
//            def getSentEmails = emailsRef.get
//          }
//        }
//      ) ++
//      LogCollector.live ++
//      ZLayer.succeed(ConsoleReporter)

//  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
//    ZLayer.succeed(new UserRepo {
//      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//    }) ++
//      ZLayer.fromZIO(
//        Ref.make(List.empty[String]).map { emailsRef =>
//          new EmailService {
//            def sendResetEmail(email: String) = emailsRef.update(email :: _)
//            def getSentEmails = emailsRef.get
//          }
//        }
//      ) ++
//      LogCollector.live ++
//      ZLayer.succeed(new Reporter {
//        def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
//      })

  def spec: Spec[TestEnvironment with Scope, Any] = suite("ScenarioRunner")(
    test("run valid scenario with background") {
      val content = """
                      |Feature: User Management
                      |  Background:
                      |    Given a user exists with name {name:String}
                      |  Scenario: Successful password reset with logging
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name    | email             |
                      |    | Default | default@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 4,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Default",
        results.head(0).output.isInstanceOf[User],
        results.head(1).step == "the user requests a password reset",
        results.head(1).output == (),
        results.head(2).step == "the reset email is logged",
        results.head(2).output == ("Logged", 42),
        results.head(3).step == "an email should be sent to default@example.com",
        results.head(3).output == (),
        results.head.exists(_.logs.exists(_.toString.contains("Creating user with name: Default")))
      )
    },
    test("run scenario outline with examples") {
      val content = """
                      |Feature: User Validation
                      |  Scenario Outline: Validate reset emails
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name  | email             |
                      |    | Alice | alice@example.com |
                      |    | Bob   | bob@example.com   |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results.forall(_.length == 3),
        results.forall(_.forall(_.succeeded)),
        results(0)(0).step == "a user exists with name Alice",
        results(0)(0).output.isInstanceOf[User],
        results(0)(1).step == "the user requests a password reset",
        results(0)(2).step == "an email should be sent to alice@example.com",
        results(1)(0).step == "a user exists with name Bob",
        results(1)(1).step == "the user requests a password reset",
        results(1)(2).step == "an email should be sent to bob@example.com"
      )
    },
    test("run scenario with retry on failure") {
      val content = """
                      |Feature: Retry Test
                      |  @Retry(3)
                      |  Scenario: Retry on failure
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name | email            |
                      |    | Fail | fail@example.com |
                """.stripMargin
      val failingSteps = new ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
        Given("a user exists with name {name:String}") { (name: String) =>
          ZIO.succeed(User(name, s"$name@example.com"))
        }
        When("the user requests a password reset") { (user: User) =>
          ZIO.fail(new Exception("Reset failed"))
        }
        Then("an email should be sent to {email:String}") { (email: String) =>
          ZIO.unit
        }
      }
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(failingSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head(0).succeeded,
        !results.head(1).succeeded,
        results.head(1).error.contains("Reset failed"),
        !results.head(2).succeeded,
        results.head(2).error.contains("Skipped due to prior failure"),
        feature.scenarios.head.metadata.retryCount == 3
      )
    },
    test("run scenario with repeat") {
      val content = """
                      |Feature: Repeat Test
                      |  @Repeat(2)
                      |  Scenario: Repeat execution
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |  Examples:
                      |    | name   |
                      |    | Repeat |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 4,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Repeat",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "a user exists with name Repeat",
        results.head(3).step == "the user requests a password reset",
        feature.scenarios.head.metadata.repeatCount == 2
      )
    },
    test("fail on unmatched step") {
      val content = """
                      |Feature: Unmatched Step Test
                      |  Scenario: Unmatched step
                      |    Given a user exists with name {name:String}
                      |    When an undefined step runs
                      |  Examples:
                      |    | name    |
                      |    | Default |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 2,
        results.head(0).succeeded,
        !results.head(1).succeeded,
        results.head(1).error.contains("No step definition matches"),
        results.head(1).step == "an undefined step runs"
      )
    },
    test("fail on invalid input type") {
      val content = """
                      |Feature: Invalid Input Test
                      |  Scenario: Invalid input type
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name    | email |
                      |    | Default | 123   |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head(0).succeeded,
        results.head(1).succeeded,
        !results.head(2).succeeded,
        results.head(2).error.contains("Invalid input for Then step: expected a valid email address"),
        results.head(2).step == "an email should be sent to 123"
      )
    },
    test("run empty scenario") {
      val content = """
                      |Feature: Empty Test
                      |  Scenario: Empty scenario
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.isEmpty
      )
    },
    test("run scenario with flaky tag") {
      val content = """
                      |Feature: Flaky Test
                      |  @Flaky
                      |  Scenario: Flaky scenario
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |  Examples:
                      |    | name  |
                      |    | Flaky |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 2,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Flaky",
        results.head(1).step == "the user requests a password reset",
        feature.scenarios.head.metadata.isFlaky
      )
    },
    test("run multiple scenarios") {
      val content = """
                      |Feature: Multi Scenario Test
                      |  Background:
                      |    Given a user exists with name {name:String}
                      |  Scenario: Reset scenario
                      |    When the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |  Scenario: Another reset scenario
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |  Examples:
                      |    | name    | email             |
                      |    | Default | default@example.com |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results(0).length == 3,
        results(1).length == 3,
        results.forall(_.forall(_.succeeded)),
        results(0)(0).step == "a user exists with name Default",
        results(0)(1).step == "the user requests a password reset",
        results(0)(2).step == "an email should be sent to default@example.com",
        results(1)(0).step == "a user exists with name Default",
        results(1)(1).step == "the user requests a password reset",
        results(1)(2).step == "the reset email is logged"
      )
    },
    test("run simple given-when-then without placeholders") {
      val content = """
                      |Feature: Simple Test
                      |  Scenario: Basic user action
                      |    Given a user exists with name Simple
                      |    When the user requests a password reset
                      |    Then an email should be sent to simple@example.com
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Simple",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "an email should be sent to simple@example.com"
      )
    },
    test("run scenario with given-and-when") {
      val content = """
                      |Feature: Given-And-When Test
                      |  Scenario: User setup and action
                      |    Given a user exists with name {name:String}
                      |    And the user requests a password reset
                      |    When the user requests a password reset
                      |  Examples:
                      |    | name   |
                      |    | Tester |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Tester",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "the user requests a password reset"
      )
    },
    test("run scenario with given-when-and") {
      val content = """
                      |Feature: Given-When-And Test
                      |  Scenario: User action with logging
                      |    Given a user exists with name {name:String}
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |  Examples:
                      |    | name   |
                      |    | Logger |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Logger",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "the reset email is logged",
        results.head(2).output == ("Logged", 42)
      )
    },
    test("run scenario with given-and-when-and-then") {
      val content = """
                      |Feature: Given-And-When-And-Then Test
                      |  Scenario: Full user flow
                      |    Given a user exists with name {name:String}
                      |    And the user requests a password reset
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name  | email            |
                      |    | Full  | full@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 5,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Full",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "the user requests a password reset",
        results.head(3).step == "the reset email is logged",
        results.head(3).output == ("Logged", 42),
        results.head(4).step == "an email should be sent to full@example.com"
      )
    },
    test("run scenario with when-and-then-and") {
      val content = """
                      |Feature: When-And-Then-And Test
                      |  Scenario: Action and verification
                      |    When a user exists with name {name:String}
                      |    And the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |    And the reset email is logged
                      |  Examples:
                      |    | name    | email             |
                      |    | Action  | action@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 4,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Action",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "an email should be sent to action@example.com",
        results.head(3).step == "the reset email is logged",
        results.head(3).output == ("Logged", 42)
      )
    },
    test("run scenario with multiple and steps") {
      val content = """
                      |Feature: Multiple And Test
                      |  Scenario: Complex user flow
                      |    Given a user exists with name {name:String}
                      |    And the user requests a password reset
                      |    And the reset email is logged
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |    Then an email should be sent to {email:String}
                      |    And the reset email is logged
                      |  Examples:
                      |    | name   | email            |
                      |    | Multi  | multi@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 7,
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Multi",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "the reset email is logged",
        results.head(2).output == ("Logged", 42),
        results.head(3).step == "the user requests a password reset",
        results.head(4).step == "the reset email is logged",
        results.head(4).output == ("Logged", 42),
        results.head(5).step == "an email should be sent to multi@example.com",
        results.head(6).step == "the reset email is logged",
        results.head(6).output == ("Logged", 42)
      )
    },
    test("run scenario with background and multiple examples") {
      val content = """
                      |Feature: Background Multi Example Test
                      |  Background:
                      |    Given a user exists with name {name:String}
                      |  Scenario Outline: Reset with tags
                      |    When the user requests a password reset
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name  | email             |
                      |    | User1 | user1@example.com |
                      |    | User2 | user2@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results.forall(_.length == 3),
        results.forall(_.forall(_.succeeded)),
        results(0)(0).step == "a user exists with name User1",
        results(0)(1).step == "the user requests a password reset",
        results(0)(2).step == "an email should be sent to user1@example.com",
        results(1)(0).step == "a user exists with name User2",
        results(1)(1).step == "the user requests a password reset",
        results(1)(2).step == "an email should be sent to user2@example.com"
      )
    },
    test("run complex scenario with all features") {
      val content = """
                      |Feature: Complex Test
                      |  Background:
                      |    Given a user exists with name {name:String}
                      |    And the user requests a password reset
                      |  @Retry(2) @Flaky @Repeat(3)
                      |  Scenario Outline: Full feature test
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |    Then an email should be sent to {email:String}
                      |  Examples:
                      |    | name    | email             |
                      |    | Complex | complex@example.com |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 15, // 5 steps x 3 repeats
        results.head.forall(_.succeeded),
        results.head(0).step == "a user exists with name Complex",
        results.head(1).step == "the user requests a password reset",
        results.head(2).step == "the user requests a password reset",
        results.head(3).step == "the reset email is logged",
        results.head(4).step == "an email should be sent to complex@example.com",
        results.head(5).step == "a user exists with name Complex", // Repeat 2
        results.head(9).step == "an email should be sent to complex@example.com", // Repeat 2 end
        results.head(10).step == "a user exists with name Complex", // Repeat 3
        results.head(14).step == "an email should be sent to complex@example.com", // Repeat 3 end
        feature.scenarios.head.metadata.retryCount == 2,
        feature.scenarios.head.metadata.isFlaky,
        feature.scenarios.head.metadata.repeatCount == 3
      )
    }
  ).provideLayer(testEnv)
}

//package zio.bdd.example
//
//import zio.*
//import zio.test.*
//import zio.bdd.core.*
//import zio.bdd.gherkin.*
//
//object ScenarioRunnerTest extends ZIOSpecDefault {
//  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
//    ZLayer.succeed(new UserRepo {
//      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//    }) ++
//      ZLayer.fromZIO(
//        Ref.make(List.empty[String]).map { emailsRef =>
//          new EmailService {
//            def sendResetEmail(email: String) = emailsRef.update(email :: _)
//            def getSentEmails = emailsRef.get
//          }
//        }
//      ) ++
//      LogCollector.live ++
//      ZLayer.succeed(new Reporter {
//        def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//        def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
//      })
//
//  def spec: Spec[TestEnvironment with Scope, Any] = suite("ScenarioRunner")(
//    test("run valid scenario with background") {
//      val content = """
//                      |Feature: User Management
//                      |  Background:
//                      |    Given a user exists with name {name:String}
//                      |  Scenario: Successful password reset with logging
//                      |    When the user requests a password reset
//                      |    And the reset email is logged
//                      |    Then an email should be sent to {email:String}
//                      |  Examples:
//                      |    | name    | email             |
//                      |    | Default | default@example.com |
//                    """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 4,
//        results.head.forall(_.succeeded),
//        results.head(0).step == "a user exists with name Default",
//        results.head(0).output.isInstanceOf[User],
//        results.head(1).step == "the user requests a password reset",
//        results.head(1).output == (),
//        results.head(2).step == "the reset email is logged",
//        results.head(2).output == ("Logged", 42),
//        results.head(3).step == "an email should be sent to default@example.com",
//        results.head(3).output == (),
//        results.head.exists(_.logs.exists(_.toString.contains("Creating user with name: Default")))
//      )
//    },
//    test("run scenario outline with examples") {
//      val content = """
//                      |Feature: User Validation
//                      |  Scenario Outline: Validate reset emails
//                      |    Given a user exists with name {name:String}
//                      |    When the user requests a password reset
//                      |    Then an email should be sent to {email:String}
//                      |  Examples:
//                      |    | name  | email             |
//                      |    | Alice | alice@example.com |
//                      |    | Bob   | bob@example.com   |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
//      } yield assertTrue(
//        results.length == 2,
//        results.forall(_.length == 3),
//        results.forall(_.forall(_.succeeded)),
//        results(0)(0).step == "a user exists with name Alice",
//        results(0)(0).output.isInstanceOf[User],
//        results(0)(1).step == "the user requests a password reset",
//        results(0)(2).step == "an email should be sent to alice@example.com",
//        results(1)(0).step == "a user exists with name Bob",
//        results(1)(1).step == "the user requests a password reset",
//        results(1)(2).step == "an email should be sent to bob@example.com"
//      )
//    },
//    test("run scenario with retry on failure") {
//      val content = """
//                      |Feature: Retry Test
//                      |  @Retry(3)
//                      |  Scenario: Retry on failure
//                      |    Given a user exists with name {name:String}
//                      |    When the user requests a password reset
//                      |    Then an email should be sent to {email:String}
//                      |  Examples:
//                      |    | name | email            |
//                      |    | Fail | fail@example.com |
//                """.stripMargin
//      val failingSteps = new ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
//        Given("a user exists with name {name:String}") { (name: String) =>
//          ZIO.succeed(User(name, s"$name@example.com"))
//        }
//        When("the user requests a password reset") { (user: User) =>
//          ZIO.fail(new Exception("Reset failed"))
//        }
//        Then("an email should be sent to {email:String}") { (email: String) =>
//          ZIO.unit
//        }
//      }
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(failingSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 3,
//        results.head(0).succeeded,
//        !results.head(1).succeeded,
//        results.head(1).error.contains("Reset failed"),
//        !results.head(2).succeeded,
//        results.head(2).error.contains("Skipped due to prior failure"),
//        feature.scenarios.head.metadata.retryCount == 3
//      )
//    },
//    test("run scenario with repeat") {
//      val content = """
//                      |Feature: Repeat Test
//                      |  @Repeat(2)
//                      |  Scenario: Repeat execution
//                      |    Given a user exists with name {name:String}
//                      |    When the user requests a password reset
//                      |  Examples:
//                      |    | name   |
//                      |    | Repeat |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 4,
//        results.head.forall(_.succeeded),
//        results.head(0).step == "a user exists with name Repeat",
//        results.head(1).step == "the user requests a password reset",
//        results.head(2).step == "a user exists with name Repeat",
//        results.head(3).step == "the user requests a password reset",
//        feature.scenarios.head.metadata.repeatCount == 2
//      )
//    },
//    test("fail on unmatched step") {
//      val content = """
//                      |Feature: Unmatched Step Test
//                      |  Scenario: Unmatched step
//                      |    Given a user exists with name {name:String}
//                      |    When an undefined step runs
//                      |  Examples:
//                      |    | name    |
//                      |    | Default |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 2,
//        results.head(0).succeeded,
//        !results.head(1).succeeded,
//        results.head(1).error.contains("No step definition matches"),
//        results.head(1).step == "an undefined step runs"
//      )
//    },
//    test("fail on invalid input type") {
//      val content = """
//                      |Feature: Invalid Input Test
//                      |  Scenario: Invalid input type
//                      |    Given a user exists with name {name:String}
//                      |    When the user requests a password reset
//                      |    Then an email should be sent to {email:String}
//                      |  Examples:
//                      |    | name    | email |
//                      |    | Default | 123   |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 3,
//        results.head(0).succeeded,
//        results.head(1).succeeded,
//        !results.head(2).succeeded,
//        results.head(2).error.contains("Invalid input for Then step: expected a valid email address"), // Updated
//        results.head(2).step == "an email should be sent to 123"
//      )
//    },
//    test("run empty scenario") {
//      val content = """
//                      |Feature: Empty Test
//                      |  Scenario: Empty scenario
//                    """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.isEmpty
//      )
//    },
//    test("run scenario with flaky tag") {
//      val content = """
//                      |Feature: Flaky Test
//                      |  @Flaky
//                      |  Scenario: Flaky scenario
//                      |    Given a user exists with name {name:String}
//                      |    When the user requests a password reset
//                      |  Examples:
//                      |    | name  |
//                      |    | Flaky |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//      } yield assertTrue(
//        results.length == 1,
//        results.head.length == 2,
//        results.head.forall(_.succeeded),
//        results.head(0).step == "a user exists with name Flaky",
//        results.head(1).step == "the user requests a password reset",
//        feature.scenarios.head.metadata.isFlaky
//      )
//    },
//    test("run multiple scenarios") {
//      val content = """
//                      |Feature: Multi Scenario Test
//                      |  Background:
//                      |    Given a user exists with name {name:String}
//                      |  Scenario: Reset scenario
//                      |    When the user requests a password reset
//                      |    Then an email should be sent to {email:String}
//                      |  Scenario: Another reset scenario
//                      |    When the user requests a password reset
//                      |    And the reset email is logged
//                      |  Examples:
//                      |    | name    | email             |
//                      |    | Default | default@example.com |
//                """.stripMargin
//      for {
//        feature <- GherkinParser.parseFeature(content)
//        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
//      } yield assertTrue(
//        results.length == 2,
//        results(0).length == 3,
//        results(1).length == 3,
//        results.forall(_.forall(_.succeeded)),
//        results(0)(0).step == "a user exists with name Default",
//        results(0)(1).step == "the user requests a password reset",
//        results(0)(2).step == "an email should be sent to default@example.com",
//        results(1)(0).step == "a user exists with name Default",
//        results(1)(1).step == "the user requests a password reset",
//        results(1)(2).step == "the reset email is logged"
//      )
//    }
//  ).provideLayer(testEnv)
//}
//
////package zio.bdd.core
////
////import zio.*
////import zio.test.*
////import zio.bdd.core.*
////import zio.bdd.gherkin.*
////
////object ScenarioRunnerTest extends ZIOSpecDefault {
//////  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
//////    ZLayer.succeed(new UserRepo {
//////      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//////    }) ++
//////      ZLayer.succeed(new EmailService {
//////        private var emails: List[String] = Nil
//////        def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
//////        def getSentEmails = ZIO.succeed(emails)
//////      }) ++
//////      LogCollector.live ++
//////      ZLayer.succeed(new Reporter {
//////        def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
//////      })
////
////  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
////    ZLayer.succeed(new UserRepo {
////      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
////    }) ++
////      ZLayer.scoped(
////        ZIO.succeed {
////          new EmailService {
////            private val emailsRef = Ref.make(List.empty[String]).unsafeRun(Runtime.default)
////
////            def sendResetEmail(email: String) = emailsRef.update(email :: _)
////
////            def getSentEmails = emailsRef.get
////          }
////        }
////      ) ++
////      LogCollector.live ++
////      ZLayer.succeed(new Reporter {
////        def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
////
////        def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = ZIO.unit
////
////        def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit
////
////        def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit
////
////        def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit
////
////        def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
////      })
////
////  def spec: Spec[TestEnvironment with Scope, Any] = suite("ScenarioRunner")(
////    test("run valid scenario with background and examples") {
////      val content = """
////                      |Feature: User Management
////                      |  Background:
////                      |    Given a user exists with name {name:String}
////                      |  Scenario: Successful password reset with logging
////                      |    When the user requests a password reset
////                      |    And the reset email is logged
////                      |    Then an email should be sent to {email:String}
////                      |  Examples:
////                      |    | name    | email             |
////                      |    | Default | default@example.com |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 4,
////        results.head.forall(_.succeeded),
////        results.head(0).step == "a user exists with name Default", // No quotes
////        results.head(0).output.isInstanceOf[User],
////        results.head(1).step == "the user requests a password reset",
////        results.head(1).output == (),
////        results.head(2).step == "the reset email is logged",
////        results.head(2).output == ("Logged", 42),
////        results.head(3).step == "an email should be sent to default@example.com", // No quotes
////        results.head(3).output == (),
////        results.head.exists(_.logs.exists(_.toString.contains("Creating user with name: Default")))
////      )
////    },
////    test("run scenario outline with examples") {
////      val content = """
////                      |Feature: User Validation
////                      |  Scenario Outline: Validate reset emails
////                      |    Given a user exists with name {name:String}
////                      |    When the user requests a password reset
////                      |    Then an email should be sent to {email:String}
////                      |  Examples:
////                      |    | name  | email             |
////                      |    | Alice | alice@example.com |
////                      |    | Bob   | bob@example.com   |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
////      } yield assertTrue(
////        results.length == 2,
////        results.forall(_.length == 3),
////        results.forall(_.forall(_.succeeded)),
////        results(0)(0).step == "a user exists with name Alice",
////        results(0)(0).output.isInstanceOf[User],
////        results(0)(1).step == "the user requests a password reset",
////        results(0)(2).step == "an email should be sent to alice@example.com",
////        results(1)(0).step == "a user exists with name Bob",
////        results(1)(1).step == "the user requests a password reset",
////        results(1)(2).step == "an email should be sent to bob@example.com"
////      )
////    },
////    test("run scenario with retry on failure") {
////      val content = """
////                      |Feature: Retry Test
////                      |  @Retry(3)
////                      |  Scenario: Retry on failure
////                      |    Given a user exists with name {name:String}
////                      |    When the user requests a password reset
////                      |    Then an email should be sent to {email:String}
////                      |  Examples:
////                      |    | name | email            |
////                      |    | Fail | fail@example.com |
////                """.stripMargin
////      val failingSteps = new ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
////        Given("a user exists with name {name:String}") { (name: String) =>
////          ZIO.succeed(User(name, s"$name@example.com"))
////        }
////        When("the user requests a password reset") { (user: User) =>
////          ZIO.fail(new Exception("Reset failed"))
////        }
////        Then("an email should be sent to {email:String}") { (email: String) =>
////          ZIO.unit
////        }
////      }
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(failingSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 3,
////        results.head(0).succeeded,
////        !results.head(1).succeeded,
////        results.head(1).error.contains("Reset failed"),
////        !results.head(2).succeeded,
////        results.head(2).error.contains("Skipped due to prior failure"),
////        feature.scenarios.head.metadata.retryCount == 3
////      )
////    },
////    test("run scenario with repeat") {
////      val content = """
////                      |Feature: Repeat Test
////                      |  @Repeat(2)
////                      |  Scenario: Repeat execution
////                      |    Given a user exists with name {name:String}
////                      |    When the user requests a password reset
////                      |  Examples:
////                      |    | name   |
////                      |    | Repeat |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 4,
////        results.head.forall(_.succeeded),
////        results.head(0).step == "a user exists with name Repeat",
////        results.head(1).step == "the user requests a password reset",
////        results.head(2).step == "a user exists with name Repeat",
////        results.head(3).step == "the user requests a password reset",
////        feature.scenarios.head.metadata.repeatCount == 2
////      )
////    },
////    test("fail on unmatched step") {
////      val content = """
////                      |Feature: Unmatched Step Test
////                      |  Scenario: Unmatched step
////                      |    Given a user exists with name {name:String}
////                      |    When an undefined step runs
////                      |  Examples:
////                      |    | name    |
////                      |    | Default |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 2,
////        results.head(0).succeeded,
////        !results.head(1).succeeded,
////        results.head(1).error.contains("No step definition matches"),
////        results.head(1).step == "an undefined step runs"
////      )
////    },
////    test("fail on invalid input type") {
////      val content = """
////                      |Feature: Invalid Input Test
////                      |  Scenario: Invalid input type
////                      |    Given a user exists with name {name:String}
////                      |    When the user requests a password reset
////                      |    Then an email should be sent to {email:String}
////                      |  Examples:
////                      |    | name    | email |
////                      |    | Default | 123   |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 3,
////        results.head(0).succeeded,
////        results.head(1).succeeded,
////        !results.head(2).succeeded,
////        results.head(2).error.contains("Invalid input for Then step"),
////        results.head(2).step == "an email should be sent to 123"
////      )
////    },
////    test("run empty scenario") {
////      val content = """
////                      |Feature: Empty Test
////                      |  Scenario: Empty scenario
////                    """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.isEmpty
////      )
////    },
////    test("run scenario with flaky tag") {
////      val content = """
////                      |Feature: Flaky Test
////                      |  @Flaky
////                      |  Scenario: Flaky scenario
////                      |    Given a user exists with name {name:String}
////                      |    When the user requests a password reset
////                      |  Examples:
////                      |    | name  |
////                      |    | Flaky |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
////      } yield assertTrue(
////        results.length == 1,
////        results.head.length == 2,
////        results.head.forall(_.succeeded),
////        results.head(0).step == "a user exists with name Flaky",
////        results.head(1).step == "the user requests a password reset",
////        feature.scenarios.head.metadata.isFlaky
////      )
////    },
////    test("run multiple scenarios") {
////      val content = """
////                      |Feature: Multi Scenario Test
////                      |  Background:
////                      |    Given a user exists with name {name:String}
////                      |  Scenario: Reset scenario
////                      |    When the user requests a password reset
////                      |    Then an email should be sent to {email:String}
////                      |  Scenario: Another reset scenario
////                      |    When the user requests a password reset
////                      |    And the reset email is logged
////                      |  Examples:
////                      |    | name    | email             |
////                      |    | Default | default@example.com |
////                """.stripMargin
////      for {
////        feature <- GherkinParser.parseFeature(content)
////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
////      } yield assertTrue(
////        results.length == 2,
////        results(0).length == 3,
////        results(1).length == 3,
////        results.forall(_.forall(_.succeeded)),
////        results(0)(0).step == """a user exists with name Default"""",
////        results(0)(1).step == "the user requests a password reset",
////        results(0)(2).step == """an email should be sent to default@example.com"""",
////        results(1)(0).step == """a user exists with name Default"""",
////        results(1)(1).step == "the user requests a password reset",
////        results(1)(2).step == "the reset email is logged"
////      )
////    }
////  ).provideLayer(testEnv)
////}
////
//////package zio.bdd.core
//////
//////import zio.*
//////import zio.test.*
//////import zio.bdd.core.*
//////import zio.bdd.gherkin.*
//////
//////object ScenarioRunnerTest extends ZIOSpecDefault {
//////  val testEnv: ZLayer[Any, Nothing, UserRepo & EmailService & LogCollector & Reporter] =
//////    ZLayer.succeed(new UserRepo {
//////      def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//////    }) ++
//////      ZLayer.succeed(new EmailService {
//////        private var emails: List[String] = Nil
//////        def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
//////        def getSentEmails = ZIO.succeed(emails)
//////      }) ++
//////      LogCollector.live ++
//////      ZLayer.succeed(new Reporter {
//////        def startFeature(feature: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endFeature(feature: String, results: List[List[StepResult]]): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def startScenario(scenario: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endScenario(scenario: String, results: List[StepResult]): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def startStep(step: String): ZIO[Any, Nothing, Unit] = ZIO.unit
//////        def endStep(step: String, result: StepResult): ZIO[Any, Nothing, Unit] = ZIO.unit
//////      })
//////
//////  def spec: Spec[TestEnvironment with Scope, Any] = suite("ScenarioRunner")(
//////    test("run valid scenario with background") {
//////      val content = """
//////                      |Feature: User Management
//////                      |  Background:
//////                      |    Given a user exists with name "Default"
//////                      |  Scenario: Successful password reset with logging
//////                      |    When the user requests a password reset
//////                      |    And the reset email is logged
//////                      |    Then an email should be sent to "default@example.com"
//////                    """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1, // One scenario
//////        results.head.length == 4, // 4 steps (1 background + 3 scenario)
//////        results.head.forall(_.succeeded),
//////        results.head(0).step == """a user exists with name "Default"""",
//////        results.head(0).output.isInstanceOf[User],
//////        results.head(1).step == "the user requests a password reset",
//////        results.head(1).output == (),
//////        results.head(2).step == "the reset email is logged",
//////        results.head(2).output == ("Logged", 42),
//////        results.head(3).step == """an email should be sent to "default@example.com"""",
//////        results.head(3).output == (),
//////        results.head.exists(_.logs.exists(_.toString.contains("Creating user with name: Default")))
//////      )
//////    },
//////    test("run scenario outline with examples") {
//////      val content = """
//////                      |Feature: User Validation
//////                      |  Scenario Outline: Validate reset emails
//////                      |    Given a user exists with name {string}
//////                      |    When the user requests a password reset
//////                      |    Then an email should be sent to {string}
//////                      |  Examples:
//////                      |    | name  | email             |
//////                      |    | Alice | alice@example.com |
//////                      |    | Bob   | bob@example.com   |
//////                """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
//////      } yield assertTrue(
//////        results.length == 2, // 2 example rows
//////        results.forall(_.length == 3), // 3 steps per scenario
//////        results.forall(_.forall(_.succeeded)),
//////        results(0)(0).step == "a user exists with name Alice",
//////        results(0)(0).output.isInstanceOf[User],
//////        results(0)(1).step == "the user requests a password reset",
//////        results(0)(2).step == "an email should be sent to alice@example.com",
//////        results(1)(0).step == "a user exists with name Bob",
//////        results(1)(1).step == "the user requests a password reset",
//////        results(1)(2).step == "an email should be sent to bob@example.com"
//////      )
//////    },
////////    test("fail on mismatched placeholder count") {
////////      val content = """
////////                      |Feature: Mismatch Test
////////                      |  Scenario Outline: Too many placeholders
////////                      |    Given a user exists with name {string} and id {string}
////////                      |  Examples:
////////                      |    | name  |
////////                      |    | Alice |
////////                """.stripMargin
////////      for {
////////        feature <- GherkinParser.parseFeature(content)
////////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1).exit
////////      } yield assertTrue(results.isFailure && results match {
////////        case Exit.Failure(cause) => cause.untraced.toString.contains("Step 'a user exists with name {string} and id {string}' has 2 placeholders but only 1 example values")
////////        case _                   => false
////////      })
////////    },
//////    test("run scenario with retry on failure") {
//////      val content = """
//////                      |Feature: Retry Test
//////                      |  @Retry(3)
//////                      |  Scenario: Retry on failure
//////                      |    Given a user exists with name "Fail"
//////                      |    When the user requests a password reset
//////                      |    Then an email should be sent to "fail@example.com"
//////                """.stripMargin
//////      val failingSteps = new ZIOSteps.Default[UserRepo & EmailService & LogCollector] {
//////        Given("a user exists with name {string}") { (name: String) =>
//////          ZIO.succeed(User(name, s"$name@example.com"))
//////        }
//////        When("the user requests a password reset") { (user: User) =>
//////          ZIO.fail(new Exception("Reset failed"))
//////        }
//////        Then("an email should be sent to {string}") { (email: String) =>
//////          ZIO.unit
//////        }
//////      }
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(failingSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.length == 3,
//////        results.head(0).succeeded,
//////        !results.head(1).succeeded,
//////        results.head(1).error.contains("Reset failed"),
//////        !results.head(2).succeeded,
//////        results.head(2).error.contains("Skipped due to prior failure"),
//////        feature.scenarios.head.metadata.retryCount == 3
//////      )
//////    },
//////    test("run scenario with repeat") {
//////      val content = """
//////                      |Feature: Repeat Test
//////                      |  @Repeat(2)
//////                      |  Scenario: Repeat execution
//////                      |    Given a user exists with name "Repeat"
//////                      |    When the user requests a password reset
//////                """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.length == 4, // 2 steps x 2 repeats
//////        results.head.forall(_.succeeded),
//////        results.head(0).step == "a user exists with name \"Repeat\"",
//////        results.head(1).step == "the user requests a password reset",
//////        results.head(2).step == "a user exists with name \"Repeat\"",
//////        results.head(3).step == "the user requests a password reset",
//////        feature.scenarios.head.metadata.repeatCount == 2
//////      )
//////    },
//////    test("fail on unmatched step") {
//////      val content = """
//////                      |Feature: Unmatched Step Test
//////                      |  Scenario: Unmatched step
//////                      |    Given a user exists with name "Default"
//////                      |    When an undefined step runs
//////                    """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.length == 2,
//////        results.head(0).succeeded,
//////        !results.head(1).succeeded,
//////        results.head(1).error.contains("No step definition matches"),
//////        results.head(1).step == "an undefined step runs"
//////      )
//////    },
//////    test("fail on invalid input type") {
//////      val content = """
//////                      |Feature: Invalid Input Test
//////                      |  Scenario: Invalid input type
//////                      |    Given a user exists with name "Default"
//////                      |    When the user requests a password reset
//////                      |    Then an email should be sent to 123
//////                """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.length == 3,
//////        results.head(0).succeeded,
//////        results.head(1).succeeded,
//////        !results.head(2).succeeded,
//////        results.head(2).error.contains("Invalid input for Then step"),
//////        results.head(2).step == "an email should be sent to 123"
//////      )
//////    },
//////    test("run empty scenario") {
//////      val content = """
//////                      |Feature: Empty Test
//////                      |  Scenario: Empty scenario
//////                    """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.isEmpty
//////      )
//////    },
//////    test("run scenario with flaky tag") {
//////      val content = """
//////                      |Feature: Flaky Test
//////                      |  @Flaky
//////                      |  Scenario: Flaky scenario
//////                      |    Given a user exists with name "Flaky"
//////                      |    When the user requests a password reset
//////                    """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 1)
//////      } yield assertTrue(
//////        results.length == 1,
//////        results.head.length == 2,
//////        results.head.forall(_.succeeded),
//////        results.head(0).step == """a user exists with name "Flaky"""",
//////        results.head(1).step == "the user requests a password reset",
//////        feature.scenarios.head.metadata.isFlaky
//////      )
//////    },
//////    test("run multiple scenarios") {
//////      val content = """
//////                      |Feature: Multi Scenario Test
//////                      |  Background:
//////                      |    Given a user exists with name "Default"
//////                      |  Scenario: Reset scenario
//////                      |    When the user requests a password reset
//////                      |    Then an email should be sent to "default@example.com"
//////                      |  Scenario: Another reset scenario
//////                      |    When the user requests a password reset
//////                      |    And the reset email is logged
//////                    """.stripMargin
//////      for {
//////        feature <- GherkinParser.parseFeature(content)
//////        results <- ScenarioRunner.runScenarios(UserSteps, feature, 2)
//////      } yield assertTrue(
//////        results.length == 2,
//////        results(0).length == 3, // Background + 2 steps
//////        results(1).length == 3, // Background + 2 steps
//////        results.forall(_.forall(_.succeeded)),
//////        results(0)(0).step == """a user exists with name "Default"""",
//////        results(0)(1).step == "the user requests a password reset",
//////        results(0)(2).step == """an email should be sent to "default@example.com"""",
//////        results(1)(0).step == """a user exists with name "Default"""",
//////        results(1)(1).step == "the user requests a password reset",
//////        results(1)(2).step == "the reset email is logged"
//////      )
//////    }
//////  ).provideLayer(testEnv)
//////}