package zio.bdd.core

import zio.test.*
import zio.test.Assertion.*
import zio.bdd.gherkin.*
import zio.bdd.core.step.ZIOSteps
import zio.*
import zio.schema.{DeriveSchema, Schema}

object FeatureExecutorSpec extends ZIOSpecDefault {

  object Domain {
    case class User(name: String, age: Int, role: String)

    case class SystemState(users: Map[String, User] = Map.empty, loggedInUser: Option[String] = None)

    implicit val cartSchema: Schema[User]               = DeriveSchema.gen[User]
    implicit val systemStateSchema: Schema[SystemState] = DeriveSchema.gen[SystemState]
  }

  import Domain._

  private class UserSteps extends ZIOSteps[Any, SystemState] {
    Given("a system is running") {
      ScenarioContext.update(_ => SystemState())
    }

    Given("a user named " / string / " with age " / int / " and role " / string) {
      (name: String, age: Int, role: String) =>
        for {
          state <- ScenarioContext.get
          _     <- ScenarioContext.update(_.copy(users = state.users + (name -> User(name, age, role))))
        } yield ()
    }

    Given("the following users are created" / table[User]) { (users: List[User]) =>
      for {
        state       <- ScenarioContext.get
        updatedUsers = users.map(user => user.name -> user).toMap
        _           <- ScenarioContext.update(_.copy(users = state.users ++ updatedUsers))
      } yield ()
    }

    When("the user " / string / " logs in") { (name: String) =>
      for {
        state <- ScenarioContext.get
        _ <- if (state.users.contains(name)) ScenarioContext.update(_.copy(loggedInUser = Some(name)))
             else ZIO.fail(new RuntimeException(s"User $name does not exist"))
      } yield ()
    }

    Then("the user " / string / " should exist") { (name: String) =>
      for {
        state <- ScenarioContext.get
        _     <- Assertions.assertTrue(state.users.contains(name), s"User $name should exist")
      } yield ()
    }

    Then("the logged-in user should be " / string) { (expectedName: String) =>
      for {
        state <- ScenarioContext.get
        _     <- Assertions.assertEquals(state.loggedInUser, Some(expectedName), "Logged-in user mismatch")
      } yield ()
    }

    Then("an error occurs") {
      ZIO.fail(new RuntimeException("Simulated error")).unit
    }
  }

  private val testFile = "test.feature"
  private val steps    = new UserSteps {}

  def spec: Spec[TestEnvironment & Scope, Any] = suite("FeatureExecutor")(
    test("execute basic feature with single scenario") {
      val content = """
                      |Feature: User Management
                      |  Scenario: Create user
                      |    Given a system is running
                      |    Given a user named "Alice" with age 30 and role "admin"
                      |    Then the user "Alice" should exist
      """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
      } yield assertTrue(results.head.isPassed)
    },
    test("execute feature with background") {
      val content = """
                      |Feature: User Authentication
                      |  Background:
                      |    Given a system is running
                      |    Given a user named "Alice" with age 30 and role "admin"
                      |  Scenario: Login
                      |    When the user "Alice" logs in
                      |    Then the logged-in user should be "Alice"
      """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
      } yield assertTrue(results.head.isPassed)
    },
    test("execute scenario with ignore tag") {
      val content = """
                      |Feature: Payment Processing
                      |  @ignore
                      |  Scenario: Process payment
                      |    Given a system is running
                      |    Then the user "Alice" should exist
      """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
      } yield assertTrue(results.head.scenarioResults.head.isIgnored)
    },
    test("execute scenario outline with examples") {
      val content = """
                      |Feature: Login Validation
                      |  Scenario Outline: Validate credentials
                      |    Given a system is running
                      |    Given a user named <name> with age 25 and role "user"
                      |    When the user <name> logs in
                      |    Then the logged-in user should be <name>
                      |  Examples:
                      |    | name  |
                      |    | Alice |
                      |    | Bob   |
      """.stripMargin
      for {
        feature        <- GherkinParser.parseFeature(content, testFile)
        results        <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
        scenarioResults = results.head.scenarioResults
      } yield assertTrue(scenarioResults.length == 2) &&
        assertTrue(scenarioResults.forall(_.isPassed))
    },
    test("execute step with multi-row data table") {
      val content = """
                      |Feature: Batch User Creation
                      |  Scenario: Create multiple users
                      |    Given a system is running
                      |    Given the following users are created
                      |      | name  | age | role  |
                      |      | Alice | 30  | admin |
                      |      | Bob   | 25  | user  |
                      |    Then the user "Alice" should exist
                      |    And the user "Bob" should exist
      """.stripMargin

      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
      } yield assertTrue(results.head.isPassed)
    },
    test("execute feature with multiple scenarios") {
      val content = """
                      |Feature: User Operations
                      |  Scenario: Create user
                      |    Given a system is running
                      |    Given a user named "Alice" with age 30 and role "admin"
                      |    Then the user "Alice" should exist
                      |  Scenario: Login user
                      |    Given a system is running
                      |    Given a user named "Bob" with age 25 and role "user"
                      |    When the user "Bob" logs in
                      |    Then the logged-in user should be "Bob"
      """.stripMargin

      for {
        feature        <- GherkinParser.parseFeature(content, testFile)
        results        <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
        scenarioResults = results.head.scenarioResults
      } yield assertTrue(scenarioResults.length == 2) &&
        assertTrue(scenarioResults.forall(_.isPassed))
    },
    test("execute scenario with error") {
      val content = """
                      |Feature: Error Handling
                      |  Scenario: Trigger error
                      |    Given a system is running
                      |    Then an error occurs
      """.stripMargin

      for {
        feature      <- GherkinParser.parseFeature(content, testFile)
        results      <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
        featureResult = results.head
      } yield assertTrue(!featureResult.isPassed) &&
        assert(featureResult.error)(isSome(isSubtype[RuntimeException](anything))) &&
        assert(featureResult.error.map(_.getMessage))(isSome(equalTo("Simulated error")))
    },
    test("execute feature with background and data table") {
      val content = """
                      |Feature: User Setup
                      |  Background:
                      |    Given a system is running
                      |  Scenario: Add users with details
                      |    Given the following users are created
                      |      | name  | age | role  |
                      |      | Alice | 30  | admin |
                      |    Then the user "Alice" should exist
      """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
      } yield assertTrue(results.head.isPassed)
    }
  )
}
