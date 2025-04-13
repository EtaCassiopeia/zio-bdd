package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.io.File

object GherkinParserSpec extends ZIOSpecDefault {

  val testFile = "test.feature"

  def spec: Spec[TestEnvironment & Scope, Any] = suite("GherkinParser")(
    test("parse basic feature with single scenario") {
      val content = """
                      |Feature: User Management
                      |  Scenario: Create user
                      |    Given a system is running
                      |    When a user is created
                      |    Then the user exists
    """.stripMargin
      checkParse(content, testFile) { feature =>
        assertTrue(
          feature.name == "User Management",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.background.isEmpty,
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Create user",
          feature.scenarios.head.file.contains(testFile),
          feature.scenarios.head.line.contains(3),
          feature.scenarios.head.steps == List(
            Step(StepType.GivenStep, "a system is running", file = Some(testFile), line = Some(4)),
            Step(StepType.WhenStep, "a user is created", file = Some(testFile), line = Some(5)),
            Step(StepType.ThenStep, "the user exists", file = Some(testFile), line = Some(6))
          ),
          feature.scenarios.head.examples.isEmpty
        )
      }
    },
    test("parse feature with background") {
      val content = """
                      |Feature: User Authentication
                      |  Background:
                      |    Given a system is running
                      |  Scenario: Login
                      |    When user logs in
                      |    Then user is authenticated
        """.stripMargin
      checkParse(content, testFile) { feature =>
        assertTrue(
          feature.name == "User Authentication",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.background == List(
            Step(StepType.GivenStep, "a system is running", file = Some(testFile), line = Some(4))
          ),
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Login",
          feature.scenarios.head.file.contains(testFile),
          feature.scenarios.head.line.contains(5),
          feature.scenarios.head.steps == List(
            Step(StepType.WhenStep, "user logs in", file = Some(testFile), line = Some(6)),
            Step(StepType.ThenStep, "user is authenticated", file = Some(testFile), line = Some(7))
          )
        )
      }
    },
    test("parse scenario with tags") {
      val content = """
                      |Feature: Payment Processing
                      |  @ignore
                      |  Scenario: Process payment
                      |    Given a payment request
                      |    When payment is processed
                      |    Then payment succeeds
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Payment Processing",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Process payment",
          scenario.file.contains(testFile),
          scenario.line.contains(4),
          scenario.isIgnored,
          scenario.steps == List(
            Step(StepType.GivenStep, "a payment request", file = Some(testFile), line = Some(5)),
            Step(StepType.WhenStep, "payment is processed", file = Some(testFile), line = Some(6)),
            Step(StepType.ThenStep, "payment succeeds", file = Some(testFile), line = Some(7))
          )
        )
      }
    },
    test("parse scenario outline with examples") {
      val content = """
                      |Feature: Login Validation
                      |  Scenario Outline: Validate credentials
                      |    Given user <name> exists
                      |    When user enters password <password>
                      |    Then login <result>
                      |  Examples:
                      |    | name  | password | result  |
                      |    | Alice | pass123  | succeeds |
                      |    | Bob   | wrong    | fails    |
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Login Validation",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Validate credentials",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps == List(
            Step(StepType.GivenStep, "user <name> exists", file = Some(testFile), line = Some(4)),
            Step(StepType.WhenStep, "user enters password <password>", file = Some(testFile), line = Some(5)),
            Step(StepType.ThenStep, "login <result>", file = Some(testFile), line = Some(6))
          ),
          scenario.examples.length == 2,
          scenario.examples.head.data == Map(
            "name"     -> "Alice",
            "password" -> "pass123",
            "result"   -> "succeeds"
          ),
          scenario.examples(1).data == Map(
            "name"     -> "Bob",
            "password" -> "wrong",
            "result"   -> "fails"
          )
        )
      }
    },
    test("parse scenario outline with typed placeholders ({name:String})") {
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
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "User Validation",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Validate reset emails",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps == List(
            Step(StepType.GivenStep, "a user exists with name {name:String}", file = Some(testFile), line = Some(4)),
            Step(StepType.WhenStep, "the user requests a password reset", file = Some(testFile), line = Some(5)),
            Step(StepType.ThenStep, "an email should be sent to {email:String}", file = Some(testFile), line = Some(6))
          ),
          scenario.examples.length == 2,
          scenario.examples.head.data == Map(
            "name"  -> "Alice",
            "email" -> "alice@example.com"
          ),
          scenario.examples(1).data == Map(
            "name"  -> "Bob",
            "email" -> "bob@example.com"
          )
        )
      }
    },
    test("parse step with single-row data table") {
      val content = """
                      |Feature: User Login
                      |  Scenario: Login with credentials
                      |    Given the user enters credentials
                      |      | username | password |
                      |      | admin    | pass123  |
                      |    When the user submits the form
                      |    Then the user is logged in
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "User Login",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Login with credentials",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps.length == 3,
          scenario.steps(0) == Step(
            StepType.GivenStep,
            "the user enters credentials",
            Some(
              DataTable(
                headers = List("username", "password"),
                rows = List(DataTableRow(List("admin", "pass123")))
              )
            ),
            Some(testFile),
            Some(4)
          ),
          scenario.steps(1) == Step(
            StepType.WhenStep,
            "the user submits the form",
            None,
            file = Some(testFile),
            line = Some(7)
          ),
          scenario
            .steps(2) == Step(StepType.ThenStep, "the user is logged in", None, file = Some(testFile), line = Some(8)),
          scenario.examples.isEmpty
        )
      }
    },
    test("parse step with multi-row data table") {
      val content = """
                      |Feature: Batch Processing
                      |  Scenario: Process multiple users
                      |    Given the system processes users
                      |      | name  | age | role  |
                      |      | Alice | 30  | admin |
                      |      | Bob   | 25  | user  |
                      |      | Carol | 35  | guest |
                      |    Then the users are processed
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Batch Processing",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Process multiple users",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps.length == 2,
          scenario.steps(0) == Step(
            StepType.GivenStep,
            "the system processes users",
            Some(
              DataTable(
                headers = List("name", "age", "role"),
                rows = List(
                  DataTableRow(List("Alice", "30", "admin")),
                  DataTableRow(List("Bob", "25", "user")),
                  DataTableRow(List("Carol", "35", "guest"))
                )
              )
            ),
            Some(testFile),
            Some(4)
          ),
          scenario.steps(1) == Step(
            StepType.ThenStep,
            "the users are processed",
            None,
            file = Some(testFile),
            line = Some(9)
          ),
          scenario.examples.isEmpty
        )
      }
    },
    test("parse multiple steps with data tables in a scenario") {
      val content = """
                      |Feature: Data-Driven Actions
                      |  Scenario: Perform actions with data
                      |    Given users are added
                      |      | name  | id  |
                      |      | Alice | 001 |
                      |    When actions are performed
                      |      | action | target |
                      |      | login  | Alice  |
                      |    Then results are verified
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Data-Driven Actions",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Perform actions with data",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps.length == 3,
          scenario.steps(0) == Step(
            StepType.GivenStep,
            "users are added",
            Some(
              DataTable(
                headers = List("name", "id"),
                rows = List(DataTableRow(List("Alice", "001")))
              )
            ),
            Some(testFile),
            Some(4)
          ),
          scenario.steps(1) == Step(
            StepType.WhenStep,
            "actions are performed",
            Some(
              DataTable(
                headers = List("action", "target"),
                rows = List(DataTableRow(List("login", "Alice")))
              )
            ),
            Some(testFile),
            Some(7)
          ),
          scenario
            .steps(2) == Step(StepType.ThenStep, "results are verified", None, file = Some(testFile), line = Some(10)),
          scenario.examples.isEmpty
        )
      }
    },
    test("parse step with data table containing empty cells") {
      val content = """
                      |Feature: Optional Data
                      |  Scenario: Handle optional fields
                      |    Given the system processes optional data
                      |      | name  | phone |
                      |      | Alice |       |
                      |      | Bob   | 12345 |
                      |    Then the data is stored
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Optional Data",
          feature.file.contains(testFile),
          feature.line.contains(2),
          scenario.name == "Handle optional fields",
          scenario.file.contains(testFile),
          scenario.line.contains(3),
          scenario.steps.length == 2,
          scenario.steps(0) == Step(
            StepType.GivenStep,
            "the system processes optional data",
            Some(
              DataTable(
                headers = List("name", "phone"),
                rows = List(
                  DataTableRow(List("Alice", "")),
                  DataTableRow(List("Bob", "12345"))
                )
              )
            ),
            Some(testFile),
            Some(4)
          ),
          scenario
            .steps(1) == Step(StepType.ThenStep, "the data is stored", None, file = Some(testFile), line = Some(8)),
          scenario.examples.isEmpty
        )
      }
    },
    test("parse feature with background and step with data table") {
      val content = """
                      |Feature: User Setup
                      |  Background:
                      |    Given the system is initialized
                      |  Scenario: Add users with details
                      |    Given users are configured
                      |      | name  | role  |
                      |      | Alice | admin |
                      |    Then the configuration is applied
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "User Setup",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.background == List(
            Step(StepType.GivenStep, "the system is initialized", None, file = Some(testFile), line = Some(4))
          ),
          scenario.name == "Add users with details",
          scenario.file.contains(testFile),
          scenario.line.contains(5),
          scenario.steps.length == 2,
          scenario.steps(0) == Step(
            StepType.GivenStep,
            "users are configured",
            Some(
              DataTable(
                headers = List("name", "role"),
                rows = List(DataTableRow(List("Alice", "admin")))
              )
            ),
            Some(testFile),
            Some(6)
          ),
          scenario.steps(1) == Step(
            StepType.ThenStep,
            "the configuration is applied",
            None,
            file = Some(testFile),
            line = Some(9)
          ),
          scenario.examples.isEmpty
        )
      }
    },
    test("parse feature with multiple scenarios") {
      val content = """
                      |Feature: User Operations
                      |  Scenario: Create user
                      |    Given system ready
                      |    When create user
                      |    Then user exists
                      |  Scenario: Delete user
                      |    Given user exists
                      |    When delete user
                      |    Then user gone
        """.stripMargin
      checkParse(content, testFile) { feature =>
        assertTrue(
          feature.name == "User Operations",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.scenarios.length == 2,
          feature.scenarios.head.name == "Create user",
          feature.scenarios.head.file.contains(testFile),
          feature.scenarios.head.line.contains(3),
          feature.scenarios.head.steps == List(
            Step(StepType.GivenStep, "system ready", file = Some(testFile), line = Some(4)),
            Step(StepType.WhenStep, "create user", file = Some(testFile), line = Some(5)),
            Step(StepType.ThenStep, "user exists", file = Some(testFile), line = Some(6))
          ),
          feature.scenarios(1).name == "Delete user",
          feature.scenarios(1).file.contains(testFile),
          feature.scenarios(1).line.contains(7),
          feature.scenarios(1).steps == List(
            Step(StepType.GivenStep, "user exists", file = Some(testFile), line = Some(8)),
            Step(StepType.WhenStep, "delete user", file = Some(testFile), line = Some(9)),
            Step(StepType.ThenStep, "user gone", file = Some(testFile), line = Some(10))
          )
        )
      }
    },
    test("parse feature with all elements") {
      val content = """
                      |Feature: Complex Feature
                      |  Background:
                      |    Given system running
                      |    And database ready
                      |  Scenario Outline: Complex scenario
                      |    Given user <name>
                      |    When action <action>
                      |    Then result <result>
                      |    And cleanup done
                      |  Examples:
                      |    | name  | action | result |
                      |    | Alice | login  | ok     |
        """.stripMargin
      checkParse(content, testFile) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Complex Feature",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.background == List(
            Step(StepType.GivenStep, "system running", file = Some(testFile), line = Some(4)),
            Step(StepType.AndStep, "database ready", file = Some(testFile), line = Some(5))
          ),
          scenario.file.contains(testFile),
          scenario.line.contains(6),
          scenario.steps == List(
            Step(StepType.GivenStep, "user <name>", file = Some(testFile), line = Some(7)),
            Step(StepType.WhenStep, "action <action>", file = Some(testFile), line = Some(8)),
            Step(StepType.ThenStep, "result <result>", file = Some(testFile), line = Some(9)),
            Step(StepType.AndStep, "cleanup done", file = Some(testFile), line = Some(10))
          ),
          scenario.examples.head.data == Map(
            "name"   -> "Alice",
            "action" -> "login",
            "result" -> "ok"
          )
        )
      }
    },
    test("parse specific user password reset feature") {
      val content = """
                      |Feature: User Password Reset
                      |  Background:
                      |    Given a user exists with name "Default"
                      |  Scenario: Successful password reset with logging
                      |    When the user requests a password reset
                      |    And the reset email is logged
                      |    Then an email should be sent to "default@example.com"
        """.stripMargin
      checkParse(content, testFile) { feature =>
        assertTrue(
          feature.name == "User Password Reset",
          feature.file.contains(testFile),
          feature.line.contains(2),
          feature.background == List(
            Step(StepType.GivenStep, """a user exists with name "Default"""", file = Some(testFile), line = Some(4))
          ),
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Successful password reset with logging",
          feature.scenarios.head.file.contains(testFile),
          feature.scenarios.head.line.contains(5),
          feature.scenarios.head.steps == List(
            Step(StepType.WhenStep, "the user requests a password reset", file = Some(testFile), line = Some(6)),
            Step(StepType.AndStep, "the reset email is logged", file = Some(testFile), line = Some(7)),
            Step(
              StepType.ThenStep,
              """an email should be sent to "default@example.com"""",
              file = Some(testFile),
              line = Some(8)
            )
          ),
          feature.scenarios.head.examples.isEmpty
        )
      }
    },
    test("parse feature with comments at multiple levels") {
      val content = """
                      |# File-level comment about password reset
                      |Feature: User Password Reset with Comments
                      |  # Background comment explaining setup
                      |  Background:
                      |    # Comment before Given step
                      |    Given a user exists with name "Default"
                      |  Scenario: Successful password reset with logging
                      |    # Comment before When step
                      |    When the user requests a password reset
                      |    # Comment explaining logging
                      |    And the reset email is logged
                      |    # Comment before Then step
                      |    Then an email should be sent to "default@example.com"
        """.stripMargin
      checkParse(content, testFile) { feature =>
        assertTrue(
          feature.name == "User Password Reset with Comments",
          feature.file.contains(testFile),
          feature.line.contains(3),
          feature.background == List(
            Step(StepType.GivenStep, """a user exists with name "Default"""", file = Some(testFile), line = Some(7))
          ),
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Successful password reset with logging",
          feature.scenarios.head.file.contains(testFile),
          feature.scenarios.head.line.contains(8),
          feature.scenarios.head.steps == List(
            Step(StepType.WhenStep, "the user requests a password reset", file = Some(testFile), line = Some(10)),
            Step(StepType.AndStep, "the reset email is logged", file = Some(testFile), line = Some(12)),
            Step(
              StepType.ThenStep,
              """an email should be sent to "default@example.com"""",
              file = Some(testFile),
              line = Some(14)
            )
          )
        )
      }
    },
    test("loadFeatures from directory") {
      ZIO.scoped {
        for {
          tempDir <-
            ZIO.acquireRelease(
              ZIO.attempt(
                new File(java.lang.System.getProperty("java.io.tmpdir"), s"gherkin-test-${java.util.UUID.randomUUID()}")
              )
            )(dir => ZIO.attempt(dir.delete()).orDie)
          _ <- ZIO.attempt {
                 tempDir.mkdirs()
                 val file   = new File(tempDir, testFile)
                 val writer = new java.io.PrintWriter(file)
                 try {
                   writer.write("Feature: Test\n  Scenario: Simple\n    Given step")
                 } finally {
                   writer.close()
                 }
               }
          features <- GherkinParser.loadFeatures(tempDir)
        } yield assertTrue(
          features.length == 1,
          features.head.name == "Test",
          features.head.file.contains(new File(tempDir, testFile).getAbsolutePath),
          features.head.line.contains(1),
          features.head.scenarios.head.name == "Simple",
          features.head.scenarios.head.file.contains(new File(tempDir, testFile).getAbsolutePath),
          features.head.scenarios.head.line.contains(2),
          features.head.scenarios.head.steps == List(
            Step(StepType.GivenStep, "step", file = Some(new File(tempDir, testFile).getAbsolutePath), line = Some(3))
          )
        )
      }
    },
    test("loadFeatures empty directory") {
      ZIO.scoped {
        for {
          tempDir <- ZIO.acquireRelease(
                       ZIO.attempt(
                         new File(
                           java.lang.System.getProperty("java.io.tmpdir"),
                           s"gherkin-empty-${java.util.UUID.randomUUID()}"
                         )
                       )
                     )(dir => ZIO.attempt(dir.delete()).orDie)
          _        <- ZIO.attempt(tempDir.mkdirs())
          features <- GherkinParser.loadFeatures(tempDir)
        } yield assertTrue(features.isEmpty)
      }
    },
    test("fail on invalid syntax") {
      val content = """
                      |Feature: Invalid
                      |  InvalidKeyword: test
        """.stripMargin
      for {
        result <- GherkinParser.parseFeature(content, testFile).either
      } yield assertTrue(
        result.isLeft,
        result.left.toOption.exists(_.isInstanceOf[Exception]),
        result.left.toOption.exists(_.getMessage.contains("Failed to parse Gherkin content"))
      )
    }
  )

  private def checkParse(content: String, file: String = testFile)(
    assertion: Feature => TestResult
  ): ZIO[Any, Throwable, TestResult] =
    GherkinParser
      .parseFeature(content, file)
      .fold(
        failure = { error =>
          println(error.getMessage)
          error.printStackTrace()
          assertTrue(false)
        },
        success = { feature =>
          assertion(feature)
        }
      )
}
