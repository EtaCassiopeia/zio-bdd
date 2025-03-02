package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.io.File

object GherkinParserSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment with Scope, Any] = suite("GherkinParser")(
    test("parse basic feature with single scenario") {
      val content = """
                      |Feature: User Management
                      |  Scenario: Create user
                      |    Given a system is running
                      |    When a user is created
                      |    Then the user exists
        """.stripMargin
      checkParse(content) { feature =>
        println(feature.scenarios.head.metadata)
        assertTrue(
          feature.name == "User Management",
          feature.background.isEmpty,
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Create user",
          feature.scenarios.head.steps == List(
            "a system is running",
            "a user is created",
            "the user exists"
          ),
          feature.scenarios.head.examples.isEmpty,
          !feature.scenarios.head.metadata.isFlaky,
          feature.scenarios.head.metadata.repeatCount == 1,
          feature.scenarios.head.metadata.retryCount == 0
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
      checkParse(content) { feature =>
        assertTrue(
          feature.name == "User Authentication",
          feature.background == List("a system is running"),
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Login",
          feature.scenarios.head.steps == List(
            "user logs in",
            "user is authenticated"
          )
        )
      }
    },
    test("parse scenario with tags") {
      val content = """
                      |Feature: Payment Processing
                      |  @Retry(3) @Flaky
                      |  Scenario: Process payment
                      |    Given a payment request
                      |    When payment is processed
                      |    Then payment succeeds
        """.stripMargin
      checkParse(content) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Payment Processing",
          scenario.name == "Process payment",
          scenario.metadata.retryCount == 3,
          scenario.metadata.isFlaky,
          scenario.metadata.repeatCount == 1
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
      checkParse(content) { feature =>
        println(feature)
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "Login Validation",
          scenario.name == "Validate credentials",
          scenario.steps == List(
            "user <name> exists",
            "user enters password <password>",
            "login <result>"
          ),
          scenario.examples.length == 2,
          scenario.examples(0).data == Map(
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
      checkParse(content) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.name == "User Validation",
          scenario.name == "Validate reset emails",
          scenario.steps == List(
            "a user exists with name {name:String}",
            "the user requests a password reset",
            "an email should be sent to {email:String}"
          ),
          scenario.examples.length == 2,
          scenario.examples(0).data == Map(
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
    test("parse feature with multiple scenarios") {
      val content = """
                      |Feature: User Operations
                      |  Scenario: Create user
                      |    Given system ready
                      |    When create user
                      |    Then user exists
                      |  @Repeat(2)
                      |  Scenario: Delete user
                      |    Given user exists
                      |    When delete user
                      |    Then user gone
        """.stripMargin
      checkParse(content) { feature =>
        assertTrue(
          feature.name == "User Operations",
          feature.scenarios.length == 2,
          feature.scenarios(0).name == "Create user",
          feature.scenarios(1).name == "Delete user",
          feature.scenarios(1).metadata.repeatCount == 2
        )
      }
    },
    test("parse feature with all elements") {
      val content = """
                      |Feature: Complex Feature
                      |  Background:
                      |    Given system running
                      |    And database ready
                      |  @Retry(2) @Flaky @Repeat(3)
                      |  Scenario Outline: Complex scenario
                      |    Given user <name>
                      |    When action <action>
                      |    Then result <result>
                      |    And cleanup done
                      |  Examples:
                      |    | name  | action | result |
                      |    | Alice | login  | ok     |
        """.stripMargin
      checkParse(content) { feature =>
        val scenario = feature.scenarios.head
        assertTrue(
          feature.background == List("system running", "database ready"),
          scenario.metadata.retryCount == 2,
          scenario.metadata.isFlaky == true,
          scenario.metadata.repeatCount == 3,
          scenario.steps == List(
            "user <name>",
            "action <action>",
            "result <result>",
            "cleanup done"
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
      checkParse(content) { feature =>
        assertTrue(
          feature.name == "User Password Reset",
          feature.background == List("""a user exists with name "Default""""),
          feature.scenarios.length == 1,
          feature.scenarios.head.name == "Successful password reset with logging",
          feature.scenarios.head.steps == List(
            "the user requests a password reset",
            "the reset email is logged",
            """an email should be sent to "default@example.com""""
          ),
          feature.scenarios.head.examples.isEmpty,
          !feature.scenarios.head.metadata.isFlaky,
          feature.scenarios.head.metadata.repeatCount == 1,
          feature.scenarios.head.metadata.retryCount == 0
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
                 val file   = new File(tempDir, "test.feature")
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
          features.head.scenarios.head.name == "Simple"
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
        result <- GherkinParser.parseFeature(content).either
      } yield assertTrue(
        result.isLeft,
        result.left.toOption.exists(_.isInstanceOf[Exception]),
        result.left.toOption.exists(_.getMessage.contains("Failed to parse Gherkin content"))
      )
    }
  )

  // Helper method to check parsing with improved error handling
  private def checkParse(content: String)(assertion: Feature => TestResult): ZIO[Any, Throwable, TestResult] =
    GherkinParser
      .parseFeature(content)
      .fold(
        failure = { error =>
          println(s"Failed to parse feature: ${error.getMessage}")
          println(s"Failed content: $content")
          assertTrue(false)
        },
        success = { feature =>
          assertion(feature)
        }
      )
}
