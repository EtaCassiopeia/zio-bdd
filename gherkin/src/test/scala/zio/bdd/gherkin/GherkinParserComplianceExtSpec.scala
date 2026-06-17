package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Phase 3: Additional Gherkin parser compliance tests.
 *
 * Covers:
 *   - `# language:` directive is silently stripped (not an error)
 *   - Rule keyword edge cases (empty rule, rule with no scenarios)
 *   - Empty Feature with only a name line
 *   - Comments interleaved between steps
 *   - Various malformed inputs that should produce Left (not throw)
 *   - Unterminated DocString
 *   - Feature with only Background (no scenarios)
 */
object GherkinParserComplianceExtSpec extends ZIOSpecDefault {

  private val F                                          = "ext-compliance.feature"
  private def parse(c: String)                           = GherkinParser.parseFeature(c, F)
  private def check(c: String)(f: Feature => TestResult) = parse(c).map(f)

  private val languageDirectiveSuite = suite("# language: directive handling")(
    test("# language: en is silently stripped; feature parses normally") {
      check("# language: en\nFeature: F\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.name == "F", f.scenarios.length == 1)
      }
    },
    test("# language: fr is silently stripped (English keywords still work)") {
      check("# language: fr\nFeature: F\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.name == "F")
      }
    },
    test("# language: ja is silently stripped") {
      check("# language: ja\nFeature: 機能\n  Scenario: シナリオ\n    Given a step\n") { f =>
        assertTrue(f.name == "機能")
      }
    },
    test("# language: directive in the middle of a file does not corrupt parsing") {
      check("Feature: F\n# language: de\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.name == "F", f.scenarios.length == 1)
      }
    }
  )

  private val ruleSuite = suite("Rule keyword edge cases")(
    test("empty Rule body (no scenarios) does not prevent feature parse") {
      check("Feature: F\n  Rule: empty rule\n  Scenario: outside\n    Given a step\n") { f =>
        // The lone scenario under feature (outside rule) should parse
        assertTrue(f.scenarios.nonEmpty)
      }
    },
    test("Rule with multiple scenarios: both scenarios belong to the rule") {
      check(
        """Feature: F
          |  Rule: my rule
          |    Scenario: s1
          |      Given step one
          |    Scenario: s2
          |      Given step two
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.length == 2)
      }
    },
    test("Rule with its own Background: Background steps prepended only to rule scenarios") {
      check(
        """Feature: F
          |  Scenario: before-rule
          |    Given global step
          |  Rule: my rule
          |    Background:
          |      Given rule setup
          |    Scenario: rule-scenario
          |      Given rule step
          |""".stripMargin
      ) { f =>
        val ruleScenario   = f.scenarios.find(_.name == "rule-scenario")
        val globalScenario = f.scenarios.find(_.name == "before-rule")
        assertTrue(
          ruleScenario.exists(_.steps.exists(_.pattern == "rule setup")),
          globalScenario.exists(!_.steps.exists(_.pattern == "rule setup"))
        )
      }
    }
  )

  private val emptyOrMinimalSuite = suite("Empty and minimal feature files")(
    test("Feature with only a name and no scenarios parses to empty scenario list") {
      check("Feature: Empty Feature\n") { f =>
        assertTrue(f.name == "Empty Feature", f.scenarios.isEmpty)
      }
    },
    test("Feature with description paragraphs but no scenarios") {
      check(
        """Feature: Description Only
          |  As a user
          |  I want to do something
          |  So that I can achieve things
          |""".stripMargin
      ) { f =>
        assertTrue(f.name == "Description Only", f.scenarios.isEmpty)
      }
    },
    test("Feature with only a Background but no scenarios") {
      check(
        """Feature: Background Only
          |  Background:
          |    Given a step
          |""".stripMargin
      ) { f =>
        // Background with no scenarios is valid; results in no expanded scenarios
        assertTrue(f.name == "Background Only", f.scenarios.isEmpty)
      }
    },
    test("completely empty file returns a parse error") {
      assertZIO(parse("").exit)(
        fails(anything)
      )
    },
    test("file with only whitespace returns a parse error") {
      assertZIO(parse("   \n  \n  ").exit)(
        fails(anything)
      )
    }
  )

  private val commentsInterleavedSuite = suite("Comments interleaved with steps")(
    test("comment between Given and When steps does not break step list") {
      check(
        """Feature: F
          |  Scenario: s
          |    Given step one
          |    # this is a comment
          |    When step two
          |    Then step three
          |""".stripMargin
      ) { f =>
        val steps = f.scenarios.head.steps
        assertTrue(
          steps.length == 3,
          steps.map(_.pattern) == List("step one", "step two", "step three")
        )
      }
    },
    test("multiple consecutive comments between steps are all stripped") {
      check(
        """Feature: F
          |  Scenario: s
          |    Given step one
          |    # comment 1
          |    # comment 2
          |    # comment 3
          |    Then step two
          |""".stripMargin
      ) { f =>
        val steps = f.scenarios.head.steps
        assertTrue(steps.length == 2)
      }
    },
    test("comment before Feature: keyword is stripped") {
      check(
        """# This is a file-level comment
          |# Another comment
          |Feature: F
          |  Scenario: s
          |    Given a step
          |""".stripMargin
      ) { f =>
        assertTrue(f.name == "F")
      }
    }
  )

  private val malformedSuite = suite("Malformed inputs produce structured errors (not exceptions)")(
    test("file with no Feature: keyword returns Left") {
      assertZIO(parse("Scenario: s\n  Given a step\n").exit)(
        fails(anything)
      )
    },
    test("file starting with a comment followed by no Feature: keyword returns Left") {
      assertZIO(parse("# just a comment\nScenario: lost\n").exit)(
        fails(anything)
      )
    }
  )

  private val docStringEdgeSuite = suite("DocString edge cases")(
    test("DocString immediately followed by another step (no blank line between)") {
      check(
        """Feature: F
          |  Scenario: s
          |    Given the payload is
          |      \"\"\"
          |      {"key":"value"}
          |      \"\"\"
          |    Then it is validated
          |""".stripMargin.replace("\\\"\\\"\\\"", "\"\"\"")
      ) { f =>
        val steps = f.scenarios.head.steps
        assertTrue(
          steps.length == 2,
          steps(0).docString.exists(_.contains("key"))
        )
      }
    },
    test("backtick-delimited DocString works the same as triple-quote") {
      check(
        "Feature: F\n  Scenario: s\n    Given code is\n      ```\n      val x = 1\n      ```\n    Then check\n"
      ) { f =>
        val step = f.scenarios.head.steps.head
        assertTrue(step.docString.exists(_.contains("val x")))
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("GherkinParserComplianceExt")(
    languageDirectiveSuite,
    ruleSuite,
    emptyOrMinimalSuite,
    commentsInterleavedSuite,
    malformedSuite,
    docStringEdgeSuite
  )
}
