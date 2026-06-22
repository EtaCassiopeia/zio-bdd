package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Gherkin spec-compliance and robustness test suite.
 *
 * Strategy:
 *   1. Deterministic tests for specific spec requirements (exact inputs /
 *      outputs) 2. Property-based tests for structural invariants:
 *      - whitespace variation should not change parse results
 *      - encoding edge cases (BOM, CRLF, tabs) should be transparent
 *      - table and outline sizes should scale linearly
 *
 * Each test is named after the spec behaviour, not the implementation bug it
 * was introduced to catch (bug IDs are noted in comments for traceability).
 */
object GherkinParserComplianceSpec extends ZIOSpecDefault {

  private val F                                          = "compliance.feature"
  private def parse(c: String)                           = GherkinParser.parseFeature(c, F)
  private def check(c: String)(f: Feature => TestResult) = parse(c).map(f)

  // ── Preprocessing ────────────────────────────────────────────────────────

  private val preprocessing = suite("Preprocessing")(
    test("UTF-8 BOM at file start is stripped transparently") {
      // BUG-03: BOM previously caused a parse failure
      check("﻿Feature: BOM\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.name == "BOM")
      }
    },
    test("BOM with preceding tags still parses correctly") {
      check("﻿@tag\nFeature: F\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.tags.contains("tag"))
      }
    },
    test("Windows CRLF line endings are normalised to LF") {
      // CRLF must produce the same result as LF
      for {
        fLF   <- parse("Feature: F\n  Scenario: s\n    Given a step\n")
        fCRLF <- parse("Feature: F\r\n  Scenario: s\r\n    Given a step\r\n")
      } yield assertTrue(
        fLF.name == fCRLF.name,
        fLF.scenarios.head.steps.head.pattern == fCRLF.scenarios.head.steps.head.pattern
      )
    },
    test("old-style CR-only line endings are normalised") {
      check("Feature: F\r  Scenario: s\r    Given a step\r") { f =>
        assertTrue(f.scenarios.nonEmpty)
      }
    },
    test("# language: directive line is stripped without failing") {
      check("# language: en\nFeature: F\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.name == "F")
      }
    }
  )

  // ── Keyword robustness ───────────────────────────────────────────────────

  private val keywords = suite("Keyword word-boundary and colon variants")(
    test("word starting with 'Given' is not parsed as a Given step") {
      // BUG-06: prefix-match caused e.g. "Givenness" → GivenStep("ness")
      check("Feature: F\n  Scenario: s\n    Given first\n    Then Givenness matters\n") { f =>
        val step = f.scenarios.head.steps(1)
        assertTrue(step.stepType == StepType.ThenStep, step.pattern == "Givenness matters")
      }
    },
    test("word starting with 'And' is not parsed as an And step") {
      check("Feature: F\n  Scenario: s\n    Given g\n    Then Andromeda galaxy\n") { f =>
        val step = f.scenarios.head.steps(1)
        assertTrue(step.stepType == StepType.ThenStep, step.pattern == "Andromeda galaxy")
      }
    },
    test("'Given:' with colon after keyword is accepted") {
      // BUG-06 related: colon directly after keyword should work
      check("Feature: F\n  Scenario: s\n    Given: a step\n") { f =>
        assertTrue(f.scenarios.head.steps.head.pattern == "a step")
      }
    },
    test("wildcard step bullet '*' is accepted as an And-style step") {
      check("Feature: F\n  Scenario: s\n    Given first\n    * second\n    * third\n") { f =>
        val steps = f.scenarios.head.steps
        assertTrue(steps(1).stepType == StepType.AndStep, steps(1).pattern == "second")
      }
    },
    test("Feature with empty name (just 'Feature:') does not eat the next line") {
      // BUG-04: empty name line ate the next scenario name
      check("Feature:\n  Scenario: real name\n    Given a step\n") { f =>
        assertTrue(f.name == "", f.scenarios.head.name == "real name")
      }
    },
    test("Scenario with empty name does not eat the first step as name") {
      // BUG-04: same issue for Scenario:
      check("Feature: F\n  Scenario:\n    Given a step\n") { f =>
        assertTrue(f.scenarios.head.name == "", f.scenarios.head.steps.head.pattern == "a step")
      }
    }
  )

  // ── Description paragraphs ───────────────────────────────────────────────

  private val descriptionParagraphs = suite("Description paragraphs")(
    test("narrative text between Feature: and first Scenario is tolerated") {
      // BUG-05: description text previously caused a parse failure
      check(
        """Feature: User Login
          |  As a user I want to log in
          |  so that I can access my account.
          |
          |  Scenario: happy path
          |    Given on the login page
          |    Then logged in
          |""".stripMargin
      ) { f =>
        assertTrue(f.name == "User Login", f.scenarios.length == 1)
      }
    },
    test("description text inside a scenario block is tolerated") {
      check(
        """Feature: F
          |  Scenario: s
          |    This describes the scenario.
          |    Given a step
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.head.steps.length == 1, f.scenarios.head.steps.head.pattern == "a step")
      }
    },
    test("feature with only description and no scenarios is valid") {
      // GAP-04: feature with no scenarios
      check("Feature: Planned\n  These scenarios will be added later.\n") { f =>
        assertTrue(f.name == "Planned", f.scenarios.isEmpty)
      }
    },
    test("feature with Background and no scenarios is valid") {
      // GAP-04
      check("Feature: Setup\n  Background:\n    Given the database is seeded\n") { f =>
        assertTrue(f.name == "Setup", f.scenarios.isEmpty)
      }
    },
    test("blank line between two description paragraphs does not drop the feature") {
      // Regression: a blank line inside the free-text description block (between
      // Feature: and the first Scenario/tag) was previously mistaken for the end
      // of the description, leaving the second paragraph's line unconsumed and
      // silently zeroing out the whole feature (reported with 0 scenarios parsed).
      check(
        """Feature: Example
          |
          |  First paragraph of description text, no special characters.
          |
          |  Second paragraph, also no special characters - just a blank line above it.
          |
          |  Scenario: Anything
          |    Then some step
          |""".stripMargin
      ) { f =>
        assertTrue(f.name == "Example", f.scenarios.length == 1, f.scenarios.head.steps.length == 1)
      }
    }
  )

  // ── Data table spec ──────────────────────────────────────────────────────

  private val tableSpec = suite("Data table spec compliance")(
    test("pipe-escaped cell values are unescaped correctly") {
      // GAP-02: \\| escape was not handled
      check("Feature: F\n  Scenario: s\n    Given data:\n      | pattern  |\n      | a\\|b\\|c |\n") { f =>
        val cell = f.scenarios.head.steps.head.dataTable.flatMap(_.rows.headOption).map(_.cells.head)
        assertTrue(cell.contains("a|b|c"))
      }
    },
    test("\\n in cell value is unescaped to a newline character") {
      check("Feature: F\n  Scenario: s\n    Given data:\n      | text          |\n      | line1\\nline2 |\n") { f =>
        val cell = f.scenarios.head.steps.head.dataTable.flatMap(_.rows.headOption).map(_.cells.head)
        assertTrue(cell.contains("line1\nline2"))
      }
    },
    test("double backslash becomes a single backslash") {
      check("Feature: F\n  Scenario: s\n    Given data:\n      | path      |\n      | C:\\\\Users |\n") { f =>
        val cell = f.scenarios.head.steps.head.dataTable.flatMap(_.rows.headOption).map(_.cells.head)
        assertTrue(cell.contains("C:\\Users"))
      }
    },
    test("table ending the file without a trailing newline is parsed") {
      // BUG-01: trailing-newline sensitivity
      check("Feature: F\n  Scenario: s\n    Given data:\n      | k | v |\n      | a | 1 |") { f =>
        assertTrue(f.scenarios.head.steps.head.dataTable.isDefined)
      }
    }
  )

  // ── Doc string spec ──────────────────────────────────────────────────────

  private val docStringSpec = suite("Doc string spec compliance")(
    test("doc string indentation of opening delimiter is stripped from content") {
      // BUG-08: .trim was used instead of spec-compliant indentation stripping
      val content =
        "Feature: F\n  Scenario: s\n    Given body:\n      \"\"\"\n      line1\n        line2\n      \"\"\"\n"
      check(content) { f =>
        val ds = f.scenarios.head.steps.head.docString.getOrElse("")
        // "line1" at indent 0, "  line2" retaining 2 extra spaces (relative to opening """)
        assertTrue(ds.startsWith("line1"), ds.contains("  line2"))
      }
    },
    test("empty lines inside a doc string are preserved") {
      val content =
        "Feature: F\n  Scenario: s\n    Given body:\n      \"\"\"\n      first\n\n      third\n      \"\"\"\n"
      check(content) { f =>
        val ds = f.scenarios.head.steps.head.docString.getOrElse("")
        // "first" then blank then "third"
        assertTrue(ds.contains("first"), ds.contains("third"))
      }
    },
    test("backtick doc string delimiter is accepted") {
      // GAP-01
      val content = "Feature: F\n  Scenario: s\n    Given body:\n      ```\n      content\n      ```\n"
      check(content) { f =>
        assertTrue(f.scenarios.head.steps.head.docString.isDefined)
      }
    }
  )

  // ── Tag spec ─────────────────────────────────────────────────────────────

  private val tagSpec = suite("Tag spec compliance")(
    test("tags with dashes and dots parse correctly (regression #37)") {
      check("Feature: F\n  @spec-1.4.7 @not-ready\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(
          f.scenarios.head.tags.contains("spec-1.4.7"),
          f.scenarios.head.tags.contains("not-ready")
        )
      }
    },
    test("tags with parentheses (e.g. retry annotations) parse correctly") {
      check("Feature: F\n  @retry(3) @flaky\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.scenarios.head.tags.contains("retry(3)"))
      }
    },
    test("unicode characters in tag names are accepted") {
      check("Feature: F\n  @日本語\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.scenarios.head.tags.nonEmpty)
      }
    }
  )

  // ── Property-based: whitespace invariants ────────────────────────────────

  private val whitespaceProps = suite("Property: whitespace invariants")(
    test("0 to 10 blank lines between Feature and first Scenario produce the same scenario count") {
      ZIO
        .foreach(0 to 10) { n =>
          val content = s"Feature: F\n${"\n" * n}  Scenario: s\n    Given a step\n"
          parse(content).map(f => assertTrue(f.scenarios.length == 1))
        }
        .map(_.reduce(_ && _))
    },
    test("step indentation from 0 to 8 spaces produces the same step pattern") {
      ZIO
        .foreach(0 to 8) { n =>
          val content = s"Feature: F\n  Scenario: s\n${" " * n}Given a step\n"
          parse(content).map(f => assertTrue(f.scenarios.head.steps.head.pattern == "a step"))
        }
        .map(_.reduce(_ && _))
    },
    test("tab indentation produces the same step pattern as spaces") {
      for {
        fSpaces <- parse("Feature: F\n  Scenario: s\n    Given a step\n")
        fTabs   <- parse("Feature: F\n\tScenario: s\n\t\tGiven a step\n")
      } yield assertTrue(
        fSpaces.scenarios.head.steps.head.pattern == fTabs.scenarios.head.steps.head.pattern
      )
    },
    test("trailing whitespace on step lines is stripped") {
      ZIO
        .foreach(List("", " ", "   ", "\t")) { trailing =>
          val content = s"Feature: F\n  Scenario: s\n    Given a step$trailing\n"
          parse(content).map(f => assertTrue(f.scenarios.head.steps.head.pattern == "a step"))
        }
        .map(_.reduce(_ && _))
    },
    test("blank lines within a step sequence are transparent") {
      for {
        fDense  <- parse("Feature: F\n  Scenario: s\n    Given a\n    When b\n    Then c\n")
        fSparse <- parse("Feature: F\n  Scenario: s\n    Given a\n\n    When b\n\n\n    Then c\n")
      } yield assertTrue(
        fDense.scenarios.head.steps.map(_.pattern) == fSparse.scenarios.head.steps.map(_.pattern)
      )
    },
    test("CRLF and LF content produce identical parse results") {
      val lf   = "Feature: F\n  @smoke\n  Scenario: s\n    Given a step\n    Then done\n"
      val crlf = lf.replace("\n", "\r\n")
      for {
        fLF   <- parse(lf)
        fCRLF <- parse(crlf)
      } yield assertTrue(
        fLF.name == fCRLF.name,
        fLF.scenarios.head.tags == fCRLF.scenarios.head.tags,
        fLF.scenarios.head.steps.map(_.pattern) == fCRLF.scenarios.head.steps.map(_.pattern)
      )
    }
  )

  // ── Property-based: table dimensions ────────────────────────────────────

  private val tableProps = suite("Property: data table dimensions")(
    test("table with 1 to 5 columns parsed with correct header count") {
      ZIO
        .foreach(1 to 5) { cols =>
          val header  = (1 to cols).map(i => s"col$i").mkString("| ", " | ", " |")
          val row     = (1 to cols).map(i => s"val$i").mkString("| ", " | ", " |")
          val content = s"Feature: F\n  Scenario: s\n    Given data:\n      $header\n      $row\n"
          parse(content).map(f => assertTrue(f.scenarios.head.steps.head.dataTable.exists(_.headers.length == cols)))
        }
        .map(_.reduce(_ && _))
    },
    test("table with 0 to 5 data rows parsed with correct row count") {
      ZIO
        .foreach(0 to 5) { rows =>
          val rowLines = (1 to rows).map(i => s"      | val$i |").mkString("\n")
          val content  = s"Feature: F\n  Scenario: s\n    Given data:\n      | x |\n$rowLines\n"
          parse(content).map(f => assertTrue(f.scenarios.head.steps.head.dataTable.exists(_.rows.length == rows)))
        }
        .map(_.reduce(_ && _))
    }
  )

  // ── Property-based: outline expansion ───────────────────────────────────

  private val outlineProps = suite("Property: Scenario Outline expansion")(
    test("outline with N example rows expands to exactly N scenarios") {
      ZIO
        .foreach(1 to 8) { n =>
          val rows    = (1 to n).map(i => s"    | val$i |").mkString("\n")
          val content = s"Feature: F\n  Scenario Outline: test <x>\n    Given <x>\n  Examples:\n    | x |\n$rows\n"
          parse(content).map(f => assertTrue(f.scenarios.length == n))
        }
        .map(_.reduce(_ && _))
    },
    test("background steps appear in all expanded scenarios") {
      ZIO
        .foreach(1 to 4) { n =>
          val rows = (1 to n).map(i => s"    | v$i |").mkString("\n")
          val content =
            s"Feature: F\n  Background:\n    Given setup\n  Scenario Outline: <x>\n    When <x>\n  Examples:\n    | x |\n$rows\n"
          parse(content).map(f => assertTrue(f.scenarios.forall(_.steps.head.pattern == "setup")))
        }
        .map(_.reduce(_ && _))
    },
    test("placeholder is substituted in every step of every expanded scenario") {
      ZIO
        .foreach(2 to 5) { n =>
          val rows = (1 to n).map(i => s"    | item$i |").mkString("\n")
          val content =
            s"Feature: F\n  Scenario Outline: <x> processing\n    Given <x> exists\n    When <x> is processed\n    Then <x> is done\n  Examples:\n    | x |\n$rows\n"
          parse(content).map { f =>
            val allStepPatterns = f.scenarios.flatMap(_.steps.map(_.pattern))
            assertTrue(allStepPatterns.forall(!_.contains("<x>")))
          }
        }
        .map(_.reduce(_ && _))
    }
  )

  // ── Property-based: structural identity ─────────────────────────────────

  private val structuralProps = suite("Property: structural identity")(
    test("step patterns never have leading or trailing whitespace after parse") {
      val stepTexts = List("  leading", "trailing  ", "  both  ", "normal")
      val steps = stepTexts.zipWithIndex.map { case (t, i) =>
        val kw = List("Given", "When", "Then", "And")(i % 4)
        s"    $kw $t"
      }.mkString("\n")
      val content = s"Feature: F\n  Scenario: s\n$steps\n"
      parse(content).map { f =>
        val patterns = f.scenarios.head.steps.map(_.pattern)
        assertTrue(patterns.forall(p => p == p.strip()))
      }
    },
    test("scenario names never have leading or trailing whitespace") {
      ZIO
        .foreach(List("  leading", "trailing  ", "  both  ", "normal")) { name =>
          val content = s"Feature: F\n  Scenario: $name\n    Given a\n"
          parse(content).map(f => assertTrue(f.scenarios.head.name == f.scenarios.head.name.strip()))
        }
        .map(_.reduce(_ && _))
    },
    test("feature name never has leading or trailing whitespace") {
      ZIO
        .foreach(List("  leading", "trailing  ", "  both  ", "normal")) { name =>
          val content = s"Feature: $name\n  Scenario: s\n    Given a\n"
          parse(content).map(f => assertTrue(f.name == f.name.strip()))
        }
        .map(_.reduce(_ && _))
    }
  )

  // ── Real-world integration test ──────────────────────────────────────────

  private val realWorld = suite("Real-world multi-construct feature file")(
    test("full-featured file with all constructs parses to correct shape") {
      val content =
        """# project comment
          |# language: en
          |
          |@project @smoke
          |Feature: Account Lifecycle
          |  As a financial system I want to manage accounts.
          |
          |  Background:
          |    Given the system is running
          |    And the following instrument classes exist:
          |      | Class         | Version |
          |      | SimpleSavings | 1.0.0   |
          |
          |  @provision @smoke
          |  Scenario: Provision an account
          |    When a provision request is sent
          |    Then the ledger returns a 200 status code
          |
          |  @post
          |  Scenario Outline: Post a <type> transaction
          |    Given a provisioned account
          |    When a <type> post of <amount> is sent
          |    Then the balance changes by <amount>
          |
          |  Examples: Deposits
          |    | type    | amount |
          |    | deposit | 1000   |
          |    | deposit | 5000   |
          |
          |  @smoke
          |  Examples: Withdrawals
          |    | type       | amount |
          |    | withdrawal | 500    |
          |
          |  Rule: EOD must be idempotent
          |
          |    Background:
          |      Given a provisioned account with a balance
          |
          |    Scenario: EOD run twice is idempotent
          |      When EOD is run on 2025-01-01
          |      And EOD is run again on 2025-01-01
          |      Then the account state is unchanged
          |""".stripMargin
      check(content) { f =>
        val outlineScenarios = f.scenarios.filter(_.name.startsWith("Post a"))
        val ruleScenario     = f.scenarios.find(_.name == "EOD run twice is idempotent")
        assertTrue(
          f.name == "Account Lifecycle",
          f.tags.toSet == Set("project", "smoke"),
          // Every scenario gets the Background steps prepended
          f.scenarios.forall(_.steps.nonEmpty),
          // Outline expanded to 3: 2 deposits + 1 withdrawal
          outlineScenarios.length == 3,
          // Withdrawal scenario has @smoke (from its Examples block)
          outlineScenarios.last.tags.contains("smoke"),
          // Rule scenario exists
          ruleScenario.isDefined,
          // Rule scenario has: feature bg (2 steps) + rule bg (1 step) + own steps (3 steps)
          ruleScenario.exists(_.steps.length == 6)
        )
      }
    },
    test("Windows CRLF + BOM file parses to correct shape") {
      val content = "﻿@tag\r\nFeature: CRLF\r\n  Scenario: s\r\n    Given a step\r\n    Then done\r\n"
      check(content) { f =>
        assertTrue(
          f.name == "CRLF",
          f.tags.contains("tag"),
          f.scenarios.head.steps.length == 2
        )
      }
    }
  )

  // ── Spec ─────────────────────────────────────────────────────────────────

  def spec: Spec[TestEnvironment & Scope, Any] = suite("GherkinParserCompliance")(
    preprocessing,
    keywords,
    descriptionParagraphs,
    tableSpec,
    docStringSpec,
    tagSpec,
    whitespaceProps,
    tableProps,
    outlineProps,
    structuralProps,
    realWorld
  )
}
