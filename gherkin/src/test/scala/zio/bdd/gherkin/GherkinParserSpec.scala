package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Unit tests for GherkinParser: parse correctness, structural integrity,
 * line-number tracking, and file I/O.
 *
 * Organisation:
 *   - Feature structure (name, tags, file, line)
 *   - Scenarios and step types
 *   - Background steps
 *   - Data tables
 *   - Scenario Outlines / Examples expansion
 *   - Comments
 *   - File I/O (loadFeatures)
 *   - Parse failure cases
 *
 * Line-number assertions are intentionally omitted where they would make tests
 * brittle. They are covered by GherkinParserComplianceSpec only when the spec
 * explicitly requires a specific line position.
 */
object GherkinParserSpec extends ZIOSpecDefault {

  // ── Helpers ────────────────────────────────────────────────────────────────

  private val F = "test.feature"

  private def parse(content: String) =
    GherkinParser.parseFeature(content, F)

  private def check(content: String)(f: Feature => TestResult): ZIO[Any, Throwable, TestResult] =
    parse(content).map(f)

  private def mkStep(t: StepType, p: String): Step = Step(t, p)

  // ── Feature structure ──────────────────────────────────────────────────────

  private val featureStructure = suite("Feature structure")(
    test("feature name is captured") {
      check("Feature: User Management\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.name == "User Management")
      }
    },
    test("feature-level tags are captured") {
      check("@billing @smoke\nFeature: Payments\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.tags.toSet == Set("billing", "smoke"))
      }
    },
    test("feature tags do not bleed onto scenario tags") {
      check("@feat-tag\nFeature: F\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(!f.scenarios.head.tags.contains("feat-tag"))
      }
    },
    test("feature file and line are recorded") {
      parse("Feature: F\n  Scenario: s\n    Given a step\n").map { f =>
        assertTrue(f.file.contains(F), f.line.exists(_ >= 1))
      }
    }
  )

  // ── Scenarios and step types ───────────────────────────────────────────────

  private val scenarioStructure = suite("Scenarios and step types")(
    test("scenario name and tags are captured") {
      check("Feature: F\n  @smoke\n  Scenario: Login\n    Given a step\n") { f =>
        val sc = f.scenarios.head
        assertTrue(sc.name == "Login", sc.tags.contains("smoke"))
      }
    },
    test("multiple scenarios in a feature are all present") {
      check("Feature: F\n  Scenario: A\n    Given a\n  Scenario: B\n    When b\n") { f =>
        assertTrue(f.scenarios.map(_.name) == List("A", "B"))
      }
    },
    test("all five step keywords produce correct StepType values") {
      check(
        "Feature: F\n  Scenario: s\n    Given g\n    When w\n    Then t\n    And a\n    But b\n"
      ) { f =>
        val types = f.scenarios.head.steps.map(_.stepType)
        assertTrue(
          types == List(
            StepType.GivenStep,
            StepType.WhenStep,
            StepType.ThenStep,
            StepType.AndStep,
            StepType.ButStep
          )
        )
      }
    },
    test("step patterns are captured correctly including quoted text") {
      check(
        """Feature: F
          |  Scenario: s
          |    Given a user exists with name "Default"
          |    Then an email should be sent to "default@example.com"
          |""".stripMargin
      ) { f =>
        val patterns = f.scenarios.head.steps.map(_.pattern)
        assertTrue(
          patterns(0) == """a user exists with name "Default"""",
          patterns(1) == """an email should be sent to "default@example.com""""
        )
      }
    },
    test("But and And step types are recorded as-is (effective-type resolution is executor concern)") {
      check("Feature: F\n  Scenario: s\n    Given g\n    And a\n    But b\n") { f =>
        val steps = f.scenarios.head.steps
        assertTrue(
          steps(1).stepType == StepType.AndStep,
          steps(2).stepType == StepType.ButStep
        )
      }
    },
    test("@ignore tag makes scenario.isIgnored return true") {
      check("Feature: F\n  @ignore\n  Scenario: s\n    Given a step\n") { f =>
        assertTrue(f.scenarios.head.isIgnored)
      }
    },
    test("multiple tag lines before a scenario are merged") {
      check("Feature: F\n  @a\n  @b\n  @c\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.scenarios.head.tags == List("a", "b", "c"))
      }
    }
  )

  // ── Background ────────────────────────────────────────────────────────────

  private val background = suite("Background")(
    test("background steps are prepended to each scenario's steps") {
      check(
        """Feature: F
          |  Background:
          |    Given setup
          |  Scenario: s
          |    When action
          |    Then result
          |""".stripMargin
      ) { f =>
        val steps = f.scenarios.head.steps.map(_.pattern)
        assertTrue(steps == List("setup", "action", "result"))
      }
    },
    test("background steps are prepended to every scenario when multiple exist") {
      check(
        """Feature: F
          |  Background:
          |    Given bg
          |  Scenario: A
          |    When a
          |  Scenario: B
          |    When b
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.forall(_.steps.head.pattern == "bg"))
      }
    },
    test("background step with data table argument preserves the table") {
      check(
        """Feature: F
          |  Background:
          |    Given users exist:
          |      | name  | role  |
          |      | Alice | admin |
          |  Scenario: s
          |    When action
          |""".stripMargin
      ) { f =>
        val bgStep = f.scenarios.head.steps.head
        assertTrue(
          bgStep.pattern == "users exist:",
          bgStep.dataTable.isDefined,
          bgStep.dataTable.exists(_.headers == List("name", "role"))
        )
      }
    }
  )

  // ── Data tables ───────────────────────────────────────────────────────────

  private val dataTables = suite("Data tables")(
    test("single-row table is parsed with header and one data row") {
      check(
        "Feature: F\n  Scenario: s\n    Given data:\n      | key | value |\n      | a   | 1     |\n"
      ) { f =>
        val tbl = f.scenarios.head.steps.head.dataTable
        assertTrue(
          tbl.isDefined,
          tbl.exists(_.headers == List("key", "value")),
          tbl.exists(_.rows == List(DataTableRow(List("a", "1"))))
        )
      }
    },
    test("multi-row table produces multiple DataTableRows") {
      check(
        "Feature: F\n  Scenario: s\n    Given data:\n      | k | v |\n      | a | 1 |\n      | b | 2 |\n      | c | 3 |\n"
      ) { f =>
        val tbl = f.scenarios.head.steps.head.dataTable
        assertTrue(tbl.exists(_.rows.length == 3))
      }
    },
    test("multiple steps each with a table are parsed independently") {
      check(
        """Feature: F
          |  Scenario: s
          |    Given first:
          |      | a |
          |      | 1 |
          |    And second:
          |      | b |
          |      | 2 |
          |    Then done
          |""".stripMargin
      ) { f =>
        val steps = f.scenarios.head.steps
        assertTrue(
          steps(0).dataTable.isDefined,
          steps(1).dataTable.isDefined,
          steps(2).dataTable.isEmpty
        )
      }
    },
    test("empty cell in a table is represented as an empty string") {
      check(
        "Feature: F\n  Scenario: s\n    Given data:\n      | name  | phone |\n      | Alice |       |\n"
      ) { f =>
        val row = f.scenarios.head.steps.head.dataTable.flatMap(_.rows.headOption)
        assertTrue(row.exists(_.cells == List("Alice", "")))
      }
    },
    test("cell values are trimmed") {
      check(
        "Feature: F\n  Scenario: s\n    Given data:\n      |  name  |  value  |\n      |  Alice  |  42  |\n"
      ) { f =>
        val row = f.scenarios.head.steps.head.dataTable.flatMap(_.rows.headOption)
        assertTrue(row.exists(_.cells == List("Alice", "42")))
      }
    },
    test("3+ consecutive scenarios each ending with a table all parse correctly (regression #39)") {
      check(
        """Feature: F
          |  Scenario: A
          |    Given a:
          |      | k | v |
          |      | a | 1 |
          |  Scenario: B
          |    Given b:
          |      | x | y |
          |      | 2 | 3 |
          |  Scenario: C
          |    Given c:
          |      | p | q |
          |      | 4 | 5 |
          |  Scenario: D
          |    Given d:
          |      | m | n |
          |      | 6 | 7 |
          |""".stripMargin
      ) { f =>
        assertTrue(
          f.scenarios.length == 4,
          f.scenarios.forall(_.steps.head.dataTable.isDefined)
        )
      }
    }
  )

  // ── Scenario Outlines ─────────────────────────────────────────────────────

  private val outlines = suite("Scenario Outlines / Examples expansion")(
    test("outline with two example rows expands to two scenarios") {
      check(
        """Feature: F
          |  Scenario Outline: Login <user>
          |    Given user <user> exists
          |    When <user> logs in
          |  Examples:
          |    | user  |
          |    | Alice |
          |    | Bob   |
          |""".stripMargin
      ) { f =>
        assertTrue(
          f.scenarios.length == 2,
          f.scenarios(0).name == "Login <user> - Example 1",
          f.scenarios(1).name == "Login <user> - Example 2",
          f.scenarios(0).steps.head.pattern == "user Alice exists",
          f.scenarios(1).steps.head.pattern == "user Bob exists"
        )
      }
    },
    test("outline placeholder is substituted in all steps") {
      check(
        """Feature: F
          |  Scenario Outline: <x>
          |    Given <x> is set
          |    When <x> is processed
          |    Then <x> is done
          |  Examples:
          |    | x   |
          |    | foo |
          |""".stripMargin
      ) { f =>
        val sc = f.scenarios.head
        assertTrue(sc.steps.map(_.pattern) == List("foo is set", "foo is processed", "foo is done"))
      }
    },
    test("named Examples block name appears in expanded scenario title") {
      check(
        """Feature: F
          |  Scenario Outline: Validate <x>
          |    Given <x>
          |  Examples: Happy cases
          |    | x   |
          |    | foo |
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.head.name == "Validate <x> - Happy cases - Example 1")
      }
    },
    test("multiple Examples blocks produce scenarios for each block") {
      check(
        """Feature: F
          |  Scenario Outline: Test <x>
          |    Given <x>
          |  Examples: Block A
          |    | x |
          |    | a |
          |  Examples: Block B
          |    | x |
          |    | b |
          |""".stripMargin
      ) { f =>
        assertTrue(
          f.scenarios.length == 2,
          f.scenarios(0).name.contains("Block A"),
          f.scenarios(1).name.contains("Block B")
        )
      }
    },
    test("tags on Examples block are propagated to expanded scenarios") {
      check(
        """Feature: F
          |  Scenario Outline: <x>
          |    Given <x>
          |  @smoke
          |  Examples:
          |    | x |
          |    | a |
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.head.tags.contains("smoke"))
      }
    },
    test("Example: keyword is accepted as an alias for Scenario:") {
      check("Feature: F\n  Example: s\n    Given a step\n") { f =>
        assertTrue(f.scenarios.head.name == "s")
      }
    },
    test("Scenario Template: is accepted as an alias for Scenario Outline:") {
      check(
        "Feature: F\n  Scenario Template: <x>\n    Given <x>\n  Examples:\n    | x |\n    | a |\n"
      ) { f =>
        assertTrue(f.scenarios.length == 1, f.scenarios.head.name.contains("Example 1"))
      }
    },
    test("Scenarios: is accepted as an alias for Examples:") {
      check(
        "Feature: F\n  Scenario Outline: <x>\n    Given <x>\n  Scenarios:\n    | x |\n    | a |\n    | b |\n"
      ) { f =>
        assertTrue(f.scenarios.length == 2)
      }
    }
  )

  // ── Doc strings ───────────────────────────────────────────────────────────

  private val docStrings = suite("Doc strings")(
    test("triple-quote doc string is captured on the step") {
      val content =
        "Feature: F\n  Scenario: s\n    Given body:\n      \"\"\"\n      {\"k\":\"v\"}\n      \"\"\"\n    Then done\n"
      check(content) { f =>
        val step = f.scenarios.head.steps.head
        assertTrue(step.docString.isDefined, step.docString.exists(_.contains("k")))
      }
    },
    test("triple-backtick doc string is also captured") {
      val content =
        "Feature: F\n  Scenario: s\n    Given body:\n      ```\n      some content\n      ```\n    Then done\n"
      check(content) { f =>
        assertTrue(f.scenarios.head.steps.head.docString.isDefined)
      }
    },
    test("step following a doc-string step is parsed correctly") {
      val content =
        "Feature: F\n  Scenario: s\n    Given body:\n      \"\"\"\n      text\n      \"\"\"\n    Then done\n"
      check(content) { f =>
        assertTrue(f.scenarios.head.steps.length == 2, f.scenarios.head.steps.last.pattern == "done")
      }
    }
  )

  // ── Rule keyword ─────────────────────────────────────────────────────────

  private val ruleKeyword = suite("Rule: keyword")(
    test("Rule block groups scenarios under a heading") {
      check(
        """Feature: F
          |  Rule: Must be active
          |    Scenario: happy path
          |      Given an active account
          |      Then it works
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.map(_.name) == List("happy path"))
      }
    },
    test("multiple Rule blocks produce all their scenarios") {
      check(
        """Feature: F
          |  Rule: A
          |    Scenario: A1
          |      Given a
          |  Rule: B
          |    Scenario: B1
          |      Given b
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.map(_.name) == List("A1", "B1"))
      }
    },
    test("Rule inner Background is prepended only to that rule's scenarios") {
      check(
        """Feature: F
          |  Background:
          |    Given feature bg
          |  Rule: R
          |    Background:
          |      Given rule bg
          |    Scenario: s
          |      When action
          |""".stripMargin
      ) { f =>
        assertTrue(
          f.scenarios.head.steps.map(_.pattern) == List("feature bg", "rule bg", "action")
        )
      }
    }
  )

  // ── Comments ──────────────────────────────────────────────────────────────

  private val comments = suite("Comments")(
    test("file-level comment before Feature is stripped") {
      check("# comment\nFeature: F\n  Scenario: s\n    Given a\n") { f =>
        assertTrue(f.name == "F")
      }
    },
    test("comments between Background steps are stripped") {
      check(
        "Feature: F\n  Background:\n    Given first\n    # a comment\n    And second\n  Scenario: s\n    When action\n"
      ) { f =>
        val bgSteps = f.scenarios.head.steps.take(2).map(_.pattern)
        assertTrue(bgSteps == List("first", "second"))
      }
    },
    test("comments at multiple indent levels are all stripped") {
      check(
        """Feature: F
          |  # feature comment
          |  Background:
          |    # bg comment
          |    Given bg step
          |  Scenario: s
          |    # scenario comment
          |    When action
          |    # another comment
          |    Then result
          |""".stripMargin
      ) { f =>
        assertTrue(f.scenarios.head.steps.length == 3)
      }
    }
  )

  // ── File I/O ──────────────────────────────────────────────────────────────

  private val fileIO = suite("File I/O")(
    test("loadFeatures reads a single .feature file from a directory") {
      ZIO.scoped {
        for {
          dir <- makeDir("gherkin-io-single")
          _   <- writeFile(dir, "test.feature", "Feature: Test\n  Scenario: s\n    Given a step\n")
          fs  <- GherkinParser.loadFeatures(dir)
        } yield assertTrue(fs.length == 1, fs.head.name == "Test")
      }
    },
    test("loadFeatures returns empty list for an empty directory") {
      ZIO.scoped {
        for {
          dir <- makeDir("gherkin-io-empty")
          fs  <- GherkinParser.loadFeatures(dir)
        } yield assertTrue(fs.isEmpty)
      }
    },
    test("loadFeatures skips unparseable files when failFast=false (regression #46)") {
      ZIO.scoped {
        for {
          dir <- makeDir("gherkin-io-resilience")
          _   <- writeFile(dir, "good.feature", "Feature: Good\n  Scenario: s\n    Given a\n")
          _   <- writeFile(dir, "bad.feature", "not valid gherkin at all")
          fs  <- GherkinParser.loadFeatures(dir, failFast = false)
        } yield assertTrue(fs.length == 1, fs.head.name == "Good")
      }
    },
    test("loadFeatures with failFast=true fails on first bad file") {
      ZIO.scoped {
        for {
          dir    <- makeDir("gherkin-io-failfast")
          _      <- writeFile(dir, "bad.feature", "not valid gherkin")
          result <- GherkinParser.loadFeatures(dir, failFast = true).either
        } yield assertTrue(result.isLeft)
      }
    }
  )

  // ── Parse failure cases ───────────────────────────────────────────────────

  private val failures = suite("Parse failure cases")(
    test("content with no Feature keyword returns Left") {
      for {
        r <- parse("This is not a feature file at all.\nJust random text.\n").either
      } yield assertTrue(r.isLeft)
    },
    test("completely empty content returns Left") {
      for {
        r <- parse("").either
      } yield assertTrue(r.isLeft)
    }
  )

  // ── Directory helpers ─────────────────────────────────────────────────────

  private def makeDir(prefix: String) =
    ZIO.acquireRelease(
      ZIO
        .attempt(
          new java.io.File(
            java.lang.System.getProperty("java.io.tmpdir"),
            s"$prefix-${java.util.UUID.randomUUID()}"
          )
        )
        .tap(d => ZIO.attempt(d.mkdirs()))
    )(d => ZIO.attempt { Option(d.listFiles()).foreach(_.foreach(_.delete())); d.delete() }.orDie)

  private def writeFile(dir: java.io.File, name: String, content: String) =
    ZIO.attempt {
      val f = new java.io.File(dir, name)
      java.nio.file.Files.writeString(f.toPath, content)
    }

  // ── Spec ──────────────────────────────────────────────────────────────────

  def spec: Spec[TestEnvironment & Scope, Any] = suite("GherkinParser")(
    featureStructure,
    scenarioStructure,
    background,
    dataTables,
    outlines,
    docStrings,
    ruleKeyword,
    comments,
    fileIO,
    failures
  )
}
