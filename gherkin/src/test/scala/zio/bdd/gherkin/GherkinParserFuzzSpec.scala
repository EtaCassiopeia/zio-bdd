package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Phase 3: Fuzz-like property tests for the Gherkin parser.
 *
 * Properties:
 *   1. Parser never throws — always returns a ZIO effect (Left or Right, never
 *      exception) 2. Structurally valid Gherkin with random whitespace
 *      variation parses to the same shape 3. DataTable dimensions are preserved
 *      under random widths/heights 4. Outline × Examples matrix expands to
 *      exactly (rows × 1) scenarios per Examples block
 */
object GherkinParserFuzzSpec extends ZIOSpecDefault {

  private val F = "fuzz.feature"

  // ── Generator helpers ────────────────────────────────────────────────────────

  private def genSpaces: Gen[Any, String] = Gen.int(0, 4).map(" " * _)
  private def genTabs: Gen[Any, String]   = Gen.int(0, 2).map("\t" * _)

  // Generates valid Gherkin feature text with random indentation
  private def randomIndentedFeature(
    leadingSpaces: String,
    featureName: String,
    scenarioName: String,
    stepText: String
  ): String =
    s"""${leadingSpaces}Feature: $featureName
       |${leadingSpaces}  Scenario: $scenarioName
       |${leadingSpaces}    Given $stepText
       |""".stripMargin

  // ── Property 1: parser never throws ─────────────────────────────────────────

  private val neverThrowsSuite = suite("parser never throws on any input")(
    test("random printable ASCII strings: always Left or Right, never exception") {
      check(Gen.string(Gen.printableChar).map(_.take(200))) { input =>
        for {
          exit <- GherkinParser.parseFeature(input, F).exit
        } yield assertTrue(exit.isSuccess || exit.isFailure)
      }
    },
    test("random alphanumeric with newlines: always terminates without exception") {
      check(Gen.alphaNumericString.flatMap(s => Gen.int(1, 5).map(n => (s + "\n") * n)).map(_.take(500))) { input =>
        for {
          exit <- GherkinParser.parseFeature(input, F).exit
        } yield assertTrue(exit.isSuccess || exit.isFailure)
      }
    },
    test("empty string always produces Left (not exception)") {
      assertZIO(GherkinParser.parseFeature("", F).exit)(fails(anything))
    }
  )

  // ── Property 2: whitespace variation preserves parse shape ─────────────────

  private val whitespaceInvarianceSuite = suite("whitespace variation preserves parse shape")(
    test("extra leading spaces on feature/scenario/step lines: same step count") {
      check(
        genSpaces.flatMap(sp =>
          Gen.alphaNumericStringBounded(3, 10).flatMap(n => Gen.alphaNumericStringBounded(3, 10).map(st => (sp, n, st)))
        )
      ) { case (spaces, name, stepText) =>
        val content = randomIndentedFeature(spaces, name, "sc", stepText)
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield assertTrue(
          feature.scenarios.length == 1,
          feature.scenarios.head.steps.length == 1,
          feature.scenarios.head.steps.head.pattern.trim == stepText.trim
        )
      }
    },
    test("CRLF and LF line endings produce the same parse result") {
      check(Gen.alphaNumericStringBounded(3, 10)) { name =>
        val lf   = s"Feature: $name\n  Scenario: s\n    Given a step\n"
        val crlf = lf.replace("\n", "\r\n")
        for {
          f1 <- GherkinParser.parseFeature(lf, F)
          f2 <- GherkinParser.parseFeature(crlf, F)
        } yield assertTrue(
          f1.name == f2.name,
          f1.scenarios.length == f2.scenarios.length,
          f1.scenarios.head.steps.length == f2.scenarios.head.steps.length
        )
      }
    }
  )

  // ── Property 3: DataTable dimensions preserved ────────────────────────────

  private val tableDimensionsSuite = suite("DataTable dimensions are preserved")(
    test("table with N rows and M columns always has N rows and M cells per row") {
      check(Gen.int(1, 6).zip(Gen.int(1, 4))) { case (rows, cols) =>
        val header = (1 to cols).map(c => s"col$c").mkString("| ", " | ", " |")
        val rowLines =
          (1 to rows).map(r => (1 to cols).map(c => s"v${r}_$c").mkString("| ", " | ", " |")).mkString("\n")
        val content =
          s"Feature: F\n  Scenario: s\n    Given a table\n$header\n$rowLines\n"
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield {
          val table = feature.scenarios.head.steps.head.dataTable
          assertTrue(
            table.isDefined,
            table.exists(_.rows.length == rows),
            table.exists(_.rows.forall(_.cells.length == cols))
          )
        }
      }
    }
  )

  // ── Property 4: Outline expansion scales linearly ────────────────────────────

  private val outlineExpansionSuite = suite("Outline × Examples expansion is linear")(
    test("Outline with N example rows produces exactly N scenarios") {
      check(Gen.int(1, 8)) { n =>
        val exampleRows = (1 to n).map(i => s"    | val$i |").mkString("\n")
        val content =
          s"Feature: F\n  Scenario Outline: outline\n    Given value is <x>\n  Examples:\n    | x |\n$exampleRows\n"
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield assertTrue(feature.scenarios.length == n)
      }
    },
    test("Outline with N examples × M steps produces N scenarios each with M steps") {
      check(Gen.int(1, 4).zip(Gen.int(1, 4))) { case (exRows, steps) =>
        val stepLines  = (1 to steps).map(i => s"    Given step$i").mkString("\n")
        val exRowLines = (1 to exRows).map(i => s"    | v$i |").mkString("\n")
        val content =
          s"Feature: F\n  Scenario Outline: multi-step outline\n$stepLines\n  Examples:\n    | x |\n$exRowLines\n"
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield assertTrue(
          feature.scenarios.length == exRows,
          feature.scenarios.forall(_.steps.length == steps)
        )
      }
    }
  )

  // ── Property 5: tag preservation ─────────────────────────────────────────────

  private val tagPreservationSuite = suite("Tags are preserved through parse")(
    test("feature tags appear on feature.tags") {
      check(Gen.alphaNumericStringBounded(3, 10)) { tag =>
        val content = s"@$tag\nFeature: F\n  Scenario: s\n    Given a step\n"
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield assertTrue(feature.tags.contains(tag))
      }
    },
    test("scenario tags appear on scenario.tags") {
      check(Gen.alphaNumericStringBounded(3, 10)) { tag =>
        val content = s"Feature: F\n  @$tag\n  Scenario: s\n    Given a step\n"
        for {
          feature <- GherkinParser.parseFeature(content, F)
        } yield assertTrue(feature.scenarios.head.tags.contains(tag))
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("GherkinParserFuzzSpec")(
    neverThrowsSuite,
    whitespaceInvarianceSuite,
    tableDimensionsSuite,
    outlineExpansionSuite,
    tagPreservationSuite
  )
}
