package zio.bdd.gherkin

import zio.test.*

/**
 * Strict/lint mode: an unrecognized line appearing after a scenario's steps
 * have begun is almost always a misspelled step keyword. By default such lines
 * are logged as warnings and dropped (back-compat); in strict mode they fail
 * the parse. Leading description text (before the first step) is legitimate
 * Gherkin and must never trigger a warning or failure.
 */
object LintWarningSpec extends ZIOSpecDefault:

  // "Gven" is a typo for "Given": it lands after a real step, so it is suspicious.
  private val misspelledAfterStep =
    """Feature: F
      |  Scenario: s
      |    Given a real step
      |    Gven another step
      |    Then a result
      |""".stripMargin

  // A narrative line BEFORE the first step is a valid scenario description.
  private val leadingDescription =
    """Feature: F
      |  Scenario: s
      |    This line describes the scenario.
      |    Given a real step
      |    Then a result
      |""".stripMargin

  def spec = suite("LintWarningSpec")(
    test("non-strict mode tolerates a misspelled-keyword line (dropped, no failure)") {
      for f <- GherkinParser.parseFeature(misspelledAfterStep, "lint.feature")
      yield assertTrue(
        f.scenarios.head.steps.map(_.pattern) == List("a real step", "a result")
      )
    },
    test("strict mode fails on a misspelled-keyword line after steps begin") {
      for exit <- GherkinParser.parseFeature(misspelledAfterStep, "lint.feature", strict = true).exit
      yield assertTrue(
        exit.isFailure,
        exit.causeOption.exists(_.failureOption.exists(_.getMessage.contains("Gven another step")))
      )
    },
    test("strict mode does NOT flag a leading scenario description (no false positive)") {
      for f <- GherkinParser.parseFeature(leadingDescription, "lint.feature", strict = true)
      yield assertTrue(
        f.scenarios.head.steps.map(_.pattern) == List("a real step", "a result")
      )
    }
  )
