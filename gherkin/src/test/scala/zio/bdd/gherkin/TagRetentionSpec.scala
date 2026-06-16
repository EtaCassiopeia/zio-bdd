package zio.bdd.gherkin

import zio.test.*

/** Regression: a scenario's own tag line must not be swallowed by the preceding scenario's Examples-collection loop.
  * Before the fix, `parseScenario` greedily consumed the next scenario's tags while looking for an Examples block and
  * discarded them when none followed, so every scenario lost its tags to its predecessor.
  */
object TagRetentionSpec extends ZIOSpecDefault:
  private val feature =
    """Feature: F
      |  Background:
      |    Given setup
      |
      |  @first-tag
      |  Scenario: First
      |    Given setup
      |
      |  # a comment line
      |  # another comment
      |  @second-tag @spec-1.2.3
      |  Scenario: Second
      |    Given setup
      |
      |  @outline-tag
      |  Scenario Outline: Third
      |    Given a <x> thing
      |    @ex-tag
      |    Examples:
      |      | x   |
      |      | foo |
      |""".stripMargin

  def spec = suite("TagRetentionSpec")(
    test("each scenario keeps its own tags (incl. after comments and before Examples blocks)") {
      for f <- GherkinParser.parseFeature(feature, "repro.feature")
      yield assertTrue(
        f.scenarios.find(_.name == "First").exists(_.tags.contains("first-tag")),
        f.scenarios.find(_.name == "Second").exists(s => s.tags.contains("second-tag") && s.tags.contains("spec-1.2.3")),
        f.scenarios.exists(s => s.name.startsWith("Third") && s.tags.contains("outline-tag") && s.tags.contains("ex-tag"))
      )
    }
  )
