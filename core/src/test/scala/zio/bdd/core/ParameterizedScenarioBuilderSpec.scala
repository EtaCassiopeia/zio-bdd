package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{ExampleRow, Feature, Scenario, ScenarioMetadata, StepType, Step as GherkinStep, GherkinParser}
import zio.test.*
import zio.test.Assertion.*

object ParameterizedScenarioBuilderSpec extends ZIOSpecDefault {

  private def mkStep(
    stepType: StepType,
    pattern: String,
    file: Option[String] = Some("unknown.feature"),
    line: Option[Int] = None
  ): GherkinStep = GherkinStep(stepType, pattern, file = file, line = line)

  override def spec: Spec[Any, ParameterizedScenarioBuilder.BuildError] =
    suite("ParameterizedScenarioBuilder")(
      test("buildScenarios handles a feature with no examples") {
        val feature = Feature(
          name = "Simple Feature",
          background = List(mkStep(StepType.GivenStep, "I have a system")),
          scenarios = List(
            Scenario(
              name = "Simple Scenario",
              steps = List(
                mkStep(StepType.WhenStep, "I perform an action"),
                mkStep(StepType.ThenStep, "I see a result")
              ),
              examples = Nil,
              metadata = ScenarioMetadata()
            )
          )
        )

        val expected = List(
          (
            "Simple Scenario",
            List(
              mkStep(StepType.GivenStep, "I have a system"),
              mkStep(StepType.WhenStep, "I perform an action"),
              mkStep(StepType.ThenStep, "I see a result")
            ),
            ScenarioMetadata()
          )
        )

        for {
          result <- ParameterizedScenarioBuilder.buildScenarios(feature)
        } yield assertTrue(result == expected)
      },
      test("buildScenarios parameterizes steps with examples") {
        val scenarioName = "Parameterized Scenario"
        val feature = Feature(
          name = "Parameterized Feature",
          background = List(mkStep(StepType.GivenStep, "I start with {initial:int}")),
          scenarios = List(
            Scenario(
              name = scenarioName,
              steps = List(
                mkStep(StepType.WhenStep, "I add {value:int}"),
                mkStep(StepType.ThenStep, "I get {result:double}")
              ),
              examples = List(
                ExampleRow(Map("initial" -> "1", "value" -> "2", "result" -> "3.0")),
                ExampleRow(Map("initial" -> "5", "value" -> "10", "result" -> "15.0"))
              ),
              metadata = ScenarioMetadata()
            )
          )
        )

        val expected = List(
          (
            scenarioName,
            List(
              mkStep(StepType.GivenStep, "I start with 1"),
              mkStep(StepType.WhenStep, "I add 2"),
              mkStep(StepType.ThenStep, "I get 3.0")
            ),
            ScenarioMetadata()
          ),
          (
            scenarioName,
            List(
              mkStep(StepType.GivenStep, "I start with 5"),
              mkStep(StepType.WhenStep, "I add 10"),
              mkStep(StepType.ThenStep, "I get 15.0")
            ),
            ScenarioMetadata()
          )
        )

        for {
          result <- ParameterizedScenarioBuilder.buildScenarios(feature)
        } yield assertTrue(result == expected)
      },
      test("buildScenarios fails with MissingPlaceholder when a placeholder is not in examples") {
        val feature = Feature(
          name = "Error Feature",
          background = Nil,
          scenarios = List(
            Scenario(
              name = "Missing Placeholder Scenario",
              steps = List(mkStep(StepType.GivenStep, "I use {missing:string}")),
              examples = List(ExampleRow(Map("other" -> "value"))),
              metadata = ScenarioMetadata()
            )
          )
        )

        val expectedError = ParameterizedScenarioBuilder.MissingPlaceholder(
          placeholder = "missing",
          stepPattern = "I use {missing:string}",
          exampleData = Map("other" -> "value")
        )

        for {
          result <- ParameterizedScenarioBuilder.buildScenarios(feature).either
        } yield assert(result)(isLeft(equalTo(expectedError)))
      },
      test("buildScenarios fails with InvalidFormat when a value cannot be converted") {
        val feature = Feature(
          name = "Error Feature",
          background = Nil,
          scenarios = List(
            Scenario(
              name = "Invalid Format Scenario",
              steps = List(mkStep(StepType.GivenStep, "I use {number:int}")),
              examples = List(ExampleRow(Map("number" -> "not-a-number"))),
              metadata = ScenarioMetadata()
            )
          )
        )

        for {
          result <- ParameterizedScenarioBuilder.buildScenarios(feature).either
        } yield assert(result)(isLeft(isSubtype[ParameterizedScenarioBuilder.InvalidFormat](anything)))
      },
      test("buildScenarios fails with UnsupportedType when a placeholder type is unknown") {
        val feature = Feature(
          name = "Error Feature",
          background = Nil,
          scenarios = List(
            Scenario(
              name = "Unsupported Type Scenario",
              steps = List(mkStep(StepType.GivenStep, "I use {value:unknown}")),
              examples = List(ExampleRow(Map("value" -> "test"))),
              metadata = ScenarioMetadata()
            )
          )
        )

        val expectedError = ParameterizedScenarioBuilder.UnsupportedType(
          placeholder = "value",
          placeholderType = "unknown",
          stepPattern = "I use {value:unknown}"
        )

        for {
          result <- ParameterizedScenarioBuilder.buildScenarios(feature).either
        } yield assert(result)(isLeft(equalTo(expectedError)))
      },
      test("buildScenarios handles a simple Gherkin feature with no examples") {
        val gherkin =
          """Feature: Basic Arithmetic
            |  Background:
            |    Given I have a calculator
            |  Scenario: Addition
            |    When I add 2 and 3
            |    Then I see 5
            |""".stripMargin

        val expected = List(
          (
            "Addition",
            List(
              mkStep(StepType.GivenStep, "I have a calculator", line = Some(3)),
              mkStep(StepType.WhenStep, "I add 2 and 3", line = Some(5)),
              mkStep(StepType.ThenStep, "I see 5", line = Some(6))
            ),
            ScenarioMetadata()
          )
        )

        for {
          feature <-
            GherkinParser
              .parseFeature(gherkin)
              .mapError(e => throw new RuntimeException("Parse error: " + e.getMessage) // Fail test if parsing fails
              )
          result <- ParameterizedScenarioBuilder.buildScenarios(feature)
        } yield assertTrue(result == expected)
      },
      test("buildScenarios parameterizes a Gherkin feature with examples") {
        val scenarioName = "Addition"
        val gherkin =
          """Feature: Parameterized Arithmetic
            |  Background:
            |    Given I start with {initial:int}
            |  Scenario Outline: Addition
            |    When I add {value:int}
            |    Then I get {result:double}
            |  Examples:
            |    | initial | value | result |
            |    | 1       | 2     | 3.0    |
            |    | 5       | 10    | 15.0   |
            |""".stripMargin

        val expected = List(
          (
            scenarioName,
            List(
              mkStep(StepType.GivenStep, "I start with 1", line = Some(3)),
              mkStep(StepType.WhenStep, "I add 2", line = Some(5)),
              mkStep(StepType.ThenStep, "I get 3.0", line = Some(6))
            ),
            ScenarioMetadata()
          ),
          (
            scenarioName,
            List(
              mkStep(StepType.GivenStep, "I start with 5", line = Some(3)),
              mkStep(StepType.WhenStep, "I add 10", line = Some(5)),
              mkStep(StepType.ThenStep, "I get 15.0", line = Some(6))
            ),
            ScenarioMetadata()
          )
        )

        for {
          feature <-
            GherkinParser
              .parseFeature(gherkin)
              .mapError(e => throw new RuntimeException("Parse error: " + e.getMessage) // Fail test if parsing fails
              )
          result <- ParameterizedScenarioBuilder.buildScenarios(feature)
        } yield assertTrue(result == expected)
      },
      test("buildScenarios fails with MissingPlaceholder for Gherkin with missing placeholder") {
        val gherkin =
          """Feature: Error Case
            |  Scenario Outline: Missing Placeholder
            |    Given I use {missing:string}
            |  Examples:
            |    | other |
            |    | value |
            |""".stripMargin

        val expectedError = ParameterizedScenarioBuilder.MissingPlaceholder(
          placeholder = "missing",
          stepPattern = "I use {missing:string}",
          exampleData = Map("other" -> "value")
        )

        for {
          feature <-
            GherkinParser
              .parseFeature(gherkin)
              .mapError(e => throw new RuntimeException("Parse error: " + e.getMessage) // Fail test if parsing fails
              )
          result <- ParameterizedScenarioBuilder.buildScenarios(feature).either
        } yield assert(result)(isLeft(equalTo(expectedError)))
      }
    )
}
