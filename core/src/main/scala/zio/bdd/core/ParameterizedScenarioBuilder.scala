package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step => GherkinStep}

object ParameterizedScenarioBuilder {

  sealed trait BuildError
  case class MissingPlaceholder(placeholder: String, stepPattern: String, exampleData: Map[String, String])
      extends BuildError
  case class InvalidFormat(
    placeholder: String,
    placeholderType: String,
    value: String,
    stepPattern: String,
    cause: Throwable
  ) extends BuildError
  case class UnsupportedType(placeholder: String, placeholderType: String, stepPattern: String) extends BuildError

  /**
   * Builds a list of scenarios from a Gherkin Feature, parameterizing steps
   * with data from Examples tables if present. Each scenario is returned as a
   * tuple of its steps and metadata.
   *
   * @param feature
   *   The Gherkin Feature containing scenarios, background steps, and optional
   *   Examples
   * @return
   *   A ZIO effect producing a list of (steps, metadata) pairs, failing with
   *   BuildError if parameterization fails
   */
  def buildScenarios(feature: Feature): ZIO[Any, BuildError, List[(List[GherkinStep], ScenarioMetadata)]] = {
    val allExamples = feature.scenarios.flatMap(_.examples)

    if (allExamples.isEmpty) {
      // Case 1: No Examples table - use the base steps as-is
      ZIO.succeed(
        feature.scenarios.map { scenario =>
          // Combine background steps (common to all scenarios) with scenario-specific steps
          val baseSteps = feature.background ++ scenario.steps
          (baseSteps, scenario.metadata)
        }
      )
    } else {
      // Case 2: Examples table present - parameterize steps for each row
      ZIO
        .foreach(allExamples) { row =>
          val exampleData = row.data // Map of placeholder names to values for this row
          ZIO.foreach(feature.scenarios) { scenario =>
            val baseSteps = feature.background ++ scenario.steps // Combine background and scenario steps
            ZIO
              .foreach(baseSteps) { step =>
                parameterizeStep(step, exampleData) // Replace placeholders with values from this row
              }
              .map { parameterizedSteps =>
                (parameterizedSteps, scenario.metadata) // Pair the parameterized steps with metadata
              }
          }
        }
        .map(_.flatten.toList) // Flatten the nested structure into a single list of scenarios
    }
  }

  /**
   * Parameterizes a single Gherkin step by replacing placeholders (e.g.,
   * "{name:string}") with values from exampleData.
   *
   * @param step
   *   The Gherkin step to parameterize
   * @param exampleData
   *   Map of placeholder names to values from an Examples row
   * @return
   *   A ZIO effect producing the parameterized step, failing with BuildError if
   *   issues arise
   */
  private def parameterizeStep(
    step: GherkinStep,
    exampleData: Map[String, String]
  ): ZIO[Any, BuildError, GherkinStep] = {
    val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
    ZIO
      .foldLeft(placeholderPattern.findAllMatchIn(step.pattern).toList)(step.pattern) {
        (currentPattern, placeholderMatch) =>
          val placeholderName = placeholderMatch.group(1) // e.g., "name"
          val placeholderType = placeholderMatch.group(2) // e.g., "string"

          ZIO
            .fromOption(exampleData.get(placeholderName))
            .orElseFail(MissingPlaceholder(placeholderName, step.pattern, exampleData))
            .flatMap { rawValue =>
              // Validate and convert the raw value to the specified type
              validateAndConvert(placeholderName, placeholderType, rawValue, step.pattern).map { validatedValue =>
                currentPattern.replace(s"{$placeholderName:$placeholderType}", validatedValue)
              }
            }
      }
      .map(newPattern => GherkinStep(step.stepType, newPattern))
  }

  /**
   * Validates and converts a raw value to the type specified in the placeholder
   * (e.g., "int", "double").
   *
   * @param placeholderName
   *   Name of the placeholder (for error reporting)
   * @param placeholderType
   *   Expected type of the value (e.g., "string", "int")
   * @param rawValue
   *   The value from the Examples table
   * @param stepPattern
   *   The original step pattern (for error reporting)
   * @return
   *   A ZIO effect producing the converted value as a string, failing with
   *   BuildError if validation fails
   */
  private def validateAndConvert(
    placeholderName: String,
    placeholderType: String,
    rawValue: String,
    stepPattern: String
  ): ZIO[Any, BuildError, String] =
    placeholderType.toLowerCase match {
      case "string" => ZIO.succeed(rawValue)
      case "int" =>
        ZIO
          .attempt(rawValue.toInt.toString)
          .mapError(e => InvalidFormat(placeholderName, placeholderType, rawValue, stepPattern, e))
      case "double" =>
        ZIO
          .attempt(rawValue.toDouble.toString)
          .mapError(e => InvalidFormat(placeholderName, placeholderType, rawValue, stepPattern, e))
      case unknown =>
        ZIO.fail(UnsupportedType(placeholderName, unknown, stepPattern))
    }
}
