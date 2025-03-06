package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{Feature, ScenarioMetadata, Step as GherkinStep}

object ParameterizedScenarioBuilder {

  // Builds a list of scenarios with their steps and metadata, handling parameterization from Examples
  def buildScenarios(feature: Feature): List[(List[GherkinStep], ScenarioMetadata)] = {
    val allExamples = feature.scenarios.flatMap(_.examples)
    if (allExamples.isEmpty) {
      // No examples: use base steps directly
      feature.scenarios.map { scenario =>
        val baseSteps = feature.background ++ scenario.steps
        (baseSteps, scenario.metadata)
      }
    } else {
      // Parameterize steps for each example row
      allExamples.flatMap { row =>
        val exampleData = row.data
        feature.scenarios.map { scenario =>
          val baseSteps = feature.background ++ scenario.steps
          val parameterizedSteps = baseSteps.map { step =>
            val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
            val newPattern = placeholderPattern.findAllMatchIn(step.pattern).foldLeft(step.pattern) {
              (currentPattern, placeholderMatch) =>
                val placeholderName = placeholderMatch.group(1)
                val placeholderType = placeholderMatch.group(2)
                val rawValue = exampleData.getOrElse(
                  placeholderName,
                  throw new IllegalArgumentException(
                    s"Placeholder '{$placeholderName}' in step '${step.pattern}' not found in Examples row: $exampleData"
                  )
                )
                // Validate and convert the placeholder value based on its type
                val validatedValue = placeholderType.toLowerCase match {
                  case "string" => rawValue
                  case "int" =>
                    try rawValue.toInt.toString
                    catch {
                      case _: NumberFormatException =>
                        throw new IllegalArgumentException(
                          s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}' is not an Int"
                        )
                    }
                  case "double" =>
                    try rawValue.toDouble.toString
                    catch {
                      case _: NumberFormatException =>
                        throw new IllegalArgumentException(
                          s"Value '$rawValue' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}' is not a Double"
                        )
                    }
                  case unknown =>
                    throw new IllegalArgumentException(
                      s"Unsupported type '$unknown' for placeholder '{$placeholderName:$placeholderType}' in step '${step.pattern}'"
                    )
                }
                currentPattern.replace(s"{$placeholderName:$placeholderType}", validatedValue)
            }
            GherkinStep(step.stepType, newPattern)
          }
          (parameterizedSteps, scenario.metadata)
        }
      }
    }
  }
}
