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

  def buildScenarios(feature: Feature): ZIO[Any, BuildError, List[(List[GherkinStep], ScenarioMetadata)]] = {
    val allExamples = feature.scenarios.flatMap(_.examples)

    if (allExamples.isEmpty) {
      ZIO.succeed(
        feature.scenarios.map { scenario =>
          val baseSteps = feature.background ++ scenario.steps
          (baseSteps, scenario.metadata)
        }
      )
    } else {
      ZIO
        .foreach(allExamples) { row =>
          val exampleData = row.data
          ZIO.foreach(feature.scenarios) { scenario =>
            val baseSteps = feature.background ++ scenario.steps
            ZIO
              .foreach(baseSteps) { step =>
                parameterizeStep(step, exampleData)
              }
              .map { parameterizedSteps =>
                (parameterizedSteps, scenario.metadata)
              }
          }
        }
        .map(_.flatten.toList)
    }
  }

  private def parameterizeStep(
    step: GherkinStep,
    exampleData: Map[String, String]
  ): ZIO[Any, BuildError, GherkinStep] = {
    val placeholderPattern = "\\{([^:]+):([^}]+)\\}".r
    ZIO
      .foldLeft(placeholderPattern.findAllMatchIn(step.pattern).toList)(step.pattern) {
        (currentPattern, placeholderMatch) =>
          val placeholderName = placeholderMatch.group(1)
          val placeholderType = placeholderMatch.group(2)

          ZIO
            .fromOption(exampleData.get(placeholderName))
            .orElseFail(MissingPlaceholder(placeholderName, step.pattern, exampleData))
            .flatMap { rawValue =>
              validateAndConvert(placeholderName, placeholderType, rawValue, step.pattern).map { validatedValue =>
                currentPattern.replace(s"{$placeholderName:$placeholderType}", validatedValue)
              }
            }
      }
      .map(newPattern => GherkinStep(step.stepType, newPattern))
  }

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
