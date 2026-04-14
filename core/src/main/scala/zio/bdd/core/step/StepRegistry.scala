package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType

/**
 * Represents all ways step lookup can fail — typed, not wrapped in Throwable.
 */
sealed trait StepLookupError extends Product with Serializable:
  def toException: Throwable

/** No registered step definition matched the given step text. */
final case class NoMatchingStep(stepType: StepType, input: StepInput, snippet: String) extends StepLookupError:
  def toException: Throwable = new RuntimeException(snippet)

/** Two or more step definitions matched the same step text. */
final case class AmbiguousStep(stepType: StepType, input: StepInput, patterns: List[String]) extends StepLookupError:
  def toException: Throwable = new AmbiguousStepDefinitionException(stepType, input, patterns)

/** Thrown when two or more step definitions match the same step text. */
class AmbiguousStepDefinitionException(stepType: StepType, input: StepInput, patterns: List[String])
    extends RuntimeException(
      s"Ambiguous step definitions for $stepType '${input.text}'.\n" +
        s"Matched ${patterns.length} definitions:\n" +
        patterns.zipWithIndex.map { case (p, i) => s"  ${i + 1}. $p" }.mkString("\n") +
        "\nRename one of the step definitions to remove the ambiguity."
    )

trait StepRegistry[R, S]:
  /** Look up the effect to run for a given step. */
  def findStep(stepType: StepType, input: StepInput): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]]

final case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S]:
  def findStep(stepType: StepType, input: StepInput): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]] = {
    val typedMatches = steps.filter { case StepDefImpl(rt, _, _) => rt == stepType }
      .flatMap(s => s.tryExecute(input).map(effect => (s, effect)))

    typedMatches match {
      case Nil =>
        // Fall back to And steps as universal glue, then to any step type.
        // This matches Cucumber's semantics: keyword on the step definition is just
        // documentation — any step can match any keyword at runtime.
        val andMatches = steps.filter { case StepDefImpl(rt, _, _) => rt == StepType.AndStep }
          .flatMap(s => s.tryExecute(input).map(effect => (s, effect)))
        andMatches match {
          case (_, effect) :: Nil => ZIO.succeed(effect)
          case Nil                =>
            // Cross-keyword fallback: find a match across all step types.
            val allMatches = steps
              .flatMap(s => s.tryExecute(input).map(effect => (s, effect)))
            allMatches match {
              case Nil =>
                ZIO.fail(NoMatchingStep(stepType, input, generateSnippet(stepType, input)))
              case (_, effect) :: Nil =>
                ZIO.succeed(effect)
              case multiple =>
                disambiguate(multiple, stepType, input)
            }
          case multiple =>
            disambiguate(multiple, stepType, input)
        }

      case (_, effect) :: Nil =>
        ZIO.succeed(effect)

      case multiple =>
        disambiguate(multiple, stepType, input)
    }
  }

  // When several definitions match the same text, prefer the most specific one —
  // the definition with the fewest extractor placeholders. This lets a literal step
  // like `the response body is not empty` win over a wildcard `the response body is {string}`.
  // Only a tie at the most-specific level is reported as a genuine ambiguity.
  private def disambiguate(
    matches: List[(StepDef[R, S], RIO[R & State[S] & Scope, Unit])],
    stepType: StepType,
    input: StepInput
  ): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]] = {
    def extractorCount(sd: StepDef[R, S]): Int = sd match {
      case StepDefImpl(_, expr, _) => expr.parts.count { case _: Extractor[?] => true; case _ => false }
    }
    val fewest  = matches.map { case (sd, _) => extractorCount(sd) }.min
    val winners = matches.filter { case (sd, _) => extractorCount(sd) == fewest }
    winners match {
      case (_, effect) :: Nil => ZIO.succeed(effect)
      case _ =>
        ZIO.fail(AmbiguousStep(stepType, input, winners.map { case (StepDefImpl(_, expr, _), _) => expr.toString }))
    }
  }

  private def generateSnippet(stepType: StepType, input: StepInput): String = {
    val keyword = stepType match {
      case StepType.GivenStep => "Given"; case StepType.WhenStep => "When"
      case StepType.ThenStep  => "Then"; case StepType.ButStep   => "But"
      case StepType.AndStep   => "And"
    }
    val tableParam = if (input.table.isDefined) " / table[T]" else ""
    val docParam   = if (input.docString.isDefined) " / docString" else ""
    s"""No matching step found for $keyword '${input.text}'.

Implement it as:

  $keyword("${input.text}"$tableParam$docParam) { ZIO.unit }

Or with extractors (e.g.):

  $keyword("${inferStepExpr(input.text)}"$tableParam$docParam) { /* params */ => ZIO.unit }
"""
  }

  private val intPattern  = "-?\\d+".r
  private val boolPattern = "(true|false)".r

  private def inferStepExpr(text: String): String = {
    var result = text
    result = boolPattern.replaceAllIn(result, _ => "\" / boolean / \"")
    result = intPattern.replaceAllIn(result, _ => "\" / int / \"")
    s""""$result""""
  }

object StepRegistry:
  def layer[R: Tag, S: Tag](steps: List[StepDef[R, S]]): ZLayer[Any, Nothing, StepRegistry[R, S]] =
    ZLayer.succeed(StepRegistryLive(steps))

  def findStep[R: Tag, S: Tag](
    stepType: StepType,
    input: StepInput
  ): ZIO[StepRegistry[R, S], StepLookupError, RIO[R & State[S] & Scope, Unit]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(stepType, input))
