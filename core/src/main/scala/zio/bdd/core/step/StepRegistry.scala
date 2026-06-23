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

/**
 * Represents all ways resolving a `@property` step template (one that still has
 * `<col>` placeholders, e.g. "an account with balance <balance>") against the
 * registry can fail.
 */
sealed trait TemplateLookupError extends Product with Serializable:
  def toException: Throwable

/** No registered step definition structurally matches the given template. */
final case class NoMatchingTemplate(stepType: StepType, template: String) extends TemplateLookupError:
  def toException: Throwable =
    new RuntimeException(s"No step definition structurally matches $stepType template: '$template'")

/** Two or more step definitions structurally match the same template. */
final case class AmbiguousTemplate(stepType: StepType, template: String, patterns: List[String])
    extends TemplateLookupError:
  def toException: Throwable = new AmbiguousTemplateException(stepType, template, patterns)

/**
 * Thrown when two or more step definitions structurally match the same
 * template.
 */
class AmbiguousTemplateException(stepType: StepType, template: String, patterns: List[String])
    extends RuntimeException(
      s"Ambiguous step definitions for $stepType template '$template'.\n" +
        s"Matched ${patterns.length} definitions structurally:\n" +
        patterns.zipWithIndex.map { case (p, i) => s"  ${i + 1}. $p" }.mkString("\n") +
        "\nRename one of the step definitions to remove the ambiguity."
    )

trait StepRegistry[R, S]:
  /** Look up the effect to run for a given step. */
  def findStep(stepType: StepType, input: StepInput): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]]

  /**
   * Resolve which extractor governs each `<col>` placeholder in a `@property`
   * step template, by structurally matching it against the registered StepDefs
   * for `stepType` (see `StepExpression.structuralMatch`). Returns the ordered
   * `(columnName, Tag[_])` pairs from whichever StepDef matches.
   */
  def resolveTemplateColumns(stepType: StepType, template: String): IO[TemplateLookupError, List[(String, Tag[?])]]

final case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S]:
  // Index definitions by their declared keyword once at construction, so per-step lookup
  // is a map access rather than a full scan. Registration order is preserved per bucket.
  private val byKeyword: Map[StepType, List[StepDef[R, S]]] =
    steps.groupBy { case StepDefImpl(rt, _, _) => rt }

  private def matchesFor(
    stepType: StepType,
    input: StepInput
  ): List[(StepDef[R, S], RIO[R & State[S] & Scope, Unit])] =
    byKeyword.getOrElse(stepType, Nil).flatMap(s => s.tryExecute(input).map(effect => (s, effect)))

  def findStep(stepType: StepType, input: StepInput): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]] = {
    val typedMatches = matchesFor(stepType, input)

    typedMatches match {
      case Nil =>
        // Fall back to And steps as universal glue, then to any step type.
        // This matches Cucumber's semantics: keyword on the step definition is just
        // documentation — any step can match any keyword at runtime.
        val andMatches = matchesFor(StepType.AndStep, input)
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

  // The definition with the fewest extractor placeholders is the most specific (most
  // literal) one. Shared by literal-text disambiguation (`disambiguate`) and template
  // disambiguation (`disambiguateTemplate`) — both prefer the same specificity ordering.
  private def extractorCount(sd: StepDef[R, S]): Int = sd match {
    case StepDefImpl(_, expr, _) => expr.parts.count { case _: Extractor[?] => true; case _ => false }
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
    val fewest  = matches.map { case (sd, _) => extractorCount(sd) }.min
    val winners = matches.filter { case (sd, _) => extractorCount(sd) == fewest }
    winners match {
      case (_, effect) :: Nil => ZIO.succeed(effect)
      case _ =>
        ZIO.fail(AmbiguousStep(stepType, input, winners.map { case (StepDefImpl(_, expr, _), _) => expr.toString }))
    }
  }

  def resolveTemplateColumns(
    stepType: StepType,
    template: String
  ): IO[TemplateLookupError, List[(String, Tag[?])]] = {
    def structuralMatchesFor(st: StepType): List[(StepDef[R, S], List[(String, Tag[?])])] =
      byKeyword
        .getOrElse(st, Nil)
        .collect { case sd @ StepDefImpl(_, expr, _) =>
          expr.structuralMatch(template).map(cols => (sd, cols))
        }
        .flatten

    val typedMatches = structuralMatchesFor(stepType)
    typedMatches match {
      case Nil =>
        // Same fallback chain as findStep: And steps as universal glue, then any step type.
        val andMatches = structuralMatchesFor(StepType.AndStep)
        andMatches match {
          case (_, cols) :: Nil => ZIO.succeed(cols)
          case Nil =>
            val allMatches = steps.collect { case sd @ StepDefImpl(_, expr, _) =>
              expr.structuralMatch(template).map(cols => (sd, cols))
            }.flatten
            allMatches match {
              case Nil              => ZIO.fail(NoMatchingTemplate(stepType, template))
              case (_, cols) :: Nil => ZIO.succeed(cols)
              case multiple         => disambiguateTemplate(multiple, stepType, template)
            }
          case multiple => disambiguateTemplate(multiple, stepType, template)
        }
      case (_, cols) :: Nil => ZIO.succeed(cols)
      case multiple         => disambiguateTemplate(multiple, stepType, template)
    }
  }

  // Same fewest-extractors specificity heuristic as `disambiguate`, applied to templates.
  private def disambiguateTemplate(
    matches: List[(StepDef[R, S], List[(String, Tag[?])])],
    stepType: StepType,
    template: String
  ): IO[TemplateLookupError, List[(String, Tag[?])]] = {
    val fewest  = matches.map { case (sd, _) => extractorCount(sd) }.min
    val winners = matches.filter { case (sd, _) => extractorCount(sd) == fewest }
    winners match {
      case (_, cols) :: Nil => ZIO.succeed(cols)
      case _ =>
        ZIO.fail(
          AmbiguousTemplate(stepType, template, winners.map { case (StepDefImpl(_, expr, _), _) => expr.toString })
        )
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

  def resolveTemplateColumns[R: Tag, S: Tag](
    stepType: StepType,
    template: String
  ): ZIO[StepRegistry[R, S], TemplateLookupError, List[(String, Tag[?])]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.resolveTemplateColumns(stepType, template))
