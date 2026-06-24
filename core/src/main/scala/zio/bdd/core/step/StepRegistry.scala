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

/**
 * The same `<col>` placeholder appears more than once in `template`, governed
 * by extractors that produce different types (e.g. `<amount>` captured once as
 * `Tag[Int]` and once as `Tag[Long]`). `PropertyExecutor` samples exactly one
 * string value per column name and substitutes it into every occurrence, so a
 * single sampled value can never satisfy two different extractor types for the
 * same placeholder — this is a step-definition bug, not ambiguity.
 */
final case class ConflictingColumnType(
  stepType: StepType,
  template: String,
  column: String,
  first: Tag[?],
  second: Tag[?]
) extends TemplateLookupError:
  def toException: Throwable = new RuntimeException(
    s"Column '$column' in $stepType template '$template' is captured as both $first and $second.\n" +
      "Use one consistent extractor type for this placeholder, or rename one of the occurrences."
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
  // Everything below is read-only after construction (`steps`, `byKeyword`, and every
  // method here are pure functions over immutable data), so a single StepRegistryLive
  // instance is safe to share and call concurrently across parallel scenario fibers —
  // there's no mutable state to race on.
  private val byKeyword: Map[StepType, List[StepDef[R, S]]] =
    steps.groupBy { case StepDefImpl(rt, _, _) => rt }

  /**
   * Shared fallback cascade for both literal-text lookup (`findStep`) and
   * template lookup (`resolveTemplateColumns`): try the exact keyword first; if
   * nothing matches at all, fall back to And steps as universal glue, then to
   * any step type. This mirrors Cucumber semantics — the keyword on a step
   * definition is just documentation; any step can match any keyword at
   * runtime. Stops at the first level that has at least one match, even if that
   * level has several (disambiguation happens one level up).
   */
  private def candidatesWithFallback[A](
    stepType: StepType,
    tryMatch: StepDef[R, S] => Option[A]
  ): List[(StepDef[R, S], A)] = {
    def matchesIn(candidates: List[StepDef[R, S]]): List[(StepDef[R, S], A)] =
      candidates.flatMap(sd => tryMatch(sd).map(a => (sd, a)))

    val exact = matchesIn(byKeyword.getOrElse(stepType, Nil))
    if (exact.nonEmpty) exact
    else {
      val and = matchesIn(byKeyword.getOrElse(StepType.AndStep, Nil))
      if (and.nonEmpty) and else matchesIn(steps)
    }
  }

  // The definition with the fewest extractor placeholders is the most specific (most
  // literal) one. This lets a literal step like `the response body is not empty` win
  // over a wildcard `the response body is {string}`. Only a tie at the most-specific
  // level is a genuine ambiguity. Shared by both literal-text and template lookup —
  // both prefer the same specificity ordering.
  private def extractorCount(sd: StepDef[R, S]): Int = sd match {
    case StepDefImpl(_, expr, _) => expr.parts.count { case _: Extractor[?] => true; case _ => false }
  }

  private def fewestExtractorsWinner[A](matches: List[(StepDef[R, S], A)]): Either[List[String], A] = {
    val fewest  = matches.map { case (sd, _) => extractorCount(sd) }.min
    val winners = matches.filter { case (sd, _) => extractorCount(sd) == fewest }
    winners match {
      case (_, a) :: Nil => Right(a)
      case _             => Left(winners.map { case (StepDefImpl(_, expr, _), _) => expr.toString })
    }
  }

  def findStep(stepType: StepType, input: StepInput): IO[StepLookupError, RIO[R & State[S] & Scope, Unit]] =
    candidatesWithFallback(stepType, (sd: StepDef[R, S]) => sd.tryExecute(input)) match {
      case Nil                => ZIO.fail(NoMatchingStep(stepType, input, generateSnippet(stepType, input)))
      case (_, effect) :: Nil => ZIO.succeed(effect)
      case multiple =>
        ZIO.fromEither(fewestExtractorsWinner(multiple).left.map(patterns => AmbiguousStep(stepType, input, patterns)))
    }

  def resolveTemplateColumns(
    stepType: StepType,
    template: String
  ): IO[TemplateLookupError, List[(String, Tag[?])]] = {
    def structuralMatchOf(sd: StepDef[R, S]): Option[List[(String, Tag[?])]] = sd match {
      case StepDefImpl(_, expr, _) => expr.structuralMatch(template)
    }

    val resolved: IO[TemplateLookupError, List[(String, Tag[?])]] =
      candidatesWithFallback(stepType, structuralMatchOf) match {
        case Nil              => ZIO.fail(NoMatchingTemplate(stepType, template))
        case (_, cols) :: Nil => ZIO.succeed(cols)
        case multiple =>
          ZIO.fromEither(
            fewestExtractorsWinner(multiple).left.map(patterns => AmbiguousTemplate(stepType, template, patterns))
          )
      }
    resolved.flatMap(cols => validateColumnTypes(stepType, template, cols))
  }

  // A `<col>` placeholder can legitimately appear more than once in one template (the
  // same sampled value is substituted everywhere) — but only if every occurrence is
  // governed by the same Tag. If two occurrences disagree, no single sampled value can
  // satisfy both, so this is reported distinctly from "no match"/"ambiguous": it's a
  // bug in how the matching StepDef's extractors are typed, not a lookup failure.
  private def validateColumnTypes(
    stepType: StepType,
    template: String,
    cols: List[(String, Tag[?])]
  ): IO[TemplateLookupError, List[(String, Tag[?])]] = {
    // Walk left to right (template order) rather than `groupBy` (Map iteration order is
    // unspecified) so the reported conflict is always the first one a developer would
    // spot reading the template themselves.
    @scala.annotation.tailrec
    def firstConflict(remaining: List[(String, Tag[?])], seen: Map[String, Tag[?]]): Option[ConflictingColumnType] =
      remaining match {
        case Nil => None
        case (column, tag) :: rest =>
          seen.get(column) match {
            case Some(prior) if prior != tag => Some(ConflictingColumnType(stepType, template, column, prior, tag))
            case _                           => firstConflict(rest, seen.updated(column, tag))
          }
      }
    firstConflict(cols, Map.empty) match {
      case Some(conflict) => ZIO.fail(conflict)
      case None           => ZIO.succeed(cols)
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
