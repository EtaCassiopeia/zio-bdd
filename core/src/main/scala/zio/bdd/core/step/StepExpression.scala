package zio.bdd.core.step

import izumi.reflect.Tag

import java.util.regex.Pattern

sealed trait StepPart
case class Literal(value: String)                     extends StepPart
case class Extractor[T](extractor: TypedExtractor[T]) extends StepPart

/**
 * A typed step expression built from literal parts and typed extractors. The
 * regex is compiled once (lazy) rather than on every match call.
 */
case class StepExpression[Out <: Tuple](parts: List[StepPart]) {

  // Compiled once on first use, not on every extract() call (r08 perf fix)
  private lazy val compiledPattern: Pattern =
    parts.map {
      case Literal(s)   => Pattern.quote(s)
      case Extractor(e) => e.pattern
    }.mkString.r.pattern

  /**
   * Structural match against a `@property` step template that still has `<col>`
   * placeholders in place of real values (e.g. "an account with balance
   * <balance>"). Unlike `extract`, this does not require the placeholder text
   * to satisfy an extractor's value-conformance regex — it only checks that
   * literal segments align exactly and that each extractor position lines up
   * with a placeholder, left to right.
   *
   * Returns the ordered `(columnName, Tag[_])` pairs naming which extractor
   * governs each placeholder, or `None` if `template`'s literal/extractor
   * structure doesn't align with this `StepExpression` at all (including the
   * common case of a template with no placeholders facing a StepExpression that
   * has extractors — there's no placeholder to supply their type).
   *
   * Purely structural: if the same column name appears more than once, every
   * occurrence is included in order, even if they're governed by different
   * `Tag`s. This method doesn't judge whether that's usable — that's a
   * cross-cutting concern about the whole template, not about whether one
   * `StepExpression` aligns with it, so `StepRegistry.resolveTemplateColumns`
   * is what rejects that case (as `ConflictingColumnType`).
   */
  def structuralMatch(template: String): Option[List[(String, Tag[?])]] =
    StepExpression.structuralMatch(template, parts)

  def extract(input: StepInput): Option[Out] = {
    val matcher = compiledPattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      extractValues(input, groups, 0, parts.collect { case Extractor(e) => e })
    } else None
  }

  private def extractValues(
    input: StepInput,
    groups: List[String],
    index: Int,
    extractors: List[TypedExtractor[?]]
  ): Option[Out] =
    extractors match {
      case Nil => Some(EmptyTuple.asInstanceOf[Out])
      case head :: tail =>
        head.extract(input, groups, index) match {
          case Right((value, nextIndex)) =>
            extractValues(input, groups, nextIndex, tail).map(rest => (value *: rest).asInstanceOf[Out])
          case Left(_) => None
        }
    }

  override def toString: String =
    parts.map {
      case Literal(s)   => s"\"$s\""
      case Extractor(e) => s"<${e.getClass.getSimpleName.stripSuffix("$")}>"
    }.mkString(" / ")
}

object StepExpression {

  private val placeholderRe = "<([^>]+)>".r

  /**
   * Split a property-test template into literal text and `<col>` placeholder
   * tokens, in left-to-right order. Mirrors the same `<col>` syntax
   * `PropertyExecutor.substituteSteps` substitutes real values into.
   */
  private[step] def templateTokens(template: String): List[Either[String, String]] = {
    val tokens = List.newBuilder[Either[String, String]]
    var idx    = 0
    placeholderRe.findAllMatchIn(template).foreach { m =>
      val literal = template.substring(idx, m.start)
      if (literal.nonEmpty) tokens += Left(literal)
      tokens += Right(m.group(1))
      idx = m.end
    }
    val tail = template.substring(idx)
    if (tail.nonEmpty) tokens += Left(tail)
    tokens.result()
  }

  /**
   * Collapse a parts list into the same `Left(literal)/Right(extractor)` token
   * shape as `templateTokens` — merging adjacent literals and dropping empty
   * ones — so the two sequences can be compared positionally.
   */
  private[step] def partTokens(parts: List[StepPart]): List[Either[String, TypedExtractor[?]]] =
    parts
      .foldLeft(List.empty[Either[String, TypedExtractor[?]]]) { (acc, part) =>
        part match {
          case Literal(s) if s.isEmpty => acc
          case Literal(s) =>
            acc match {
              case Left(prev) :: rest => Left(prev + s) :: rest
              case _                  => Left(s) :: acc
            }
          case Extractor(e) => Right(e) :: acc
        }
      }
      .reverse

  private[step] def structuralMatch(template: String, parts: List[StepPart]): Option[List[(String, Tag[?])]] = {
    @scala.annotation.tailrec
    def go(
      pTokens: List[Either[String, TypedExtractor[?]]],
      tTokens: List[Either[String, String]],
      acc: List[(String, Tag[?])]
    ): Option[List[(String, Tag[?])]] =
      (pTokens, tTokens) match {
        case (Nil, Nil) => Some(acc.reverse)
        case (Left(pLit) :: pRest, Left(tLit) :: tRest) if pLit == tLit =>
          go(pRest, tRest, acc)
        case (Right(e) :: pRest, Right(col) :: tRest) =>
          go(pRest, tRest, (col, e.tag) :: acc)
        case _ => None
      }
    go(partTokens(parts), templateTokens(template), Nil)
  }
}
