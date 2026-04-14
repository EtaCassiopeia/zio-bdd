package zio.bdd.core.step

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
