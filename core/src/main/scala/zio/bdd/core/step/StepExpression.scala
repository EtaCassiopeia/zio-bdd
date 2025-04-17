package zio.bdd.core.step

import java.util.regex.Pattern
import scala.language.implicitConversions

sealed trait StepPart
case class Literal(value: String)                     extends StepPart
case class Extractor[T](extractor: TypedExtractor[T]) extends StepPart

case class StepExpression[Out <: Tuple](parts: List[StepPart]) {
  private def regex: String = parts.map {
    case Literal(s)   => Pattern.quote(s)
    case Extractor(e) => e.pattern
  }.mkString

  def extract(input: StepInput): Option[Out] = {
    val pattern = regex.r.pattern
    val matcher = pattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      extractValues(input, groups, 0, parts.collect { case Extractor(e) => e })
    } else None
  }

  private def extractValues(
    input: StepInput,
    groups: List[String],
    index: Int,
    extractors: List[TypedExtractor[_]]
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
}
