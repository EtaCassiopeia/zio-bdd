package zio.bdd.core

import izumi.reflect.Tag
import zio.bdd.gherkin.StepType
import zio.{ZIO, ZLayer}

trait ZIOSteps[R] extends Hooks[R] with GeneratedStepMethods[R] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  def getSteps: List[StepDef[R, ?, ?]]

  protected def register[I: Tag, O: Tag](stepType: StepType, pattern: String, fn: Step[I, O]): Unit

  def environment: ZLayer[Any, Any, R]
}

case class StepDef[R, I, O](
  stepType: StepType,
  pattern: String,
  fn: I => ZIO[R, Throwable, O],
  iTag: Tag[I],
  oTag: Tag[O],
  paramTags: List[Tag[Any]]
)

object ZIOSteps {
  trait Default[R] extends ZIOSteps[R] {
    private var steps: List[StepDef[R, ?, ?]] = Nil

    override def getSteps: List[StepDef[R, ?, ?]] = steps.reverse

    override protected def register[I: Tag, O: Tag](
      stepType: StepType,
      pattern: String,
      fn: Step[I, O]
    ): Unit = {
      val iTag      = implicitly[Tag[I]]
      val oTag      = implicitly[Tag[O]]
      val paramTags = extractParamTags(pattern)
      steps = StepDef[R, I, O](stepType, pattern, fn, iTag, oTag, paramTags) :: steps
    }

    private def extractParamTags(pattern: String): List[Tag[Any]] =
      if (pattern.startsWith("^") && pattern.endsWith("$")) {
        // Regular expression pattern
        val placeholderPattern = """\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\)""".r
        val placeholders       = placeholderPattern.findAllIn(pattern).toList
        placeholders.map(_ => Tag[String].asInstanceOf[Tag[Any]])
      } else {
        // Gherkin-style pattern
        val tokens             = pattern.split("\\s+")
        val placeholderPattern = """\{[^:]+(?::[^}]+)?\}""".r
        val placeholders       = tokens.filter(token => placeholderPattern.pattern.matcher(token).matches()).toList

        val typeMap: Map[String, Tag[_]] = Map(
          "int"     -> Tag[Int],
          "string"  -> Tag[String],
          "float"   -> Tag[Float],
          "double"  -> Tag[Double],
          "boolean" -> Tag[Boolean]
        )

        placeholders.map { placeholder =>
          val placeholderType = placeholder.stripPrefix("{").stripSuffix("}").split(":").last.toLowerCase
          typeMap.getOrElse(placeholderType, Tag[String]).asInstanceOf[Tag[Any]]
        }
      }

    override def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }

  def empty[R]: ZIOSteps[R] = new ZIOSteps[R] {
    override def getSteps: List[StepDef[R, ?, ?]] = Nil
    override protected def register[I: Tag, O: Tag](
      stepType: StepType,
      pattern: String,
      fn: Step[I, O]
    ): Unit = ()
    override def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }
}
