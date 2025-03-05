package zio.bdd.core

import zio.*
import zio.bdd.gherkin.StepType

import scala.quoted.*
import scala.util.matching.Regex

trait ZIOSteps[R] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  final case class StepDef[I, O](stepType: StepType, pattern: Regex, fn: Step[I, O])

  def getSteps: List[StepDef[?, ?]]

  protected def register[I, O](stepType: StepType, pattern: String, fn: Step[I, O]): Unit

  def environment: ZLayer[Any, Any, R]

  inline def Given[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.givenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def When[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.whenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def Then[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.thenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def And[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.andImpl[R, I, O]('pattern, 'fn, 'this) }
}

object ZIOSteps {
  trait Default[R] extends ZIOSteps[R] {
    private var steps: List[StepDef[?, ?]] = Nil

    override def getSteps: List[StepDef[?, ?]] = steps.reverse

    override protected def register[I, O](stepType: StepType, pattern: String, fn: Step[I, O]): Unit = {
      val regexPattern = pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(\\d+)")
        .replace("{float}", "(\\d+\\.\\d+)")
        .replaceAll("\\{[^:]+:[^}]+\\}", "(.+)")
        .r
      steps = StepDef(stepType, regexPattern, fn) :: steps
    }

    override def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }

  def givenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{ $self.register(StepType.GivenStep, $pattern, $fn) }

  def whenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{ $self.register(StepType.WhenStep, $pattern, $fn) }

  def thenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{ $self.register(StepType.ThenStep, $pattern, $fn) }

  def andImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{ $self.register(StepType.AndStep, $pattern, $fn) }
}
