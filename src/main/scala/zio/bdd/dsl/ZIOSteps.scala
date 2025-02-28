package zio.bdd.dsl

import zio.*
import scala.quoted.*
import scala.util.matching.Regex

trait ZIOSteps[R] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  final case class StepDef[I, O](pattern: Regex, fn: Step[I, O])

  def getSteps: List[StepDef[?, ?]]

  protected def register[I, O](pattern: String, fn: Step[I, O]): Unit

  inline def Given[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.givenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def When[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.whenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def Then[I, O](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.thenImpl[R, I, O]('pattern, 'fn, 'this) }
}

object ZIOSteps {
  trait Default[R] extends ZIOSteps[R] {
    private var steps: List[StepDef[?, ?]] = Nil

    override def getSteps: List[StepDef[?, ?]] = steps

    override protected def register[I, O](pattern: String, fn: Step[I, O]): Unit = {
      val regex = patternToRegex(pattern)
      steps = StepDef(regex, fn) :: steps
    }

    private def patternToRegex(pattern: String): Regex =
      pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(\\d+)")
        .replace("{float}", "(\\d+\\.\\d+)")
        .r
  }

  def givenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register($pattern, $fn)
  }

  def whenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register($pattern, $fn)
  }

  def thenImpl[R: Type, I: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register($pattern, $fn)
  }
}
