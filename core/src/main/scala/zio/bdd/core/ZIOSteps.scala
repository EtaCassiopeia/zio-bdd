package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType
import scala.quoted.*

trait ZIOSteps[R] extends Hooks[R] {
  type Step[I <: Matchable, O] = I => ZIO[R, Throwable, O]

  final case class StepDef[I <: Matchable, O](
    stepType: StepType,
    pattern: String,
    fn: Step[I, O]
  )(implicit val iTag: Tag[I], val oTag: Tag[O])

  def getSteps: List[StepDef[? <: Matchable, ?]]

  protected def register[I <: Matchable: Tag, O: Tag](stepType: StepType, pattern: String, fn: Step[I, O]): Unit

  def environment: ZLayer[Any, Any, R]

  inline def Given[I <: Matchable: Tag, O: Tag](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.givenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def When[I <: Matchable: Tag, O: Tag](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.whenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def Then[I <: Matchable: Tag, O: Tag](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.thenImpl[R, I, O]('pattern, 'fn, 'this) }

  inline def And[I <: Matchable: Tag, O: Tag](pattern: String)(fn: Step[I, O]): Unit =
    ${ ZIOSteps.andImpl[R, I, O]('pattern, 'fn, 'this) }
}

object ZIOSteps {
  trait Default[R] extends ZIOSteps[R] {
    private var steps: List[StepDef[? <: Matchable, ?]] = Nil

    override def getSteps: List[StepDef[? <: Matchable, ?]] = steps.reverse

    override protected def register[I <: Matchable: Tag, O: Tag](
      stepType: StepType,
      pattern: String,
      fn: Step[I, O]
    ): Unit =
      steps = StepDef(stepType, pattern, fn) :: steps

    override def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }

  def givenImpl[R: Type, I <: Matchable: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register(StepType.GivenStep, $pattern, $fn)
  }

  def whenImpl[R: Type, I <: Matchable: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register(StepType.WhenStep, $pattern, $fn)
  }

  def thenImpl[R: Type, I <: Matchable: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register(StepType.ThenStep, $pattern, $fn)
  }

  def andImpl[R: Type, I <: Matchable: Type, O: Type](
    pattern: Expr[String],
    fn: Expr[I => ZIO[R, Throwable, O]],
    self: Expr[ZIOSteps[R]]
  )(using Quotes): Expr[Unit] = '{
    $self.register(StepType.AndStep, $pattern, $fn)
  }

  def empty[R]: ZIOSteps[R] = new ZIOSteps[R] {
    override def getSteps: List[StepDef[? <: Matchable, ?]] = Nil
    override protected def register[I <: Matchable: Tag, O: Tag](
      stepType: StepType,
      pattern: String,
      fn: Step[I, O]
    ): Unit = ()
    override def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }
}
