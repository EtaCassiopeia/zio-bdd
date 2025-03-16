package zio.bdd.core

import zio.*
import zio.bdd.gherkin.StepType

import scala.quoted.*

trait ZIOSteps[R] extends Hooks[R] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  final case class StepDef[I, O](stepType: StepType, pattern: String, fn: Step[I, O])

  def getSteps: List[StepDef[?, ?]]

  protected def register[I, O](stepType: StepType, pattern: String, fn: Step[I, O]): Unit

  def environment: ZLayer[Any, Any, R]

  // Note: Step functions receive inputs from previous step outputs and/or pattern parameters.
  // Given steps don’t receive prior step outputs; they start with statement parameters only.
  // And steps inherit the type of the last non-And step (Given, When, Then) for matching.
  // When both prior output and parameters are present, inputs are combined into a flat tuple with params last.

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

    override protected def register[I, O](stepType: StepType, pattern: String, fn: Step[I, O]): Unit =
      steps = StepDef(stepType, pattern, fn) :: steps

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

  // empty method for testing purposes
  def empty[R]: ZIOSteps[R] = new ZIOSteps[R] {
    override def getSteps: List[StepDef[?, ?]]                                                       = Nil
    override protected def register[I, O](stepType: StepType, pattern: String, fn: Step[I, O]): Unit = ()
    override def environment: ZLayer[Any, Any, R]                                                    = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
  }
}
