package zio.bdd.core.step

import zio.*

trait GeneratedStepMethods[R, S] { self: ZIOSteps[R, S] =>
  def Given(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def Given[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }

  def Given[A, B](stepExpr: StepExpression[(A, B)])(f: (A, B) => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: ((A, B)) => RIO[R with State[S], Unit] = { case (a, b) => f(a, b) }
    self.register(StepDefImpl[R, S, (A, B)](stepExpr, adaptedF))
  }

  def When(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def When[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }

  def Then(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def Then[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }
}
