package zio.bdd.core.step

import zio.*
import zio.bdd.gherkin.StepType

trait StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]]
}

case class StepDefImpl[R, S, Out <: Tuple](
  stepType: StepType,
  stepExpr: StepExpression[Out],
  f: Out => RIO[R with State[S], Unit]
) extends StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]] =
    stepExpr.extract(input).map(f)
}
