package zio.bdd.core.step

import zio.*
import zio.bdd.core.Hooks

import scala.collection.mutable
import scala.language.implicitConversions

trait ZIOSteps[R, S] extends Hooks[R] with GeneratedStepMethods[R, S] with DefaultTypedExtractor {

  type Step[I, O] = I => ZIO[R, Throwable, O]

  private val steps: mutable.ListBuffer[StepDef[R, S]] = mutable.ListBuffer.empty

  def getSteps: List[StepDef[R, S]] = steps.toList

  def register(step: StepDef[R, S]): Unit = steps += step

  def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]

  extension [Out <: Tuple](se: StepExpression[Out])
    def /(literal: String): StepExpression[Out] =
      StepExpression(se.parts :+ Literal(literal))
    def /[T](extractor: TypedExtractor[T]): StepExpression[Tuple.Concat[Out, Tuple1[T]]] =
      StepExpression(se.parts :+ Extractor(extractor))

  implicit def stringToStepExpression(str: String): StepExpression[EmptyTuple] =
    StepExpression(List(Literal(str)))
}
