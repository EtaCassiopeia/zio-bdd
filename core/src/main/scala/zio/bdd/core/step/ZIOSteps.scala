package zio.bdd.core.step

import zio.*
import zio.bdd.core.{Default, FeatureExecutor, FeatureResult, Hooks}
import zio.bdd.gherkin.Feature
import izumi.reflect.Tag

import scala.collection.mutable
import scala.language.implicitConversions

trait ZIOSteps[R: Tag, S: Tag: Default]
    extends Hooks[R, S]
    with GeneratedStepMethods[R, S]
    with DefaultTypedExtractor
    with StateOps[S] {

  type Step[I, O] = I => ZIO[R, Throwable, O]

  private val steps: mutable.ListBuffer[StepDef[R, S]] = mutable.ListBuffer.empty

  private[core] def getSteps: List[StepDef[R, S]] = steps.toList

  private[core] def register(step: StepDef[R, S]): Unit = steps += step

  def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]

  extension [Out <: Tuple](se: StepExpression[Out])
    def /(literal: String): StepExpression[Out] =
      StepExpression(se.parts :+ Literal(literal))
    def /[T](extractor: TypedExtractor[T]): StepExpression[Tuple.Concat[Out, Tuple1[T]]] =
      StepExpression(se.parts :+ Extractor(extractor))

  implicit def stringToStepExpression(str: String): StepExpression[EmptyTuple] =
    StepExpression(List(Literal(str)))

  def run(features: List[Feature]): ZIO[R, Nothing, List[FeatureResult]] =
    FeatureExecutor.executeFeatures[R, S](features, getSteps, this)
}
