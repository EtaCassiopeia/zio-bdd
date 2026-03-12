package zio.bdd.core.step

import zio.*
import zio.bdd.core.{Default, FeatureExecutor, FeatureResult, Hooks, PendingException}
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
  private var sealed_ : Boolean                        = false

  /**
   * Return all registered steps. On first call the registry is sealed and
   * validated: two definitions with identical `(keyword, pattern)` throw at
   * suite startup, not mid-test.
   */
  private[core] def getSteps: List[StepDef[R, S]] = {
    if (!sealed_) {
      sealed_ = true
      validateAmbiguity()
    }
    steps.toList
  }

  /** Protected alias for `getSteps` so subtraits can override `run`. */
  protected final def registeredSteps: List[StepDef[R, S]] = getSteps

  private[core] def register(step: StepDef[R, S]): Unit = {
    if (sealed_)
      throw new IllegalStateException(
        "Cannot register step after suite has started. " +
          "Step registration must complete during object initialisation."
      )
    steps += step
  }

  private def validateAmbiguity(): Unit = {
    val byTypeAndPattern = steps.toList.groupBy { case s: StepDefImpl[?, ?, ?] =>
      (s.stepType, s.stepExpr.toString)
    }
    val ambiguous = byTypeAndPattern.filter(_._2.length > 1)
    if (ambiguous.nonEmpty) {
      val details = ambiguous.map { case ((t, pat), defs) =>
        s"  $t \"$pat\" — registered ${defs.length} times"
      }.mkString("\n")
      throw new IllegalStateException(
        "Ambiguous step definitions detected at suite startup.\n" +
          "The following step expressions are registered multiple times:\n" + details + "\n" +
          "Rename or merge duplicate step definitions."
      )
    }
  }

  def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]

  /**
   * Override to set a default timeout for every step in this suite. `None`
   * means unlimited (default).
   *
   * {{{
   * override def stepTimeout: Option[Duration] = Some(Duration.fromSeconds(30))
   * }}}
   */
  def stepTimeout: Option[Duration] = _annotationStepTimeout

  // Set by ZIOBDDFramework when @Suite(stepTimeout=N) is present; a subclass override of
  // `stepTimeout` takes precedence because it ignores this field.
  private[core] var _annotationStepTimeout: Option[Duration] = None

  /** Called by the framework to inject the annotation-configured timeout. */
  private[bdd] final def overrideStepTimeout(d: Duration): Unit =
    _annotationStepTimeout = Some(d)

  /**
   * Mark a step as not-yet-implemented. Reported as `PENDING`, does not fail
   * the build, and does not skip the remaining steps.
   *
   * {{{
   * Given("a not-yet-built step") { pending("implement in sprint 3") }
   * }}}
   */
  def pending(reason: String = "TODO"): RIO[R with State[S], Unit] =
    ZIO.fail(new PendingException(reason))

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
