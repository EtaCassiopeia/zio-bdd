package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType

trait StepRegistry[R, S] {
  def findStep(stepType: StepType, input: StepInput): Task[RIO[R with State[S], Unit]]
}

case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S] {
  // Cross-keyword resolution priority (Cucumber semantics): the keyword on a step
  // definition is documentation only — any step can match any keyword at runtime.
  private val keywordPriority: List[StepType] =
    List(StepType.GivenStep, StepType.WhenStep, StepType.ThenStep, StepType.AndStep, StepType.ButStep)

  private def firstMatchFor(t: StepType, input: StepInput): Option[RIO[R with State[S], Unit]] =
    steps.iterator.collect { case sd @ StepDefImpl(rt, _, _) if rt == t => sd }
      .flatMap(_.tryExecute(input))
      .nextOption()

  def findStep(stepType: StepType, input: StepInput): Task[RIO[R with State[S], Unit]] = {
    // 1. Exact step-type match.
    // 2. Fall back to `And` definitions (universal glue).
    // 3. Cross-keyword fallback: try every keyword in priority order, first match wins.
    val resolved =
      firstMatchFor(stepType, input)
        .orElse(firstMatchFor(StepType.AndStep, input))
        .orElse(keywordPriority.iterator.flatMap(firstMatchFor(_, input)).nextOption())

    resolved match {
      case Some(effect) => ZIO.succeed(effect)
      case None =>
        ZIO.fail(
          new Exception(
            s"No matching step found for type $stepType and input $input " +
              s"(tried keywords: ${keywordPriority.mkString(", ")})"
          )
        )
    }
  }
}

object StepRegistry {
  def layer[R: Tag, S: Tag](steps: List[StepDef[R, S]]): ZLayer[Any, Nothing, StepRegistry[R, S]] =
    ZLayer.succeed(StepRegistryLive(steps))

  def findStep[R: Tag, S: Tag](
    stepType: StepType,
    input: StepInput
  ): ZIO[StepRegistry[R, S], Throwable, RIO[R with State[S], Unit]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(stepType, input))
}
