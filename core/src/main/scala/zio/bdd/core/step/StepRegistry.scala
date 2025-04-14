package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType

trait StepRegistry[R, S] {
  def findStep(stepType: StepType, input: StepInput): Task[RIO[R with State[S], Unit]]
}

case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S] {
  def findStep(stepType: StepType, input: StepInput): Task[RIO[R with State[S], Unit]] = {
    // First, try to find steps matching the exact step type
    val typedCandidates = steps.filter { case StepDefImpl(registeredType, _, _) =>
      registeredType == stepType
    }
    val matchingTyped = typedCandidates.iterator.map(_.tryExecute(input)).find(_.isDefined).flatten

    matchingTyped match {
      case Some(effect) => ZIO.succeed(effect)
      case None         =>
        // If no match, try "And" steps as a fallback (for any effective type)
        val andCandidates = steps.filter { case StepDefImpl(registeredType, _, _) =>
          registeredType == StepType.AndStep
        }
        val matchingAnd = andCandidates.iterator.map(_.tryExecute(input)).find(_.isDefined).flatten
        matchingAnd match {
          case Some(effect) => ZIO.succeed(effect)
          case None         => ZIO.fail(new Exception(s"No matching step found for type $stepType and input $input"))
        }
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
