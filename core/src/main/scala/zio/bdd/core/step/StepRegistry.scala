package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*

trait StepRegistry[R, S] {
  def findStep(input: StepInput): Task[RIO[R with State[S], Unit]]
}

case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S] {
  def findStep(input: StepInput): Task[RIO[R with State[S], Unit]] =
    ZIO
      .fromOption(steps.iterator.map(_.tryExecute(input)).find(_.isDefined).flatten)
      .orElseFail(new Exception("No matching step found"))
}

object StepRegistry {
  def layer[R: Tag, S: Tag](steps: List[StepDef[R, S]]): ZLayer[Any, Nothing, StepRegistry[R, S]] =
    ZLayer.succeed(StepRegistryLive(steps))

  def findStep[R: Tag, S: Tag](input: StepInput): ZIO[StepRegistry[R, S], Throwable, RIO[R with State[S], Unit]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input))
}
