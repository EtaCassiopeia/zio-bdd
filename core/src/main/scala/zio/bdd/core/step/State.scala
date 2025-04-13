package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*

trait State[S] {
  def get: UIO[S]
  def update(f: S => S): UIO[Unit]
}

object State {
  def get[S: Tag]: ZIO[State[S], Nothing, S]                  = ZIO.serviceWithZIO[State[S]](_.get)
  def update[S: Tag](f: S => S): ZIO[State[S], Nothing, Unit] = ZIO.serviceWithZIO[State[S]](_.update(f))

  def layer[S: Tag](fiberRef: FiberRef[S]): ZLayer[Any, Nothing, State[S]] =
    ZLayer.succeed(new State[S] {
      def get: UIO[S]                  = fiberRef.get
      def update(f: S => S): UIO[Unit] = fiberRef.update(f)
    })
}
