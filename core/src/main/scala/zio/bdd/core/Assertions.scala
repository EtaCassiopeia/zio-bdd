package zio.bdd.core

import zio.*

import scala.reflect.ClassTag

object Assertions {
  def assertTrue(condition: Boolean, message: String = "Assertion failed"): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      assert(condition, message)
    }

  def assertEquals[A](actual: A, expected: A, message: String = "Values not equal"): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      assert(actual == expected, s"$message: expected $expected, got $actual")
    }

  def assertThrows[E <: Throwable: ClassTag](
    zio: ZIO[Any, E, Any],
    message: String = "Expected exception not thrown"
  ): ZIO[Any, Nothing, Unit] =
    zio.either.map {
      case Left(_)  => ()
      case Right(_) => throw new AssertionError(message)
    }
}
