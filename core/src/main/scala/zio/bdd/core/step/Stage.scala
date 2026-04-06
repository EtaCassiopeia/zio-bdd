package zio.bdd.core.step

import zio.*
import scala.reflect.ClassTag

/**
 * Per-scenario typed staging store for passing data between Given and When
 * steps.
 *
 * Unlike ScenarioContext[S], Stage does not require Schema[T] — it uses
 * ClassTag for type-safe storage. Values staged in a Given step are available
 * in When/Then steps within the same scenario and are discarded when the
 * scenario ends.
 *
 * {{{
 *   Given("an invalid provision body") {
 *     Stage.put(buildInvalidEvent(...))
 *   }
 *
 *   When("a provision request is sent") {
 *     Stage.getOrElse(buildDefaultEvent).flatMap(sendRequest)
 *   }
 * }}}
 */
object Stage {

  // The staging store is a FiberRef[Map[String, Any]] keyed by class name.
  // Initialised to empty for each scenario by ScenarioExecutor.
  private[bdd] val ref: FiberRef[Map[String, Any]] =
    Unsafe.unsafe { implicit u =>
      FiberRef.unsafe.make(Map.empty[String, Any])
    }

  // Holds the Gherkin pattern of the currently executing step.
  // Set by ScenarioExecutor before each step body runs.
  // Step bodies can read it to auto-derive log labels without repeating the step text.
  private[bdd] val currentStepLabel: FiberRef[String] =
    Unsafe.unsafe { implicit u =>
      FiberRef.unsafe.make("")
    }

  /**
   * The Gherkin pattern of the currently executing step (e.g. "When a provision
   * request is sent"). Returns "" when called outside a step body.
   */
  def stepLabel: UIO[String] = currentStepLabel.get

  /** Store a value. Overwrites any previously staged value of the same type. */
  def put[A: ClassTag](value: A): UIO[Unit] =
    ref.update(m => m + (key[A] -> value))

  /** Retrieve a staged value. Fails with StagingError if not present. */
  def get[A: ClassTag]: IO[StagingError, A] =
    ref.get.flatMap { m =>
      m.get(key[A]) match {
        case Some(v: A) => ZIO.succeed(v)
        case Some(v)    => ZIO.fail(StagingError.TypeMismatch(key[A], v.getClass.getSimpleName))
        case None       => ZIO.fail(StagingError.NotFound(key[A]))
      }
    }

  /** Retrieve a staged value, falling back to `default` if not present. */
  def getOrElse[A: ClassTag](default: => A): UIO[A] =
    get[A].catchAll(_ => ZIO.succeed(default))

  /** Retrieve an Option — None if not staged. */
  def getOption[A: ClassTag]: UIO[Option[A]] =
    ref.get.map { m =>
      m.get(key[A]).collect { case v: A => v }
    }

  /** Update a staged value if present, no-op otherwise. */
  def modify[A: ClassTag](f: A => A): UIO[Unit] =
    ref.update { m =>
      m.get(key[A]) match {
        case Some(v: A) => m + (key[A] -> f(v))
        case _          => m
      }
    }

  /** Remove a staged value. */
  def remove[A: ClassTag]: UIO[Unit] =
    ref.update(m => m - key[A])

  /** Reset all staged values. Called by ScenarioExecutor between scenarios. */
  private[bdd] def reset: UIO[Unit] = ref.set(Map.empty)

  private def key[A: ClassTag]: String = implicitly[ClassTag[A]].runtimeClass.getName
}

sealed trait StagingError extends Product with Serializable {
  def message: String
}
object StagingError {
  final case class NotFound(typeName: String) extends StagingError {
    def message: String = s"No staged value of type $typeName. Call Stage.put before Stage.get."
  }
  final case class TypeMismatch(typeName: String, actualType: String) extends StagingError {
    def message: String = s"Staged value for $typeName was actually $actualType."
  }
}
