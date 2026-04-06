package zio.bdd.core

import zio.*
import scala.reflect.ClassTag

/**
 * Per-feature typed staging store for passing data across all scenarios within
 * a Feature block.
 *
 * FeatureContext uses the same ClassTag-keyed TypeMap approach as Stage, but
 * with Feature lifetime — values are reset between Features and persist across
 * all Scenarios within one Feature.
 *
 * Unlike Stage (per-scenario) and ScenarioContext (per-scenario with
 * Schema[T]), FeatureContext fills the gap for feature-scoped data: shared
 * account IDs, feature counters, session tokens.
 *
 * {{{
 *   Given("an account is created for the feature") {
 *     createAccount().flatMap(id => FeatureContext.put(AccountId(id)))
 *   }
 *
 *   When("a transaction is posted") {
 *     FeatureContext.get[AccountId].flatMap(id => postTransaction(id))
 *   }
 * }}}
 */
object FeatureContext {

  // The feature store is a FiberRef[Map[String, Any]] keyed by class name.
  // Reset by FeatureExecutor at the start of each feature.
  private[bdd] val ref: FiberRef[Map[String, Any]] =
    Unsafe.unsafe { implicit u =>
      FiberRef.unsafe.make(Map.empty[String, Any])
    }

  /** Store a value. Overwrites any previously stored value of the same type. */
  def put[A: ClassTag](value: A): UIO[Unit] =
    ref.update(m => m + (key[A] -> value))

  /** Retrieve a stored value. Fails with FeatureContextError if not present. */
  def get[A: ClassTag]: IO[FeatureContextError, A] =
    ref.get.flatMap { m =>
      m.get(key[A]) match {
        case Some(v: A) => ZIO.succeed(v)
        case Some(v)    => ZIO.fail(FeatureContextError.TypeMismatch(key[A], v.getClass.getSimpleName))
        case None       => ZIO.fail(FeatureContextError.NotFound(key[A]))
      }
    }

  /** Retrieve a stored value, falling back to `default` if not present. */
  def getOrElse[A: ClassTag](default: => A): UIO[A] =
    get[A].catchAll(_ => ZIO.succeed(default))

  /** Retrieve an Option — None if not stored. */
  def getOption[A: ClassTag]: UIO[Option[A]] =
    ref.get.map { m =>
      m.get(key[A]).collect { case v: A => v }
    }

  /** Update a stored value if present, no-op otherwise. */
  def modify[A: ClassTag](f: A => A): UIO[Unit] =
    ref.update { m =>
      m.get(key[A]) match {
        case Some(v: A) => m + (key[A] -> f(v))
        case _          => m
      }
    }

  /** Set a stored value unconditionally. */
  def set[A: ClassTag](value: A): UIO[Unit] = put(value)

  /** Remove a stored value. */
  def remove[A: ClassTag]: UIO[Unit] =
    ref.update(m => m - key[A])

  /** Reset all stored values. Called by FeatureExecutor between features. */
  private[bdd] def reset: UIO[Unit] = ref.set(Map.empty)

  private def key[A: ClassTag]: String = implicitly[ClassTag[A]].runtimeClass.getName
}

sealed trait FeatureContextError extends Product with Serializable {
  def message: String
}
object FeatureContextError {
  final case class NotFound(typeName: String) extends FeatureContextError {
    def message: String =
      s"No feature-scoped value of type $typeName. Call FeatureContext.put before FeatureContext.get."
  }
  final case class TypeMismatch(typeName: String, actualType: String) extends FeatureContextError {
    def message: String = s"Feature-scoped value for $typeName was actually $actualType."
  }
}
