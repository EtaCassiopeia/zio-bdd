package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.Default

/**
 * A type-indexed heterogeneous state map.
 *
 * TypeMap replaces a monolithic case class `S` with a map keyed by `Tag[A]`.
 * Each step module can declare, read, and update only its own state slice
 * without coupling to other modules' state:
 *
 * {{{
 * // Each module declares its own opaque state key
 * object ProvisionCtx:
 *   final case class Data(arid: String = "", transactionId: String = "")
 *   given Tag[Data] = Tag[Data]
 *   def empty: Data = Data()
 *
 * // Steps read/update only their slice
 * trait ProvisionSteps { self: ZIOSteps[R, TypeMap] =>
 *   Given("a valid provision body") {
 *     for {
 *       ctx <- TypeMap.get[ProvisionCtx.Data]
 *       _   <- TypeMap.update[ProvisionCtx.Data](_.copy(arid = newArid))
 *     } yield ()
 *   }
 * }
 * }}}
 *
 * Compared to the monolithic S approach:
 *   - Modules cannot read/corrupt each other's state
 *   - No giant case class — state grows incrementally as modules are added
 *   - No Schema[S] or Default[S] required — each slice has its own Default
 *   - Updates are local: `TypeMap.update[Slice](f)` not `s.copy(a = s.a.copy(b
 *     \= ...))`
 */
final class TypeMap private (private val underlying: Map[Any, Any]):

  /**
   * Retrieve the current value for type `A`. Returns `None` if no value has
   * been set.
   */
  def get[A: Tag]: Option[A] =
    underlying.get(summon[Tag[A]].tag).map(_.asInstanceOf[A])

  /** Retrieve the current value for type `A`, or `default` if not set. */
  def getOrDefault[A: Tag](default: => A): A =
    get[A].getOrElse(default)

  /** Store a value for type `A`, replacing any previous value. */
  def put[A: Tag](value: A): TypeMap =
    new TypeMap(underlying + (summon[Tag[A]].tag -> value))

  /**
   * Update the value for type `A` using the given function. If no value exists,
   * `Default[A].default` is used.
   */
  def modify[A: Tag: Default](f: A => A): TypeMap =
    put[A](f(getOrDefault(Default[A].default)))

  override def toString: String =
    s"TypeMap(${underlying.map { case (k, v) => s"$k → $v" }.mkString(", ")})"

object TypeMap:
  val empty: TypeMap = new TypeMap(Map.empty)

  /** Convenience: create a TypeMap with a single initial value. */
  def of[A: Tag](value: A): TypeMap = empty.put[A](value)

/**
 * ZIO service operations for TypeMap state.
 *
 * When `S = TypeMap`, step bodies can use these instead of
 * `ScenarioContext.get`:
 *
 * {{{
 * for {
 *   ctx <- TypeMap.get[MyCtx.Data]
 *   _   <- TypeMap.update[MyCtx.Data](_.copy(field = value))
 * } yield ()
 * }}}
 */
object TypeMapOps:

  /**
   * Get the current slice for type `A` from the TypeMap state. Returns `None`
   * if not set.
   */
  def get[A: Tag]: ZIO[State[TypeMap], Nothing, Option[A]] =
    State.get[TypeMap].map(_.get[A])

  /**
   * Get the current slice for type `A`, using `Default[A].default` if not set.
   */
  def getOrDefault[A: Tag: Default]: ZIO[State[TypeMap], Nothing, A] =
    State.get[TypeMap].map(_.getOrDefault(Default[A].default))

  /**
   * Update the slice for type `A`. If the slice has no current value,
   * `Default[A].default` is used as the starting point.
   */
  def update[A: Tag: Default](f: A => A): ZIO[State[TypeMap], Nothing, Unit] =
    State.update[TypeMap](_.modify[A](f))

  /** Set the slice for type `A` to a specific value. */
  def set[A: Tag](value: A): ZIO[State[TypeMap], Nothing, Unit] =
    State.update[TypeMap](_.put[A](value))
