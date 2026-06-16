package zio.bdd.core.step

import zio.*

/**
 * A simple lens for focusing on a sub-part of a larger state type.
 *
 * `HasLens[S, A]` expresses that state `S` has a "slice" of type `A` accessible
 * via `get` and replaceable via `set`. This eliminates deep nested `.copy`
 * chains:
 *
 * Before (monolithic state):
 * {{{
 *   ScenarioContext.update(s => s.copy(
 *     core = s.core.copy(
 *       provision = s.core.provision.copy(response = Some(resp))
 *     )
 *   ))
 * }}}
 *
 * After (lens-focused):
 * {{{
 *   // Define the lens once
 *   given HasLens[AppState, ProvisionState] =
 *     HasLens(_.provision, (s, a) => s.copy(provision = a))
 *
 *   // Use in steps
 *   ScenarioLens.update[AppState, ProvisionState](_.copy(response = Some(resp)))
 * }}}
 *
 * For Monocle users, any `monocle.Lens[S, A]` can be adapted via
 * `fromMonocleLike`:
 * {{{
 *   import monocle.macros.GenLens
 *   val lens = GenLens[AppState](_.provision)
 *   given HasLens[AppState, ProvisionState] =
 *     HasLens.fromMonocleLike(getter = lens.get, setter = a => s => lens.replace(a)(s))
 * }}}
 */
final class HasLens[S, A](val get: S => A, val set: (S, A) => S):

  /** Apply a transformation to the slice. */
  def modify(f: A => A)(s: S): S = set(s, f(get(s)))

  /**
   * Compose two lenses: `this` focuses on `A`, then `inner` focuses on `B`
   * within `A`.
   */
  def andThen[B](inner: HasLens[A, B]): HasLens[S, B] =
    HasLens(s => inner.get(get(s)), (s, b) => set(s, inner.set(get(s), b)))

object HasLens:
  def apply[S, A](get: S => A, set: (S, A) => S): HasLens[S, A] =
    new HasLens(get, set)

  /** Adapt from a Monocle `Lens` if the monocle-core dependency is present. */
  def fromMonocleLike[S, A](getter: S => A, setter: A => S => S): HasLens[S, A] =
    HasLens(getter, (s, a) => setter(a)(s))

/**
 * ZIO effects for lens-focused state operations. Requires both `State[S]` in
 * environment and a `HasLens[S, A]` in implicit scope.
 */
object ScenarioLens:

  /**
   * Get the sub-state `A` from the full scenario state `S`.
   *
   * {{{
   *   for {
   *     provCtx <- ScenarioLens.get[AppState, ProvisionState]
   *     ...
   *   } yield ()
   * }}}
   */
  def get[S, A](using lens: HasLens[S, A], tag: izumi.reflect.Tag[S]): ZIO[State[S], Nothing, A] =
    State.get[S].map(lens.get)

  /**
   * Update the sub-state `A` within the full scenario state `S`.
   *
   * {{{
   *   ScenarioLens.update[AppState, ProvisionState](_.copy(arid = newArid))
   * }}}
   */
  def update[S, A](f: A => A)(using lens: HasLens[S, A], tag: izumi.reflect.Tag[S]): ZIO[State[S], Nothing, Unit] =
    State.update[S](lens.modify(f))

  /**
   * Set the sub-state `A` within the full scenario state `S`.
   */
  def set[S, A](value: A)(using lens: HasLens[S, A], tag: izumi.reflect.Tag[S]): ZIO[State[S], Nothing, Unit] =
    State.update[S](s => lens.set(s, value))
