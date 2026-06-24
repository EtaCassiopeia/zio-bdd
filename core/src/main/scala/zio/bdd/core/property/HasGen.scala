package zio.bdd.core.property

import izumi.reflect.Tag
import zio.test.Gen

import java.util.UUID
import scala.collection.concurrent.TrieMap

/**
 * Typeclass that provides a ZIO Test `Gen[Any, A]` for a given type `A`.
 *
 * ==Resolution is by column name, not by type (for now)==
 * A `@property` column is just a string name parsed from the Gherkin header —
 * there is no automatic inference from a step's `TypedExtractor[T]` to a
 * `HasGen[T]`. Every property column, including ones using a built-in type
 * below, must be routed explicitly via `ZIOSteps#columnGenLookup` (see
 * `zio.bdd.core.property.ColumnGenLookup`) or a named override (see below).
 * Built-in instances exist for `Int`/`Long`/`Double`/`Boolean`/ `String`/`UUID`
 * so you don't need to write `Gen.int` etc. yourself, but you still need to
 * route the column name to `HasGen[Int]` (or whichever applies) in
 * `columnGenLookup`.
 *
 * The type-keyed registry below (`registerType`/`resolveByTag`) is the piece
 * that makes automatic by-type resolution possible — see
 * `StepRegistry.resolveTemplateColumns`, which finds the `Tag[_]` a column's
 * extractor produces. Wiring that into column resolution so `columnGenLookup`
 * becomes optional rather than mandatory is tracked separately; until then,
 * this paragraph's "must be routed explicitly" still describes the current
 * runtime behavior.
 *
 * ==Named generator overrides==
 * Register a non-default generator for a column via
 * `HasGen.named[A](name)(gen)` and reference it in the Gherkin header: `|
 * amount: smallAmounts |`. The `:smallAmounts` suffix is resolved against the
 * named registry at runtime.
 *
 * Example:
 * {{{
 * object MySteps extends ZIOSteps[MyEnv, Unit]:
 *   given HasGen[Money] with
 *     def gen = Gen.double(0, 10_000).map(Money.apply)
 *   HasGen.named("smallAmounts")(Gen.double(0.01, 10.0).map(Money.apply))
 * }}}
 */
trait HasGen[A]:
  def gen: Gen[Any, A]
  // Human-readable label surfaced in failure output, e.g. "HasGen[Int]".
  def label: String = s"HasGen[${gen.getClass.getSimpleName}]"

object HasGen:
  // ── Built-in defaults ────────────────────────────────────────────────────

  given HasGen[Int] with
    def gen            = Gen.int
    override def label = "HasGen[Int]"

  given HasGen[Long] with
    def gen            = Gen.long
    override def label = "HasGen[Long]"

  given HasGen[Double] with
    def gen            = Gen.double
    override def label = "HasGen[Double]"

  given HasGen[Boolean] with
    def gen            = Gen.boolean
    override def label = "HasGen[Boolean]"

  given HasGen[String] with
    def gen            = Gen.alphaNumericString
    override def label = "HasGen[String]"

  given HasGen[UUID] with
    def gen            = Gen.uuid
    override def label = "HasGen[UUID]"

  // ── Named generator registry ─────────────────────────────────────────────

  private val namedRegistry: TrieMap[String, HasGen[?]] = TrieMap.empty

  /**
   * Register a named generator override. Call this in your `ZIOSteps` companion
   * object or object initialiser:
   *
   * {{{
   * HasGen.named("smallAmounts")(Gen.double(0.01, 10.0).map(Money.apply))
   * }}}
   */
  def named[A](name: String)(g: Gen[Any, A]): Unit =
    namedRegistry.put(name, new HasGen[A] { def gen = g; override def label = name })

  /** Summon a HasGen[A] from implicit scope. */
  def apply[A](using hg: HasGen[A]): HasGen[A] = hg

  /** Look up a named generator. Returns `None` if not registered. */
  def resolve(name: String): Option[HasGen[?]] = namedRegistry.get(name)

  // ── Type-keyed registry ──────────────────────────────────────────────────
  //
  // Mirrors zio.bdd.core.step.TypeMap: keyed by the same `Tag[A].tag` (the
  // underlying LightTypeTag) izumi-reflect uses for its own equality/hashing,
  // so two `Tag[A]` instances for the same `A` — summoned independently, e.g.
  // one here and one from StepRegistry.resolveTemplateColumns — look up the
  // same entry. TrieMap gives lock-free concurrent reads and writes, matching
  // `namedRegistry` above; registration normally happens once at startup
  // (object init for built-ins, a suite's object initializer for custom
  // types), but nothing here assumes that — concurrent registerType/resolveByTag
  // calls from parallel scenario fibers are safe.

  private val typeRegistry: TrieMap[Any, HasGen[?]] = TrieMap.empty

  /**
   * Register a `HasGen[A]` so it can be looked up later by its runtime `Tag[A]`
   * — once per ''type'', not per column name. Scala cannot enumerate arbitrary
   * `given HasGen[_]` instances at runtime, so types beyond the six built-ins
   * below need this one-time call (e.g. in a suite's companion object or object
   * initializer):
   *
   * {{{
   * given HasGen[Money] with
   *   def gen = Gen.double(0, 10_000).map(Money.apply)
   * HasGen.registerType(HasGen[Money])
   * }}}
   */
  def registerType[A: Tag](instance: HasGen[A]): Unit =
    typeRegistry.put(summon[Tag[A]].tag, instance)

  /**
   * Look up a registered `HasGen` by runtime type tag. `tag` is taken as a
   * plain value (not a context bound) so this also accepts an existential
   * `Tag[_]` — e.g. one of the `Tag[_]`s `StepRegistry.resolveTemplateColumns`
   * returns, where the underlying type isn't statically known at the call site.
   * Returns `None` if `A` is neither a built-in nor was registered via
   * `registerType`.
   */
  def resolveByTag[A](tag: Tag[A]): Option[HasGen[A]] =
    typeRegistry.get(tag.tag).asInstanceOf[Option[HasGen[A]]]

  // Built-ins are usable by type with zero setup — pre-populate eagerly so
  // resolveByTag[Int] etc. works without any suite ever calling registerType.
  registerType(HasGen[Int])
  registerType(HasGen[Long])
  registerType(HasGen[Double])
  registerType(HasGen[Boolean])
  registerType(HasGen[String])
  registerType(HasGen[UUID])
