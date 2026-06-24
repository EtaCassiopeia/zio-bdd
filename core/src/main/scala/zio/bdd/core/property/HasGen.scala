package zio.bdd.core.property

import izumi.reflect.Tag
import zio.test.Gen

import java.util.UUID
import scala.collection.concurrent.TrieMap

/**
 * Typeclass that provides a ZIO Test `Gen[Any, A]` for a given type `A`.
 *
 * ==Resolution order==
 * A `@property` column resolves in this order: (1) a named header override (`|
 * col: genName |`, see below), (2) the suite's `ZIOSteps#columnGenLookup` (see
 * `zio.bdd.core.property.ColumnGenLookup`), (3) automatic type-based lookup —
 * `PropertyExecutor` finds the `Tag[_]` the column's step extractor produces
 * (via `StepRegistry.resolveTemplateColumns`) and resolves it through this
 * object's type-keyed registry (`registerType`/`resolveByTag` below). The six
 * built-in types (`Int`/`Long`/`Double`/`Boolean`/`String`/ `UUID`) are
 * pre-registered, so columns of those types need no `columnGenLookup` entry at
 * all. Domain types need one `registerType` call per *type* to get the same
 * automatic resolution — `columnGenLookup` is then the override path, for when
 * the automatic choice isn't the one you want.
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
   * Look up a registered `HasGen` by runtime type tag.
   *
   * Takes an existential `Tag[_]` — e.g. one of the `Tag[_]`s
   * `StepRegistry.resolveTemplateColumns` returns, where the underlying type
   * isn't statically known at the call site — rather than a statically-typed
   * `Tag[A]`. (`izumi.reflect.Tag[T <: AnyKind]`'s kind bound means a `Tag[?]`
   * value doesn't actually unify with a `Tag[A]` parameter at a generic call
   * site, since `A` there defaults to the narrower proper-type kind `*` — so
   * the existential form is the one callers can actually use.) The returned
   * `HasGen[?]` is erased the same way; callers that need it for sampling only
   * use `.gen`/`.label`, neither of which needs the concrete type back.
   *
   * Returns `None` if the type is neither a built-in nor was registered via
   * `registerType`.
   */
  def resolveByTag(tag: Tag[?]): Option[HasGen[?]] =
    typeRegistry.get(tag.tag)

  // Built-ins are usable by type with zero setup — pre-populate eagerly so
  // resolveByTag(Tag[Int]) etc. works without any suite ever calling registerType.
  registerType(HasGen[Int])
  registerType(HasGen[Long])
  registerType(HasGen[Double])
  registerType(HasGen[Boolean])
  registerType(HasGen[String])
  registerType(HasGen[UUID])
