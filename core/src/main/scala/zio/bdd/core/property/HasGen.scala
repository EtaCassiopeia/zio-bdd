package zio.bdd.core.property

import zio.test.Gen

import java.util.UUID
import scala.collection.concurrent.TrieMap

/**
 * Typeclass that provides a ZIO Test `Gen[Any, A]` for a given type `A`.
 *
 * ==Default resolution==
 * Step parameters of type `T` that also have a `HasGen[T]` implicit in scope
 * are automatically sampled during `@property` scenario execution. Built-in
 * instances are provided for primitive types matching the existing
 * `TypedExtractor` built-ins.
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
 *   given HasGen[Money] = Gen.double(0, 10_000).map(Money.apply)
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
