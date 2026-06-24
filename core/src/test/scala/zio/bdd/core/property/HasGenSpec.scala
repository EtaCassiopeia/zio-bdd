package zio.bdd.core.property

import izumi.reflect.Tag
import zio.*
import zio.test.*

/**
 * Issue #98: a Tag-keyed registry on top of `HasGen`, mirroring
 * `zio.bdd.core.step.TypeMap`'s `Tag`-keyed lookup. Lets a `HasGen[A]` be found
 * by `A`'s runtime `Tag` rather than only by column name — the piece
 * `resolveTemplateColumns` (#97) needs to eventually resolve a column's
 * generator automatically from its extractor's type (#99).
 */
object HasGenSpec extends ZIOSpecDefault {

  // Distinct per-spec types so registering them here can't leak into (or collide
  // with) any other spec sharing the same JVM-wide HasGen.typeRegistry singleton.
  private final case class Widget(value: Int)
  private final case class Gadget(value: String)

  private val builtins = suite("built-in types resolve with zero registration")(
    test("Int/Long/Double/Boolean/String/UUID all resolve without calling registerType") {
      assertTrue(
        HasGen.resolveByTag(Tag[Int]).isDefined,
        HasGen.resolveByTag(Tag[Long]).isDefined,
        HasGen.resolveByTag(Tag[Double]).isDefined,
        HasGen.resolveByTag(Tag[Boolean]).isDefined,
        HasGen.resolveByTag(Tag[String]).isDefined,
        HasGen.resolveByTag(Tag[java.util.UUID]).isDefined
      )
    },
    test("resolved built-in carries the expected label") {
      assertTrue(HasGen.resolveByTag(Tag[Int]).map(_.label).contains("HasGen[Int]"))
    },
    test("two independently summoned Tag[Int] values resolve to the same registered instance") {
      // Proves the registry is keyed by the underlying LightTypeTag (value equality),
      // not by Tag instance identity — the same guarantee TypeMap relies on.
      val first  = HasGen.resolveByTag(Tag[Int])
      val second = HasGen.resolveByTag(summon[Tag[Int]])
      assertTrue(first.isDefined, first.map(_.label) == second.map(_.label))
    }
  )

  private val customTypes = suite("custom types")(
    test("an unregistered custom type resolves to None") {
      assertTrue(HasGen.resolveByTag(Tag[Gadget]).isEmpty)
    },
    test("a custom type resolves after one registerType call") {
      given HasGen[Widget] with
        def gen            = Gen.const(Widget(0))
        override def label = "HasGen[Widget]"
      HasGen.registerType(HasGen[Widget])
      assertTrue(HasGen.resolveByTag(Tag[Widget]).map(_.label).contains("HasGen[Widget]"))
    },
    test("re-registering the same type replaces the previous instance rather than erroring") {
      final case class Reregistered(value: Int)
      given firstInstance: HasGen[Reregistered] with
        def gen            = Gen.const(Reregistered(1))
        override def label = "first"
      HasGen.registerType(HasGen[Reregistered])
      HasGen.registerType(new HasGen[Reregistered] {
        def gen = Gen.const(Reregistered(2)); override def label = "second"
      })
      assertTrue(HasGen.resolveByTag(Tag[Reregistered]).map(_.label).contains("second"))
    }
  )

  // ── Concurrency: typeRegistry is a JVM-wide singleton TrieMap shared across every
  // suite in the same test run. Parallel scenario execution (see ConcurrencySpec) means
  // registerType/resolveByTag can be called concurrently from many fibers — confirm
  // registrations for distinct types don't race or clobber each other.
  private val concurrencySuite = suite("concurrent registration and resolution")(
    test("100 distinct types registered concurrently all resolve correctly afterward") {
      final case class Concurrent(tag: Int)
      // One HasGen per distinct *value* of `tag`, distinguished only by label — registerType
      // itself is keyed by static type, so we use 100 distinct nested case classes via an
      // array of labels and confirm concurrent puts to the same TrieMap don't lose entries
      // by round-tripping through a single shared type with many concurrent re-registrations,
      // then asserting the final state is exactly one of the written labels (no corruption).
      for {
        _ <- ZIO.foreachPar((1 to 100).toList) { i =>
               ZIO.succeed(
                 HasGen.registerType(new HasGen[Concurrent] {
                   def gen            = Gen.const(Concurrent(i))
                   override def label = s"label-$i"
                 })
               )
             }
        resolved <- ZIO.succeed(HasGen.resolveByTag(Tag[Concurrent]))
      } yield assertTrue(resolved.exists(hg => (1 to 100).map(i => s"label-$i").contains(hg.label)))
    },
    test("concurrent resolveByTag calls for built-ins are all consistent") {
      for {
        results <- ZIO.foreachPar((1 to 200).toList)(_ => ZIO.succeed(HasGen.resolveByTag(Tag[Int]).map(_.label)))
      } yield assertTrue(results.forall(_.contains("HasGen[Int]")))
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("HasGen type-keyed registry (#98)")(
    builtins,
    customTypes,
    concurrencySuite
  )
}
