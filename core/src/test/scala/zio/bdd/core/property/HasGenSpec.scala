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
  // registrations for distinct types don't race or clobber each other, and that repeated
  // concurrent writes to the same type don't corrupt the final entry.
  private val concurrencySuite = suite("concurrent registration and resolution")(
    test("8 distinct types registered concurrently each resolve to their own HasGen, independently") {
      // Genuinely distinct types (not just distinct labels on one shared type) — this is
      // what actually exercises Tag/LightTypeTag equality and hashing across different keys
      // under concurrent TrieMap writes, which a single-shared-type test cannot catch.
      final case class ConcA(v: Int)
      final case class ConcB(v: Int)
      final case class ConcC(v: Int)
      final case class ConcD(v: Int)
      final case class ConcE(v: Int)
      final case class ConcF(v: Int)
      final case class ConcG(v: Int)
      final case class ConcH(v: Int)
      def hasGen[A](l: String)(value: A): HasGen[A] = new HasGen[A] {
        def gen = Gen.const(value); override def label = l
      }
      for {
        _ <- ZIO.foreachPar(
               List(
                 ZIO.succeed(HasGen.registerType(hasGen("A")(ConcA(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("B")(ConcB(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("C")(ConcC(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("D")(ConcD(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("E")(ConcE(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("F")(ConcF(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("G")(ConcG(0)))),
                 ZIO.succeed(HasGen.registerType(hasGen("H")(ConcH(0))))
               )
             )(identity)
        labels <- ZIO.succeed(
                    List(
                      HasGen.resolveByTag(Tag[ConcA]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcB]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcC]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcD]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcE]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcF]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcG]).map(_.label),
                      HasGen.resolveByTag(Tag[ConcH]).map(_.label)
                    )
                  )
      } yield assertTrue(labels == List("A", "B", "C", "D", "E", "F", "G", "H").map(Some(_)))
    },
    test("concurrent re-registration of the same type races safely: the final entry is never corrupted") {
      final case class Reregistered32(value: Int)
      for {
        _ <- ZIO.foreachPar((1 to 32).toList) { i =>
               ZIO.succeed(
                 HasGen.registerType(new HasGen[Reregistered32] {
                   def gen            = Gen.const(Reregistered32(i))
                   override def label = s"label-$i"
                 })
               )
             }
        resolved <- ZIO.succeed(HasGen.resolveByTag(Tag[Reregistered32]))
      } yield assertTrue(resolved.exists(hg => (1 to 32).map(i => s"label-$i").contains(hg.label)))
    },
    test("concurrent resolveByTag calls for built-ins are all consistent") {
      for {
        results <- ZIO.foreachPar((1 to 32).toList)(_ => ZIO.succeed(HasGen.resolveByTag(Tag[Int]).map(_.label)))
      } yield assertTrue(results.forall(_.contains("HasGen[Int]")))
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("HasGen type-keyed registry (#98)")(
    builtins,
    customTypes,
    concurrencySuite
  )
}
