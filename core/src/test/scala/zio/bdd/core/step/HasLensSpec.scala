package zio.bdd.core.step

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for HasLens — the lightweight lens for focused state updates.
 *
 * HasLens[S, A] eliminates deeply nested `.copy` chains when updating a
 * sub-part of a larger scenario state type. ScenarioLens provides ZIO effects
 * that operate through a HasLens in scope.
 */
object HasLensSpec extends ZIOSpecDefault {

  case class Inner(value: String = "", count: Int = 0)
  case class Outer(inner: Inner = Inner(), tag: String = "")

  case class Level3(x: Int = 0)
  case class Level2(l3: Level3 = Level3())
  case class Level1(l2: Level2 = Level2())

  def spec: Spec[TestEnvironment & Scope, Any] = suite("HasLens")(
    suite("HasLens pure operations")(
      test("get focuses on the target slice") {
        val lens  = HasLens[Outer, Inner](_.inner, (o, i) => o.copy(inner = i))
        val state = Outer(Inner("hello", 3))
        assertTrue(lens.get(state) == Inner("hello", 3))
      },
      test("set replaces the slice and leaves other fields unchanged") {
        val lens    = HasLens[Outer, Inner](_.inner, (o, i) => o.copy(inner = i))
        val state   = Outer(Inner("old"), tag = "keep-me")
        val updated = lens.set(state, Inner("new"))
        assertTrue(updated.inner.value == "new", updated.tag == "keep-me")
      },
      test("modify applies a function to the slice without touching siblings") {
        val lens    = HasLens[Outer, Inner](_.inner, (o, i) => o.copy(inner = i))
        val state   = Outer(Inner("x", 5), tag = "original")
        val updated = lens.modify(_.copy(count = 99))(state)
        assertTrue(updated.inner.count == 99, updated.tag == "original")
      },
      test("andThen composes two lenses for arbitrarily deep nesting") {
        val l1toL2   = HasLens[Level1, Level2](_.l2, (s, a) => s.copy(l2 = a))
        val l2toL3   = HasLens[Level2, Level3](_.l3, (s, a) => s.copy(l3 = a))
        val composed = l1toL2.andThen(l2toL3)
        val state    = Level1(Level2(Level3(x = 42)))
        assertTrue(composed.get(state).x == 42)
      },
      test("andThen set updates the deep target") {
        val l1toL2   = HasLens[Level1, Level2](_.l2, (s, a) => s.copy(l2 = a))
        val l2toL3   = HasLens[Level2, Level3](_.l3, (s, a) => s.copy(l3 = a))
        val composed = l1toL2.andThen(l2toL3)
        val state    = Level1(Level2(Level3(x = 1)))
        assertTrue(composed.set(state, Level3(x = 99)).l2.l3.x == 99)
      }
    ),
    suite("ScenarioLens ZIO effects")(
      test("get retrieves the focused slice from State[Outer]") {
        given HasLens[Outer, Inner] = HasLens(_.inner, (o, i) => o.copy(inner = i))
        for {
          ref    <- FiberRef.make(Outer(Inner("hello")))
          result <- ScenarioLens.get[Outer, Inner].provide(State.layer(ref))
        } yield assertTrue(result == Inner("hello"))
      },
      test("update modifies the focused slice without touching sibling fields") {
        given HasLens[Outer, Inner] = HasLens(_.inner, (o, i) => o.copy(inner = i))
        for {
          ref    <- FiberRef.make(Outer(Inner("old"), tag = "keep"))
          _      <- ScenarioLens.update[Outer, Inner](_.copy(value = "new")).provide(State.layer(ref))
          result <- ref.get
        } yield assertTrue(result.inner.value == "new", result.tag == "keep")
      },
      test("set replaces the focused slice") {
        given HasLens[Outer, Inner] = HasLens(_.inner, (o, i) => o.copy(inner = i))
        for {
          ref    <- FiberRef.make(Outer(Inner("old")))
          _      <- ScenarioLens.set[Outer, Inner](Inner("replaced", 7)).provide(State.layer(ref))
          result <- ref.get
        } yield assertTrue(result.inner.value == "replaced", result.inner.count == 7)
      },
      test("ScenarioLens eliminates nested copy — before vs after comparison") {
        given HasLens[Outer, Inner] = HasLens(_.inner, (o, i) => o.copy(inner = i))
        for {
          ref <- FiberRef.make(Outer(Inner("before", 0)))
          // With ScenarioLens (clean):
          _ <- ScenarioLens
                 .update[Outer, Inner](_.copy(value = "after", count = 1))
                 .provide(State.layer(ref))
          // Without lens (verbose):
          // State.update[Outer](s => s.copy(inner = s.inner.copy(value = "after", count = 1)))
          result <- ref.get
        } yield assertTrue(result.inner.value == "after", result.inner.count == 1)
      }
    )
  )
}
