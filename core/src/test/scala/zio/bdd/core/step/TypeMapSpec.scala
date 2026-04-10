package zio.bdd.core.step

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.Default
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for TypeMap — the type-indexed heterogeneous state map.
 *
 * TypeMap replaces monolithic scenario state case classes by giving each step
 * module its own isolated state slice keyed by izumi Tag[A].
 */
object TypeMapSpec extends ZIOSpecDefault {

  // State slices used across tests
  final case class UserCtx(userId: String = "", token: String = "")
  final case class CartCtx(items: List[String] = Nil, total: Double = 0.0)

  given Schema[UserCtx] = DeriveSchema.gen[UserCtx]
  given Schema[CartCtx] = DeriveSchema.gen[CartCtx]

  private val pureOps = suite("TypeMap pure operations")(
    test("empty map returns None for any key") {
      val m = TypeMap.empty
      assertTrue(m.get[UserCtx].isEmpty, m.get[CartCtx].isEmpty)
    },
    test("put stores a value and get retrieves it") {
      val m = TypeMap.empty.put(UserCtx("alice", "tok123"))
      assertTrue(m.get[UserCtx].contains(UserCtx("alice", "tok123")))
    },
    test("put[A] is isolated — does not affect get[B]") {
      val m = TypeMap.empty.put(UserCtx("alice", "tok"))
      assertTrue(m.get[CartCtx].isEmpty)
    },
    test("second put for the same type overwrites the previous value") {
      val m = TypeMap.empty
        .put(UserCtx("alice", "old-tok"))
        .put(UserCtx("alice", "new-tok"))
      assertTrue(m.get[UserCtx].exists(_.token == "new-tok"))
    },
    test("getOrDefault returns the caller-supplied default when no value is set") {
      val m = TypeMap.empty
      assertTrue(m.getOrDefault(UserCtx("default", "")) == UserCtx("default", ""))
    },
    test("getOrDefault returns the stored value when one exists") {
      val m = TypeMap.empty.put(UserCtx("real", "tok"))
      assertTrue(m.getOrDefault(UserCtx("default", "")) == UserCtx("real", "tok"))
    },
    test("modify updates an existing value using the supplied function") {
      given Default[UserCtx] = Default.from(UserCtx())
      val m                  = TypeMap.empty.put(UserCtx("alice", "old")).modify[UserCtx](_.copy(token = "new"))
      assertTrue(m.get[UserCtx].exists(_.token == "new"))
    },
    test("modify falls back to Default[A] when no value is set") {
      given Default[UserCtx] = Default.from(UserCtx())
      val m                  = TypeMap.empty.modify[UserCtx](_.copy(userId = "from-default"))
      assertTrue(m.get[UserCtx].exists(_.userId == "from-default"))
    },
    test("multiple independent slices coexist without interference") {
      val m = TypeMap.empty
        .put(UserCtx("alice", "tok"))
        .put(CartCtx(List("item1"), 42.0))
      assertTrue(
        m.get[UserCtx].exists(_.userId == "alice"),
        m.get[CartCtx].exists(_.total == 42.0)
      )
    }
  )

  // ── TypeMapOps (ZIO effects over State[TypeMap]) ───────────────────────

  private val effectOps = suite("TypeMapOps ZIO effects")(
    test("get retrieves the current slice from State[TypeMap]") {
      given Default[UserCtx] = Default.from(UserCtx())
      for {
        ref    <- FiberRef.make(TypeMap.empty.put(UserCtx("bob", "tok")))
        result <- TypeMapOps.get[UserCtx].provide(State.layer(ref))
      } yield assertTrue(result.contains(UserCtx("bob", "tok")))
    },
    test("getOrDefault returns default when slice is absent") {
      given Default[UserCtx] = Default.from(UserCtx("default", ""))
      for {
        ref    <- FiberRef.make(TypeMap.empty)
        result <- TypeMapOps.getOrDefault[UserCtx].provide(State.layer(ref))
      } yield assertTrue(result == UserCtx("default", ""))
    },
    test("update applies a function to the slice") {
      given Default[UserCtx] = Default.from(UserCtx())
      for {
        ref    <- FiberRef.make(TypeMap.empty)
        _      <- TypeMapOps.update[UserCtx](_.copy(userId = "updated")).provide(State.layer(ref))
        result <- ref.get
      } yield assertTrue(result.get[UserCtx].exists(_.userId == "updated"))
    },
    test("set replaces the slice entirely") {
      given Default[UserCtx] = Default.from(UserCtx())
      for {
        ref    <- FiberRef.make(TypeMap.empty.put(UserCtx("old", "tok")))
        _      <- TypeMapOps.set(UserCtx("new", "tok2")).provide(State.layer(ref))
        result <- ref.get
      } yield assertTrue(result.get[UserCtx].exists(_.userId == "new"))
    },
    test("two independent slices can be updated in the same State[TypeMap]") {
      given Default[UserCtx] = Default.from(UserCtx())
      given Default[CartCtx] = Default.from(CartCtx())
      for {
        ref <- FiberRef.make(TypeMap.empty)
        _   <- TypeMapOps.set(UserCtx("alice", "tok")).provide(State.layer(ref))
        _   <- TypeMapOps.set(CartCtx(List("item"), 10.0)).provide(State.layer(ref))
        m   <- ref.get
      } yield assertTrue(
        m.get[UserCtx].exists(_.userId == "alice"),
        m.get[CartCtx].exists(_.total == 10.0)
      )
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("TypeMap")(pureOps, effectOps)
}
