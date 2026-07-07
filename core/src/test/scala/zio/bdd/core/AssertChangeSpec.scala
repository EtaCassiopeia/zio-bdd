package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.core.step.State

/**
 * Gate for issue #224 — the `assertChange` before/after change builder (.by /
 * .from().to() / .toAnything / .and). Each test gets a fresh `Ref`-backed
 * `State[Counters]` so state never leaks between tests.
 */
object AssertChangeSpec extends ZIOSpecDefault {

  final case class Counters(a: Int = 0, b: Int = 0, c: Int = 0, money: BigDecimal = BigDecimal(0))

  private def withState[A](z: ZIO[State[Counters], Throwable, A]): ZIO[Any, Throwable, Exit[Throwable, A]] =
    Ref.make(Counters()).flatMap { ref =>
      val st = new State[Counters]:
        def get: UIO[Counters]                         = ref.get
        def update(f: Counters => Counters): UIO[Unit] = ref.update(f)
      z.exit.provideEnvironment(ZEnvironment(st))
    }

  private val probeA: ZIO[State[Counters], Nothing, Int]            = State.get[Counters].map(_.a)
  private val probeB: ZIO[State[Counters], Nothing, Int]            = State.get[Counters].map(_.b)
  private val probeC: ZIO[State[Counters], Nothing, Int]            = State.get[Counters].map(_.c)
  private val probeMoney: ZIO[State[Counters], Nothing, BigDecimal] = State.get[Counters].map(_.money)
  private def bumpA(by: Int): ZIO[State[Counters], Nothing, Unit]   = State.update[Counters](c => c.copy(a = c.a + by))
  private def bumpB(by: Int): ZIO[State[Counters], Nothing, Unit]   = State.update[Counters](c => c.copy(b = c.b + by))
  private def bumpC(by: Int): ZIO[State[Counters], Nothing, Unit]   = State.update[Counters](c => c.copy(c = c.c + by))
  private def bumpMoney(by: BigDecimal): ZIO[State[Counters], Nothing, Unit] =
    State.update[Counters](c => c.copy(money = c.money + by))

  private def messageOf(exit: Exit[Throwable, Unit]): String =
    exit match
      case Exit.Failure(cause) => cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
      case Exit.Success(_)     => ""

  def spec = suite("AssertChangeSpec")(
    // ── .by ───────────────────────────────────────────────────────────────
    suite(".by")(
      test("passes when the value changes by exactly the delta (AC1)") {
        withState(Assertions.assertChange(probeA)(bumpA(100)).by(100)).map(e => assertTrue(e.isSuccess))
      },
      test("fails when the change differs, showing before/after/expected (AC1, AC5)") {
        withState(Assertions.assertChange(probeA)(bumpA(70)).by(100)).map { exit =>
          val msg = messageOf(exit)
          assertTrue(exit.isFailure, msg.contains("100"), msg.contains("70"), msg.contains("before=0"))
        }
      },
      test("works for a non-Int Numeric (BigDecimal), including a negative delta (AC1)") {
        for
          up   <- withState(Assertions.assertChange(probeMoney)(bumpMoney(BigDecimal("1.25"))).by(BigDecimal("1.25")))
          down <- withState(Assertions.assertChange(probeMoney)(bumpMoney(BigDecimal("-2.50"))).by(BigDecimal("-2.50")))
        yield assertTrue(up.isSuccess, down.isSuccess)
      }
    ),
    // ── .from().to() ──────────────────────────────────────────────────────
    suite(".from.to")(
      test("passes on exact before and after values (AC2)") {
        withState(Assertions.assertChange(probeA)(bumpA(5)).from(0).to(5)).map(e => assertTrue(e.isSuccess))
      },
      test("fails when the before value does not match (AC2, AC5)") {
        withState(Assertions.assertChange(probeA)(bumpA(5)).from(1).to(6)).map(e =>
          assertTrue(e.isFailure, messageOf(e).contains("before"))
        )
      },
      test("fails when the after value does not match (AC2, AC5)") {
        withState(Assertions.assertChange(probeA)(bumpA(5)).from(0).to(99)).map(e =>
          assertTrue(e.isFailure, messageOf(e).contains("99"))
        )
      }
    ),
    // ── .toAnything ───────────────────────────────────────────────────────
    suite(".toAnything")(
      test("passes when the value changed at all (AC3)") {
        withState(Assertions.assertChange(probeA)(bumpA(1)).toAnything).map(e => assertTrue(e.isSuccess))
      },
      test("fails when the value did not change (AC3, AC5)") {
        withState(Assertions.assertChange(probeA)(bumpA(0)).toAnything).map(e =>
          assertTrue(e.isFailure, messageOf(e).toLowerCase.contains("change"))
        )
      }
    ),
    // ── .and composition ──────────────────────────────────────────────────
    suite(".and")(
      test("passes when both probes change over a single action (AC4)") {
        val action = bumpA(1) *> bumpB(2)
        withState(Assertions.assertChange(probeA)(action).and(probeB).assert).map(e => assertTrue(e.isSuccess))
      },
      test("chains 3+ probes and passes when all change (AC4)") {
        val action = bumpA(1) *> bumpB(1) *> bumpC(1)
        withState(Assertions.assertChange(probeA)(action).and(probeB).and(probeC).assert)
          .map(e => assertTrue(e.isSuccess))
      },
      test("fails when one composed probe does not change, naming it in the message (AC4, AC5)") {
        val action = bumpA(1) // b untouched
        withState(Assertions.assertChange(probeA)(action).and(probeB).assert).map { e =>
          assertTrue(e.isFailure, messageOf(e).toLowerCase.contains("change"))
        }
      },
      test("runs the action exactly once for the whole composition") {
        for
          runs  <- Ref.make(0)
          action = bumpA(1) *> bumpB(1) *> runs.update(_ + 1)
          exit  <- withState(Assertions.assertChange(probeA)(action).and(probeB).assert)
          count <- runs.get
        yield assertTrue(exit.isSuccess, count == 1)
      }
    ),
    // ── action failure propagates (not swallowed as "no change") ──────────
    test("a failing action propagates its error, it is not coerced into an assertion failure") {
      val boom: ZIO[State[Counters], Throwable, Unit] = ZIO.fail(new RuntimeException("action boom"))
      withState(Assertions.assertChange(probeA)(boom).by(1)).map(e =>
        assertTrue(e.isFailure, messageOf(e).contains("action boom"))
      )
    }
  )
}
