package zio.bdd.core

import zio.*
import zio.test.*

/**
 * Gate for issue #222 — the `during` stability combinator (inverse of
 * `eventually`). Timing tests use `withLiveClock` (ZIOSpecDefault runs on
 * TestClock, under which schedule delays never advance); delays are kept small.
 */
object DuringSpec extends ZIOSpecDefault {

  def spec = suite("DuringSpec")(
    test("during passes when the condition holds on every probe across the window (AC1, AC3)") {
      // Only condition + duration passed: interval defaults, no Schedule/Clock import at call site.
      for
        probes <- Ref.make(0)
        _      <- Assertions.during(probes.update(_ + 1), duration = 120.millis, interval = 20.millis)
        count  <- probes.get
      // A 120ms window at 20ms yields ~6 probes ideally, but fiber-scheduling jitter on a loaded CI
      // runner can bunch them up — assert only that it probed repeatedly (the point of AC1/AC3), not an
      // exact count, so this wall-clock test doesn't flake (it intermittently saw count=3 vs a >=4 bound).
      yield assertTrue(count >= 2)
    } @@ TestAspect.withLiveClock,
    test("during probes repeatedly using the default interval when only duration is given (AC4 — default)") {
      // interval left at the default (200ms); ~500ms window ⇒ a small handful of probes.
      for
        probes <- Ref.make(0)
        _      <- Assertions.during(probes.update(_ + 1), duration = 500.millis)
        count  <- probes.get
      yield assertTrue(count >= 2, count <= 6)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("during terminates (does not hang) when interval > duration (boundary)") {
      for
        start   <- Clock.nanoTime
        result  <- Assertions.during(ZIO.unit, duration = 10.millis, interval = 100.millis).timeout(5.seconds)
        elapsed <- Clock.nanoTime.map(_ - start)
      yield assertTrue(result.isDefined, elapsed < 2.seconds.toNanos)
    } @@ TestAspect.withLiveClock,
    test("during lets a defect from the condition propagate immediately, not swallowed (AC2)") {
      for exit <- Assertions.during(ZIO.succeed[Unit](throw new RuntimeException("boom")), duration = 1.second).exit
      yield exit match
        case Exit.Failure(cause) => assertTrue(cause.dieOption.exists(_.getMessage == "boom"))
        case Exit.Success(_)     => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("during fails fast on the first mid-window violation, surfacing that failure (AC2)") {
      // Condition holds for the first two probes, then starts failing.
      for
        probes <- Ref.make(0)
        condition = probes.updateAndGet(_ + 1).flatMap { n =>
                      if (n >= 3) ZIO.fail(new RuntimeException(s"violated at probe $n")) else ZIO.unit
                    }
        start   <- Clock.nanoTime
        exit    <- Assertions.during(condition, duration = 10.seconds, interval = 20.millis).exit
        elapsed <- Clock.nanoTime.map(_ - start)
      yield exit match
        case Exit.Failure(cause) =>
          assertTrue(
            cause.failureOption.exists(_.getMessage == "violated at probe 3"),
            elapsed < 5.seconds.toNanos // failed fast, did NOT wait the full 10s
          )
        case Exit.Success(_) => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("during fails immediately when the condition is already false at t=0 (AC2)") {
      for exit <- Assertions.during(ZIO.fail(new RuntimeException("false from the start")), duration = 1.second).exit
      yield exit match
        case Exit.Failure(cause) => assertTrue(cause.failureOption.exists(_.getMessage == "false from the start"))
        case Exit.Success(_)     => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("during is interruptible — an outer timeout stops it well before duration (AC4)") {
      val holds: ZIO[Any, Throwable, Unit] = ZIO.unit
      for
        start   <- Clock.nanoTime
        result  <- Assertions.during(holds, duration = 1.hour, interval = 20.millis).timeout(150.millis)
        elapsed <- Clock.nanoTime.map(_ - start)
      yield assertTrue(result.isEmpty) && assertTrue(elapsed < 5.seconds.toNanos)
    } @@ TestAspect.withLiveClock
  )
}
