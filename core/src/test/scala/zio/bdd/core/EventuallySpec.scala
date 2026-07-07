package zio.bdd.core

import zio.*
import zio.test.*

/**
 * Gate for issue #217 — `eventually` / `eventuallyAssert` polling combinators.
 *
 * Timing tests use `withLiveClock` because ZIOSpecDefault runs on TestClock,
 * under which schedule delays never advance on their own. Delays are kept small
 * (tens of ms) so the suite stays fast.
 */
object EventuallySpec extends ZIOSpecDefault {

  private val fast: Schedule[Any, Any, Any] = Schedule.spaced(10.millis)

  def spec = suite("EventuallySpec")(
    test("eventually returns immediately when the effect already succeeds (AC1 — no Schedule import)") {
      // Only the effect is passed: both maxTime and schedule use defaults.
      for result <- Assertions.eventually(ZIO.succeed(42))
      yield assertTrue(result == 42)
    },
    test("eventually retries until the effect succeeds (AC2 — custom schedule)") {
      for
        counter <- Ref.make(0)
        effect = counter.updateAndGet(_ + 1).flatMap { n =>
                   if (n >= 3) ZIO.succeed(n) else ZIO.fail(new RuntimeException(s"attempt $n too early"))
                 }
        result   <- Assertions.eventually(effect, maxTime = 2.seconds, schedule = fast)
        attempts <- counter.get
      yield assertTrue(result == 3, attempts == 3)
    } @@ TestAspect.withLiveClock,
    test("eventually retries using the default schedule when only maxTime is given (AC2 — defaults)") {
      // schedule left at the default; only maxTime overridden to keep the suite fast.
      for
        counter <- Ref.make(0)
        effect = counter.updateAndGet(_ + 1).flatMap { n =>
                   if (n >= 2) ZIO.succeed(n) else ZIO.fail(new RuntimeException("early"))
                 }
        result <- Assertions.eventually(effect, maxTime = 2.seconds)
      yield assertTrue(result == 2)
    } @@ TestAspect.withLiveClock,
    test("eventually surfaces the last underlying failure on timeout, not a generic timeout (AC3)") {
      val boom = new RuntimeException("flag not ON yet")
      for exit <- Assertions.eventually(ZIO.fail(boom), maxTime = 100.millis, schedule = fast).exit
      yield exit match
        case Exit.Failure(cause) =>
          assertTrue(cause.failureOption.exists(_.getMessage == "flag not ON yet"))
        case Exit.Success(_) =>
          assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("eventually is interruptible — an outer timeout stops it well before maxTime (AC4)") {
      val failing: ZIO[Any, Throwable, Int] = ZIO.fail(new RuntimeException("never"))
      for
        start   <- Clock.nanoTime
        result  <- Assertions.eventually(failing, maxTime = 1.hour, schedule = fast).timeout(150.millis)
        elapsed <- Clock.nanoTime.map(_ - start)
      yield assertTrue(result.isEmpty) && assertTrue(elapsed < 5.seconds.toNanos)
    } @@ TestAspect.withLiveClock,
    test("eventuallyAssert retries the fetch+assert pair until the assertion holds (AC6)") {
      for
        counter <- Ref.make(0)
        _ <- Assertions.eventuallyAssert(counter.updateAndGet(_ + 1))(
               n => Assertions.assertTrue(n >= 2, s"still $n"),
               maxTime = 2.seconds
             )
        attempts <- counter.get
      yield assertTrue(attempts >= 2)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("eventuallyAssert surfaces the assertion failure on timeout") {
      for exit <- Assertions
                    .eventuallyAssert(ZIO.succeed(1))(
                      n => Assertions.assertTrue(n == 999, s"got $n, want 999"),
                      maxTime = 100.millis
                    )
                    .exit
      yield exit match
        case Exit.Failure(cause) =>
          assertTrue(cause.failureOption.exists(t => Option(t.getMessage).exists(_.contains("got 1, want 999"))))
        case Exit.Success(_) =>
          assertTrue(false)
    } @@ TestAspect.withLiveClock
  )
}
