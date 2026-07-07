package zio.bdd.core

import zio.*
import zio.test.*

/**
 * Gate for issue #235 — the `poll` combinator. Timing tests use `withLiveClock`
 * (ZIOSpecDefault runs on TestClock, under which schedule delays never
 * advance); delays are kept small.
 */
object PollSpec extends ZIOSpecDefault {

  private val fast: Duration = 10.millis

  def spec = suite("PollSpec")(
    test("retries probe + assertion until the assertion holds (AC1)") {
      for
        counter <- Ref.make(0)
        probe    = counter.updateAndGet(_ + 1)
        result <- Assertions.poll(probe, maxTime = 2.seconds, interval = fast)(n =>
                    Assertions.assertTrue(n >= 3, s"only $n so far")
                  )
        got <- counter.get
      yield assertTrue(got >= 3)
    } @@ TestAspect.withLiveClock,
    test("on timeout, surfaces the last assertion failure and the last polled value (AC2)") {
      for exit <- Assertions
                    .poll(ZIO.succeed("PENDING"), maxTime = 100.millis, interval = fast)(status =>
                      Assertions.assertTrue(status == "DONE", s"status was $status")
                    )
                    .exit
      yield exit match
        case Exit.Failure(cause) =>
          val msg = cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
          // last polled value + the assertion's own message, not a generic "timed out".
          assertTrue(msg.contains("PENDING"), msg.contains("status was PENDING"))
        case Exit.Success(_) => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("retries using the default interval when only the probe + assertion are given (AC3)") {
      for
        counter <- Ref.make(0)
        probe    = counter.updateAndGet(_ + 1)
        _       <- Assertions.poll(probe, maxTime = 2.seconds)(n => Assertions.assertTrue(n >= 2, s"only $n"))
        got     <- counter.get
      yield assertTrue(got >= 2)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("the surfaced message shows the LAST polled value, not a stale earlier one (AC2)") {
      for
        counter <- Ref.make(0)
        probe    = counter.updateAndGet(_ + 1).map(n => s"v$n") // v1, v2, v3, …
        exit <- Assertions
                  .poll(probe, maxTime = 80.millis, interval = fast)(v => Assertions.assertTrue(v == "done", s"got $v"))
                  .exit
      yield exit match
        case Exit.Failure(cause) =>
          val msg = cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
          // the value keeps changing; the message must not still say v1.
          assertTrue(msg.contains("last polled value"), !msg.contains("v1)"))
        case Exit.Success(_) => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("a probe that itself fails surfaces its own error on timeout (no last-value annotation)") {
      for exit <-
          Assertions
            .poll(ZIO.fail(new RuntimeException("probe unavailable")), maxTime = 80.millis, interval = fast)(_ =>
              ZIO.unit
            )
            .exit
      yield exit match
        case Exit.Failure(cause) =>
          val msg = cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
          assertTrue(msg.contains("probe unavailable"), !msg.contains("last polled value"))
        case Exit.Success(_) => assertTrue(false)
    } @@ TestAspect.withLiveClock,
    test("is interruptible — an outer timeout stops it well before maxTime (AC3)") {
      for
        start <- Clock.nanoTime
        result <- Assertions
                    .poll(ZIO.succeed(0), maxTime = 1.hour, interval = fast)(_ => Assertions.assertTrue(false, "never"))
                    .timeout(150.millis)
        elapsed <- Clock.nanoTime.map(_ - start)
      yield assertTrue(result.isEmpty) && assertTrue(elapsed < 5.seconds.toNanos)
    } @@ TestAspect.withLiveClock
  )
}
