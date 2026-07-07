package zio.bdd.core

import zio.*
import zio.test.*

/** Gate for issue #233 — `assertSatisfies` / `assertSatisfiesZIO`. */
object AssertSatisfiesSpec extends ZIOSpecDefault {

  private def messageOf(exit: Exit[Throwable, Unit]): String =
    exit match
      case Exit.Failure(cause) => cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
      case Exit.Success(_)     => ""

  def spec = suite("AssertSatisfiesSpec")(
    suite("assertSatisfies (AC1, AC3)")(
      test("passes when the predicate is true") {
        Assertions.assertSatisfies(42, "is even")(_ % 2 == 0).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails when false, naming the description and the actual value") {
        Assertions.assertSatisfies(41, "is even")(_ % 2 == 0).exit.map { e =>
          val msg = messageOf(e)
          assertTrue(e.isFailure, msg.contains("41"), msg.contains("is even"))
        }
      },
      test("a throwing pure predicate surfaces as a typed failure, not a defect") {
        Assertions.assertSatisfies(1, "probe")(_ => throw new RuntimeException("pred boom")).exit.map { e =>
          // messageOf reads failureOption (typed only), so a non-empty message proves it's a Fail, not a Die.
          assertTrue(e.isFailure, messageOf(e).contains("pred boom"))
        }
      }
    ),
    suite("assertSatisfiesZIO (AC2, AC3)")(
      test("passes when the effectful predicate yields true") {
        Assertions
          .assertSatisfiesZIO("hello", "is non-empty")(s => ZIO.succeed(s.nonEmpty))
          .exit
          .map(e => assertTrue(e.isSuccess))
      },
      test("fails when the effectful predicate yields false, naming description + actual") {
        Assertions.assertSatisfiesZIO("bob", "is at least 5 chars")(s => ZIO.succeed(s.length >= 5)).exit.map { e =>
          val msg = messageOf(e)
          assertTrue(e.isFailure, msg.contains("bob"), msg.contains("is at least 5 chars"))
        }
      },
      test("propagates a failing predicate effect (not swallowed)") {
        Assertions.assertSatisfiesZIO(1, "probe")(_ => ZIO.fail(new RuntimeException("probe boom"))).exit.map { e =>
          assertTrue(e.isFailure, messageOf(e).contains("probe boom"))
        }
      },
      test("a synchronously-throwing predicate surfaces as a typed failure, not a defect") {
        Assertions.assertSatisfiesZIO(1, "probe")(_ => throw new RuntimeException("sync boom")).exit.map { e =>
          assertTrue(e.isFailure, messageOf(e).contains("sync boom"))
        }
      }
    )
  )
}
