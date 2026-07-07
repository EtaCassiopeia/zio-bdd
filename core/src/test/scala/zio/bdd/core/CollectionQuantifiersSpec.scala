package zio.bdd.core

import zio.*
import zio.test.*

/**
 * Gate for issue #223 — collection quantifiers (assertForAll / assertExists /
 * assertNoneSatisfy / assertExactly / assertBetween). "Satisfies" means the
 * predicate succeeds.
 */
object CollectionQuantifiersSpec extends ZIOSpecDefault {

  // pred: element is even
  private def isEven(n: Int): ZIO[Any, Throwable, Unit] =
    Assertions.assertTrue(n % 2 == 0, s"$n is odd")

  private def messageOf(exit: Exit[Throwable, Unit]): String =
    exit match
      case Exit.Failure(cause) => cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
      case Exit.Success(_)     => ""

  def spec = suite("CollectionQuantifiersSpec")(
    // ── assertForAll ──────────────────────────────────────────────
    suite("assertForAll")(
      test("passes when every element satisfies the predicate") {
        Assertions.assertForAll(List(2, 4, 6))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("passes vacuously on an empty collection") {
        Assertions.assertForAll(List.empty[Int])(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails and lists offending elements with indices and label (AC3)") {
        for exit <- Assertions.assertForAll(List(2, 3, 4, 5), label = "evens")(isEven).exit
        yield
          val msg = messageOf(exit)
          assertTrue(
            exit.isFailure,
            msg.contains("evens"),
            msg.contains("index 1"), // the value 3
            msg.contains("index 3"), // the value 5
            !msg.contains("index 0") // 2 satisfied, not an offender
          )
      }
    ),
    // ── assertExists ──────────────────────────────────────────────
    suite("assertExists")(
      test("passes when at least one element satisfies") {
        Assertions.assertExists(List(1, 3, 4))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails when none satisfy") {
        Assertions.assertExists(List(1, 3, 5), label = "evens")(isEven).exit.map { e =>
          assertTrue(e.isFailure, messageOf(e).contains("evens"))
        }
      },
      test("fails on an empty collection") {
        Assertions.assertExists(List.empty[Int])(isEven).exit.map(e => assertTrue(e.isFailure))
      }
    ),
    // ── assertNoneSatisfy ─────────────────────────────────────────
    suite("assertNoneSatisfy")(
      test("passes when no element satisfies") {
        Assertions.assertNoneSatisfy(List(1, 3, 5))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("passes on an empty collection") {
        Assertions.assertNoneSatisfy(List.empty[Int])(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails and lists the offending (satisfying) elements with indices") {
        for exit <- Assertions.assertNoneSatisfy(List(1, 2, 3, 4))(isEven).exit
        yield
          val msg = messageOf(exit)
          assertTrue(exit.isFailure, msg.contains("index 1"), msg.contains("index 3"))
      }
    ),
    // ── assertExactly ─────────────────────────────────────────────
    suite("assertExactly")(
      test("passes when exactly n satisfy") {
        Assertions.assertExactly(2, List(1, 2, 3, 4))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails when the count differs from n, listing the satisfying indices") {
        Assertions.assertExactly(3, List(1, 2, 3, 4), label = "evens")(isEven).exit.map { e =>
          val msg = messageOf(e)
          // 2 and 4 satisfy at indices 1 and 3; the message must name them (not a bare count).
          assertTrue(e.isFailure, msg.contains("evens"), msg.contains("index 1"), msg.contains("index 3"))
        }
      },
      test("assertExactly(0) passes on an empty collection") {
        Assertions.assertExactly(0, List.empty[Int])(isEven).exit.map(e => assertTrue(e.isSuccess))
      }
    ),
    // ── assertBetween ─────────────────────────────────────────────
    suite("assertBetween")(
      test("passes when the satisfying count is within [min, max]") {
        Assertions.assertBetween(1, 3, List(1, 2, 3, 4, 6))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("passes at the min boundary exactly") {
        // exactly 1 even (2) with min=1
        Assertions.assertBetween(1, 3, List(1, 2, 3))(isEven).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails when the count is below min") {
        Assertions.assertBetween(3, 5, List(1, 2, 3))(isEven).exit.map(e => assertTrue(e.isFailure))
      },
      test("fails when the count is above max, listing the satisfying indices and label (AC3)") {
        for exit <- Assertions.assertBetween(0, 1, List(2, 4, 6), label = "evens")(isEven).exit
        yield
          val msg = messageOf(exit)
          // 2,4,6 all satisfy at indices 0,1,2; count 3 > max 1.
          assertTrue(exit.isFailure, msg.contains("evens"), msg.contains("index 0"), msg.contains("index 2"))
      },
      test("empty collection: passes when min=0, fails when min>0") {
        for
          ok  <- Assertions.assertBetween(0, 2, List.empty[Int])(isEven).exit
          bad <- Assertions.assertBetween(1, 2, List.empty[Int])(isEven).exit
        yield assertTrue(ok.isSuccess, bad.isFailure)
      }
    ),
    // ── naming collision (AC2): the existing Option assertNone still resolves ──
    test("the pre-existing assertNone(Option) is unaffected") {
      Assertions.assertNone(Option.empty[Int]).exit.map(e => assertTrue(e.isSuccess))
    }
  )
}
