package zio.bdd.core

import zio.*
import zio.test.*
import zio.bdd.core.Assertions.shouldBeCloseTo

/** Gate for issue #236 — `assertApproxEquals` / `shouldBeCloseTo`. */
object ApproxEqualsSpec extends ZIOSpecDefault {

  private def messageOf(exit: Exit[Throwable, Unit]): String =
    exit match
      case Exit.Failure(cause) => cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
      case Exit.Success(_)     => ""

  def spec = suite("ApproxEqualsSpec")(
    suite("assertApproxEquals (AC1)")(
      test("passes when the difference is within delta (Double)") {
        Assertions.assertApproxEquals(1.0, 1.05, 0.1).exit.map(e => assertTrue(e.isSuccess))
      },
      test("passes at the exact boundary (diff == delta)") {
        // 0.5 is exactly representable, so diff == delta holds precisely (avoids float boundary noise).
        Assertions.assertApproxEquals(1.0, 1.5, 0.5).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fails outside delta, showing actual, expected, delta and the difference") {
        // Chosen so the difference (0.7) is a distinct string from actual/expected/delta, proving the
        // "(difference was …)" clause is actually present.
        Assertions.assertApproxEquals(1.0, 1.7, 0.1).exit.map { e =>
          val msg = messageOf(e)
          assertTrue(e.isFailure, msg.contains("1.0"), msg.contains("1.7"), msg.contains("0.1"), msg.contains("0.7"))
        }
      },
      test("fails closed for NaN and for a negative delta") {
        for
          nan <- Assertions.assertApproxEquals(Double.NaN, 1.0, 0.1).exit
          neg <- Assertions.assertApproxEquals(1.0, 1.0, -0.1).exit
        yield assertTrue(nan.isFailure, neg.isFailure)
      },
      test("honours a custom message prefix") {
        Assertions.assertApproxEquals(1.0, 2.0, 0.1, "balance drifted").exit.map { e =>
          assertTrue(e.isFailure, messageOf(e).contains("balance drifted"))
        }
      }
    ),
    suite("shouldBeCloseTo (AC2)")(
      test("fluent form passes within delta") {
        1.0.shouldBeCloseTo(1.05, 0.1).exit.map(e => assertTrue(e.isSuccess))
      },
      test("fluent form fails outside delta") {
        1.0.shouldBeCloseTo(2.0, 0.1).exit.map(e => assertTrue(e.isFailure))
      }
    ),
    suite("non-Double Numeric (AC3)")(
      test("works for BigDecimal within delta") {
        Assertions
          .assertApproxEquals(BigDecimal("1.00"), BigDecimal("1.02"), BigDecimal("0.05"))
          .exit
          .map(e => assertTrue(e.isSuccess))
      },
      test("works for BigDecimal outside delta") {
        Assertions
          .assertApproxEquals(BigDecimal("1.00"), BigDecimal("1.20"), BigDecimal("0.05"))
          .exit
          .map(e => assertTrue(e.isFailure))
      }
    )
  )
}
