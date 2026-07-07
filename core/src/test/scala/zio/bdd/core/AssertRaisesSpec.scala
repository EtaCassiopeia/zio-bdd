package zio.bdd.core

import zio.*
import zio.test.*

/** Gate for issue #234 — `assertRaises[E](effect) { inspect }`. */
object AssertRaisesSpec extends ZIOSpecDefault {

  class PaymentError(msg: String)             extends RuntimeException(msg)
  final class NarrowPaymentError(msg: String) extends PaymentError(msg)
  final class OtherError(msg: String)         extends RuntimeException(msg)

  private def messageOf(exit: Exit[Throwable, Unit]): String =
    exit match
      case Exit.Failure(cause) => cause.failureOption.flatMap(t => Option(t.getMessage)).getOrElse("")
      case Exit.Success(_)     => ""

  def spec = suite("AssertRaisesSpec")(
    test("passes when the effect fails with E and inspect succeeds (AC1)") {
      Assertions
        .assertRaises[PaymentError](ZIO.fail(new PaymentError("declined")))(e =>
          Assertions.assertTrue(e.getMessage == "declined")
        )
        .exit
        .map(e => assertTrue(e.isSuccess))
    },
    test("inspect can assert on the caught exception's message (AC3)") {
      Assertions
        .assertRaises[PaymentError](ZIO.fail(new PaymentError("card declined")))(e =>
          Assertions.assertTrue(e.getMessage.contains("declined"), "message should mention declined")
        )
        .exit
        .map(e => assertTrue(e.isSuccess))
    },
    test("fails when the effect succeeds — no exception thrown (AC2); default message names E") {
      Assertions.assertRaises[PaymentError](ZIO.succeed(1))(_ => ZIO.unit).exit.map { e =>
        val msg = messageOf(e)
        assertTrue(e.isFailure, msg.toLowerCase.contains("no exception"), msg.contains("PaymentError"))
      }
    },
    test("fails when the effect raises the wrong exception type (AC2)") {
      Assertions.assertRaises[PaymentError](ZIO.fail(new OtherError("boom")))(_ => ZIO.unit).exit.map { e =>
        assertTrue(e.isFailure, messageOf(e).contains("OtherError"))
      }
    },
    test("fails when inspect fails (AC2)") {
      Assertions
        .assertRaises[PaymentError](ZIO.fail(new PaymentError("declined")))(_ =>
          Assertions.assertTrue(false, "inspect deliberately fails")
        )
        .exit
        .map(e => assertTrue(e.isFailure, messageOf(e).contains("inspect deliberately fails")))
    },
    test("catches an exception raised as a defect (die) too") {
      Assertions
        .assertRaises[PaymentError](ZIO.die(new PaymentError("thrown")))(e =>
          Assertions.assertTrue(e.getMessage == "thrown")
        )
        .exit
        .map(e => assertTrue(e.isSuccess))
    },
    test("accepts a subtype of E") {
      Assertions
        .assertRaises[PaymentError](ZIO.fail(new NarrowPaymentError("narrow")))(e =>
          Assertions.assertTrue(e.getMessage == "narrow")
        )
        .exit
        .map(e => assertTrue(e.isSuccess))
    },
    test("honours an explicit custom message") {
      Assertions.assertRaises[PaymentError](ZIO.succeed(1), "custom expectation")(_ => ZIO.unit).exit.map { e =>
        assertTrue(e.isFailure, messageOf(e).contains("custom expectation"))
      }
    },
    test("interruption propagates — it is not treated as a raised exception") {
      Assertions
        .assertRaises[PaymentError](ZIO.failCause(Cause.interrupt(zio.FiberId.None)))(_ => ZIO.unit)
        .exit
        .map(e => assertTrue(e.isInterrupted))
    }
  )
}
