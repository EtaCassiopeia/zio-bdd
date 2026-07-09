package zio.bdd

import zio.{Cause, FiberFailure, FiberId}
import zio.test.*

/**
 * Gate for issue #308: a suite-level failure must be reported with its full
 * cause chain, not collapsed to `Throwable.getMessage`. `describeFailure`
 * unwraps the two wrappers that otherwise erase the real error:
 *   - `ExceptionInInitializerError` (no message of its own) → the cause chain
 *   - `FiberFailure` (one-line message) → the full `Cause.prettyPrint`
 */
object DescribeFailureSpec extends ZIOSpecDefault {

  def spec = suite("describeFailure surfaces the real cause (#308)")(
    test("an ExceptionInInitializerError surfaces its cause message (getMessage is null)") {
      val e   = new ExceptionInInitializerError(new RuntimeException("libluajit-5.1.so.2 cannot open"))
      val out = ZIOBDDTask.describeFailure(e)
      assertTrue(
        out.contains("libluajit-5.1.so.2 cannot open"), // the cause, lost by getMessage (which is null here)
        out.contains("RuntimeException")
      )
    },
    test("a FiberFailure with a defect surfaces the full prettyPrint, not just the message") {
      val ff  = FiberFailure(Cause.die(new RuntimeException("dlopen boom")))
      val out = ZIOBDDTask.describeFailure(ff)
      assertTrue(
        out.contains("dlopen boom"),
        out.contains("RuntimeException") // prettyPrint carries the type; the bare getMessage would not
      )
    },
    test("a FiberFailure with an interrupt-only cause still yields diagnostic output") {
      val ff  = FiberFailure(Cause.interrupt(FiberId(203857377, 0, zio.Trace.empty)))
      val out = ZIOBDDTask.describeFailure(ff)
      assertTrue(out.nonEmpty, out.toLowerCase.contains("interrupt"))
    }
  )
}
