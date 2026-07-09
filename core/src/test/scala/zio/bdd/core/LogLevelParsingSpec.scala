package zio.bdd.core

import zio.ZIO
import zio.bdd.ZIOBDDTask
import zio.test.*

/**
 * Gate for issue #289: `@Suite(logLevel)` / `--log-level` must accept all five
 * `InternalLogLevel` names and reject an unknown value loudly instead of
 * silently falling back to Info.
 */
object LogLevelParsingSpec extends ZIOSpecDefault {

  def spec = suite("log-level parsing")(
    test("accepts all five level names (case-insensitive)") {
      assertTrue(
        ZIOBDDTask.parseLogLevelString("debug") == Right(InternalLogLevel.Debug),
        ZIOBDDTask.parseLogLevelString("info") == Right(InternalLogLevel.Info),
        ZIOBDDTask.parseLogLevelString("warning") == Right(InternalLogLevel.Warning),
        ZIOBDDTask.parseLogLevelString("error") == Right(InternalLogLevel.Error),
        ZIOBDDTask.parseLogLevelString("fatal") == Right(InternalLogLevel.Fatal),
        ZIOBDDTask.parseLogLevelString("WARNING") == Right(InternalLogLevel.Warning),
        ZIOBDDTask.parseLogLevelString("Fatal") == Right(InternalLogLevel.Fatal)
      )
    },
    test("rejects an unknown value with a message naming the valid set") {
      val result = ZIOBDDTask.parseLogLevelString("verbose")
      assertTrue(
        result.isLeft,
        result.swap.exists(_.contains("verbose")),
        result.swap.exists(m => m.contains("warning") && m.contains("fatal"))
      )
    },
    test("throwing wrapper fails loud on an unknown value") {
      for {
        exit <- ZIO.attempt(ZIOBDDTask.parseLogLevelOrThrow("nope")).exit
      } yield assertTrue(
        exit.isFailure,
        exit.causeOption.exists(_.failures.exists(_.isInstanceOf[IllegalArgumentException]))
      )
    },
    test("throwing wrapper returns the level for a valid value") {
      assertTrue(ZIOBDDTask.parseLogLevelOrThrow("fatal") == InternalLogLevel.Fatal)
    }
  )
}
