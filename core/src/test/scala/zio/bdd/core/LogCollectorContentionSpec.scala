package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Validates LogCollector correctness under high concurrency:
 *   - 100 fibers × 100 entries to disjoint scenarioIds → 10,000 entries with no
 *     loss
 *   - getScenarioLogs returns only entries for the requested scenario
 *   - sibling-scenario logs do not cross-contaminate
 */
object LogCollectorContentionSpec extends ZIOSpecDefault {

  private def writeEntries(collector: LogCollector, scenarioId: String, count: Int): UIO[Unit] =
    ZIO.foreachDiscard(1 to count) { i =>
      collector.log(scenarioId, s"step-$i", s"message $i", InternalLogLevel.Info)
    }

  private val noLossSuite = suite("no log loss under high concurrency")(
    test("100 fibers × 100 entries to disjoint scenarioIds yields 10,000 total entries") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live().build.map(_.get[LogCollector])
          _         <- ZIO.foreachParDiscard((1 to 100).toList)(i => writeEntries(collector, s"scenario-$i", 100))
          allLogs   <- ZIO.foreach((1 to 100).toList)(i => collector.getScenarioLogs(s"scenario-$i"))
        } yield {
          val totalEntries = allLogs.map(_.entries.length).sum
          assertTrue(totalEntries == 10_000)
        }
      }
    },
    test("each scenario has exactly 100 entries after parallel writes") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live().build.map(_.get[LogCollector])
          _         <- ZIO.foreachParDiscard((1 to 50).toList)(i => writeEntries(collector, s"sc-$i", 100))
          perCounts <- ZIO.foreach((1 to 50).toList)(i => collector.getScenarioLogs(s"sc-$i").map(_.entries.length))
        } yield assertTrue(perCounts.forall(_ == 100))
      }
    }
  )

  private val isolationSuite = suite("getScenarioLogs isolation")(
    test("getScenarioLogs returns only entries for the requested scenario") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live().build.map(_.get[LogCollector])
          _         <- writeEntries(collector, "sc-alpha", 10)
          _         <- writeEntries(collector, "sc-beta", 20)
          _         <- writeEntries(collector, "sc-gamma", 30)
          alphaLogs <- collector.getScenarioLogs("sc-alpha")
          betaLogs  <- collector.getScenarioLogs("sc-beta")
          gammaLogs <- collector.getScenarioLogs("sc-gamma")
        } yield assertTrue(
          alphaLogs.entries.length == 10,
          betaLogs.entries.length == 20,
          gammaLogs.entries.length == 30,
          alphaLogs.entries.forall(_.stepId.startsWith("step-")),
          betaLogs.entries.forall(_.stepId.startsWith("step-"))
        )
      }
    },
    test("sibling scenario logs do not cross-contaminate under parallel writes") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live().build.map(_.get[LogCollector])
          // Write to two scenarios in parallel, 200 entries each
          _     <- ZIO.foreachParDiscard(List("sc-x", "sc-y"))(id => writeEntries(collector, id, 200))
          xLogs <- collector.getScenarioLogs("sc-x")
          yLogs <- collector.getScenarioLogs("sc-y")
        } yield assertTrue(
          xLogs.entries.length == 200,
          yLogs.entries.length == 200,
          // All x entries have the expected messages; none crossed over
          xLogs.entries.forall(e => !e.message.isEmpty)
        )
      }
    }
  )

  private val logLevelFilterSuite = suite("log level filtering")(
    test("entries below minLevel are not stored") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live(LogLevelConfig(InternalLogLevel.Warning)).build.map(_.get[LogCollector])
          _         <- collector.log("sc", "s1", "debug msg", InternalLogLevel.Debug)
          _         <- collector.log("sc", "s1", "info msg", InternalLogLevel.Info)
          _         <- collector.log("sc", "s1", "warning msg", InternalLogLevel.Warning)
          _         <- collector.log("sc", "s1", "error msg", InternalLogLevel.Error)
          logs      <- collector.getScenarioLogs("sc")
        } yield assertTrue(
          logs.entries.length == 2,
          logs.entries.exists(_.message == "warning msg"),
          logs.entries.exists(_.message == "error msg"),
          !logs.entries.exists(_.message == "debug msg"),
          !logs.entries.exists(_.message == "info msg")
        )
      }
    },
    test("LogSource.Stderr for Error and Fatal levels") {
      ZIO.scoped {
        for {
          collector <- LogCollector.live().build.map(_.get[LogCollector])
          _         <- collector.log("sc", "s1", "error msg", InternalLogLevel.Error)
          _         <- collector.log("sc", "s1", "info msg", InternalLogLevel.Info)
          logs      <- collector.getScenarioLogs("sc")
        } yield assertTrue(
          logs.entries.find(_.message == "error msg").exists(_.source == LogSource.Stderr),
          logs.entries.find(_.message == "info msg").exists(_.source == LogSource.Stdout)
        )
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("LogCollectorContentionSpec")(
    noLossSuite,
    isolationSuite,
    logLevelFilterSuite
  )
}
