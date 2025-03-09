package zio.bdd.core

import zio.*
import zio.test.*

object LogCollectorSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Any] = suite("LogCollectorSpec")(
    test("captures and isolates logs from different logging styles") {
      val scenarioId = "test-scenario"
      val effect = ZIO.logAnnotate("scenarioId", scenarioId) { // Set the annotation
        for {
          // Direct LogCollector usage
          _ <- LogCollector.logStdout(scenarioId, "Direct stdout log")
          _ <- LogCollector.logStderr(scenarioId, "Direct stderr log")

          // ZIO built-in logging
          _ <- ZIO.logInfo("ZIO info log")
          _ <- ZIO.logError("ZIO error log")

          collectedLogs <- LogCollector.getLogs(scenarioId) // Fetch logs for this scenarioId
        } yield assertTrue(
          collectedLogs.stdout.exists(_.message.contains("Direct stdout log")),
          collectedLogs.stderr.exists(_.message.contains("Direct stderr log")),
          collectedLogs.stdout.exists(_.message.contains("ZIO info log")),
          collectedLogs.stderr.exists(_.message.contains("ZIO error log"))
        )
      }

      effect.provide(LogCollector.live)
    }
  )
}
