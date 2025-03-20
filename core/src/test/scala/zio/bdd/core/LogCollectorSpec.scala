package zio.bdd.core

import zio.*
import zio.test.*

object LogCollectorSpec extends ZIOSpecDefault {

  override def spec: Spec[Any, Any] = suite("LogCollectorSpec")(
    test("captures and isolates logs from different logging styles") {
      val scenarioId = "test-scenario"
      val stepId     = "test-step"
      val effect = ZIO.logAnnotate("scenarioId", scenarioId) {
        ZIO.logAnnotate("stepId", stepId) { // Set the annotation
          for {
            // Direct LogCollector usage
            _ <- LogCollector.log(scenarioId, stepId, "Direct stdout log", InternalLogLevel.Info)
            _ <- LogCollector.log(scenarioId, stepId, "Direct stderr log", InternalLogLevel.Error)

            // ZIO built-in logging
            _ <- ZIO.logInfo("ZIO info log")
            _ <- ZIO.logError("ZIO error log")

            collectedLogs <-
              ZIO.serviceWithZIO[LogCollector](_.getLogs(scenarioId, stepId)) // Fetch logs for this scenarioId/stepId
          } yield assertTrue(
            collectedLogs.entries.exists(e => e.source == LogSource.Stdout && e.message.contains("Direct stdout log")),
            collectedLogs.entries.exists(e => e.source == LogSource.Stderr && e.message.contains("Direct stderr log")),
            collectedLogs.entries.exists(e => e.source == LogSource.Stdout && e.message.contains("ZIO info log")),
            collectedLogs.entries.exists(e => e.source == LogSource.Stderr && e.message.contains("ZIO error log"))
          )
        }
      }

      effect.provide(LogCollector.live)
    }
  )
}
