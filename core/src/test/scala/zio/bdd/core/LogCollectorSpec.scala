package zio.bdd.core

import zio.*
import zio.test.*

object LogCollectorSpec extends ZIOSpecDefault {

  private val debugConfig = LogLevelConfig(InternalLogLevel.Debug)
  private val warnConfig  = LogLevelConfig(InternalLogLevel.Warning)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LogCollector")(
    suite("Basic log capture")(
      test("direct LogCollector.log call is captured and retrievable") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _    <- LogCollector.log("sc1", "step1", "hello", InternalLogLevel.Info)
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(logs.entries.exists(_.message.contains("hello")))
            }
          }
          .provide(LogCollector.live(debugConfig))
      },
      test("Info log is captured to Stdout source") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _    <- LogCollector.log("sc1", "step1", "info msg", InternalLogLevel.Info)
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(
                logs.entries.exists(e => e.source == LogSource.Stdout && e.message.contains("info msg"))
              )
            }
          }
          .provide(LogCollector.live(debugConfig))
      },
      test("Error log is captured to Stderr source") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _    <- LogCollector.log("sc1", "step1", "error msg", InternalLogLevel.Error)
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(
                logs.entries.exists(e => e.source == LogSource.Stderr && e.message.contains("error msg"))
              )
            }
          }
          .provide(LogCollector.live(debugConfig))
      }
    ),
    suite("ZIO.log* integration")(
      test("ZIO.logInfo is captured when stepId annotation is set") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _    <- ZIO.logInfo("ZIO info log")
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(logs.entries.exists(_.message.contains("ZIO info log")))
            }
          }
          .provide(LogCollector.live(debugConfig))
      },
      test("ZIO.logError is captured to Stderr when stepId annotation is set") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _    <- ZIO.logError("ZIO error log")
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(
                logs.entries.exists(e => e.source == LogSource.Stderr && e.message.contains("ZIO error log"))
              )
            }
          }
          .provide(LogCollector.live(debugConfig))
      },
      test("ZIO.log* without stepId annotation is not captured (not a step)") {
        for {
          _    <- ZIO.logInfo("outside step")
          logs <- ZIO.serviceWithZIO[LogCollector](_.getScenarioLogs("sc-any"))
        } yield assertTrue(logs.entries.isEmpty)
      }.provide(LogCollector.live(debugConfig))
    ),
    suite("Log isolation")(
      test("logs for different scenario+step IDs are stored independently") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "s1") {
              for {
                _     <- LogCollector.log("sc1", "s1", "msg-s1", InternalLogLevel.Info)
                _     <- LogCollector.log("sc2", "s2", "msg-s2", InternalLogLevel.Info)
                logs1 <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "s1"))
                logs2 <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc2", "s2"))
              } yield assertTrue(
                logs1.entries.exists(_.message.contains("msg-s1")),
                !logs1.entries.exists(_.message.contains("msg-s2")),
                logs2.entries.exists(_.message.contains("msg-s2")),
                !logs2.entries.exists(_.message.contains("msg-s1"))
              )
            }
          }
          .provide(LogCollector.live(debugConfig))
      },
      test("getScenarioLogs returns all logs for a scenario regardless of step") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                _   <- LogCollector.log("sc1", "step1", "from-step1", InternalLogLevel.Info)
                _   <- LogCollector.log("sc1", "step2", "from-step2", InternalLogLevel.Info)
                all <- ZIO.serviceWithZIO[LogCollector](_.getScenarioLogs("sc1"))
              } yield assertTrue(
                all.entries.exists(_.message.contains("from-step1")),
                all.entries.exists(_.message.contains("from-step2"))
              )
            }
          }
          .provide(LogCollector.live(debugConfig))
      }
    ),
    suite("Log level filtering")(
      test("logs below the configured minimum level are dropped") {
        ZIO
          .logAnnotate("scenarioId", "sc1") {
            ZIO.logAnnotate("stepId", "step1") {
              for {
                // With Warning-minimum config, Debug and Info should be dropped
                _    <- LogCollector.log("sc1", "step1", "debug-msg", InternalLogLevel.Debug)
                _    <- LogCollector.log("sc1", "step1", "info-msg", InternalLogLevel.Info)
                _    <- LogCollector.log("sc1", "step1", "warn-msg", InternalLogLevel.Warning)
                _    <- LogCollector.log("sc1", "step1", "error-msg", InternalLogLevel.Error)
                logs <- ZIO.serviceWithZIO[LogCollector](_.getLogs("sc1", "step1"))
              } yield assertTrue(
                !logs.entries.exists(_.message.contains("debug-msg")),
                !logs.entries.exists(_.message.contains("info-msg")),
                logs.entries.exists(_.message.contains("warn-msg")),
                logs.entries.exists(_.message.contains("error-msg"))
              )
            }
          }
          .provide(LogCollector.live(warnConfig))
      }
    ),
    suite("Logger augmentation")(
      test("ZIO.log* calls do not throw even if collection fails") {
        // The custom logger's collection is `.ignore`d, so logging never kills a fiber. Depend on the
        // LogCollector service explicitly — the layer installs that logger, so it's a real dependency
        // here (not an unused, over-provided layer), even though this test only asserts logging is safe.
        (for
          _ <- ZIO.service[LogCollector]
          _ <- ZIO.logAnnotate("scenarioId", "x") {
                 ZIO.logAnnotate("stepId", "y") {
                   ZIO.logInfo("safe log")
                 }
               }
        yield assertCompletes).provide(LogCollector.live(debugConfig))
      }
    )
  )
}
