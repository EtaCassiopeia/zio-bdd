package zio.bdd.core

import zio.{Runtime, ZLogger, *}
import zio.logging.LogFormat

import java.time.Instant

enum InternalLogLevel {
  case Debug, Info, Warning, Error, Fatal
}

enum LogSource {
  case Stdout
  case Stderr
}

case class LogEntry(
  message: String,
  timestamp: Instant,
  source: LogSource,
  level: InternalLogLevel,
  stepId: String
)

case class CollectedLogs(entries: List[LogEntry] = Nil) {
  def add(entry: LogEntry): CollectedLogs = copy(entries = entries :+ entry)
  def toStepResultLogs: List[(String, Instant, InternalLogLevel)] =
    entries.map(entry => (entry.message, entry.timestamp, entry.level))
}

case class LogLevelConfig(minLevel: InternalLogLevel = InternalLogLevel.Info)

/**
 * Defines the interface for collecting and managing logs during step execution.
 * It separates logs into stdout and stderr streams, allowing reporters (e.g.,
 * JUnit XML) to process them distinctly.
 */
trait LogCollector {
  def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit]
  def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs]
  def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs]
}

/**
 * Provides a mechanism to collect logs during step execution, separating stdout
 * and stderr streams, and integrates with ZIO's logging system via a custom
 * logger added to the runtime.
 */
object LogCollector {

  private class LogCollectorImpl(config: LogLevelConfig, ref: Ref[Map[(String, String), CollectedLogs]])
      extends LogCollector {
    private def logSourceForLevel(level: InternalLogLevel): LogSource =
      level match {
        case InternalLogLevel.Error | InternalLogLevel.Fatal => LogSource.Stderr
        case _                                               => LogSource.Stdout
      }

    private def createLogEntry(
      message: String,
      level: InternalLogLevel,
      stepId: String
    ): ZIO[Any, Nothing, LogEntry] =
      Clock.instant.map { now =>
        LogEntry(message, now, logSourceForLevel(level), level, stepId)
      }

    def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit] =
      if (level.ordinal >= config.minLevel.ordinal) {
        for {
          entry <- createLogEntry(message, level, stepId)
          _ <- ref.update(_.updatedWith((scenarioId, stepId)) {
                 case Some(logs) => Some(logs.add(entry))
                 case None       => Some(CollectedLogs().add(entry))
               })
        } yield ()
      } else {
        ZIO.unit
      }

    def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs] =
      ref.get.map(_.getOrElse((scenarioId, stepId), CollectedLogs()))

    def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs] =
      ref.get.map { map =>
        map.foldLeft(CollectedLogs()) { case (acc, ((sid, _), logs)) =>
          if (sid == scenarioId) acc.copy(entries = logs.entries ++ acc.entries) else acc
        }
      }
  }

  private def customLogger(collector: LogCollector): ZLogger[String, Unit] = {
    val formatLogger = LogFormat.default.toLogger
    (
      trace: Trace,
      fiberId: FiberId,
      logLevel: LogLevel,
      message: () => String,
      cause: Cause[Any],
      context: FiberRefs,
      spans: List[LogSpan],
      annotations: Map[String, String]
    ) => {
      annotations.get("stepId") match {
        case Some(stepId) =>
          val formattedMessage = formatLogger(trace, fiberId, logLevel, message, cause, context, spans, annotations)
          Unsafe.unsafe { implicit u =>
            Runtime.default.unsafe.run {
              val scenarioId = annotations.getOrElse("scenarioId", "default")
              val level = logLevel match {
                case LogLevel.Debug   => InternalLogLevel.Debug
                case LogLevel.Info    => InternalLogLevel.Info
                case LogLevel.Warning => InternalLogLevel.Warning
                case LogLevel.Error   => InternalLogLevel.Error
                case LogLevel.Fatal   => InternalLogLevel.Fatal
              }
              collector.log(scenarioId, stepId, formattedMessage, level)
            }.getOrThrowFiberFailure()
          }
        case None =>
          () // Do nothing
      }
    }
  }

  def live(config: LogLevelConfig): ZLayer[Any, Nothing, LogCollector] =
    ZLayer.scoped {
      for {
        ref      <- Ref.make(Map.empty[(String, String), CollectedLogs])
        collector = new LogCollectorImpl(config, ref)
        _        <- FiberRef.currentLoggers.locallyScoped(Set(customLogger(collector)))
        // If the intent is to preserve default loggers, replace it with:
        // _ <- FiberRef.currentLoggers.locallyScoped(Runtime.defaultLoggers + customLogger(collector))
      } yield collector
    }

  def log(
    scenarioId: String,
    stepId: String,
    message: String,
    level: InternalLogLevel
  ): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.log(scenarioId, stepId, message, level))
}
