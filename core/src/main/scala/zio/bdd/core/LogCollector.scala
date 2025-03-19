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
  def add(entry: LogEntry): CollectedLogs = copy(entries = entry :: entries)
  def toStepResultLogs: List[(String, Instant, InternalLogLevel)] =
    entries.map(entry => (entry.message, entry.timestamp, entry.level))
  def clear: CollectedLogs = CollectedLogs()
}

case class LogLevelConfig(minLevel: InternalLogLevel = InternalLogLevel.Info)

/**
 * Defines the interface for collecting and managing logs during step execution.
 * It separates logs into stdout and stderr streams, allowing reporters (e.g.,
 * JUnit XML) to process them distinctly.
 */
trait LogCollector {
  def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit]
  def logFeature(featureId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit]
  def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs]
  def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs]
  def getFeatureLogs(featureId: String): ZIO[Any, Nothing, CollectedLogs]
  def getAllLogs: ZIO[Any, Nothing, Map[(String, String), CollectedLogs]]
  def clearLogs: ZIO[Any, Nothing, Unit]
  def setLogLevelConfig(config: LogLevelConfig): ZIO[Any, Nothing, Unit]
}

/**
 * Provides a mechanism to collect logs during step execution, separating stdout
 * and stderr streams, and integrates with ZIO's logging system via a custom
 * logger added to the runtime.
 */
object LogCollector {

  /**
   * Implements the LogCollector service to store CollectedLogs.
   */
  private val collectorImpl: ZLayer[Any, Nothing, LogCollector] = ZLayer.scoped {
    for {
      ref         <- Ref.make(Map.empty[(String, String), CollectedLogs])
      featureRef  <- Ref.make(Map.empty[String, CollectedLogs])
      logLevelRef <- Ref.make(LogLevelConfig())
    } yield new LogCollector {
      def setLogLevelConfig(config: LogLevelConfig): ZIO[Any, Nothing, Unit] =
        logLevelRef.set(config)

      // method to determine LogSource based on level
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

      // method to update logs if level is sufficient
      private def updateLogsIfLevelSufficient[K](
        ref: Ref[Map[K, CollectedLogs]],
        key: K,
        entry: LogEntry,
        config: LogLevelConfig
      ): ZIO[Any, Nothing, Unit] =
        if (entry.level.ordinal >= config.minLevel.ordinal) {
          ref.update(_.updatedWith(key)(_.map(_.add(entry)).orElse(Some(CollectedLogs().add(entry)))))
        } else {
          ZIO.unit
        }

      def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit] =
        for {
          config <- logLevelRef.get
          entry  <- createLogEntry(message, level, stepId)
          _      <- updateLogsIfLevelSufficient(ref, (scenarioId, stepId), entry, config)
        } yield ()

      def logFeature(featureId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit] =
        for {
          config <- logLevelRef.get
          entry  <- createLogEntry(message, level, "feature")
          _      <- updateLogsIfLevelSufficient(featureRef, featureId, entry, config)
        } yield ()

      // Remaining methods unchanged
      def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs] =
        ref.get.map(_.getOrElse((scenarioId, stepId), CollectedLogs()))

      def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs] =
        ref.get.map(_.foldLeft(CollectedLogs()) { case (acc, ((sid, _), logs)) =>
          if (sid == scenarioId) acc.copy(entries = logs.entries ++ acc.entries) else acc
        })

      def getFeatureLogs(featureId: String): ZIO[Any, Nothing, CollectedLogs] =
        featureRef.get.map(_.getOrElse(featureId, CollectedLogs()))

      def getAllLogs: ZIO[Any, Nothing, Map[(String, String), CollectedLogs]] =
        ref.get

      def clearLogs: ZIO[Any, Nothing, Unit] =
        ref.set(Map.empty) *> featureRef.set(Map.empty)
    }
  }

  /**
   * Creates a custom ZLogger that formats log messages using LogFormat.default
   * and routes them to the LogCollector based on log level: Error/Fatal to
   * stderr, others to stdout. This logger captures all ZIO log calls (e.g.,
   * ZIO.logInfo, ZIO.logError) and directs them to the collector.
   */
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
      val formattedMessage = formatLogger(trace, fiberId, logLevel, message, cause, context, spans, annotations)
      Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run {
          val scenarioId = annotations.getOrElse("scenarioId", "default")
          val stepId     = annotations.getOrElse("stepId", "unknown")
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
    }
  }

  /**
   * A ZLayer that configures the ZIO Runtime to use only the custom logger by
   * removing default loggers and adding the custom one. This ensures all ZIO
   * log calls (e.g., ZIO.logInfo, ZIO.logError) are captured by LogCollector,
   * replacing any console or SLF4J loggers.
   */
  private val loggingLayer: ZLayer[LogCollector, Nothing, Unit] = ZLayer.scoped {
    for {
      collector <- ZIO.service[LogCollector]
      _ <-
        FiberRef.currentLoggers.locallyScoped(
          Runtime.defaultLoggers -- Runtime.defaultLoggers ++ Chunk(customLogger(collector))
        ) // Use `Runtime.addLogger(customLogger(collector))` to add the custom logger without replacing all existing loggers
    } yield ()
  }

  val live: ZLayer[Any, Nothing, LogCollector] =
    collectorImpl >>> (collectorImpl ++ loggingLayer) >>> ZLayer.service[LogCollector]

  def log(
    scenarioId: String,
    stepId: String,
    message: String,
    level: InternalLogLevel
  ): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.log(scenarioId, stepId, message, level))

  def logFeature(featureId: String, message: String, level: InternalLogLevel): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.logFeature(featureId, message, level))
}
