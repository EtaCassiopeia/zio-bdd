package zio.bdd.core

import zio.{Runtime, ZLogger, *}
import zio.logging.LogFormat

import java.time.Instant

case class LogEntry(message: String, timestamp: Instant, source: LogSource)

enum LogSource {
  case Stdout
  case Stderr
}

case class CollectedLogs(stdout: List[LogEntry] = Nil, stderr: List[LogEntry] = Nil) {
  def add(entry: LogEntry): CollectedLogs = entry.source match {
    case LogSource.Stdout => copy(stdout = entry :: stdout)
    case LogSource.Stderr => copy(stderr = entry :: stderr)
  }

  def toStepResultLogs: List[(String, Instant)] =
    (stdout ++ stderr).map(entry => (entry.message, entry.timestamp))

  def clear: CollectedLogs = CollectedLogs()
}

/**
 * Defines the interface for collecting and managing logs during step execution.
 * It separates logs into stdout and stderr streams, allowing reporters (e.g.,
 * JUnit XML) to process them distinctly.
 */
trait LogCollector {
  def logStdout(scenarioId: String, stepId: String, message: String): ZIO[Any, Nothing, Unit]
  def logStderr(scenarioId: String, stepId: String, message: String): ZIO[Any, Nothing, Unit]
  def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs]
  def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs]
  def getAllLogs: ZIO[Any, Nothing, Map[(String, String), CollectedLogs]]
  def clearLogs: ZIO[Any, Nothing, Unit]
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
      ref <- Ref.make(Map.empty[(String, String), CollectedLogs])
    } yield new LogCollector {
      def logStdout(scenarioId: String, stepId: String, message: String): ZIO[Any, Nothing, Unit] =
        Clock.instant.flatMap(now =>
          ref.update(
            _.updatedWith((scenarioId, stepId))(
              _.map(_.add(LogEntry(message, now, LogSource.Stdout)))
                .orElse(Some(CollectedLogs(stdout = List(LogEntry(message, now, LogSource.Stdout)))))
            )
          )
        )

      def logStderr(scenarioId: String, stepId: String, message: String): ZIO[Any, Nothing, Unit] =
        Clock.instant.flatMap(now =>
          ref.update(
            _.updatedWith((scenarioId, stepId))(
              _.map(_.add(LogEntry(message, now, LogSource.Stderr)))
                .orElse(Some(CollectedLogs(stderr = List(LogEntry(message, now, LogSource.Stderr)))))
            )
          )
        )

      def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs] =
        ref.get.map(_.getOrElse((scenarioId, stepId), CollectedLogs()))

      def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs] =
        ref.get.map(_.foldLeft(CollectedLogs()) { case (acc, ((sid, _), logs)) =>
          if (sid == scenarioId) acc.copy(stdout = logs.stdout ++ acc.stdout, stderr = logs.stderr ++ acc.stderr)
          else acc
        })

      def getAllLogs: ZIO[Any, Nothing, Map[(String, String), CollectedLogs]] =
        ref.get

      def clearLogs: ZIO[Any, Nothing, Unit] =
        ref.set(Map.empty)
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
      val formattedMessage = formatLogger(
        trace,
        fiberId,
        logLevel,
        message,
        cause,
        context,
        spans,
        annotations
      )
      Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe
          .run(
            logLevel match {
              case LogLevel.Error | LogLevel.Fatal =>
                val scenarioId = annotations.getOrElse("scenarioId", "default")
                val stepId     = annotations.getOrElse("stepId", "unknown")
                collector.logStderr(scenarioId, stepId, formattedMessage)
              case _ =>
                val scenarioId = annotations.getOrElse("scenarioId", "default")
                val stepId     = annotations.getOrElse("stepId", "unknown")
                collector.logStdout(scenarioId, stepId, formattedMessage)
            }
          )
          .getOrThrowFiberFailure()
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

  def logStdout(scenarioId: String, stepId: String, message: String): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.logStdout(scenarioId, stepId, message))

  def logStderr(scenarioId: String, stepId: String, message: String): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.logStderr(scenarioId, stepId, message))

  def getLogs(scenarioId: String, stepId: String): ZIO[LogCollector, Nothing, CollectedLogs] =
    ZIO.serviceWithZIO[LogCollector](_.getLogs(scenarioId, stepId))

  def getScenarioLogs(scenarioId: String): ZIO[LogCollector, Nothing, CollectedLogs] =
    ZIO.serviceWithZIO[LogCollector](_.getScenarioLogs(scenarioId))

  def getAllLogs: ZIO[LogCollector, Nothing, Map[(String, String), CollectedLogs]] =
    ZIO.serviceWithZIO[LogCollector](_.getAllLogs)

  def clearLogs: ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.clearLogs)
}
