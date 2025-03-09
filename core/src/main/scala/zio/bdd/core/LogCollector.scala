package zio.bdd.core

import zio.{ZLogger, *}
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
 * Defines the interface for collecting and managing logs during step execution
 * in a ZIO-based BDD framework. It separates logs into stdout and stderr
 * streams, allowing reporters (e.g., JUnit XML) to process them distinctly.
 */
trait LogCollector {
  def logStdout(message: String): ZIO[Any, Nothing, Unit]
  def logStderr(message: String): ZIO[Any, Nothing, Unit]
  def getLogs: ZIO[Any, Nothing, CollectedLogs]
  def clearLogs: ZIO[Any, Nothing, Unit]
}

/**
 * Provides a mechanism to collect logs during step execution, separating stdout
 * and stderr streams, and integrates with ZIO's logging system via a custom
 * logger added to the runtime.
 */
object LogCollector {

  /**
   * Implements the LogCollector service to store CollectedLogs. ZLayer.scoped
   * is used because Ref/FiberRef.make requires a Scope to manage its lifecycle,
   * ensuring proper cleanup.
   */
  val collectorImpl: ZLayer[Any, Nothing, LogCollector] = ZLayer.scoped {
    for {
      ref <- Ref.make(CollectedLogs()) // Replace with FiberRef for fiber-specific logs
    } yield new LogCollector {
      def logStdout(message: String): ZIO[Any, Nothing, Unit] =
        Clock.instant.flatMap(now => ref.update(_.add(LogEntry(message, now, LogSource.Stdout))))

      def logStderr(message: String): ZIO[Any, Nothing, Unit] =
        Clock.instant.flatMap(now => ref.update(_.add(LogEntry(message, now, LogSource.Stderr))))

      def getLogs: ZIO[Any, Nothing, CollectedLogs] = ref.get

      def clearLogs: ZIO[Any, Nothing, Unit] = ref.set(CollectedLogs())
    }
  }

  /**
   * Creates a custom ZLogger that formats log messages using LogFormat.default
   * and routes them to the LogCollector based on log level: Error/Fatal to
   * stderr, others to stdout. This logger captures all ZIO log calls (e.g.,
   * ZIO.logInfo, ZIO.logError) and directs them to the collector.
   */
  def customLogger(collector: LogCollector): ZLogger[String, Unit] = {
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
              case LogLevel.Error | LogLevel.Fatal => collector.logStderr(formattedMessage)
              case _                               => collector.logStdout(formattedMessage)
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
  val loggingLayer: ZLayer[LogCollector, Nothing, Unit] = ZLayer.scoped {
    for {
      collector <- ZIO.service[LogCollector]

      _ <-
        FiberRef.currentLoggers.locallyScoped(
          Runtime.defaultLoggers -- Runtime.defaultLoggers ++ Chunk(customLogger(collector))
        ) // Replace with Runtime.addLogger(customLogger(collector)) to add the custom logger instead of replacing all
    } yield ()
  }

  val live: ZLayer[Any, Nothing, LogCollector] =
    collectorImpl >>> (collectorImpl ++ loggingLayer) >>> ZLayer.service[LogCollector]

  def logStdout(message: String): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.logStdout(message))

  def logStderr(message: String): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.logStderr(message))

  def getLogs: ZIO[LogCollector, Nothing, CollectedLogs] =
    ZIO.serviceWithZIO[LogCollector](_.getLogs)

  def clearLogs: ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.clearLogs)
}
