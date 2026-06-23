package zio.bdd.core

import zio.{ZLogger, *}
import zio.logging.LogFormat

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

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
 */
trait LogCollector {
  def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit]
  def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs]
  def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs]
}

object LogCollector {

  /**
   * ConcurrentHashMap-backed implementation.
   *
   * Log entries are appended in the synchronous ZLogger callback without
   * re-entering the ZIO runtime. A `ConcurrentHashMap` with list-append via
   * `merge` gives us lock-free reads and fine-grained locking on writes keyed
   * by (scenarioId, stepId) — safe under parallel scenario execution.
   *
   * The `log` method is also exposed as a `ZIO` effect for test compatibility,
   * but the hot path (called from the ZLogger callback) is the direct
   * `logDirect` method which never touches the ZIO runtime.
   */
  private final class LogCollectorImpl(config: LogLevelConfig) extends LogCollector {

    // Key: "scenarioId::stepId", Value: accumulated CollectedLogs
    private val store = new ConcurrentHashMap[String, CollectedLogs]()

    private def key(scenarioId: String, stepId: String): String = s"$scenarioId::$stepId"

    private def logSourceForLevel(level: InternalLogLevel): LogSource =
      level match {
        case InternalLogLevel.Error | InternalLogLevel.Fatal => LogSource.Stderr
        case _                                               => LogSource.Stdout
      }

    /**
     * Called directly from the synchronous ZLogger callback — no ZIO runtime
     * involved.
     */
    def logDirect(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): Unit =
      if (level.ordinal >= config.minLevel.ordinal) {
        val entry = LogEntry(
          message = message,
          timestamp = java.time.Clock.systemUTC().instant(),
          source = logSourceForLevel(level),
          level = level,
          stepId = stepId
        )
        val k = key(scenarioId, stepId)
        store.merge(
          k,
          CollectedLogs(List(entry)),
          (existing, added) => existing.copy(entries = existing.entries ++ added.entries)
        )
        ()
      }

    /**
     * ZIO-effect wrapper for test compatibility and direct calls from step
     * bodies.
     */
    def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): UIO[Unit] =
      ZIO.succeed(logDirect(scenarioId, stepId, message, level))

    def getLogs(scenarioId: String, stepId: String): UIO[CollectedLogs] =
      ZIO.succeed(Option(store.get(key(scenarioId, stepId))).getOrElse(CollectedLogs()))

    def getScenarioLogs(scenarioId: String): UIO[CollectedLogs] =
      ZIO.succeed(
        store
          .entrySet()
          .asScala
          .filter(_.getKey.startsWith(s"$scenarioId::"))
          .foldLeft(CollectedLogs())((acc, e) => acc.copy(entries = acc.entries ++ e.getValue.entries))
      )
  }

  /**
   * Creates a ZLogger that captures log output into the collector.
   *
   * Calls `logDirect` on the `LogCollectorImpl` — a direct ConcurrentHashMap
   * write with no ZIO runtime re-entry. Eliminates the `Unsafe.unsafe` bridge
   * and the associated interpreter overhead on every log call.
   *
   * The runtime default logger is REPLACED (not augmented) so that ZIO.log*
   * calls inside step bodies are captured by the collector only and not also
   * echoed to stdout by the default logger.
   */
  private def customLogger(collector: LogCollectorImpl): ZLogger[String, Unit] = {
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
      annotations.get("stepId").foreach { stepId =>
        val formattedMessage = formatLogger(trace, fiberId, logLevel, message, cause, context, spans, annotations)
        val scenarioId       = annotations.getOrElse("scenarioId", "default")
        val level = logLevel match {
          case LogLevel.Debug   => InternalLogLevel.Debug
          case LogLevel.Info    => InternalLogLevel.Info
          case LogLevel.Warning => InternalLogLevel.Warning
          case LogLevel.Error   => InternalLogLevel.Error
          case LogLevel.Fatal   => InternalLogLevel.Fatal
          case _                => InternalLogLevel.Info
        }
        collector.logDirect(scenarioId, stepId, formattedMessage, level)
      }
    }
  }

  def live(config: LogLevelConfig = LogLevelConfig()): ZLayer[Any, Nothing, LogCollector] =
    ZLayer.scoped {
      val collector = new LogCollectorImpl(config)
      FiberRef.currentLoggers
        .locallyScoped(Set(customLogger(collector)))
        .as(collector)
    }

  def log(
    scenarioId: String,
    stepId: String,
    message: String,
    level: InternalLogLevel
  ): ZIO[LogCollector, Nothing, Unit] =
    ZIO.serviceWithZIO[LogCollector](_.log(scenarioId, stepId, message, level))
}
