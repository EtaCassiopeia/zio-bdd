package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.MockError

/**
 * The ZIO-facing wrapper over [[RiftFfiBridge]] — the Panama FFM bindings to
 * `librift_ffi`. Every downcall is a blocking call into the engine's own Tokio
 * threads, so each is wrapped in `ZIO.attemptBlocking`; the crate's sentinels
 * (port `0`, rc `-1`, null) become typed [[MockError]]s, and a genuine FFM
 * failure surfaces as a [[MockError.CommunicationError]].
 */
private[embedded] final class EmbeddedEngine(bridge: RiftFfiBridge):

  /**
   * Create an imposter from a full imposter JSON config; yields its bound port.
   */
  def createImposter(configJson: String): IO[MockError, Int] =
    blocking("rift_create_imposter")(bridge.createImposter(configJson)).flatMap { port =>
      if port == 0 then
        ZIO.fail(MockError.ProvisionFailed("rift_create_imposter returned 0 (bind failure or malformed config)"))
      else ZIO.succeed(port)
    }

  /** Replace all stubs on `port` from a JSON array of Mountebank stubs. */
  def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit] =
    blocking("rift_replace_stubs")(bridge.replaceStubs(port, stubsJson)).flatMap { rc =>
      if rc == 0 then ZIO.unit
      else ZIO.fail(MockError.CommunicationError(s"rift_replace_stubs returned $rc for port $port"))
    }

  /**
   * The recorded requests for `port` as a JSON array string (`[]` when none).
   */
  def recorded(port: Int): IO[MockError, String] =
    blocking("rift_recorded")(bridge.recorded(port)).flatMap { json =>
      if json == null then ZIO.fail(MockError.CommunicationError(s"rift_recorded returned null for port $port"))
      else ZIO.succeed(json)
    }

  private def blocking[A](op: String)(thunk: => A): IO[MockError, A] =
    ZIO
      .attemptBlocking(thunk)
      .mapError { t =>
        val msg = Option(t.getMessage).filter(_.nonEmpty).fold("")(m => s": $m")
        MockError.CommunicationError(s"$op via FFM failed (${t.getClass.getSimpleName}$msg)")
      }

private[embedded] object RiftFfi:

  /**
   * Start the embedded engine from the native library at `libPath`, scoped:
   * `rift_start` on acquire, `rift_stop` (and native-library release) on the
   * scope's close.
   */
  def start(libPath: String): ZIO[Scope, MockError, EmbeddedEngine] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking(RiftFfiBridge.start(libPath))
          .mapError(t => MockError.ProvisionFailed(s"loading librift_ffi from '$libPath': ${message(t)}"))
      )(bridge => ZIO.attemptBlocking(bridge.close()).orDie)
      .map(EmbeddedEngine(_))

  private def message(t: Throwable): String =
    Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
