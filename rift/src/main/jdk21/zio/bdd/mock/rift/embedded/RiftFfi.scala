package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.MockError
import zio.json.*

/**
 * The ZIO-facing surface over the Rift engine's C-ABI v2 (rift#343,
 * `librift_ffi` ≥ v0.9.0): the data plane (create an imposter, replace all of
 * its stubs, read its recorded requests) plus the in-process admin plane
 * ([[serveAdmin]]), per-imposter delete ([[deleteImposter]]), and build
 * identity ([[buildInfo]]). A pre-v2 library fails fast at load
 * ([[RiftFfiBridge.start]]), so this surface always assumes v2.
 *
 * A trait (not just the live wrapper) so the adapter can be unit-tested against
 * a recording double without the native library.
 */
private[embedded] trait EmbeddedEngine:

  /**
   * Create an imposter from a full imposter JSON config; yields its bound port.
   */
  def createImposter(configJson: String): IO[MockError, Int]

  /** Replace all stubs on `port` from a JSON array of Mountebank stubs. */
  def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit]

  /**
   * The recorded requests for `port` as a JSON array string (`[]` when none).
   */
  def recorded(port: Int): IO[MockError, String]

  /**
   * Start the real admin API in-process on this engine's runtime, over this
   * engine's manager (so FFI-created imposters are visible to it), and yield
   * the loopback admin URL.
   */
  def serveAdmin(optionsJson: String): IO[MockError, EmbeddedEngine.AdminInfo]

  /** Delete one imposter, freeing its bound port. */
  def deleteImposter(port: Int): IO[MockError, Unit]

  /** The engine's build identity (version/commit/features). */
  def buildInfo: IO[MockError, EmbeddedEngine.BuildInfo]

private[embedded] object EmbeddedEngine:

  /**
   * The result of [[EmbeddedEngine.serveAdmin]] — where the in-process admin
   * API is reachable.
   */
  final case class AdminInfo(adminUrl: String, adminPort: Int)
  object AdminInfo:
    given JsonDecoder[AdminInfo] = DeriveJsonDecoder.gen[AdminInfo]

  /**
   * The engine's build identity from `rift_build_info`. `commit`/`builtAt` may
   * be absent.
   */
  final case class BuildInfo(
    version: String,
    commit: Option[String] = None,
    builtAt: Option[String] = None,
    features: List[String] = Nil
  )
  object BuildInfo:
    given JsonDecoder[BuildInfo] = DeriveJsonDecoder.gen[BuildInfo]

  /**
   * The live engine over [[RiftFfiBridge]] — the Panama FFM bindings to
   * `librift_ffi`. Every downcall is a blocking call into the engine's own
   * Tokio threads, so each is wrapped in `ZIO.attemptBlocking`; the crate's
   * sentinels (port `0`, rc `-1`, null) become typed [[MockError]]s, and a
   * genuine FFM failure surfaces as a [[MockError.CommunicationError]].
   */
  final class Live(bridge: RiftFfiBridge) extends EmbeddedEngine:

    // The data-plane calls return sentinels (port 0, rc -1, null) rather than throwing, so the
    // engine's reason (rift_last_error) is read on the SAME blocking thread — inside the thunk —
    // and folded into the message (the crate confines last_error to the failing thread). A success
    // is Right; a sentinel is Left(message-with-reason), turned into the typed MockError below.

    def createImposter(configJson: String): IO[MockError, Int] =
      blocking("rift_create_imposter") {
        val port = bridge.createImposter(configJson)
        if port == 0 then Left(withReason("rift_create_imposter returned 0 (bind failure or malformed config)"))
        else Right(port)
      }.flatMap {
        case Right(port) => ZIO.succeed(port)
        case Left(msg)   => ZIO.fail(MockError.ProvisionFailed(msg))
      }

    def replaceStubs(port: Int, stubsJson: String): IO[MockError, Unit] =
      blocking("rift_replace_stubs") {
        val rc = bridge.replaceStubs(port, stubsJson)
        if rc == 0 then None else Some(withReason(s"rift_replace_stubs returned $rc for port $port"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def recorded(port: Int): IO[MockError, String] =
      blocking("rift_recorded") {
        val json = bridge.recorded(port)
        if json == null then Left(withReason(s"rift_recorded returned null for port $port")) else Right(json)
      }.flatMap {
        case Right(json) => ZIO.succeed(json)
        case Left(msg)   => ZIO.fail(MockError.CommunicationError(msg))
      }

    // Append the engine's thread-local reason to a sentinel message. MUST be called on the same
    // thread as the failing downcall (inside the `blocking` thunk) — see RiftFfiBridge.lastError.
    private def withReason(base: String): String =
      Option(bridge.lastError()).filter(_.nonEmpty).fold(base)(r => s"$base: $r")

    def serveAdmin(optionsJson: String): IO[MockError, AdminInfo] =
      blocking("rift_serve_admin")(bridge.serveAdmin(optionsJson)).flatMap { json =>
        ZIO
          .fromEither(json.fromJson[AdminInfo])
          .mapError(e => MockError.CommunicationError(s"rift_serve_admin returned unparseable admin info: $e ($json)"))
      }

    def deleteImposter(port: Int): IO[MockError, Unit] =
      blocking("rift_delete_imposter") {
        val rc = bridge.deleteImposter(port)
        if rc == 0 then None else Some(withReason(s"rift_delete_imposter returned $rc for port $port"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def buildInfo: IO[MockError, BuildInfo] =
      blocking("rift_build_info")(bridge.buildInfo).flatMap { json =>
        if json == null then ZIO.fail(MockError.CommunicationError("rift_build_info returned null"))
        else
          ZIO
            .fromEither(json.fromJson[BuildInfo])
            .mapError(e => MockError.CommunicationError(s"rift_build_info returned unparseable build info: $e ($json)"))
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
      .map(EmbeddedEngine.Live(_))

  private def message(t: Throwable): String =
    Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
