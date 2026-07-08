package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.MockError
import zio.json.*

/**
 * The ZIO-facing surface over the Rift engine's C-ABI (`librift_ffi` ≥
 * v0.11.0): the data plane (create an imposter, replace all of its stubs, read
 * its recorded requests) plus the in-process admin plane ([[serveAdmin]]),
 * per-imposter delete ([[deleteImposter]]), build identity ([[buildInfo]]), and
 * — the admin long tail over direct C-ABI (rift#411) — scenario/flow state
 * ([[flowStateGet]]/[[flowStatePut]]) and the correlated space plane
 * ([[spaceAddStub]]/[[spaceDelete]]/[[spaceRecorded]]), so the adapter needs no
 * loopback HTTP. A pre-v0.11.0 library fails fast at load
 * ([[RiftFfiBridge.start]]) when the bridge binds those mandatory symbols.
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

  // The admin long tail over direct C-ABI (rift#411, librift_ffi ≥ v0.11.0): scenario/flow state +
  // correlated spaces, so the adapter drives them with no loopback HTTP admin plane.

  /**
   * The scenario/flow-state value for `(flowId, key)` as its string form
   * (scenario state is stored as a JSON string), or `None` if the key is
   * absent.
   */
  def flowStateGet(port: Int, flowId: String, key: String): IO[MockError, Option[String]]

  /**
   * Set the scenario/flow-state value for `(flowId, key)` from a bare JSON
   * value.
   */
  def flowStatePut(port: Int, flowId: String, key: String, valueJson: String): IO[MockError, Unit]

  /**
   * Register a stub scoped to `flowId` on the shared imposter (its `space` is
   * set from `flowId`).
   */
  def spaceAddStub(port: Int, flowId: String, stubJson: String): IO[MockError, Unit]

  /**
   * Tear down a space in one call (its scoped stubs, recorded requests, and
   * scenario state).
   */
  def spaceDelete(port: Int, flowId: String): IO[MockError, Unit]

  /**
   * The requests recorded under `flowId` (header-filtered by the space's
   * resolved flow id) as a JSON array string (`[]` when none).
   */
  def spaceRecorded(port: Int, flowId: String): IO[MockError, String]

  // Built-in HTTPS intercept over direct C-ABI (rift#410, #219): start the TLS-MITM forward-proxy,
  // add rules to its store, and export the CA truststore — no loopback HTTP.

  /**
   * Start the intercept/TLS-MITM forward-proxy from `optionsJson`
   * (`{"host","port"}`, or `{}`).
   */
  def startIntercept(optionsJson: String): IO[MockError, EmbeddedEngine.InterceptInfo]

  /**
   * Add one intercept rule (or a JSON array) — the shape rift's intercept rule
   * store accepts.
   */
  def interceptAddRules(rulesJson: String): IO[MockError, Unit]

  /**
   * Write a truststore (`format` = "pkcs12"/"jks") for the intercept CA to
   * `outPath`.
   */
  def interceptExportTruststore(format: String, password: String, outPath: String): IO[MockError, Unit]

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
   * The `value` field of a `rift_flow_state_get` result
   * (`{"flowId","key","value"}`), decoded as its string form — scenario state
   * (the sole reader) is stored as a JSON string. Extra fields ignored.
   */
  private[embedded] final case class FlowStateValue(value: String)
  private[embedded] object FlowStateValue:
    given JsonDecoder[FlowStateValue] = DeriveJsonDecoder.gen[FlowStateValue]

  /**
   * The bound endpoint of the intercept listener, from `rift_start_intercept`.
   */
  final case class InterceptInfo(interceptPort: Int, interceptUrl: String)
  object InterceptInfo:
    given JsonDecoder[InterceptInfo] = DeriveJsonDecoder.gen[InterceptInfo]

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

    // A null return is "key absent" (mapped to None) — the crate also returns null on a genuine
    // error, but the adapter only reads a scenario it defined on a live imposter, so absence is the
    // meaningful case (mirrors the HTTP path's `Option[String]`). rift's null conflates the two.
    def flowStateGet(port: Int, flowId: String, key: String): IO[MockError, Option[String]] =
      blocking("rift_flow_state_get")(Option(bridge.flowStateGet(port, flowId, key))).flatMap {
        case None => ZIO.none
        case Some(json) =>
          ZIO
            .fromEither(json.fromJson[FlowStateValue])
            .mapError(e => MockError.CommunicationError(s"rift_flow_state_get returned unparseable JSON: $e ($json)"))
            .map(v => Some(v.value))
      }

    def flowStatePut(port: Int, flowId: String, key: String, valueJson: String): IO[MockError, Unit] =
      blocking("rift_flow_state_put") {
        val rc = bridge.flowStatePut(port, flowId, key, valueJson)
        if rc == 0 then None else Some(withReason(s"rift_flow_state_put returned $rc for port $port"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def spaceAddStub(port: Int, flowId: String, stubJson: String): IO[MockError, Unit] =
      blocking("rift_space_add_stub") {
        val rc = bridge.spaceAddStub(port, flowId, stubJson)
        if rc == 0 then None else Some(withReason(s"rift_space_add_stub returned $rc for port $port"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def spaceDelete(port: Int, flowId: String): IO[MockError, Unit] =
      blocking("rift_space_delete") {
        val rc = bridge.spaceDelete(port, flowId)
        if rc == 0 then None else Some(withReason(s"rift_space_delete returned $rc for port $port"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def spaceRecorded(port: Int, flowId: String): IO[MockError, String] =
      blocking("rift_space_recorded") {
        val json = bridge.spaceRecorded(port, flowId)
        if json == null then Left(withReason(s"rift_space_recorded returned null for port $port")) else Right(json)
      }.flatMap {
        case Right(json) => ZIO.succeed(json)
        case Left(msg)   => ZIO.fail(MockError.CommunicationError(msg))
      }

    def startIntercept(optionsJson: String): IO[MockError, InterceptInfo] =
      blocking("rift_start_intercept")(bridge.startIntercept(optionsJson)).flatMap { json =>
        ZIO
          .fromEither(json.fromJson[InterceptInfo])
          .mapError(e => MockError.CommunicationError(s"rift_start_intercept returned unparseable info: $e ($json)"))
      }

    def interceptAddRules(rulesJson: String): IO[MockError, Unit] =
      blocking("rift_intercept_add_rules") {
        val rc = bridge.interceptAddRules(rulesJson)
        if rc == 0 then None else Some(withReason(s"rift_intercept_add_rules returned $rc"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
      }

    def interceptExportTruststore(format: String, password: String, outPath: String): IO[MockError, Unit] =
      blocking("rift_intercept_export_truststore") {
        val rc = bridge.interceptExportTruststore(format, password, outPath)
        if rc == 0 then None else Some(withReason(s"rift_intercept_export_truststore returned $rc"))
      }.flatMap {
        case None      => ZIO.unit
        case Some(msg) => ZIO.fail(MockError.CommunicationError(msg))
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
