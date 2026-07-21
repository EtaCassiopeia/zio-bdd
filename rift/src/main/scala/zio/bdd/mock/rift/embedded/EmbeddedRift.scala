package zio.bdd.mock.rift.embedded

import java.nio.file.Path

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning, SharedLayer}
import zio.bdd.mock.rift.{InterceptSettings, RiftMode, RiftModelMapping, Rift as RiftAdapter}

import rift.zio.Rift as SdkRift
import io.github.achirdlabs.rift.Rift as JRift

/**
 * Entry point for the embedded (no-Docker, in-process) Rift provider
 * (#133/#193, re-based onto the rift-scala SDK for #285): drives the engine
 * through `rift.zio.Rift.embedded`, which resolves `rift-java-embedded` + a
 * matching `rift-java-natives` classifier jar via `ServiceLoader` and starts
 * the engine over the stable Panama FFM C-ABI (JDK 22+) — a pure backend swap
 * for the container adapter, behind the same portable [[MockControl]]
 * interface.
 *
 * The embedded engine + natives are RUNTIME `ServiceLoader` dependencies, not
 * compile deps of this module (see `build.sbt`'s `rift` project:
 * `rift-java-embedded`/`rift-java-natives` are added at `Test` scope so the
 * suites here run; a consumer wiring `EmbeddedRift.layer` into an application
 * adds them itself). [[available]] reports whether they resolve for the host,
 * so a harness can park the backend (rather than fail) on an unsupported
 * platform; [[requireAvailable]] is the fail-loud form for a suite that must
 * not silently skip.
 */
object EmbeddedRift:

  /**
   * The outcome of probing `JRift.isEmbeddedAvailable()`: either it answered
   * (`Answered`), or a JDK-mismatched provider class is present but unloadable
   * on this JVM (`UnsupportedClassVersionError`, expected —
   * [[Probe.JdkMismatch]]), or loading it threw some OTHER `LinkageError`
   * (`UnsatisfiedLinkError` from a wrong-ABI/corrupt native,
   * `NoClassDefFoundError`, `ExceptionInInitializerError`, ...) — a genuine
   * regression, not a benign "unsupported JDK" — recorded as
   * [[Probe.Regressed]] so [[requireAvailable]] can surface WHY rather than
   * this reading as an unexplained green skip.
   */
  private enum Probe:
    case Answered(available: Boolean)
    case JdkMismatch
    case Regressed(cause: LinkageError)

  private def probe(): Probe =
    try Probe.Answered(JRift.isEmbeddedAvailable())
    catch
      case _: UnsupportedClassVersionError => Probe.JdkMismatch
      case e: LinkageError                 => Probe.Regressed(e)

  /**
   * True iff an embedded engine provider (`rift-java-embedded` + natives)
   * resolves for the host. `rift-java-embedded` is JDK-22 (stable FFM,
   * class-file 66) bytecode: on an older running JVM, `ServiceLoader` EAGERLY
   * loads the provider class while iterating to find it, so a
   * present-but-unloadable provider throws `UnsupportedClassVersionError` (a
   * `LinkageError`) rather than the graceful "not found" the caller expects.
   * `available` stays a total, exception-free probe on every JDK (not just 22+)
   * for BOTH the expected `JdkMismatch` case and a genuine `Regressed` one — it
   * is a pure `Boolean` a caller uses to legitimately SKIP, never the place to
   * surface a regression's cause; see [[requireAvailable]] for that.
   */
  def available: Boolean = probe() match
    case Probe.Answered(a)  => a
    case Probe.JdkMismatch  => false
    case Probe.Regressed(_) => false

  /**
   * Fail loudly — rather than SKIP — when no embedded engine resolves for the
   * host, for a suite that REQUIRES it (e.g. a hermetic intercept CI run where
   * a missing/misconfigured natives jar must not read as a green skip).
   * [[available]] is the soft boolean form for a suite that legitimately wants
   * to SKIP on an unsupported platform. On a [[Probe.Regressed]] outcome the
   * failure names the underlying `LinkageError` (class + message) so a real
   * native regression is traceable instead of looking identical to a merely
   * unsupported JDK.
   */
  def requireAvailable: IO[MockError, Unit] =
    val osName = java.lang.System.getProperty("os.name", "unknown")
    val osArch = java.lang.System.getProperty("os.arch", "unknown")
    probe() match
      case Probe.Answered(true) => ZIO.unit
      case Probe.Answered(false) | Probe.JdkMismatch =>
        ZIO.fail(
          MockError.ProvisionFailed(
            s"no embedded Rift engine available for $osName-$osArch: add rift-java-embedded and a " +
              "matching rift-java-natives classifier jar to the classpath, or set -Drift.ffi.lib to an " +
              "explicit native library path"
          )
        )
      case Probe.Regressed(cause) =>
        ZIO.fail(
          MockError.ProvisionFailed(
            s"embedded Rift engine failed to load for $osName-$osArch — this looks like a genuine " +
              s"native/classpath regression, not merely an unsupported JDK: ${cause.getClass.getName}: " +
              s"${Option(cause.getMessage).getOrElse("(no message)")}"
          )
        )

  /**
   * Bind configuration for the built-in [[zio.bdd.mock.Intercept]] TLS-MITM
   * proxy. Defaults preserve loopback bind + OS-assigned port. Set `bindHost =
   * "0.0.0.0"` (or a specific NIC address) so a SUT running elsewhere (e.g. a
   * Docker container reaching the host engine via the host-gateway) can reach
   * the proxy — a loopback socket refuses gateway-originated connections. Pin
   * `port` when the SUT's proxy target must be known *before* the test assigns
   * it. `caCert`/`caKey` (PEM file paths, **both or neither**) make the proxy
   * load a persistent, caller-provided intercept CA instead of minting a fresh
   * ephemeral one each start; absent -> ephemeral CA.
   */
  final case class InterceptConfig(
    bindHost: String = "127.0.0.1",
    port: Option[Int] = None,
    caCert: Option[Path] = None,
    caKey: Option[Path] = None
  ):
    private[embedded] def toSettings: InterceptSettings =
      InterceptSettings(bindHost, port.getOrElse(0), caCert, caKey)

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, PerInstance
   * isolation.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] = layer(RiftMode.PerInstance, InterceptConfig())

  /**
   * As [[layer]], with the intercept proxy bound per `intercept` (bind host /
   * fixed port / CA).
   */
  def layer(intercept: InterceptConfig): ZLayer[Provisioning, MockError, MockControl] =
    layer(RiftMode.PerInstance, intercept)

  /** As the `mode` overload, with the intercept proxy bound per `intercept`. */
  def layer(mode: RiftMode): ZLayer[Provisioning, MockError, MockControl] = layer(mode, InterceptConfig())

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, in the given
   * isolation `mode`: the engine starts on layer construction and stops when
   * the scope closes. Fails with [[MockError.ProvisionFailed]] when no embedded
   * engine resolves for the host.
   */
  def layer(mode: RiftMode, intercept: InterceptConfig): ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.makeSome[Provisioning, MockControl](
      SdkRift.embedded.mapError(RiftModelMapping.toMockError(None)),
      // Embedded always can start an intercept listener in-process, on the host — unlike
      // container/connect there's no netns/unconfigured-endpoint gap (#285/B5).
      RiftAdapter.adapterLayer(mode, None, intercept.toSettings, interceptCapable = true)
    )

  /**
   * A **process-wide shared** embedded [[MockControl]] (PerInstance): the
   * engine is started at most once and every `@Suite` that uses `shared` gets
   * the same instance, released on JVM shutdown. This is the supported way to
   * share one expensive embedded engine across suite classes (#309).
   *
   * Prefer this over a hand-rolled static `val` that runs
   * `Runtime.default.unsafe.run(layer.build)` at class-load: under sbt's
   * default `Test / parallelExecution := true`, that pattern runs a suite's
   * `<clinit>` on a shared-runtime worker thread while a sibling suite blocks
   * on the class-init lock, which deadlocks and surfaces as a cause-less
   * `Interrupted`. `shared` memoizes the build with a semaphore instead (see
   * [[SharedLayer.memoize]]), so concurrent suites are safe.
   */
  val shared: ZLayer[Provisioning, MockError, MockControl] = SharedLayer.memoize(layer)
