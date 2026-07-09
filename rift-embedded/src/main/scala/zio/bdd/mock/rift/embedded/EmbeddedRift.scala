package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning, SharedLayer}
import zio.bdd.mock.rift.RiftMode

import java.nio.file.{Files, Path, StandardCopyOption}

/**
 * Entry point for `MockControl.embedded` (#133): the opt-in, no-Docker,
 * in-process Rift provider, driving the engine over `librift_ffi` via Panama
 * FFM behind the portable [[MockControl]] interface — a pure backend swap for
 * the container adapter.
 *
 * The native library loads out-of-the-box (#134): [[NativeLibrary]] resolves it
 * from an explicit `-Drift.ffi.lib` override or the per-platform cdylib bundled
 * in the `zio-bdd-rift-embedded-natives` jar; a bundled resource is extracted
 * to a temp file and loaded. [[available]] reports whether a library resolves
 * for the host, so a harness can park the backend (rather than fail) on an
 * unsupported platform.
 *
 * Uses Project Panama FFM, so a JDK with FFM is required
 * (`--enable-native-access` to silence the restricted-method warning).
 * Published as two variants from this one source: `zio-bdd-rift-embedded` for
 * **JDK 22+** (stable FFM, JEP 454) and `zio-bdd-rift-embedded-jdk21` for **JDK
 * 21** (preview FFM — also needs `--enable-preview`). Requires `librift_ffi` ≥
 * v0.11.0: the adapter drives Rift entirely over FFI, including the admin long
 * tail (rift#411) — a pre-v0.11.0 library fails fast at start with a
 * missing-symbol error when the bridge binds those mandatory symbols.
 */
object EmbeddedRift:

  /**
   * True iff a `librift_ffi` resolves for the host (a bundled native resource,
   * or an override path).
   */
  def available: Boolean = NativeLibrary.available

  /**
   * Fail loudly — rather than SKIP — when no `librift_ffi` resolves for the
   * host, for a suite that REQUIRES the embedded engine (e.g. a hermetic
   * intercept CI run where a missing/misconfigured natives jar must not read as
   * a green skip). Surfaces the same host-resolution error [[layer]] fails with
   * at construction — a [[MockError.ProvisionFailed]] naming the host `os-arch`
   * and how to fix it (add the `zio-bdd-rift-embedded-natives` dependency or
   * set `-Drift.ffi.lib`). A no-op when a native resolves. [[available]] is the
   * soft boolean form for a suite that legitimately wants to SKIP on an
   * unsupported platform (zio-bdd's own cross-platform matrix); use this when a
   * skip would hide a real gap.
   */
  def requireAvailable: IO[MockError, Unit] = requireResolved(NativeLibrary.resolveSource)

  // Testable core of `requireAvailable`: a Left host-resolution error becomes a loud failure; a Right is
  // a no-op. Split out so both branches are covered without depending on the host actually lacking a lib.
  private[embedded] def requireResolved(source: Either[MockError, LibSource]): IO[MockError, Unit] =
    ZIO.fromEither(source).unit

  /**
   * Bind configuration for the built-in [[Intercept]] TLS-MITM proxy. Defaults
   * preserve today's behavior: loopback bind, OS-assigned port. Set `bindHost =
   * "0.0.0.0"` (or a specific NIC address) so a SUT running elsewhere — e.g. a
   * Docker container reaching the host engine via the host-gateway — can reach
   * the proxy; a loopback socket refuses gateway-originated connections. Pin
   * `port` when the SUT's proxy target must be known *before* the test assigns
   * it (a compose-orchestrated SUT). Binding a wider interface is strictly
   * opt-in — nothing is exposed off loopback unless asked.
   *
   * `bindHost` must be an **IP literal** (e.g. `127.0.0.1`, `0.0.0.0`, a NIC
   * address) — the engine binds via a socket-address parse, so a hostname like
   * `localhost` is rejected with [[MockError.InvalidDefinition]] on first
   * intercept use (#262), not resolved.
   *
   * `caCert`/`caKey` (PEM file paths, **both or neither**) make the proxy load
   * a **persistent, caller-provided** intercept CA instead of minting a fresh
   * ephemeral one each start (#273, needs rift ≥ 0.11.3). Commit a rift CA once
   * and a long-lived containerized SUT can trust it at JVM startup — before the
   * test starts the proxy — so the ordering problem an ephemeral CA creates
   * disappears. Absent → ephemeral CA, exactly as before. Supplying only one is
   * rejected with [[MockError.InvalidDefinition]] on first intercept use.
   */
  final case class InterceptConfig(
    bindHost: String = "127.0.0.1",
    port: Option[Int] = None,
    caCert: Option[Path] = None,
    caKey: Option[Path] = None
  )

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, PerInstance
   * isolation (own port per space). See the `mode` overload below for
   * Correlated.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] = layer(RiftMode.PerInstance, InterceptConfig())

  /**
   * As [[layer]], with the intercept proxy bound per `intercept` (bind host /
   * fixed port).
   */
  def layer(intercept: InterceptConfig): ZLayer[Provisioning, MockError, MockControl] =
    layer(RiftMode.PerInstance, intercept)

  /** As the `mode` overload, with the intercept proxy bound per `intercept`. */
  def layer(mode: RiftMode): ZLayer[Provisioning, MockError, MockControl] = layer(mode, InterceptConfig())

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, in the given
   * isolation `mode` (#203 adds Correlated — mirroring the container adapter,
   * but the shared imposter is FFI-created/-destroyed rather than
   * HTTP-managed): `rift_start` on layer construction, `rift_stop` (and, in
   * Correlated mode, the shared imposter's teardown) on close. Entirely FFI
   * (#244) — no loopback HTTP admin plane, so the public requirement stays just
   * [[Provisioning]]. Fails with [[MockError.ProvisionFailed]] when no native
   * library resolves for the host, or it cannot be loaded. The `intercept`
   * config governs the built-in intercept proxy's bind host + port (#254).
   */
  def layer(mode: RiftMode, intercept: InterceptConfig): ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      for
        prov    <- ZIO.service[Provisioning]
        engine  <- startEngine
        control <- EmbeddedRiftMockControl.make(engine, prov, mode, intercept)
      yield control
    }

  /**
   * A **process-wide shared** embedded [[MockControl]] (PerInstance): the engine
   * is started at most once and every `@Suite` that uses `shared` gets the same
   * instance, released on JVM shutdown. This is the supported way to share one
   * expensive embedded engine across suite classes (#309).
   *
   * Prefer this over a hand-rolled static `val` that runs
   * `Runtime.default.unsafe.run(layer.build)` at class-load: under sbt's default
   * `Test / parallelExecution := true`, that pattern runs a suite's `<clinit>` on
   * a shared-runtime worker thread while a sibling suite blocks on the class-init
   * lock, which deadlocks and surfaces as a cause-less `Interrupted`. `shared`
   * memoizes the build with a semaphore instead (see [[SharedLayer.memoize]]), so
   * concurrent suites are safe without setting `parallelExecution := false`.
   */
  val shared: ZLayer[Provisioning, MockError, MockControl] = SharedLayer.memoize(layer)

  // Resolve the native library and start the engine, scoped (rift_start on acquire, rift_stop on
  // close). Shared by `layer` and the live FFI spec (which drives the engine's admin plane directly).
  private[embedded] def startEngine: ZIO[Scope, MockError, EmbeddedEngine] =
    for
      source <- ZIO.fromEither(NativeLibrary.resolveSource)
      path   <- loadablePath(source)
      engine <- RiftFfi.start(path.toString)
    yield engine

  // An override is already a real path; a bundled resource is extracted to a temp file (removed when
  // the JVM exits — the engine keeps the loaded library mapped, so the file is no longer needed).
  private def loadablePath(source: LibSource): IO[MockError, Path] =
    source match
      case LibSource.Override(p) => ZIO.succeed(p)
      case LibSource.Bundled(resource) =>
        ZIO.attemptBlocking {
          val in = getClass.getClassLoader.getResourceAsStream(resource)
          if in == null then throw new IllegalStateException(s"bundled native resource not found: $resource")
          try
            val suffix = resource.substring(resource.lastIndexOf('.'))
            val tmp    = Files.createTempFile("librift_ffi-", suffix)
            tmp.toFile.deleteOnExit()
            Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
            tmp
          finally in.close()
        }.mapError { t =>
          val msg = Option(t.getMessage).filter(_.nonEmpty).fold("")(m => s": $m")
          MockError.ProvisionFailed(s"extracting bundled native '$resource': ${t.getClass.getSimpleName}$msg")
        }
