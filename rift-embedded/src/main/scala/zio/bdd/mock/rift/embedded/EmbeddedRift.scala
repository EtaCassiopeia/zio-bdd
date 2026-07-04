package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning}
import zio.bdd.mock.rift.RiftMode
import zio.http.Client
import zio.json.ast.Json

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
 * 21** (preview FFM — also needs `--enable-preview`). Requires the C-ABI v2
 * (rift#343, `librift_ffi` ≥ v0.9.0) — a pre-v2 library fails fast at start
 * with a missing-symbol error.
 */
object EmbeddedRift:

  // The engine binds the in-process admin API on loopback with an OS-assigned port (rift#343).
  private val serveAdminOptions: String = Json.Obj("host" -> Json.Str("127.0.0.1"), "port" -> Json.Num(0)).toString

  /**
   * True iff a `librift_ffi` resolves for the host (a bundled native resource,
   * or an override path).
   */
  def available: Boolean = NativeLibrary.available

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, PerInstance
   * isolation (own port per space). See the `mode` overload below for
   * Correlated.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] = layer(RiftMode.PerInstance)

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, in the given
   * isolation `mode` (#203 adds Correlated — mirroring the container adapter,
   * but the shared imposter is FFI-created/-destroyed rather than
   * HTTP-managed): `rift_start` plus `rift_serve_admin` (the in-process admin
   * plane) on layer construction, `rift_stop` (and, in Correlated mode, the
   * shared imposter's teardown) on close. Self-contained — it builds its own
   * loopback HTTP client for the admin plane, so the public requirement stays
   * just [[Provisioning]]. Fails with [[MockError.ProvisionFailed]] when no
   * native library resolves for the host, or it cannot be loaded.
   */
  def layer(mode: RiftMode): ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      for
        prov   <- ZIO.service[Provisioning]
        engine <- startEngine
        // Standing up the admin plane is part of constructing the backend, so a failure here is a
        // ProvisionFailed (matching this layer's contract), not the raw CommunicationError.
        adminInfo <-
          engine
            .serveAdmin(serveAdminOptions)
            .mapError(e => MockError.ProvisionFailed(s"starting the embedded Rift admin plane: $e"))
        client <- adminClient
        control <- EmbeddedRiftMockControl.make(
                     engine,
                     prov,
                     EmbeddedRiftMockControl.Admin(adminInfo.adminUrl, client),
                     mode
                   )
      yield control
    }

  // Resolve the native library and start the engine, scoped (rift_start on acquire, rift_stop on
  // close). Shared by `layer` and the live FFI spec (which drives the engine's admin plane directly).
  private[embedded] def startEngine: ZIO[Scope, MockError, EmbeddedEngine] =
    for
      source <- ZIO.fromEither(NativeLibrary.resolveSource)
      path   <- loadablePath(source)
      engine <- RiftFfi.start(path.toString)
    yield engine

  // The loopback HTTP client for the in-process admin plane, owned by the layer's scope so the
  // embedded adapter needs nothing from the environment but Provisioning (mirrors the container
  // adapter's Client, but self-provided).
  private def adminClient: ZIO[Scope, MockError, Client] =
    Client.default.build
      .map(_.get[Client])
      .mapError(t => MockError.ProvisionFailed(s"building embedded Rift admin HTTP client: ${message(t)}"))

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

  private def message(t: Throwable): String =
    Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
