package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning}
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
   * A scoped [[MockControl]] backed by the in-process Rift engine, PerInstance
   * isolation (own port per space). See the `mode` overload below for
   * Correlated.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] = layer(RiftMode.PerInstance)

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine, in the given
   * isolation `mode` (#203 adds Correlated — mirroring the container adapter,
   * but the shared imposter is FFI-created/-destroyed rather than
   * HTTP-managed): `rift_start` on layer construction, `rift_stop` (and, in
   * Correlated mode, the shared imposter's teardown) on close. Entirely FFI
   * (#244) — no loopback HTTP admin plane, so the public requirement stays just
   * [[Provisioning]]. Fails with [[MockError.ProvisionFailed]] when no native
   * library resolves for the host, or it cannot be loaded.
   */
  def layer(mode: RiftMode): ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      for
        prov    <- ZIO.service[Provisioning]
        engine  <- startEngine
        control <- EmbeddedRiftMockControl.make(engine, prov, mode)
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
