package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning}

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
 * Requires Project Panama FFM, a preview API on JDK 21: the host JVM must run
 * with `--enable-preview` (and `--enable-native-access` to silence the
 * restricted-method warning).
 */
object EmbeddedRift:

  /**
   * True iff a `librift_ffi` resolves for the host (a bundled native resource,
   * or an override path).
   */
  def available: Boolean = NativeLibrary.available

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine: `rift_start`
   * on layer construction, `rift_stop` on close. Fails with
   * [[MockError.ProvisionFailed]] when no native library resolves for the host,
   * or it cannot be loaded.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      for
        prov    <- ZIO.service[Provisioning]
        source  <- ZIO.fromEither(NativeLibrary.resolveSource)
        path    <- loadablePath(source)
        engine  <- RiftFfi.start(path.toString)
        control <- EmbeddedRiftMockControl.make(engine, prov)
      yield control
    }

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
