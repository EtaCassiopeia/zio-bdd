package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning}

import java.nio.file.{Files, Paths}

/**
 * Entry point for `MockControl.embedded` (#133): the opt-in, no-Docker,
 * in-process Rift provider, driving the engine over `librift_ffi` via Panama
 * FFM behind the portable [[MockControl]] interface — a pure backend swap for
 * the container adapter.
 *
 * The native library is located via the `rift.ffi.lib` system property (falling
 * back to the `RIFT_FFI_LIB` environment variable). [[available]] reports
 * whether that path resolves to a real file, so a harness can park the backend
 * (rather than fail) on a platform where the library has not been provisioned.
 *
 * Requires Project Panama FFM, a preview API on JDK 21: the host JVM must run
 * with `--enable-preview` (and `--enable-native-access` to silence the
 * restricted-method warning).
 */
object EmbeddedRift:

  /**
   * System property naming the absolute path to the `librift_ffi` shared
   * library.
   */
  val LibPathProperty = "rift.ffi.lib"

  /**
   * Environment variable naming the `librift_ffi` shared library (fallback for
   * the property).
   */
  val LibPathEnv = "RIFT_FFI_LIB"

  /**
   * The configured native-library path, if set (property takes precedence over
   * the env var).
   */
  def libPath: Option[String] =
    sys.props.get(LibPathProperty).orElse(sys.env.get(LibPathEnv)).map(_.trim).filter(_.nonEmpty)

  /**
   * True iff the native library path is configured and points to an existing
   * regular file.
   */
  def available: Boolean =
    libPath.exists(p => Files.isRegularFile(Paths.get(p)))

  /**
   * A scoped [[MockControl]] backed by the in-process Rift engine: `rift_start`
   * on layer construction, `rift_stop` on close. Fails with
   * [[MockError.ProvisionFailed]] if the native library path is not configured
   * or cannot be loaded.
   */
  def layer: ZLayer[Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      for
        prov <- ZIO.service[Provisioning]
        path <- ZIO
                  .fromOption(libPath)
                  .orElseFail(
                    MockError.ProvisionFailed(s"native library path not set; set -D$LibPathProperty or $LibPathEnv")
                  )
        engine  <- RiftFfi.start(path)
        control <- EmbeddedRiftMockControl.make(engine, prov)
      yield control
    }
