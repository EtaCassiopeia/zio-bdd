package zio.bdd.mock.rift.embedded

import zio.bdd.mock.MockError

import java.nio.file.{Files, Path, Paths}

/**
 * Where a `librift_ffi` to load comes from: an explicit override path, or a
 * bundled classpath resource.
 */
private[embedded] enum LibSource:
  case Override(path: Path)
  case Bundled(resource: String)

/**
 * Locates the `librift_ffi` native library for the host platform (#134),
 * resolved in order:
 *
 *   1. an explicit `-Drift.ffi.lib` system property (or `RIFT_FFI_LIB` env var)
 *      pointing at a file — the escape hatch for a custom/local build; 2. the
 *      per-platform cdylib bundled as a classpath resource
 *      (`native/<os>-<arch>/librift_ffi.<ext>`) by the
 *      `zio-bdd-rift-embedded-natives` jar.
 *
 * When neither resolves — an unsupported host, or the natives jar absent from
 * the classpath — it returns a clear [[MockError.ProvisionFailed]] rather than
 * failing obscurely at `dlopen`.
 *
 * [[resolveSourceFor]] is the pure, host-parameterised core (so resolution +
 * bundled-resource presence are unit-testable without depending on the live
 * host); extraction of a bundled resource to a temp file is an effect owned by
 * [[EmbeddedRift]].
 */
private[embedded] object NativeLibrary:

  /**
   * System property naming an explicit path to a `librift_ffi` shared library
   * (overrides bundling).
   */
  val LibPathProperty = "rift.ffi.lib"

  /**
   * Environment variable naming an explicit `librift_ffi` path (fallback for
   * the property).
   */
  val LibPathEnv = "RIFT_FFI_LIB"

  /**
   * The raw configured override (property over env var), or `None` if neither
   * is set/non-empty.
   */
  def configuredOverride: Option[String] =
    sys.props.get(LibPathProperty).orElse(sys.env.get(LibPathEnv)).map(_.trim).filter(_.nonEmpty)

  /**
   * The (os, arch) triples the `zio-bdd-rift-embedded-natives` jar actually
   * ships (#134).
   */
  val shippedTriples: Set[(String, String)] =
    Set(("linux", "x86_64"), ("linux", "aarch64"), ("darwin", "x86_64"), ("darwin", "aarch64"))

  /**
   * Canonical OS token for the bundled-resource path, or `None` if unsupported.
   */
  def osToken(raw: String): Option[String] =
    val s = raw.toLowerCase
    if s.contains("mac") || s.contains("darwin") then Some("darwin")
    else if s.contains("linux") then Some("linux")
    else if s.contains("win") then Some("windows")
    else None

  /**
   * Canonical architecture token for the bundled-resource path, or `None` if
   * unsupported.
   */
  def archToken(raw: String): Option[String] =
    raw.toLowerCase match
      case "aarch64" | "arm64" => Some("aarch64")
      case "x86_64" | "amd64"  => Some("x86_64")
      case _                   => None

  /** The shared-library extension for an OS token. */
  def libExtension(os: String): Option[String] =
    os match
      case "linux"   => Some("so")
      case "darwin"  => Some("dylib")
      case "windows" => Some("dll")
      case _         => None

  /**
   * The bundled classpath resource path for a host triple (independent of
   * whether it is present).
   */
  def resourceFor(os: String, arch: String): Option[String] =
    libExtension(os).map(ext => s"native/$os-$arch/librift_ffi.$ext")

  private def resourceExists(resource: String): Boolean =
    getClass.getClassLoader.getResource(resource) != null

  /**
   * Resolve the library source for an explicit host OS/arch and raw configured
   * override (the pure, testable core). An override takes precedence but must
   * point at an existing file — a set-but-invalid override is a loud error,
   * never a silent fall-through to bundling. Otherwise the bundled resource for
   * the host is used; failing that, the message distinguishes a missing natives
   * jar (a shipped triple) from a platform this project does not ship a native
   * for.
   */
  def resolveSourceFor(osRaw: String, archRaw: String, ovr: Option[String]): Either[MockError, LibSource] =
    ovr match
      case Some(raw) =>
        val p = Paths.get(raw)
        if Files.isRegularFile(p) then Right(LibSource.Override(p))
        else
          Left(
            MockError.ProvisionFailed(
              s"-D$LibPathProperty is set to '$raw' but no regular file exists there; " +
                s"fix the path or unset it to load the bundled library"
            )
          )
      case None =>
        (osToken(osRaw), archToken(archRaw)) match
          case (Some(os), Some(arch)) =>
            resourceFor(os, arch).filter(resourceExists) match
              case Some(resource) => Right(LibSource.Bundled(resource))
              case None if shippedTriples((os, arch)) =>
                Left(
                  MockError.ProvisionFailed(
                    s"no bundled librift_ffi for host $os-$arch on the classpath; add the " +
                      s"zio-bdd-rift-embedded-natives dependency, or set -D$LibPathProperty to a librift_ffi path"
                  )
                )
              case None =>
                Left(
                  MockError.ProvisionFailed(
                    s"embedded Rift ships no native library for host $os-$arch; " +
                      s"set -D$LibPathProperty to a librift_ffi path"
                  )
                )
          case _ =>
            Left(
              MockError.ProvisionFailed(
                s"unsupported host platform '$osRaw'/'$archRaw' for embedded Rift; " +
                  s"set -D$LibPathProperty to a librift_ffi path"
              )
            )

  /** Resolve the library source for the live host. */
  def resolveSource: Either[MockError, LibSource] =
    resolveSourceFor(sys.props.getOrElse("os.name", ""), sys.props.getOrElse("os.arch", ""), configuredOverride)

  /**
   * True iff a library can be resolved for the host (an override, or a bundled
   * resource).
   */
  def available: Boolean = resolveSource.isRight
