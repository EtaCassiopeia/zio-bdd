package zio.bdd.mock.rift.embedded

import zio.bdd.mock.MockError
import zio.test.*

import java.nio.file.Files

/**
 * Verifies the #134 native-library packaging + resolution: the bundled
 * `zio-bdd-rift-embedded-natives` resources are present for every supported
 * triple, a valid override takes precedence (and an invalid one is a loud
 * error), and unsupported / unshipped hosts fail with a clear, host-specific
 * error rather than an obscure `dlopen` failure.
 */
object NativeLibrarySpec extends ZIOSpecDefault:

  // (os.name, os.arch, expected bundled resource) for each platform the natives jar must carry —
  // including the `arm64` arch alias some JVMs report on Apple Silicon.
  private val supportedTriples = List(
    ("Linux", "amd64", "native/linux-x86_64/librift_ffi.so"),
    ("Linux", "aarch64", "native/linux-aarch64/librift_ffi.so"),
    ("Mac OS X", "x86_64", "native/darwin-x86_64/librift_ffi.dylib"),
    ("Mac OS X", "aarch64", "native/darwin-aarch64/librift_ffi.dylib"),
    ("Mac OS X", "arm64", "native/darwin-aarch64/librift_ffi.dylib")
  )

  def spec = suite("NativeLibrary")(
    test("every supported triple resolves to its bundled resource (present on the classpath)") {
      assertTrue(
        supportedTriples.forall((os, arch, res) =>
          NativeLibrary.resolveSourceFor(os, arch, None) == Right(LibSource.Bundled(res))
        )
      )
    },
    test("a valid override path takes precedence over bundling and is used verbatim") {
      // Resolve strictly while the file exists, then clean up — assertTrue captures the expression and
      // the runner evaluates it later, so a `finally`-delete would race the file-existence check.
      val f      = Files.createTempFile("custom-librift_ffi-", ".so")
      val result = NativeLibrary.resolveSourceFor("Linux", "amd64", Some(f.toString))
      Files.deleteIfExists(f)
      assertTrue(result == Right(LibSource.Override(f)))
    },
    test("a set-but-missing override path fails loudly (never a silent fall-through to bundling)") {
      assertTrue(NativeLibrary.resolveSourceFor("Linux", "amd64", Some("/no/such/librift_ffi.so")) match
        case Left(MockError.ProvisionFailed(m)) => m.contains("is set to '/no/such/librift_ffi.so'")
        case _                                  => false
      )
    },
    test("a recognized host this project ships no native for fails with a clear error (windows)") {
      assertTrue(NativeLibrary.resolveSourceFor("Windows 11", "amd64", None) match
        case Left(MockError.ProvisionFailed(m)) => m.contains("ships no native library for host windows-x86_64")
        case _                                  => false
      )
    },
    test("an unsupported host platform fails with a clear error") {
      assertTrue(NativeLibrary.resolveSourceFor("Plan 9", "sparc", None) match
        case Left(MockError.ProvisionFailed(m)) => m.contains("unsupported host platform")
        case _                                  => false
      )
    },
    test("the live host resolves out-of-the-box — no -Drift.ffi.lib override set, yet available") {
      assertTrue(!sys.props.contains(NativeLibrary.LibPathProperty), EmbeddedRift.available)
    }
  )
