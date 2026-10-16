package samples.mock

import samples.mock.support.Backends
import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.embedded.EmbeddedRift
import zio.bdd.mock.rift.embedded.EmbeddedRift.InterceptConfig
import zio.test.*

import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * #62 — Plane B coverage of `EmbeddedRift.InterceptConfig`: the embedded
 * MockControl backend's intercept proxy honoring a **non-loopback bind host**,
 * a **pinned fixed port**, and a **persistent, caller-provided CA**
 * (`caCert`/`caKey`, rift ≥ 0.11.3). `Backends`' plain `EmbeddedRift.layer`
 * only ever exercises the defaults (loopback bind, OS-assigned port, ephemeral
 * CA), so a real consumer's `InterceptConfig(bindHost="0.0.0.0",
 * port=Some(fixed), caCert=…, caKey=…)` — used so a Docker SUT reaches the host
 * proxy via the gateway and trusts it at container start — had no conformance
 * scenario.
 *
 * White-box on the portable `Intercept` capability rather than a Gherkin SUT
 * flow: the full container-reachability leg hits the macOS Docker
 * host-networking limit (#45), but the core guarantees — the configured
 * host/port are bound, and the exported truststore carries the *supplied* CA
 * (not a freshly minted one) — are assertable directly. Plane A's
 * `@intercept-caller-ca` (#55) covers the standalone binary's
 * `--intercept-ca-cert/--key`, a different subsystem; this exercises the
 * embedded `InterceptConfig(caCert, caKey)` constructor.
 *
 * Self-gating (mirrors `samples.rift.RiftEmbeddedHarnessSpec`): a no-op pass
 * unless `MOCK_BACKEND=embedded` AND a `librift_ffi` resolves for this JVM (JDK
 * 22+ with the natives jar). The `mockEmbedded` alias (the JDK-25 CI leg) is
 * the only place these assertions actually run; on JDK < 22 the embedded
 * backend is unavailable and this skips rather than reads as a false green.
 */
object EmbeddedInterceptConfigSpec extends ZIOSpecDefault:

  // The committed throwaway test CA (rift-fixtures/intercept, the fixture rift-conformance's Plane A
  // caller-CA check uses). rift binds the caller CA from filesystem paths, so extract the classpath
  // PEMs to temp files.
  private def caFixture: Task[(Path, Path)] =
    for
      certPem <- readResource("/rift-fixtures/intercept/ca-cert.pem")
      keyPem  <- readResource("/rift-fixtures/intercept/ca-key.pem")
      cert    <- writeTemp("rift-62-ca-cert-", certPem)
      key     <- writeTemp("rift-62-ca-key-", keyPem)
    yield (cert, key)

  private def readResource(path: String): Task[String] =
    ZIO.attemptBlocking {
      val is = getClass.getResourceAsStream(path)
      if is == null then throw new IllegalStateException(s"missing test resource: $path")
      try new String(is.readAllBytes(), StandardCharsets.UTF_8)
      finally is.close()
    }

  private def writeTemp(prefix: String, content: String): Task[Path] =
    ZIO.attemptBlocking {
      val p = Files.createTempFile(prefix, ".pem")
      Files.writeString(p, content)
      p
    }

  private def parseCert(pem: Path): Task[X509Certificate] =
    ZIO.attemptBlocking {
      val cf = CertificateFactory.getInstance("X.509")
      Using.resource(Files.newInputStream(pem))(in => cf.generateCertificate(in).asInstanceOf[X509Certificate])
    }

  // The single trusted CA cert inside an exported PKCS#12 truststore (a CA-only trustedCertEntry).
  private def truststoreCert(ts: TrustStore): Task[X509Certificate] =
    ZIO.attemptBlocking {
      val ks = KeyStore.getInstance(ts.format.keystoreName)
      Using.resource(Files.newInputStream(ts.path))(in => ks.load(in, ts.password.toCharArray))
      ks.aliases().asScala.find(ks.isCertificateEntry) match
        case Some(alias) => ks.getCertificate(alias).asInstanceOf[X509Certificate]
        case None        => throw new IllegalStateException("exported truststore has no trustedCertEntry")
    }

  // Pin a currently-free port: asserting the proxy binds exactly it proves fixed-port honoring (vs.
  // an OS-assigned one). Serial test execution + a just-freed port keeps this race-free enough.
  private def freePort: Task[Int] =
    ZIO.attemptBlocking(Using.resource(new ServerSocket(0))(_.getLocalPort))

  def spec = suite("EmbeddedRift InterceptConfig (Plane B, #62)")(
    test(
      "binds the configured non-loopback host + fixed port and exports the supplied persistent CA"
    ) {
      if Backends.selected != "embedded" || !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped {
          for
            fixture        <- caFixture
            (caCert, caKey) = fixture
            expected       <- parseCert(caCert)
            port           <- freePort
            cfg = InterceptConfig(
                    bindHost = "0.0.0.0",
                    port = Some(port),
                    caCert = Some(caCert),
                    caKey = Some(caKey)
                  )
            env <- (ZLayer(Provisioning.make) >>> EmbeddedRift.layer(cfg)).build
                     .mapError(e => new RuntimeException(s"building embedded layer with InterceptConfig: $e"))
            ic <- env
                    .get[MockControl]
                    .intercept
                    .mapError(u => new RuntimeException(s"intercept capability: $u"))
            endpoint <- ic.proxyEndpoint.mapError(e => new RuntimeException(s"proxyEndpoint: $e"))
            ts       <- ic.trustStore().mapError(e => new RuntimeException(s"trustStore export: $e"))
            exported <- truststoreCert(ts)
            // Compare the DER bytes OUTSIDE `assertTrue`: that macro mis-reifies an inline
            // `java.util.Arrays.equals(...)` into a non-existent `java.util.Arrays$` module access,
            // which throws NoClassDefFoundError at runtime — so bind the plain Boolean first.
            caMatches = exported.getEncoded.sameElements(expected.getEncoded)
          yield assertTrue(
            endpoint._1 == "0.0.0.0",
            endpoint._2 == port,
            caMatches
          )
        }
    }
  )
