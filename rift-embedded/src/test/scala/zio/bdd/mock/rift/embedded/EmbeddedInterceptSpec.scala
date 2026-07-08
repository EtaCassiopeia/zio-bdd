package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.test.*

import java.net.{InetSocketAddress, ProxySelector, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files
import java.security.KeyStore
import javax.net.ssl.{SSLContext, TrustManagerFactory}

/**
 * End-to-end acceptance for the built-in HTTPS intercept capability (#219,
 * AC4/AC6): the embedded engine runs the TLS-MITM forward-proxy with no extra
 * container, and a real JVM HTTPS client — the stand-in SUT — trusting the
 * exported CA and proxying through the intercept port has its call to a
 * hard-coded external host intercepted and answered by the mock. This is the
 * mitmproxy replacement.
 *
 * Gated on the native library ([[EmbeddedRift.available]]), mirroring the other
 * embedded specs; the whole suite SKIPs (passes) when no library resolves for
 * the host.
 */
object EmbeddedInterceptSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  // The stand-in SUT: a JDK HttpClient trusting `ts` and proxying HTTPS through the intercept port.
  // Forced HTTP/1.1 — the MITM tunnel serves 1.1 (an h2 upgrade over the tunnel would EOF, zb-183).
  private def sutGet(url: String, proxyPort: Int, ts: TrustStore): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val ks = KeyStore.getInstance(ts.format match
        case TrustStoreFormat.Pkcs12 => "PKCS12"
        case TrustStoreFormat.Jks    => "JKS"
      )
      val in = Files.newInputStream(ts.path)
      try ks.load(in, ts.password.toCharArray)
      finally in.close()
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(ks)
      val ssl = SSLContext.getInstance("TLS")
      ssl.init(null, tmf.getTrustManagers, null)
      val client = HttpClient
        .newBuilder()
        .version(HttpClient.Version.HTTP_1_1)
        .sslContext(ssl)
        .proxy(ProxySelector.of(new InetSocketAddress("127.0.0.1", proxyPort)))
        .build()
      val resp =
        client.send(HttpRequest.newBuilder(URI.create(url)).GET().build(), HttpResponse.BodyHandlers.ofString())
      (resp.statusCode, resp.body)
    }

  private val cdnConfig = MockSource.Dsl(
    MockSpec(
      List(
        MockRule(
          RequestMatch(path = PathMatch.Exact("/config.json")),
          ResponseDef(status = 200, body = Body.Text("""{"cdn":"mocked"}"""))
        )
      )
    )
  )

  def spec = suite("EmbeddedIntercept (#219)")(
    test("redirectTo: an HTTPS call to a hard-coded external host is intercepted and served by the mock imposter") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          mc    <- ZIO.service[MockControl]
          space <- mc.provision(cdnConfig).mapError(asT).map(_.head)
          ic    <- mc.intercept.mapError(u => new RuntimeException(u.message))
          _     <- ic.redirectTo("cdn.example.com", space).mapError(asT)
          ts    <- ic.trustStore().mapError(asT) // default PKCS#12 (JVM-loadable since rift#418)
          port  <- ic.proxyPort.mapError(asT)
          res   <- sutGet("https://cdn.example.com/config.json", port, ts)
        yield assertTrue(res._1 == 200, res._2.contains("mocked")))
          .provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("respondWith: an intercepted host is answered inline from the CA-trusting SUT") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          mc <- ZIO.service[MockControl]
          ic <- mc.intercept.mapError(u => new RuntimeException(u.message))
          _ <-
            ic.respondWith("api.example.com", InterceptStub(status = 418, body = Some("inline-teapot"))).mapError(asT)
          ts   <- ic.trustStore(TrustStoreFormat.Jks).mapError(asT)
          port <- ic.proxyPort.mapError(asT)
          res  <- sutGet("https://api.example.com/anything", port, ts)
        yield assertTrue(res._1 == 418, res._2.contains("inline-teapot")))
          .provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential
