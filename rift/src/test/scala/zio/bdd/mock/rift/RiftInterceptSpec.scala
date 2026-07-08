package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.Client
import zio.test.*

import java.net.{InetSocketAddress, ProxySelector, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files
import java.security.KeyStore
import javax.net.ssl.{SSLContext, TrustManagerFactory}

/**
 * End-to-end acceptance for the container Rift's HTTPS intercept capability
 * (#253) — a mechanical adaptation of `EmbeddedInterceptSpec` (the embedded
 * provider's proven template): the same redirectTo/respondWith/trustStore
 * checks, but driving a real containerized Rift started with a fixed
 * `--intercept-port` and reached over the admin HTTP API instead of the FFI.
 *
 * Opt-in, exactly like [[RiftContainerSpec]]: runs only when `RIFT_IT` is set,
 * so the default `sbt test` stays hermetic and Docker-free.
 */
object RiftInterceptSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  // A container-internal port for the intercept listener, exposed by Rift.managed alongside the
  // admin + imposter ports and mapped by testcontainers like the rest.
  private val InterceptContainerPort = 8474

  // The stand-in SUT: a JDK HttpClient trusting `ts` and proxying HTTPS through the intercept port.
  // Forced HTTP/1.1 — the MITM tunnel serves 1.1 (an h2 upgrade over the tunnel would EOF, zb-183),
  // mirroring EmbeddedInterceptSpec's sutGet.
  private def sutGet(url: String, proxyPort: Int, ts: TrustStore): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val ks = KeyStore.getInstance(ts.format.keystoreName)
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

  private val realSuite = suite("RiftInterceptSpec (real Rift container)")(
    test("redirectTo: an HTTPS call to a hard-coded external host is intercepted and served by the mock imposter") {
      for
        mc    <- ZIO.service[MockControl]
        space <- mc.provision(cdnConfig).mapError(asT).map(_.head)
        ic    <- mc.intercept.mapError(u => new RuntimeException(u.message))
        _     <- ic.redirectTo("cdn.example.com", space).mapError(asT)
        ts    <- ic.trustStore().mapError(asT) // default PKCS#12
        port  <- ic.proxyPort.mapError(asT)
        res   <- sutGet("https://cdn.example.com/config.json", port, ts)
      yield assertTrue(res._1 == 200, res._2.contains("mocked"))
    },
    test("respondWith: an intercepted host is answered inline from the CA-trusting SUT") {
      for
        mc <- ZIO.service[MockControl]
        ic <- mc.intercept.mapError(u => new RuntimeException(u.message))
        _ <-
          ic.respondWith("api.example.com", InterceptStub(status = 418, body = Some("inline-teapot"))).mapError(asT)
        ts   <- ic.trustStore(TrustStoreFormat.Jks).mapError(asT)
        port <- ic.proxyPort.mapError(asT)
        res  <- sutGet("https://api.example.com/anything", port, ts)
      yield assertTrue(res._1 == 418, res._2.contains("inline-teapot"))
    }
  ).provideSome[Client](Provisioning.live, riftBackend) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  private def riftBackend: ZLayer[Client & Provisioning, MockError, MockControl] =
    Rift.managed(interceptPort = Some(InterceptContainerPort))

  def spec =
    if sys.env.contains("RIFT_IT") then suite("Rift intercept ITs")(realSuite).provideShared(Client.default)
    else suite("RiftInterceptSpec (skipped — set RIFT_IT=1 to run against a real Rift container)")()
