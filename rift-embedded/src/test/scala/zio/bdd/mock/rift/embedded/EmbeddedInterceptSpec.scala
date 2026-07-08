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

  // A non-loopback site-local IPv4 of this host — what a container reaches the host engine at (the
  // Docker host-gateway routes to it). None on hosts with no such NIC (e.g. loopback-only CI) → skip.
  private def siteLocalIPv4: Option[String] =
    import scala.jdk.CollectionConverters.*
    scala.util
      .Try(
        java.net.NetworkInterface.getNetworkInterfaces.asScala.toList
          .filter(ni => ni.isUp && !ni.isLoopback)
          .flatMap(_.getInetAddresses.asScala.toList)
          .collectFirst { case a: java.net.Inet4Address if a.isSiteLocalAddress => a.getHostAddress }
      )
      .toOption
      .flatten

  // Reserve a currently-free TCP port, then release it — a pinned port for the fixed-port test.
  private val reserveFreePort: Task[Int] =
    ZIO.attemptBlocking {
      val s = new java.net.ServerSocket(0)
      try s.getLocalPort
      finally s.close()
    }

  // Count the trusted-cert entries in a truststore file — CA-only vs merged (#259).
  private def anchorCount(ts: TrustStore): Task[Int] =
    ZIO.attemptBlocking {
      import scala.jdk.CollectionConverters.*
      val ks = KeyStore.getInstance(ts.format.keystoreName)
      val in = Files.newInputStream(ts.path)
      try ks.load(in, ts.password.toCharArray)
      finally in.close()
      ks.aliases().asScala.count(ks.isCertificateEntry)
    }

  // The stand-in SUT: a JDK HttpClient trusting `ts` and proxying HTTPS through the intercept port.
  // Forced HTTP/1.1 — the MITM tunnel serves 1.1 (an h2 upgrade over the tunnel would EOF, zb-183).
  // `proxyHost` is loopback by default; a non-loopback address exercises the wider-bind topology (#254).
  private def sutGet(
    url: String,
    proxyPort: Int,
    ts: TrustStore,
    proxyHost: String = "127.0.0.1"
  ): Task[(Int, String)] =
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
        .proxy(ProxySelector.of(new InetSocketAddress(proxyHost, proxyPort)))
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
    },
    test("bindHost 0.0.0.0: the proxy binds a wider interface, reports it, and a redirect completes through it") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        // Bind 0.0.0.0 (all interfaces). Prefer a real non-loopback NIC — the container-via-host-gateway
        // topology — but fall back to loopback, which a 0.0.0.0 socket also accepts, so this always
        // exercises the bind + `proxyEndpoint` reporting instead of silently skipping on a loopback-only host.
        val proxyHost = siteLocalIPv4.getOrElse("127.0.0.1")
        (for
          mc    <- ZIO.service[MockControl]
          space <- mc.provision(cdnConfig).mapError(asT).map(_.head)
          ic    <- mc.intercept.mapError(u => new RuntimeException(u.message))
          _     <- ic.redirectTo("cdn.example.com", space).mapError(asT)
          ts    <- ic.trustStore().mapError(asT)
          ep    <- ic.proxyEndpoint.mapError(asT)
          res   <- sutGet("https://cdn.example.com/config.json", ep._2, ts, proxyHost = proxyHost)
        yield assertTrue(res._1 == 200, res._2.contains("mocked"), ep._1 == "0.0.0.0"))
          .provide(
            Provisioning.live,
            EmbeddedRift.layer(EmbeddedRift.InterceptConfig(bindHost = "0.0.0.0")).mapError(asT)
          )
    },
    test("fixed port: the intercept listener binds the pinned port instead of an OS-assigned one") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        reserveFreePort.flatMap { pinned =>
          (for
            mc   <- ZIO.service[MockControl]
            ic   <- mc.intercept.mapError(u => new RuntimeException(u.message))
            port <- ic.proxyPort.mapError(asT)
            ep   <- ic.proxyEndpoint.mapError(asT)
          yield assertTrue(port == pinned, ep == ("127.0.0.1", pinned)))
            .provide(
              Provisioning.live,
              EmbeddedRift.layer(EmbeddedRift.InterceptConfig(port = Some(pinned))).mapError(asT)
            )
        }
    },
    // Pure translation of InterceptConfig -> FFI start-options JSON; runs even without the native library,
    // so the host/port wiring is covered on any host (the e2e tests above SKIP when no library resolves).
    test("startOptions maps InterceptConfig to the FFI host/port JSON; default stays loopback + OS-assigned") {
      val default = EmbeddedIntercept.startOptions(EmbeddedRift.InterceptConfig())
      val configured =
        EmbeddedIntercept.startOptions(EmbeddedRift.InterceptConfig(bindHost = "0.0.0.0", port = Some(8888)))
      assertTrue(
        default == """{"host":"127.0.0.1","port":0}""",
        configured == """{"host":"0.0.0.0","port":8888}"""
      )
    },
    test("trustStoreWithSystemCAs: the merged store trusts the intercepted host AND keeps the JVM default anchors") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          mc      <- ZIO.service[MockControl]
          space   <- mc.provision(cdnConfig).mapError(asT).map(_.head)
          ic      <- mc.intercept.mapError(u => new RuntimeException(u.message))
          _       <- ic.redirectTo("cdn.example.com", space).mapError(asT)
          caOnly  <- ic.trustStore().mapError(asT)
          merged  <- ic.trustStoreWithSystemCAs().mapError(asT)
          port    <- ic.proxyPort.mapError(asT)
          caN     <- anchorCount(caOnly)
          mergedN <- anchorCount(merged)
          // (a) the merged store still trusts the intercept CA → the redirected HTTPS call succeeds through it.
          res <- sutGet("https://cdn.example.com/config.json", port, merged)
        // (b) the merged store also carries the JVM default anchors → strictly more entries than CA-only.
        yield assertTrue(res._1 == 200, res._2.contains("mocked"), caN >= 1, mergedN > caN))
          .provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential
