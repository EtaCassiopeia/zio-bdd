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

  // A committed CA (cert + PKCS#8 key, CA:TRUE) for the persistent-CA reuse test (#273). Two engine
  // instances loading this same pair mint leaves under one CA, so a truststore exported from one validates
  // the other's intercepted TLS — which an ephemeral (per-start) CA does not.
  private val CaCertPem: String =
    """|-----BEGIN CERTIFICATE-----
       |MIIDOzCCAiOgAwIBAgIUd4FP1xedkoSU6TmCHZDCYe5wTFwwDQYJKoZIhvcNAQEL
       |BQAwJTEjMCEGA1UEAwwaemlvLWJkZC1wZXJzaXN0ZW50LXRlc3QtY2EwHhcNMjYw
       |NzA4MTEzNjI5WhcNMzYwNzA1MTEzNjI5WjAlMSMwIQYDVQQDDBp6aW8tYmRkLXBl
       |cnNpc3RlbnQtdGVzdC1jYTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
       |AN8lKT3RxrWABSDYAyl2FH57vx1wZe/qPV4SuXun3Z47bGT/ppaoqZIU9cd/IZye
       |Zp3fqoONKmJHbLfsCN9RUXWIFtRC/1OcBoP4xZ2cNgcUoZ5EUTRHiV2xWzmib+th
       |wqNh8ywICn8KhhTRlp6S/PLW680IzDEtcotOP20eFk5w/W5acHdmG78YBVS0IbZQ
       |ONGFysJcvekVsQINyao3XN+5eVqeFDA5VTllj2lxNVTQQYhGfyfnBZKqTTOihJ99
       |+ZMGhTUIPznzeinYA72b+eZqyqjKMPtbQERB0J3F7oPFtsVtvUDvr7y6ijr60LLU
       |H6xvtuQVugAWIrhtiz5aAWMCAwEAAaNjMGEwHQYDVR0OBBYEFAjgTsN9t6fBGRF3
       |dVQkUPiFNRdPMB8GA1UdIwQYMBaAFAjgTsN9t6fBGRF3dVQkUPiFNRdPMA8GA1Ud
       |EwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgEGMA0GCSqGSIb3DQEBCwUAA4IBAQAW
       |peycXfE/ifmuNDlDSyqsGnSDr2LurluFfP3/bRS1U3rn2/XDXGMJA6lG6OaKbl/8
       |EQ8kcJjuG3Xx5h4PbvO2saDAEGMB2v7Le8YFiM8H3Zzl+4OiFhm5l7LcTbWMS0B+
       |NfZh58ixe1sqUOL+ITOxrQgIew8bv3DOZM16eTj24unpCDwiRg9jJHb8TDPyynL9
       |tjAUlq8TDJBuKfjVyM6VRwPxGL1atjIc9EiMkmIBtmnbRbwUyb05h1kdOwDpYPSj
       |I/W9WWL3S+jKKexw66A141DgRPU5cmhUBI9VH3UcfrIZpsaFsK2xsTK5wvHqIGPS
       |vI7vtjy6CP3WIgFqfrua
       |-----END CERTIFICATE-----""".stripMargin

  private val CaKeyPem: String =
    """|-----BEGIN PRIVATE KEY-----
       |MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQDfJSk90ca1gAUg
       |2AMpdhR+e78dcGXv6j1eErl7p92eO2xk/6aWqKmSFPXHfyGcnmad36qDjSpiR2y3
       |7AjfUVF1iBbUQv9TnAaD+MWdnDYHFKGeRFE0R4ldsVs5om/rYcKjYfMsCAp/CoYU
       |0Zaekvzy1uvNCMwxLXKLTj9tHhZOcP1uWnB3Zhu/GAVUtCG2UDjRhcrCXL3pFbEC
       |DcmqN1zfuXlanhQwOVU5ZY9pcTVU0EGIRn8n5wWSqk0zooSfffmTBoU1CD8583op
       |2AO9m/nmasqoyjD7W0BEQdCdxe6DxbbFbb1A76+8uoo6+tCy1B+sb7bkFboAFiK4
       |bYs+WgFjAgMBAAECggEAFlrNMmXBH+621M7/Jhivveocv9DCTZIsWq/KiDJNd+wh
       |FE8uO7qi3kEeTEGWtu+BfUBqfypVvCnGoPhS/ThSzlD3ZIVfudsQJgl2lq8PFI+D
       |D21tqyydfNbnOjNlPfH5w09tQpR9pLODsjM7ASDdmWBhmBVRC34vkvggT4irmGj2
       |ftdvt9f3hJq739Iybm2GkOtiIj++FFS4v4qo5OlPZ2cYZwrXHW1s4p3GFfztjyw2
       |O+kHo/IbFdryLK8w44UuscpP2VC5YtYZabhhxZurVen0urMJoPFHMmmjJmwEFKK0
       |dKlWixZ0tuYqSpXr7pr+m0stG5ZmWzVaj7CPIR6QrQKBgQDwz0mNf+bDZ5rNhvPa
       |lRRkd7LQeVdeoHtrB7ylWSjC1T7i/jB+zBGEOeRQukbhtFsGOTdX/tRiiO0gqTM5
       |cJ0tpKR1pGE7/Hg893TQ5ZIU7eyuB+qkQVxAiLNEJRZox9tAFVtMkY7Gtac6T4MG
       |GrAyUWqNlsLlM9gwPY63jwsfPQKBgQDtOJ4wr5HuFisQNJvAlYczYDOE4EZM0XOm
       |I/uxraKGIJtPGS1qpVkb9T5suSWMYBJzKOBDcyq7hr7AgllloXjRHj6/lo6Jmmsv
       |KrbRDZiWNEZoFz3bm4cYE1VnaFJ39s08XIVXOfSAZilQyY5s5Wy8Y+Y0rWYYZ7uR
       |Y5F0dcmtHwKBgQDT3r0rJvYkxHn6DJtunK3Ve08zhx8s3WvZDnSW0iw/x5lN5DFM
       |zkU3HixYLpyApstTXXHpFQdOLoTbsKKmDgPsjag47Uizm306vsnjpahyi6cqs0Jq
       |BCujh+KZuTvPMtAfwOqUIpkJEfgxIJX5/+84RlPGgXe2a3fxcaeors0tTQKBgQCX
       |CYh22SJh481xWP2eBdZ6WJMU1DMOcAFhU+HKRBKJsbMRDqfDEhoFWgVv9gY5NWYp
       |2mnHqEkZ8vX63kOLGN8yzj4HgBWq+R2rf1e9DRoM8KWvWrTw4TqHkq+60kpHxWTx
       |PLtNSUIqimFd5acAIKc136lP+uNZQJrFPA79ho1s4wKBgQCl99+8lWVI75FWbOmF
       |8JafS9hRZza7RTW9uMfGm1rTzJfV+EsdhtQO2kFkvvr+W12nEG9pCcMlV7aUgYe/
       |2kNhEH/RROoCpCYWTX7FpcvhP8T7oBmtiB/2iLfPBuiiysWgEmI7B+H6/79WeHCs
       |UALkJ4TuH6dsEar2G9caVFSobw==
       |-----END PRIVATE KEY-----""".stripMargin

  // Run `use` against a fresh scoped embedded MockControl built with `cfg` (its own engine, torn down on
  // scope close) — so a test can run two independent instances with the same committed CA (#273).
  private def withEmbedded[A](cfg: EmbeddedRift.InterceptConfig)(use: MockControl => Task[A]): Task[A] =
    ZIO
      .scoped(EmbeddedRift.layer(cfg).build.mapError(asT).flatMap(env => use(env.get[MockControl])))
      .provide(Provisioning.live)

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
    // Pure CA start-options + both-or-neither validation (#273); no native library needed.
    test("startOptions emits caCertPath/caKeyPath only when both are set; validateCaPair rejects a lone one") {
      val none = EmbeddedIntercept.startOptions(EmbeddedRift.InterceptConfig())
      val both = EmbeddedIntercept.startOptions(
        EmbeddedRift.InterceptConfig(
          caCert = Some(java.nio.file.Path.of("/ca.pem")),
          caKey = Some(java.nio.file.Path.of("/ca.key"))
        )
      )
      assertTrue(
        !none.contains("caCertPath") && !none.contains("caKeyPath"), // absent → no CA keys (deny_unknown_fields ok)
        both.contains(""""caCertPath":"/ca.pem"""") && both.contains(""""caKeyPath":"/ca.key""""),
        EmbeddedIntercept.validateCaPair(EmbeddedRift.InterceptConfig()).isRight,
        EmbeddedIntercept
          .validateCaPair(
            EmbeddedRift.InterceptConfig(
              caCert = Some(java.nio.file.Path.of("/ca.pem")),
              caKey = Some(java.nio.file.Path.of("/ca.key"))
            )
          )
          .isRight,
        EmbeddedIntercept.validateCaPair(
          EmbeddedRift.InterceptConfig(caCert = Some(java.nio.file.Path.of("/ca.pem")))
        ) match
          case Left(MockError.InvalidDefinition(m)) => m.contains("both or neither")
          case _                                    => false
      )
    },
    // Pure bind-host validation (#262); runs without the native library.
    test("validateBindHost: IP literals pass; a hostname is rejected with a clear InvalidDefinition naming it") {
      val ok  = List("127.0.0.1", "0.0.0.0", "192.168.1.5", "10.0.0.1", "255.255.255.255", "::1", "fe80::1")
      val bad = List("localhost", "myhost.local", "example.com", "not-an-ip", "999.1.1.1", "127.0.0.01", "1.2.3")
      assertTrue(
        ok.forall(h => EmbeddedIntercept.validateBindHost(h).isRight),
        bad.forall { h =>
          EmbeddedIntercept.validateBindHost(h) match
            case Left(MockError.InvalidDefinition(msg)) => msg.contains(h)
            case _                                      => false
        }
      )
    },
    // Pure parse of the engine's interceptUrl into the reported host (#264); runs on any host. The engine
    // value is ground truth (rift#425); a malformed / host-less URL yields None (caller → bindHost fallback).
    test("hostOf reads the engine's bound host from interceptUrl; None when absent") {
      assertTrue(
        EmbeddedIntercept.hostOf("http://127.0.0.1:38080").contains("127.0.0.1"),
        EmbeddedIntercept.hostOf("http://0.0.0.0:38080").contains("0.0.0.0"),
        // a specific-NIC ground truth is surfaced verbatim (it would win over a requested wildcard bind)
        EmbeddedIntercept.hostOf("http://192.168.1.5:38080").contains("192.168.1.5"),
        EmbeddedIntercept.hostOf("").isEmpty,         // no host → None
        EmbeddedIntercept.hostOf("not a url").isEmpty // unparseable → None
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
    },
    test(
      "trustStore(to = Some(path)) / trustStoreWithSystemCAs(to = …): export to a caller path, JVM-loadable there (#263)"
    ) {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          mc           <- ZIO.service[MockControl]
          _            <- mc.provision(cdnConfig).mapError(asT)
          ic           <- mc.intercept.mapError(u => new RuntimeException(u.message))
          base         <- ZIO.attemptBlocking(Files.createTempDirectory("rift-mnt"))
          caPath        = base.resolve("ca").resolve("intercept.p12")     // parent 'ca' does not exist yet
          mergedPath    = base.resolve("merged").resolve("intercept.jks") // parent 'merged' does not exist yet
          ca           <- ic.trustStore(to = Some(caPath)).mapError(asT)
          merged       <- ic.trustStoreWithSystemCAs(TrustStoreFormat.Jks, to = Some(mergedPath)).mapError(asT)
          caExists     <- ZIO.attemptBlocking(Files.exists(caPath))
          mergedExists <- ZIO.attemptBlocking(Files.exists(mergedPath))
          caN          <- anchorCount(ca)
          mergedN      <- anchorCount(merged)
        yield assertTrue(
          ca.path == caPath,
          caExists,
          caN >= 1, // CA-only store written to + loaded from the caller path
          merged.path == mergedPath,
          merged.format == TrustStoreFormat.Jks,
          mergedExists,
          mergedN > caN // merged store (CA + system anchors) written to + loaded from the caller path
        ))
          .provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("caller-provided CA is reused across instances: run A's truststore validates run B's intercepted TLS (#273)") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        for
          dir   <- ZIO.attemptBlocking(Files.createTempDirectory("rift-ca"))
          caCert = dir.resolve("ca.pem")
          caKey  = dir.resolve("ca.key")
          _     <- ZIO.attemptBlocking { Files.writeString(caCert, CaCertPem); Files.writeString(caKey, CaKeyPem) }
          cfg    = EmbeddedRift.InterceptConfig(caCert = Some(caCert), caKey = Some(caKey))
          // Run A (committed CA): export a truststore for that CA; A's engine is then torn down.
          tsA <- withEmbedded(cfg)(mc =>
                   mc.intercept.mapError(u => new RuntimeException(u.message)).flatMap(_.trustStore().mapError(asT))
                 )
          // Run B (the SAME committed CA): a SUT trusting tsA completes an intercepted call — only possible
          // because B's per-host leaf is signed by the shared CA that tsA already trusts.
          shared <- withEmbedded(cfg)(mc =>
                      for
                        space <- mc.provision(cdnConfig).mapError(asT).map(_.head)
                        ic    <- mc.intercept.mapError(u => new RuntimeException(u.message))
                        _     <- ic.redirectTo("cdn.example.com", space).mapError(asT)
                        port  <- ic.proxyPort.mapError(asT)
                        res   <- sutGet("https://cdn.example.com/config.json", port, tsA)
                      yield res
                    )
          // Negative control: an EPHEMERAL instance mints its leaf under a different CA → tsA does NOT trust it.
          ephemeral <- withEmbedded(EmbeddedRift.InterceptConfig())(mc =>
                         for
                           space <- mc.provision(cdnConfig).mapError(asT).map(_.head)
                           ic    <- mc.intercept.mapError(u => new RuntimeException(u.message))
                           _     <- ic.redirectTo("cdn.example.com", space).mapError(asT)
                           port  <- ic.proxyPort.mapError(asT)
                           res   <- sutGet("https://cdn.example.com/config.json", port, tsA).either
                         yield res
                       )
        yield assertTrue(
          shared._1 == 200,
          shared._2.contains("mocked"), // shared committed CA → run A's truststore validates run B's TLS
          // ephemeral CA → the same truststore does NOT validate: a TLS/cert-trust failure (lenient across
          // JDKs), not merely "some error" — pins the negative control to the CA-distrust cause.
          ephemeral.left.exists { t =>
            val s = (t.toString + Option(t.getCause).fold("")(_.toString)).toLowerCase
            s.contains("ssl") || s.contains("certif") || s.contains("handshake") || s.contains("trust")
          }
        )
    },
    test("misconfigured caller CA fails loudly on first intercept use: a lone caCert, and unreadable paths (#273)") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        for
          // (1) a lone caCert (no caKey) → validateCaPair rejects it with InvalidDefinition, via the real call.
          lone <-
            withEmbedded(EmbeddedRift.InterceptConfig(caCert = Some(java.nio.file.Path.of("/no/such/ca.pem"))))(mc =>
              mc.intercept.mapError(u => new RuntimeException(u.message)).flatMap(_.proxyPort.either)
            )
          // (2) both set but pointing at nonexistent PEM files → rift's CA load fails → a loud MockError, not a hang.
          bad <- withEmbedded(
                   EmbeddedRift.InterceptConfig(
                     caCert = Some(java.nio.file.Path.of("/no/such/ca.pem")),
                     caKey = Some(java.nio.file.Path.of("/no/such/ca.key"))
                   )
                 )(mc => mc.intercept.mapError(u => new RuntimeException(u.message)).flatMap(_.proxyPort.either))
        yield assertTrue(
          lone match
            case Left(MockError.InvalidDefinition(m)) => m.contains("both or neither")
            case _                                    => false
          ,
          bad.isLeft // a nonexistent CA file surfaces as a MockError (loud), never a silent success or hang
        )
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.sequential
