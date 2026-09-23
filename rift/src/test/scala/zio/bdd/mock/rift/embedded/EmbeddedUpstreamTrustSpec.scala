package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.RiftMode
import zio.test.*

import com.sun.net.httpserver.{HttpsConfigurator, HttpsServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.security.KeyStore
import javax.net.ssl.{KeyManagerFactory, SSLContext}

import EmbeddedRift.UpstreamTrust

/**
 * Outbound TLS trust for proxy stubs (engine 0.18.0, rift-scala
 * `upstreamTrust`): a `proxyRecord` proxy dialing an HTTPS origin whose
 * certificate chains to a private CA fails by default, and succeeds once the
 * embedded engine is given that CA (`CaFile`) or told to skip verification
 * (`SkipVerify`). A bad trust setting fails the layer with a typed
 * `InvalidDefinition` before any engine starts, so those cases need no engine.
 */
object EmbeddedUpstreamTrustSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private val StorePass = "changeit"

  // A private CA plus a localhost leaf it signed, minted with the JDK's own keytool so the test needs
  // no crypto library. Returns (the leaf's PKCS#12 keystore holding the full chain, the CA as PEM).
  private def mintPrivateCa(dir: Path): Task[(Path, Path)] =
    ZIO.attemptBlocking {
      val keytool = Path.of(java.lang.System.getProperty("java.home"), "bin", "keytool").toString
      def run(args: String*): Unit =
        val p   = new ProcessBuilder((keytool +: args)*).redirectErrorStream(true).start()
        val out = new String(p.getInputStream.readAllBytes())
        if p.waitFor() != 0 then throw new RuntimeException(s"keytool ${args.head} failed: $out")
      val ca    = dir.resolve("ca.p12").toString
      val leaf  = dir.resolve("leaf.p12").toString
      val caPem = dir.resolve("ca.pem")
      val csr   = dir.resolve("leaf.csr").toString
      val crt   = dir.resolve("leaf.pem").toString
      val store = Seq("-storepass", StorePass, "-storetype", "PKCS12")
      run(
        Seq("-genkeypair", "-alias", "ca", "-keyalg", "RSA", "-keysize", "2048", "-validity", "2") ++
          Seq("-dname", "CN=zio-bdd-upstream-test-ca", "-ext", "bc:c", "-keystore", ca) ++ store*
      )
      run(Seq("-exportcert", "-rfc", "-alias", "ca", "-keystore", ca, "-file", caPem.toString) ++ store*)
      run(
        Seq("-genkeypair", "-alias", "leaf", "-keyalg", "RSA", "-keysize", "2048", "-validity", "2") ++
          Seq("-dname", "CN=localhost", "-keystore", leaf) ++ store*
      )
      run(Seq("-certreq", "-alias", "leaf", "-keystore", leaf, "-file", csr) ++ store*)
      run(
        Seq("-gencert", "-rfc", "-alias", "ca", "-keystore", ca, "-infile", csr, "-outfile", crt) ++
          Seq("-ext", "SAN=dns:localhost,ip:127.0.0.1", "-ext", "EKU=serverAuth", "-validity", "2") ++ store*
      )
      run(Seq("-importcert", "-noprompt", "-alias", "ca", "-file", caPem.toString, "-keystore", leaf) ++ store*)
      run(Seq("-importcert", "-noprompt", "-alias", "leaf", "-file", crt, "-keystore", leaf) ++ store*)
      (Path.of(leaf), caPem)
    }

  // An HTTPS origin serving `body` on every path, presenting the private-CA-signed leaf.
  private def httpsOrigin(keystore: Path, body: String): ZIO[Scope, Throwable, Int] =
    ZIO
      .acquireRelease(ZIO.attemptBlocking {
        val ks = KeyStore.getInstance("PKCS12")
        val in = Files.newInputStream(keystore)
        try ks.load(in, StorePass.toCharArray)
        finally in.close()
        val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
        kmf.init(ks, StorePass.toCharArray)
        val ssl = SSLContext.getInstance("TLS")
        ssl.init(kmf.getKeyManagers, null, null)
        val server = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
        server.setHttpsConfigurator(new HttpsConfigurator(ssl))
        server.createContext(
          "/",
          ex =>
            val bytes = body.getBytes("UTF-8")
            ex.sendResponseHeaders(200, bytes.length.toLong)
            ex.getResponseBody.write(bytes)
            ex.close()
        )
        server.start()
        server
      })(s => ZIO.succeedBlocking(s.stop(0)))
      .map(_.getAddress.getPort)

  private def get(base: String, path: String): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()
      val resp =
        client.send(HttpRequest.newBuilder(URI.create(base + path)).GET().build(), HttpResponse.BodyHandlers.ofString())
      (resp.statusCode, resp.body)
    }

  // Proxy `/secure` on a fresh space to the HTTPS origin, through an engine built with `trust`.
  private def proxiedVia(trust: Option[UpstreamTrust], originPort: Int): Task[(Int, String)] =
    ZIO.scoped {
      EmbeddedRift
        .layer(RiftMode.PerInstance, EmbeddedRift.InterceptConfig(), trust)
        .build
        .provideSome[Scope](Provisioning.live)
        .mapError(asT)
        .flatMap { env =>
          val control = env.get[MockControl]
          for
            front <- control.provision(MockSource.Dsl(MockSpec(Nil))).mapError(asT).map(_.head)
            api   <- control.proxyRecord.mapError(u => new RuntimeException(u.message))
            _ <- api
                   .proxy(front, RequestMatch(path = PathMatch.Exact("/secure")), s"https://localhost:$originPort")
                   .mapError(asT)
            res <- get(front.baseUri, "/secure")
          yield res
        }
    }

  def spec = suite("EmbeddedUpstreamTrust")(
    test("a CaPem with no certificate block fails the layer with a typed InvalidDefinition") {
      for exit <-
          ZIO
            .scoped(
              EmbeddedRift
                .layer(RiftMode.PerInstance, EmbeddedRift.InterceptConfig(), Some(UpstreamTrust.CaPem("not a pem")))
                .build
            )
            .provide(Provisioning.live)
            .exit
      yield assertTrue(exit match
        case Exit.Failure(c) => c.failureOption.exists { case MockError.InvalidDefinition(_) => true; case _ => false }
        case _               => false
      )
    },
    test("a CaFile that does not exist fails the layer with a typed InvalidDefinition") {
      for exit <- ZIO
                    .scoped(
                      EmbeddedRift
                        .layer(
                          RiftMode.PerInstance,
                          EmbeddedRift.InterceptConfig(),
                          Some(UpstreamTrust.CaFile(Path.of("/nonexistent/zio-bdd-ca.pem")))
                        )
                        .build
                    )
                    .provide(Provisioning.live)
                    .exit
      yield assertTrue(exit match
        case Exit.Failure(c) => c.failureOption.exists { case MockError.InvalidDefinition(_) => true; case _ => false }
        case _               => false
      )
    },
    test("a proxy to a private-CA HTTPS origin fails by default, and succeeds with CaFile or SkipVerify") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped {
          for
            dir <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("zio-bdd-upstream-ca")))(d =>
                     ZIO.succeedBlocking(d.toFile.listFiles.foreach(_.delete())) *> ZIO.succeedBlocking(
                       Files.deleteIfExists(d)
                     )
                   )
            minted    <- mintPrivateCa(dir)
            port      <- httpsOrigin(minted._1, "secure-upstream")
            untrusted <- proxiedVia(None, port)
            viaCa     <- proxiedVia(Some(UpstreamTrust.CaFile(minted._2)), port)
            viaSkip   <- proxiedVia(Some(UpstreamTrust.SkipVerify), port)
          yield assertTrue(
            untrusted != (200, "secure-upstream"), // public roots only: the private CA is not trusted
            viaCa == (200, "secure-upstream"),
            viaSkip == (200, "secure-upstream")
          )
        }
    }
  ) @@ TestAspect.withLiveClock
