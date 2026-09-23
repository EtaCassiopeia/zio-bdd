package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*

import com.sun.net.httpserver.{HttpsConfigurator, HttpsServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.security.KeyStore
import javax.net.ssl.{KeyManagerFactory, SSLContext}

/**
 * Test fixture for outbound TLS trust (`upstreamTrust`): an HTTPS origin whose
 * certificate chains to a private CA, minted with the JDK's own keytool so the
 * tests need no crypto library. `caPem` is that CA, for `UpstreamTrust.CaFile`.
 */
final case class PrivateCaOrigin(port: Int, caPem: Path)

object PrivateCaOrigin:

  private val StorePass = "changeit"

  /**
   * Start an HTTPS origin serving `body` on every path. The leaf certificate is
   * valid for `localhost`, `127.0.0.1` and `host.testcontainers.internal` (the
   * name a container uses to reach a host port exposed with
   * `Testcontainers.exposeHostPorts`). Stopped, and its files removed, when the
   * scope closes.
   */
  def start(body: String): ZIO[Scope, Throwable, PrivateCaOrigin] =
    for
      dir <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("zio-bdd-upstream-ca")))(d =>
               ZIO.succeedBlocking {
                 d.toFile.listFiles.foreach(_.delete())
                 Files.deleteIfExists(d)
               }
             )
      minted <- mint(dir)
      port   <- serve(minted._1, body)
    yield PrivateCaOrigin(port, minted._2)

  // Returns (the leaf's PKCS#12 keystore holding the full chain, the CA as PEM).
  private def mint(dir: Path): Task[(Path, Path)] =
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
      val san   = "SAN=dns:localhost,dns:host.testcontainers.internal,ip:127.0.0.1"
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
          Seq("-ext", san, "-ext", "EKU=serverAuth", "-validity", "2") ++ store*
      )
      run(Seq("-importcert", "-noprompt", "-alias", "ca", "-file", caPem.toString, "-keystore", leaf) ++ store*)
      run(Seq("-importcert", "-noprompt", "-alias", "leaf", "-file", crt, "-keystore", leaf) ++ store*)
      (Path.of(leaf), caPem)
    }

  // Bound on all interfaces so a container reaching the host through the testcontainers gateway connects.
  private def serve(keystore: Path, body: String): ZIO[Scope, Throwable, Int] =
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
        val server = HttpsServer.create(new InetSocketAddress("0.0.0.0", 0), 0)
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

  /**
   * Proxy `/secure` on a fresh space of `control` to `upstream`, then GET it
   * through the space: the (status, body) the SUT would see.
   */
  def proxiedThrough(control: MockControl, upstream: String): Task[(Int, String)] =
    for
      front <- control.provision(MockSource.Dsl(MockSpec(Nil))).mapError(asT).map(_.head)
      api   <- control.proxyRecord.mapError(u => new RuntimeException(u.message))
      _     <- api.proxy(front, RequestMatch(path = PathMatch.Exact("/secure")), upstream).mapError(asT)
      res <- ZIO.attemptBlocking {
               val client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()
               val req    = HttpRequest.newBuilder(URI.create(front.baseUri + "/secure")).GET().build()
               val resp   = client.send(req, HttpResponse.BodyHandlers.ofString())
               (resp.statusCode, resp.body)
             }
    yield res

  /** True iff `exit` failed with a typed [[MockError.InvalidDefinition]]. */
  def isInvalidDefinition(exit: Exit[MockError, Any]): Boolean = exit match
    case Exit.Failure(c) => c.failureOption.exists { case MockError.InvalidDefinition(_) => true; case _ => false }
    case _               => false

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")
