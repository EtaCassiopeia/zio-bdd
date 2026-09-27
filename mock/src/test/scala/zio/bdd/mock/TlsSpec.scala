package zio.bdd.mock

import zio.*
import zio.bdd.mock.dsl.*
import zio.test.*

import com.sun.net.httpserver.{HttpsConfigurator, HttpsParameters, HttpsServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import TlsTestPems.*

object TlsSpec extends ZIOSpecDefault:

  private val serverMaterial = TlsMaterial(serverCert, serverKey)

  // A JDK HTTPS origin serving GET / -> 200 "secure", presenting the test server certificate. With
  // `clientCa` it demands a client certificate chaining to that CA (the mTLS shape).
  private def httpsOrigin(clientCa: Option[String]): ZIO[Scope, Throwable, Int] =
    ZIO
      .acquireRelease(ZIO.attemptBlocking {
        val pass = "changeit".toCharArray
        val ks   = Tls.keyStore(serverMaterial, pass).fold(e => throw new IllegalStateException(e), identity)
        val kmf  = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
        kmf.init(ks, pass)
        val tms = clientCa.map { ca =>
          val ts  = Tls.trustStore(List(ca)).fold(e => throw new IllegalStateException(e), identity)
          val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
          tmf.init(ts)
          tmf.getTrustManagers
        }
        val ctx = SSLContext.getInstance("TLS")
        ctx.init(kmf.getKeyManagers, tms.orNull, null)
        val server = HttpsServer.create(new InetSocketAddress("localhost", 0), 0)
        server.setHttpsConfigurator(new HttpsConfigurator(ctx) {
          override def configure(params: HttpsParameters): Unit =
            val p = ctx.getDefaultSSLParameters
            p.setNeedClientAuth(clientCa.isDefined)
            params.setSSLParameters(p)
        })
        server.createContext(
          "/",
          exchange => {
            val bytes = "secure".getBytes(StandardCharsets.UTF_8)
            exchange.sendResponseHeaders(200, bytes.length.toLong)
            exchange.getResponseBody.write(bytes)
            exchange.close()
          }
        )
        server.start()
        server
      })(s => ZIO.succeed(s.stop(0)))
      .map(_.getAddress.getPort)

  private def spaceAt(port: Int): MockSpace = MockSpace(s"https://localhost:$port", identity, SpaceId("tls"))

  private def isInvalid(e: MockError, fragment: String): Boolean = e match
    case MockError.InvalidDefinition(reason) => reason.contains(fragment)
    case _                                   => false

  // A TLS refusal surfaces as an IOException (SSLHandshakeException, or a reset after a failed
  // client-certificate check) — not some unrelated failure.
  private def refusedByTransport(exit: Exit[Throwable, SutResponse]): Boolean =
    exit.causeOption.flatMap(_.failureOption).exists(_.isInstanceOf[java.io.IOException])

  private val notPem = "this is not a PEM document"
  // A PKCS#1 ("traditional") EC key header: syntactically a PEM block, but not the PKCS#8 form the
  // adapters need, so it must be refused with a hint rather than a raw KeyFactory error.
  private val pkcs1Key =
    "-----BEGIN EC PRIVATE KEY-----\nMHcCAQEEIBkZ\n-----END EC PRIVATE KEY-----\n"

  def spec = suite("Tls (#343)")(
    suite("SutClient over TLS")(
      test("a client trusting the test CA completes a round-trip to an HTTPS origin") {
        for
          port <- httpsOrigin(None)
          ssl  <- Tls.trust(caCert)
          resp <- SutClient.make(spaceAt(port), ssl).send(Method.Get, "/")
        yield assertTrue(resp.status == 200, resp.body == "secure")
      },
      test("the default SutClient (JVM trust) refuses the test-CA origin") {
        for
          port <- httpsOrigin(None)
          exit <- SutClient.make(spaceAt(port)).send(Method.Get, "/").exit
        yield assertTrue(refusedByTransport(exit))
      },
      test("a client trusting a different CA is refused") {
        for
          port <- httpsOrigin(None)
          ssl  <- Tls.trust(otherCaCert)
          exit <- SutClient.make(spaceAt(port), ssl).send(Method.Get, "/").exit
        yield assertTrue(refusedByTransport(exit))
      },
      test("SutClient.layer(space, ssl) binds the TLS client") {
        for
          port <- httpsOrigin(None)
          ssl  <- Tls.trust(caCert)
          resp <- ZIO.serviceWithZIO[SutClient](_.send(Method.Get, "/")).provide(SutClient.layer(spaceAt(port), ssl))
        yield assertTrue(resp.body == "secure")
      },
      test("clientIdentity presents a certificate an mTLS origin accepts") {
        for
          port <- httpsOrigin(Some(caCert))
          ssl  <- Tls.clientIdentity(clientCert, clientKey, caCert)
          resp <- SutClient.make(spaceAt(port), ssl).send(Method.Get, "/")
        yield assertTrue(resp.status == 200)
      },
      test("an mTLS origin refuses a client that presents no certificate") {
        for
          port <- httpsOrigin(Some(caCert))
          ssl  <- Tls.trust(caCert)
          exit <- SutClient.make(spaceAt(port), ssl).send(Method.Get, "/").exit
        yield assertTrue(refusedByTransport(exit))
      },
      test("an mTLS origin refuses a client certificate from an untrusted CA") {
        for
          port <- httpsOrigin(Some(caCert))
          ssl  <- Tls.clientIdentity(otherClientCert, otherClientKey, caCert)
          exit <- SutClient.make(spaceAt(port), ssl).send(Method.Get, "/").exit
        yield assertTrue(refusedByTransport(exit))
      }
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds),
    suite("PEM helpers fail typed")(
      test("trust refuses a document with no certificate") {
        Tls.trust(notPem).flip.map(e => assertTrue(isInvalid(e, "certificate")))
      },
      test("trust refuses when any one of several anchors is malformed") {
        Tls.trust(caCert, notPem).flip.map(e => assertTrue(isInvalid(e, "certificate")))
      },
      test("clientIdentity refuses a PKCS#1 key, naming PKCS#8") {
        Tls.clientIdentity(clientCert, pkcs1Key, caCert).flip.map(e => assertTrue(isInvalid(e, "PKCS#8")))
      },
      test("clientIdentity refuses a key that is not PEM at all") {
        Tls.clientIdentity(clientCert, notPem, caCert).flip.map(e => assertTrue(isInvalid(e, "private key")))
      },
      test("clientIdentity refuses a malformed client certificate") {
        Tls.clientIdentity(notPem, clientKey, caCert).flip.map(e => assertTrue(isInvalid(e, "certificate")))
      }
    ),
    suite("DSL")(
      test("https attaches server material with client auth off") {
        val spec = mock().https(serverCert, serverKey)
        assertTrue(spec.tls == Some(Tls(TlsMaterial(serverCert, serverKey), ClientAuth.Off)))
      },
      test("mutualTls attaches server material and every trusted client CA, in order") {
        val spec = mock().mutualTls(serverCert, serverKey, caCert, otherCaCert)
        assertTrue(
          spec.tls == Some(
            Tls(TlsMaterial(serverCert, serverKey), ClientAuth.Required(NonEmptyChunk(caCert, otherCaCert)))
          )
        )
      },
      test("a spec without https carries no TLS") {
        assertTrue(mock().tls.isEmpty, mock().onPort(9000).tls.isEmpty)
      }
    ),
    suite("Provisioning")(
      test("a DSL source carries its TLS settings through normalization") {
        val tls = Tls(serverMaterial, ClientAuth.Required(NonEmptyChunk(caCert)))
        for
          prov <- Provisioning.make
          ns   <- prov.normalize(MockSource.Dsl(mock().withTls(tls)))
        yield assertTrue(ns.map(_.tls) == List(Some(tls)))
      },
      test("a raw source carries no TLS settings") {
        for
          prov <- Provisioning.make
          ns   <- prov.normalize(MockSource.Json("{}"))
        yield assertTrue(ns.map(_.tls) == List(None))
      },
      test("a malformed server certificate fails provisioning with InvalidDefinition") {
        for
          prov <- Provisioning.make
          err  <- prov.normalize(MockSource.Dsl(mock().https(notPem, serverKey))).flip
        yield assertTrue(isInvalid(err, "certificate"))
      },
      test("a PKCS#1 server key fails provisioning, naming PKCS#8") {
        for
          prov <- Provisioning.make
          err  <- prov.normalize(MockSource.Dsl(mock().https(serverCert, pkcs1Key))).flip
        yield assertTrue(isInvalid(err, "PKCS#8"))
      },
      test("a server key that does not match the certificate fails provisioning") {
        for
          prov <- Provisioning.make
          err  <- prov.normalize(MockSource.Dsl(mock().https(serverCert, clientKey))).flip
        yield assertTrue(isInvalid(err, "does not match"))
      },
      test("a malformed client CA fails provisioning with InvalidDefinition") {
        for
          prov <- Provisioning.make
          err  <- prov.normalize(MockSource.Dsl(mock().mutualTls(serverCert, serverKey, notPem))).flip
        yield assertTrue(isInvalid(err, "certificate"))
      }
    )
  )
