package samples.mock

import samples.mock.support.{Backends, MockState, MockSupport}
import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{InetSocketAddress, ProxySelector, URI}
import java.nio.file.Files
import java.security.KeyStore
import javax.net.ssl.{SSLContext, TrustManagerFactory}

/**
 * 24 — the portable MockControl `Intercept` capability (#219, Plane B mirror of
 * Plane A feature 12, #54): an intercept-capable backend (embedded FFM) runs a
 * TLS-MITM forward proxy; a SUT trusting the exported truststore and proxying
 * HTTPS through the intercept port has its call to a hard-coded external host
 * intercepted and answered by the mock (`respondWith` inline, or `redirectTo` a
 * mock space's imposter). Tagged `@intercept` so it runs only on backends that
 * advertise the capability (excluded from the wiremock + rift-external aliases,
 * which don't).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/24-intercept.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Intercept24Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:
  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

  /** The intercepted response, stashed for the assertion steps. */
  private final case class InterceptResult(status: Int, body: String)

  // The Intercept capability, requiring it up front so an unsupported backend is rejected loudly
  // (rather than a confusing NPE later). Both `require` and `intercept` fail with the typed
  // `Unsupported`; surface it on the Throwable channel like the rest of MockSupport.
  private val intercept: ZIO[MockControl, Throwable, Intercept] =
    ZIO.serviceWithZIO[MockControl](mc =>
      (mc.require(Capability.Intercept) *> mc.intercept)
        .mapError(u => new RuntimeException(u.message))
    )

  private def interceptResult: IO[Throwable, InterceptResult] =
    Stage
      .get[InterceptResult]
      .mapError(e => new IllegalStateException(s"no intercepted response — call through the proxy first ($e)"))

  /**
   * The stand-in SUT: a JDK HttpClient trusting the exported truststore and
   * proxying HTTPS through the intercept port (`CONNECT`-tunnelled, TLS-MITM'd
   * by the backend). Mirrors zio-bdd's own `EmbeddedInterceptSpec`. No DNS for
   * the host is needed — the proxy tunnels by name.
   */
  private def sutGet(url: String, proxyPort: Int, ts: TrustStore): Task[InterceptResult] =
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
        client.send(
          HttpRequest.newBuilder(URI.create(url)).GET().build(),
          HttpResponse.BodyHandlers.ofString()
        )
      InterceptResult(resp.statusCode, resp.body)
    }

  // ── steps ────────────────────────────────────────────────────────────────────────────────

  Given("the mock intercepts " / string / " answering " / int / " " / string) {
    (host: String, code: Int, body: String) =>
      intercept.flatMap(ic =>
        ic.respondWith(host, InterceptStub(status = code, body = Some(body)))
          .mapError(e => new RuntimeException(s"intercept respondWith: $e"))
      )
  }

  Given("a mock stub " / string / " " / string / " returns " / int / " text " / string) {
    (m: String, path: String, code: Int, body: String) =>
      methodOf(m).flatMap(method => stageRule(request(method, path).respondWith(status(code).text(body))))
  }

  Given("the mock intercepts " / string / " redirecting to the mock") { (host: String) =>
    for
      space <- activeSpace
      ic    <- intercept
      _ <- ic
             .redirectTo(host, space)
             .mapError(e => new RuntimeException(s"intercept redirectTo: $e"))
    yield ()
  }

  When("the SUT calls " / string / " through the intercept proxy") { (url: String) =>
    for
      ic   <- intercept
      port <- ic.proxyPort.mapError(e => new RuntimeException(s"intercept proxyPort: $e"))
      ts   <- ic.trustStore().mapError(e => new RuntimeException(s"intercept trustStore: $e"))
      res  <- sutGet(url, port, ts)
      _    <- Stage.put(res)
    yield ()
  }

  Then("the intercepted status is " / int) { (code: Int) =>
    interceptResult.flatMap(r =>
      Assertions.assertTrue(r.status == code, s"expected intercepted status $code, got ${r.status}")
    )
  }

  Then("the intercepted body is " / string) { (body: String) =>
    interceptResult.flatMap(r =>
      Assertions.assertTrue(r.body == body, s"expected intercepted body '$body', got '${r.body}'")
    )
  }
