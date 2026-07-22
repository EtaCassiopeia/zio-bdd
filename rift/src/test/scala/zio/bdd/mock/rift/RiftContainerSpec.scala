package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.test.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}

/**
 * End-to-end acceptance against a REAL Rift container (#113 AC1/AC2), driven
 * over the SDK (`rift-scala-zio`, #285) rather than a hand-rolled admin client.
 * Opt-in: runs only when `RIFT_IT` is set, so the default `sbt test` stays
 * hermetic and Docker-free. Run it with `RIFT_IT=1 sbt rift/test`.
 */
object RiftContainerSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    respond = ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A backend-native Rift imposter document for the escape hatch (#119).
  // No top-level port: since #214 honours a raw doc's own port, a fixed out-of-pool port
  // (e.g. 9999) would bind inside the container on a port testcontainers never mapped and
  // be host-unreachable. Omit it so the imposter draws a mapped pool port.
  private val nativeImposter =
    """{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/native"}}],"responses":[{"is":{"statusCode":200,"body":"native!"}}]}]}"""

  // Imposter binds slightly after provision returns; retry the first hit briefly.
  private val upWithin: Schedule[Any, Any, Any] = Schedule.recurs(20) && Schedule.spaced(100.millis)

  private val http1Client: HttpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  private def httpGet(base: String, path: String): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val req = JHttpRequest.newBuilder(URI.create(base + path)).GET().build()
      val res = http1Client.send(req, JHttpResponse.BodyHandlers.ofString())
      (res.statusCode, res.body)
    }

  // Teardown is observed even through the client's pooled keep-alive connection:
  // as of Rift v0.2.0 (rift#207) DELETE /imposters/:port closes existing
  // connections, so a destroyed imposter stops serving. Poll briefly since the
  // socket close races the DELETE response.
  private def waitUntilGone(base: String): UIO[Boolean] =
    (ZIO.sleep(100.millis) *> httpGet(base, "/ping").either.map(_.isLeft))
      .repeatUntilEquals(true)
      .timeoutTo(false)(identity)(5.seconds)

  private val realSuite = suite("RiftContainerSpec (real Rift container)")(
    test("provisions, serves, records, and destroys a space") {
      for
        control  <- ZIO.service[MockControl]
        spaces   <- control.provision(pingSource)
        space     = spaces.head
        served   <- httpGet(space.baseUri, "/ping").retry(upWithin)
        recorded <- control.received(space)
        _        <- control.destroy(space)
        gone     <- waitUntilGone(space.baseUri)
      yield assertTrue(
        spaces.size == 1,
        served == (200, "pong"),                                          // serves
        recorded.exists(r => r.method == Method.Get && r.uri == "/ping"), // records
        gone                                                              // destroyed
      )
    },
    test("destroy(A) leaves B intact (never a global reset)") {
      for
        control <- ZIO.service[MockControl]
        a       <- control.provision(pingSource).map(_.head)
        b       <- control.provision(pingSource).map(_.head)
        _       <- httpGet(a.baseUri, "/ping").retry(upWithin)
        _       <- httpGet(b.baseUri, "/ping").retry(upWithin)
        _       <- control.destroy(a)
        gone    <- waitUntilGone(a.baseUri)
        bAfter  <- httpGet(b.baseUri, "/ping") // B must still serve after A is destroyed
      yield assertTrue(gone, bAfter == (200, "pong"))
    },
    test("a natively-provisioned space serves, records, and is space-local on destroy") {
      for
        control  <- ZIO.service[MockControl]
        spaces   <- control.provisionNative(NativeSpec.Rift(nativeImposter))
        space     = spaces.head
        served   <- httpGet(space.baseUri, "/native").retry(upWithin)
        recorded <- control.received(space)
        _        <- control.destroy(space)
        gone     <- waitUntilGone(space.baseUri)
      yield assertTrue(
        spaces.size == 1,
        served == (200, "native!"),                                         // serves
        recorded.exists(r => r.method == Method.Get && r.uri == "/native"), // records
        gone                                                                // space-local destroy
      )
    },
    test("an unmatched request returns 404, not Mountebank's 200-empty default (#165)") {
      for
        control <- ZIO.service[MockControl]
        space   <- control.provision(pingSource).map(_.head)
        _       <- httpGet(space.baseUri, "/ping").retry(upWithin) // ensure the imposter is up
        miss    <- httpGet(space.baseUri, "/no-such-path")
        _       <- control.destroy(space)
      yield assertTrue(miss._1 == 404)
    }
  ).provide(Provisioning.live, riftBackend) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  // Correlated isolation (#156): one shared imposter, spaces told apart by the
  // X-Mock-Space header. The SAME checks pass with a one-line layer swap (AC1),
  // and received(A) returns only A's traffic on the shared imposter (AC2).
  private val correlatedSuite = suite("RiftContainerSpec — Correlated (shared imposter)")(
    test("provision/serve/record/destroy pass on correlated() too (the one-line swap)") {
      for
        control <- ZIO.service[MockControl]
        space   <- control.provision(pingSource).map(_.head)
        resp    <- SutClient.make(space).send(Method.Get, "/ping").retry(upWithin) // inject stamps X-Mock-Space
        recv    <- control.received(space)
        _       <- control.destroy(space)
      yield assertTrue(
        control.isolation == Isolation.Correlated,
        resp.status == 200 && resp.body == "pong",                   // serves via inject
        recv.exists(r => r.method == Method.Get && r.uri == "/ping") // records (header-filtered)
      )
    },
    test("received(A) returns only A's traffic on the shared imposter (rift#201 filter)") {
      for
        control <- ZIO.service[MockControl]
        a       <- control.provision(pingSource).map(_.head)
        b       <- control.provision(pingSource).map(_.head)
        _       <- SutClient.make(a).send(Method.Get, "/ping").retry(upWithin)
        _       <- SutClient.make(b).send(Method.Get, "/ping").retry(upWithin)
        _       <- SutClient.make(b).send(Method.Get, "/ping")
        ra      <- control.received(a)
        rb      <- control.received(b)
      yield assertTrue(
        a.baseUri == b.baseUri, // one shared imposter
        ra.size == 1,           // only A's request
        rb.size == 2            // only B's
      )
    },
    test("an unmatched request returns 404 on the shared imposter (#165)") {
      for
        control <- ZIO.service[MockControl]
        space   <- control.provision(pingSource).map(_.head)
        _       <- SutClient.make(space).send(Method.Get, "/ping").retry(upWithin)
        miss    <- SutClient.make(space).send(Method.Get, "/no-such-path") // injected, but no stub matches
        _       <- control.destroy(space)
      yield assertTrue(miss.status == 404)
    }
  ).provide(Provisioning.live, correlatedBackend) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  // Two ways to reach a real Rift: the SDK's testcontainers transport spins one up (default), or —
  // when RIFT_ADMIN points at an already-running Rift admin URL whose imposter ports are mapped 1:1
  // to localhost — connect to it directly. The latter exercises the same adapter without depending on
  // the host's testcontainers Docker discovery.
  private def riftBackend: ZLayer[Provisioning, MockError, MockControl] =
    sys.env.get("RIFT_ADMIN") match
      case Some(admin) => Rift.connect(admin, (4545 until 4561).toList)(p => s"http://localhost:$p")
      case None        => Rift.managed()

  private def correlatedBackend: ZLayer[Provisioning, MockError, MockControl] =
    sys.env.get("RIFT_ADMIN") match
      case Some(admin) =>
        Rift.connect(admin, (4545 until 4561).toList, RiftMode.correlated)(p => s"http://localhost:$p")
      case None => Rift.managed(mode = RiftMode.correlated)

  def spec =
    if sys.env.contains("RIFT_IT") then suite("Rift container ITs")(realSuite, correlatedSuite)
    else suite("RiftContainerSpec (skipped — set RIFT_IT=1 to run against a real Rift container)")()
