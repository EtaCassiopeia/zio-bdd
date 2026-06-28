package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.{Client, Request, URL}
import zio.test.*

/**
 * End-to-end acceptance against a REAL Rift container (#113 AC1/AC2). Opt-in:
 * runs only when `RIFT_IT` is set, so the default `sbt test` stays hermetic and
 * Docker-free. Run it with `RIFT_IT=1 sbt rift/test`.
 */
object RiftContainerSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    respond = ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A backend-native Rift imposter document for the escape hatch (#119).
  private val nativeImposter =
    """{"port":9999,"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/native"}}],"responses":[{"is":{"statusCode":200,"body":"native!"}}]}]}"""

  // Imposter binds slightly after provision returns; retry the first hit briefly.
  private val upWithin: Schedule[Any, Any, Any] = Schedule.recurs(20) && Schedule.spaced(100.millis)

  private def httpGet(base: String, path: String): ZIO[Client, Throwable, (Int, String)] =
    for
      url  <- ZIO.fromEither(URL.decode(base + path))
      resp <- Client.batched(Request.get(url))
      body <- resp.body.asString
    yield (resp.status.code, body)

  // Teardown is observed even through the client's pooled keep-alive connection:
  // as of Rift v0.2.0 (rift#207) DELETE /imposters/:port closes existing
  // connections, so a destroyed imposter stops serving. Poll briefly since the
  // socket close races the DELETE response.
  private def waitUntilGone(base: String): ZIO[Client, Nothing, Boolean] =
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
    }
  ).provideSome[Client](Provisioning.live, riftBackend) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  // Two ways to reach a real Rift: testcontainers spins one up (default), or —
  // when RIFT_ADMIN points at an already-running Rift admin URL whose imposter
  // ports are mapped 1:1 to localhost — connect to it directly. The latter
  // exercises the same adapter without depending on the host's testcontainers
  // Docker discovery.
  private def riftBackend: ZLayer[Client & Provisioning, MockError, MockControl] =
    sys.env.get("RIFT_ADMIN") match
      case Some(admin) => Rift.connect(admin, (4545 until 4561).toList)(p => s"http://localhost:$p")
      case None        => Rift.managed()

  def spec =
    if sys.env.contains("RIFT_IT") then realSuite.provideShared(Client.default)
    else suite("RiftContainerSpec (skipped — set RIFT_IT=1 to run against a real Rift container)")()
