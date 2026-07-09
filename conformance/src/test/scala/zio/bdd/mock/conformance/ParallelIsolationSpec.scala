package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.{Rift, RiftMode}
import zio.bdd.mock.wiremock.WireMock
import zio.http.Client
import zio.test.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}

/**
 * The headline isolation contract (#126) under genuine concurrency, run
 * repeatedly (`nonFlaky`) against EVERY isolation mode each adapter advertises:
 * WireMock correlated + perInstance (always, in-process) and Rift perInstance +
 * correlated (gated behind `RIFT_IT`, like
 * [[zio.bdd.mock.rift.RiftContainerSpec]] — the `rift-it` CI job runs this spec
 * under `RIFT_IT`, so the Rift backends + the native-source isolation test
 * execute there, not in the default Docker-free lane).
 *
 * Tests within a backend run sequentially so only one N-space wave is live at a
 * time — Rift PerInstance gives each space its own imposter+port, so a generous
 * pool (> N) must cover one wave, not all tests at once. Each wave is scoped,
 * so its spaces are torn down (ports freed) before the next test/repetition.
 */
object ParallelIsolationSpec extends ZIOSpecDefault:

  private val N    = 50 // "50+ concurrent spaces"
  private val Reps = 3  // surface races without exploding the real-container CI time

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  // A GET that does NOT apply space.inject — used to prove inject is load-bearing.
  private def rawGet(baseUri: String, path: String): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val req  = JHttpRequest.newBuilder(URI.create(baseUri + path)).GET().build()
      val resp = HttpClient.newHttpClient().send(req, JHttpResponse.BodyHandlers.ofString())
      (resp.statusCode, resp.body)
    }

  private def srcText(path: String, body: String): MockSource =
    MockSource.Dsl(
      MockSpec(
        List(MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status = 200, body = Body.Text(body))))
      )
    )

  // A native Rift imposter serving `body` at /fp, with no pinned port. Since #214 honours a
  // raw doc's own top-level port, two provisions must not share one (they'd collide, and an
  // out-of-pool port is host-unreachable on the container adapter) — omit it so each draws a
  // distinct auto port. Fixed-port *honouring* is covered by RiftMockControlSpec (container,
  // hermetic) and EmbeddedRiftCapabilitiesSpec (embedded).
  private def riftNativeSource(body: String): String =
    s"""{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/fp"}}],"responses":[{"is":{"statusCode":200,"body":"$body"}}]}]}"""

  // provision -> destroy on scope close (even on failure), so the pool is freed.
  // ignoreLogged (not bare ignore): teardown must not override the verdict, but a
  // destroy failure that could starve the pool across waves should be diagnosable.
  private def serving(control: MockControl, path: String, body: String): RIO[Scope, MockSpace] =
    ZIO.acquireRelease(control.provision(srcText(path, body)).mapError(asT).map(_.head))(s =>
      control.destroy(s).ignoreLogged
    )

  private def servingNative(control: MockControl, imposter: String): RIO[Scope, MockSpace] =
    ZIO.acquireRelease(control.provisionNative(NativeSpec.Rift(imposter)).mapError(asT).map(_.head))(s =>
      control.destroy(s).ignoreLogged
    )

  private final case class Backend(
    name: String,
    layer: ZLayer[Any, Throwable, MockControl],
    isolation: Isolation,
    native: Boolean
  )

  private def backendSuite(b: Backend) =
    val nativeOnly = if b.native then TestAspect.identity else TestAspect.ignore
    suite(b.name)(
      test(s"$N concurrent spaces each serve only their own body (no behaviour or recording cross-talk)") {
        ZIO.scoped {
          for
            control <- ZIO.service[MockControl]
            spaces  <- ZIO.foreachPar(1 to N)(i => serving(control, "/p", s"body-$i").map(i -> _))
            results <- ZIO.foreachPar(spaces) { (i, s) =>
                         for
                           resp <- SutClient.make(s).send(Method.Get, "/p")
                           recv <- control.received(s).mapError(asT)
                         yield (i, resp.body, recv.size, recv.forall(_.uri == "/p"))
                       }
          yield assertTrue(
            results.length == N,                                   // all N spaces actually ran (not a short list)
            results.forall((i, body, _, _) => body == s"body-$i"), // own body — no behaviour cross-talk
            results.forall((_, _, size, ok) => size == 1 && ok)    // own single recording — no recording cross-talk
          )
        }
      } @@ TestAspect.nonFlaky(Reps),
      test("overlays applied to a subset under concurrency stay space-local") {
        ZIO.scoped {
          for
            control <- ZIO.service[MockControl]
            spaces  <- ZIO.foreachPar(1 to N)(i => serving(control, "/p", s"base-$i").map(i -> _))
            _ <- ZIO.foreachPar(spaces.filter((i, _) => i % 2 == 0)) { (i, s) =>
                   control
                     .addRule(
                       s,
                       MockRule(
                         RequestMatch(path = PathMatch.Exact("/p")),
                         ResponseDef(status = 200, body = Body.Text(s"overlay-$i"))
                       ),
                       Priority.Overlay
                     )
                     .mapError(asT)
                 }
            bodies <- ZIO.foreachPar(spaces)((i, s) => SutClient.make(s).send(Method.Get, "/p").map(i -> _.body))
          yield assertTrue(
            bodies.length == N,
            bodies.count((_, body) => body.startsWith("overlay")) == N / 2, // the subset was overlaid, no more
            bodies.forall((i, body) => body == (if i % 2 == 0 then s"overlay-$i" else s"base-$i"))
          )
        }
      } @@ TestAspect.nonFlaky(Reps),
      // RIFT_IT-only: a raw native source is a Rift concept; WireMock rejects it.
      test("two native (unpinned) sources provisioned in parallel isolate on distinct ports") {
        ZIO.scoped {
          for
            a  <- ZIO.service[MockControl].flatMap(servingNative(_, riftNativeSource("A")))
            b2 <- ZIO.service[MockControl].flatMap(servingNative(_, riftNativeSource("B")))
            ra <- SutClient.make(a).send(Method.Get, "/fp")
            rb <- SutClient.make(b2).send(Method.Get, "/fp")
          yield assertTrue(a.baseUri != b2.baseUri, ra.body == "A", rb.body == "B")
        }
      } @@ nativeOnly @@ TestAspect.nonFlaky(Reps),
      test("inject is load-bearing (Correlated drops an un-injected request; PerInstance inject is identity)") {
        ZIO.scoped {
          for
            control <- ZIO.service[MockControl]
            s       <- serving(control, "/p", "body")
            _       <- SutClient.make(s).send(Method.Get, "/p") // injected -> reaches + records
            result <- b.isolation match
                        // A raw GET without the correlation header must not land in the space.
                        case Isolation.Correlated =>
                          rawGet(s.baseUri, "/p") *>
                            control
                              .received(s)
                              .mapError(asT)
                              .map(recv => assertTrue(recv.size == 1, recv.forall(_.uri == "/p")))
                        // PerInstance has nothing to drop — inject is identity, the load-bearing invariant there.
                        case Isolation.PerInstance =>
                          val req = HttpRequest(Method.Get, s.baseUri + "/p")
                          ZIO.succeed(assertTrue(s.inject(req) == req))
          yield result
        }
      }
    ).provideShared(b.layer) @@ TestAspect.sequential

  private val riftEnabled = sys.env.contains("RIFT_IT")

  private val wiremockBackends = List(
    Backend("wiremock-correlated", Provisioning.live >>> WireMock.correlated(), Isolation.Correlated, native = false),
    Backend("wiremock-perinstance", Provisioning.live >>> WireMock.perInstance, Isolation.PerInstance, native = false)
  )

  // Rift PerInstance needs a pool wide enough for one N-space wave (+ the 2 native spaces).
  private val riftBackends = List(
    Backend(
      "rift-perinstance",
      (Client.default ++ Provisioning.live) >>> Rift.managed(poolSize = N + 8).mapError(asT),
      Isolation.PerInstance,
      native = true
    ),
    Backend(
      "rift-correlated",
      (Client.default ++ Provisioning.live) >>> Rift.managed(poolSize = 16, mode = RiftMode.correlated).mapError(asT),
      Isolation.Correlated,
      native = true
    )
  )

  private val backends = wiremockBackends ++ (if riftEnabled then riftBackends else Nil)

  def spec =
    suite("ParallelIsolation")(backends.map(backendSuite)*) @@ TestAspect.sequential @@ TestAspect.withLiveClock
