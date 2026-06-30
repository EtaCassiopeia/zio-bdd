package zio.bdd.mock.wiremock

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import zio.test.*

import java.net.{URI, http as jhttp}

object WireMockControlSpec extends ZIOSpecDefault:

  private val pingRule =
    MockRule(
      RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
      ResponseDef(status = 200, body = Body.Text("pong"))
    )
  private val healthRule =
    MockRule(
      RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/health")),
      ResponseDef(status = 200, body = Body.Text("ok"))
    )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  private val baseHealth = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/h")),
    ResponseDef(status = 200, body = Body.Text("base"))
  )
  private val overlayHealth = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/h")),
    ResponseDef(status = 200, body = Body.Text("overlay"))
  )
  private val slowSource =
    MockSource.Dsl(
      MockSpec(
        List(
          MockRule(
            RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/slow")),
            ResponseDef(status = 200, body = Body.Text("slow"), delay = Some(300.millis))
          )
        )
      )
    )
  private val nativeStubJson =
    """{"request":{"method":"GET","url":"/native"},"response":{"status":201,"body":"native!"}}"""

  private val correlated  = PortAllocator.layer >>> Provisioning.layer >>> WireMock.correlated()
  private val perInstance = PortAllocator.layer >>> Provisioning.layer >>> WireMock.perInstance
  private val traceparent = PortAllocator.layer >>> Provisioning.layer >>> WireMock.correlated(Correlation.traceparent)

  private def die[A](z: IO[MockError, A]): UIO[A]    = z.orDieWith(e => new RuntimeException(e.toString))
  private def dieU[A](z: IO[Unsupported, A]): UIO[A] = z.orDieWith(u => new RuntimeException(u.toString))

  private val emptySource = MockSource.Dsl(MockSpec(Nil))

  // Send straight to a base URI with no inject (and an optional raw header), to
  // model a SUT that did NOT carry the space tag.
  private def rawGet(url: String, header: Option[(String, String)] = None): IO[Throwable, Int] =
    ZIO.attemptBlocking {
      val b   = jhttp.HttpRequest.newBuilder(URI.create(url)).GET()
      val req = header.fold(b)((k, v) => b.header(k, v)).build()
      jhttp.HttpClient.newHttpClient().send(req, jhttp.HttpResponse.BodyHandlers.ofString()).statusCode
    }

  def spec = suite("WireMockControl")(
    suite("Correlated (default)")(
      test("provision serves a portable rule, records the injected request, and destroy removes the space's stubs") {
        for
          control <- ZIO.service[MockControl]
          spaces  <- die(control.provision(pingSource))
          space    = spaces.head
          resp    <- SutClient.make(space).send(Method.Get, "/ping").orDie
          recv    <- die(control.received(space))
          _       <- die(control.destroy(space))
          after   <- SutClient.make(space).send(Method.Get, "/ping").orDie
        yield assertTrue(
          spaces.size == 1,
          resp.status == 200,
          resp.body == "pong",
          recv.map(r => (r.method, r.uri)) == List((Method.Get, "/ping")),
          after.status == 404 // the space's stub is gone after destroy
        )
      },
      test("destroy(A) removes only A's stubs — B still serves and records") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          b       <- die(control.provision(pingSource)).map(_.head)
          _       <- SutClient.make(b).send(Method.Get, "/ping").orDie
          _       <- die(control.destroy(a))
          rb      <- die(control.received(b))
          respB   <- SutClient.make(b).send(Method.Get, "/ping").orDie
          respA   <- SutClient.make(a).send(Method.Get, "/ping").orDie
        yield assertTrue(rb.nonEmpty, respB.status == 200, respA.status == 404)
      },
      test("a request WITHOUT the space header gets 404 and leaks into NO Correlated space") {
        for
          control  <- ZIO.service[MockControl]
          a        <- die(control.provision(pingSource)).map(_.head)
          withResp <- SutClient.make(a).send(Method.Get, "/ping").orDie // inject stamps X-Mock-Space
          rawResp  <- rawGet(a.baseUri + "/ping").orDie                 // no header
          recv     <- die(control.received(a))
        yield assertTrue(
          withResp.status == 200,
          rawResp == 404,                   // unmatched without the space tag
          recv.count(_.uri == "/ping") == 1 // only the injected request, never the raw one
        )
      },
      test("addRule serves a new rule; removeRule stops it") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          rid     <- die(control.addRule(a, healthRule, Priority.Overlay))
          r1      <- SutClient.make(a).send(Method.Get, "/health").orDie
          _       <- die(control.removeRule(a, rid))
          r2      <- SutClient.make(a).send(Method.Get, "/health").orDie
        yield assertTrue(r1.status == 200, r1.body == "ok", r2.status == 404)
      },
      test("replaceRules swaps the space's whole rule set") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          _       <- die(control.replaceRules(a, List(healthRule)))
          ping    <- SutClient.make(a).send(Method.Get, "/ping").orDie
          health  <- SutClient.make(a).send(Method.Get, "/health").orDie
        yield assertTrue(ping.status == 404, health.status == 200, health.body == "ok")
      },
      test("removeRule of an unknown id fails RuleNotFound") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          res     <- control.removeRule(a, RuleId("ghost")).either
        yield assertTrue(res == Left(MockError.RuleNotFound(a.id, RuleId("ghost"))))
      },
      test("an Overlay rule shadows a Base rule on the same path") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(MockSource.Dsl(MockSpec(List(baseHealth))))).map(_.head)
          _       <- die(control.addRule(a, overlayHealth, Priority.Overlay))
          resp    <- SutClient.make(a).send(Method.Get, "/h").orDie
        yield assertTrue(resp.status == 200, resp.body == "overlay") // higher priority (atPriority 1) wins
      },
      test("the response delay is applied to served responses") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(slowSource)).map(_.head)
          start   <- Clock.nanoTime
          resp    <- SutClient.make(a).send(Method.Get, "/slow").orDie
          elapsed <- Clock.nanoTime.map(_ - start)
        yield assertTrue(resp.status == 200, elapsed >= 250.millis.toNanos) // ~300ms fixed delay
      } @@ TestAspect.withLiveClock,
      test("provisionNative(WireMock) serves a raw stub on its own server; a Rift spec is rejected") {
        for
          control <- ZIO.service[MockControl]
          spaces  <- die(control.provisionNative(NativeSpec.WireMock(nativeStubJson)))
          a        = spaces.head
          resp    <- rawGet(a.baseUri + "/native").orDie // its own server, no space header needed
          rift    <- control.provisionNative(NativeSpec.Rift("{}")).either
        yield assertTrue(resp == 201, rift.isLeft)
      },
      test("the Correlated adapter reports Correlated isolation") {
        ZIO.serviceWith[MockControl](c => assertTrue(c.isolation == Isolation.Correlated))
      },
      test("received(A) returns only A's traffic when A and B share one server") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          b       <- die(control.provision(pingSource)).map(_.head)
          _       <- SutClient.make(a).send(Method.Get, "/ping").orDie
          _       <- SutClient.make(b).send(Method.Get, "/ping").orDie
          _       <- SutClient.make(b).send(Method.Get, "/ping").orDie
          ra      <- die(control.received(a))
          rb      <- die(control.received(b))
        yield assertTrue(ra.size == 1, rb.size == 2) // received filters by space header on the shared server
      },
      // --- stateful scenarios (#130): edges the portable cap-stateful catalog can't express ---
      suite("stateful scenarios")(
        test("define replaces an existing scenario of the same name (replacing any existing)") {
          for
            control <- ZIO.service[MockControl]
            ss      <- dieU(control.scenarios)
            a       <- die(control.provision(emptySource)).map(_.head)
            _       <- die(ss.define(a, scenario("sc").when("Started", get("/x")).respond(ok.text("v1")).stay.build))
            r1      <- SutClient.make(a).send(Method.Get, "/x").orDie
            _       <- die(ss.define(a, scenario("sc").when("Started", get("/x")).respond(ok.text("v2")).stay.build))
            r2      <- SutClient.make(a).send(Method.Get, "/x").orDie
          yield assertTrue(r1.body == "v1", r2.body == "v2") // the old edge is gone, not shadowed
        },
        test("a non-Started initial state is honoured by define and restored by reset") {
          val oc =
            scenario("oc")
              .startingAt("Open")
              .when("Open", get("/o"))
              .respond(ok.text("open"))
              .goTo("Closed")
              .when("Closed", get("/o"))
              .respond(ok.text("closed"))
              .stay
              .build
          for
            control <- ZIO.service[MockControl]
            ss      <- dieU(control.scenarios)
            si      <- dieU(control.stateInspection)
            a       <- die(control.provision(emptySource)).map(_.head)
            _       <- die(ss.define(a, oc))
            s0      <- die(si.currentState(a, "oc"))
            r1      <- SutClient.make(a).send(Method.Get, "/o").orDie // Open -> "open", -> Closed
            r2      <- SutClient.make(a).send(Method.Get, "/o").orDie // Closed -> "closed"
            _       <- die(ss.reset(a, "oc"))
            s1      <- die(si.currentState(a, "oc"))
            r3      <- SutClient.make(a).send(Method.Get, "/o").orDie // back in Open -> "open"
          yield assertTrue(
            s0 == ScenarioState("Open"),
            r1.body == "open",
            r2.body == "closed",
            s1 == ScenarioState("Open"),
            r3.body == "open"
          )
        },
        test("reset / currentState / setState of an unknown scenario fail with InvalidDefinition") {
          for
            control <- ZIO.service[MockControl]
            ss      <- dieU(control.scenarios)
            si      <- dieU(control.stateInspection)
            a       <- die(control.provision(emptySource)).map(_.head)
            e1      <- ss.reset(a, "ghost").either
            e2      <- si.currentState(a, "ghost").either
            e3      <- si.setState(a, "ghost", ScenarioState("X")).either
            expected = MockError.InvalidDefinition(s"no scenario 'ghost' on space ${a.id.value}")
          yield assertTrue(e1 == Left(expected), e2 == Left(expected), e3 == Left(expected))
        },
        test("destroy removes the scenario's stubs from the shared server") {
          for
            control <- ZIO.service[MockControl]
            ss      <- dieU(control.scenarios)
            a       <- die(control.provision(emptySource)).map(_.head)
            _       <- die(ss.define(a, scenario("sc").when("Started", get("/x")).respond(ok.text("v1")).stay.build))
            before  <- SutClient.make(a).send(Method.Get, "/x").orDie
            _       <- die(control.destroy(a))
            after   <- SutClient.make(a).send(Method.Get, "/x").orDie
          yield assertTrue(before.status == 200, before.body == "v1", after.status == 404)
        }
      )
    ).provide(correlated),
    test("the PerInstance-mode adapter reports PerInstance isolation") {
      ZIO.serviceWith[MockControl](c => assertTrue(c.isolation == Isolation.PerInstance))
    }.provide(perInstance),
    test("PerInstance gives each space its own server (distinct ports, inject identity), destroy stops only that one") {
      for
        control <- ZIO.service[MockControl]
        a       <- die(control.provision(pingSource)).map(_.head)
        b       <- die(control.provision(pingSource)).map(_.head)
        ra      <- SutClient.make(a).send(Method.Get, "/ping").orDie
        _       <- die(control.destroy(a))
        rb      <- SutClient.make(b).send(Method.Get, "/ping").orDie
        aGone   <- rawGet(a.baseUri + "/ping").either
      yield assertTrue(
        a.baseUri != b.baseUri, // a fresh server (port) per space
        ra.status == 200,
        rb.status == 200, // B's server is untouched by destroy(A)
        aGone.isLeft      // A's server was stopped — connection refused
      )
    }.provide(perInstance),
    suite("traceparent correlation")(
      test("inject stamps a traceparent; a foreign trace-id does not reach the space") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          resp    <- SutClient.make(a).send(Method.Get, "/ping").orDie
          recv    <- die(control.received(a))
          foreign <- rawGet(
                       a.baseUri + "/ping",
                       Some("traceparent" -> "00-ffffffffffffffffffffffffffffffff-0000000000000001-01")
                     ).orDie
        yield assertTrue(resp.status == 200, recv.nonEmpty, foreign == 404)
      },
      test("a request reusing the space's trace-id but a DIFFERENT span-id still matches") {
        for
          control <- ZIO.service[MockControl]
          a       <- die(control.provision(pingSource)).map(_.head)
          _       <- SutClient.make(a).send(Method.Get, "/ping").orDie // records the space's traceparent
          recv    <- die(control.received(a))
          tp       = recv.head.headers.first("traceparent").get        // 00-<traceId>-<span>-01
          traceId  = tp.split("-")(1)
          // same trace-id, a span-id the space never injected
          varied <- rawGet(a.baseUri + "/ping", Some("traceparent" -> s"00-$traceId-ffffffffffffffff-01")).orDie
        yield assertTrue(varied == 200) // matched by trace-id; the span-id is free to vary
      }
    ).provide(traceparent)
  )
