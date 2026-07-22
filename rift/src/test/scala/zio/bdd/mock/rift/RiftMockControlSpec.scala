package zio.bdd.mock.rift

import java.net.URI
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.embedded.EmbeddedRift
import zio.test.*

import _root_.rift.RiftError
import _root_.rift.json.Json
import _root_.rift.model.{ApplyResult, EngineInfo, Port}
import _root_.rift.dsl.ImposterBuilder
import _root_.rift.bridge.{ImposterDefinition, InterceptConfig as SdkInterceptConfig}
import _root_.rift.zio.{ImposterHandle, InterceptHandle, Rift as SdkRift}

/**
 * The Rift adapter (`RiftMockControl`), re-based onto the rift-scala SDK for
 * #285. Pre-#285 this spec drove the adapter against `FakeRift` — a hand-rolled
 * fake of the container's admin HTTP protocol. The SDK now owns that whole
 * protocol (and the in-process transport too), so a hermetic protocol double is
 * no longer meaningful here: `FakeRift` is deleted, and this spec instead
 * covers
 *
 *   - an ENGINE-FREE contract suite (mirrors `rift-scala`'s own
 *     `RiftScalaBackendContractSpec`): everything that must hold or fail typed
 *     BEFORE any downcall reaches the engine, driven against a `FailingRift`
 *     double whose every member dies loudly if actually invoked; and
 *   - a LIVE behavioral suite against the real embedded engine
 *     (`EmbeddedRift.layer`), guarded by [[EmbeddedRift.available]] so it
 *     degrades to a skip (never a false green OR a hard failure) on a host with
 *     no `rift-java-embedded`/natives on the classpath.
 *
 * The container transport's own behavior (port pooling under Docker's mapping,
 * `Rift.managed`) is covered by [[RiftContainerSpec]] (RIFT_IT-gated — no
 * Docker on this dev machine); the pool mechanics themselves ([[RiftPortPool]])
 * are pure and unit-tested directly here, with no engine needed.
 */
object RiftMockControlSpec extends ZIOSpecDefault:

  // ── engine-free contract ─────────────────────────────────────────────────────────────────────

  private object FailingRift extends SdkRift:
    private def die[A]: IO[RiftError, A]                                              = ZIO.die(new NotImplementedError("FailingRift"))
    def create(definition: ImposterDefinition): IO[RiftError, ImposterHandle]         = die
    def create(builder: ImposterBuilder): IO[RiftError, ImposterHandle]               = die
    def createFromJson(json: String): IO[RiftError, ImposterHandle]                   = die
    def imposter(port: Port): IO[RiftError, ImposterHandle]                           = die
    def imposters: IO[RiftError, Chunk[ImposterHandle]]                               = die
    def deleteAll: IO[RiftError, Unit]                                                = die
    def replaceAll(definitions: Chunk[ImposterDefinition]): IO[RiftError, Unit]       = die
    def applyConfig(config: Json): IO[RiftError, ApplyResult]                         = die
    def info: IO[RiftError, EngineInfo]                                               = die
    def adminUri: UIO[URI]                                                            = ZIO.die(new NotImplementedError("FailingRift"))
    def intercept(config: SdkInterceptConfig): ZIO[Scope, RiftError, InterceptHandle] = die

  private def controlOf(mode: RiftMode, interceptCapable: Boolean = true): ULayer[MockControl] =
    Provisioning.live >>> ZLayer.scoped(
      RiftMockControl.make(FailingRift, None, mode, InterceptSettings(), interceptCapable)
    )

  private val contractLayer: ULayer[MockControl] = controlOf(RiftMode.PerInstance)

  private val unknownSpace = MockSpace("http://localhost:0", identity, SpaceId("nope-0"))

  private val contractSuite = suite("RiftMockControl contract (engine-free)")(
    test("advertises the full capability set and negotiates require() for every member") {
      for
        control <- ZIO.service[MockControl]
        _       <- control.require(Capability.values*)
      yield assertTrue(control.backendName == "rift", control.capabilities == Capability.values.toSet)
    },
    test("reports the isolation mode it was built with") {
      for
        perInstance <- ZIO.service[MockControl]
        correlated  <- ZIO.service[MockControl].provideLayer(controlOf(RiftMode.correlated))
      yield assertTrue(
        perInstance.isolation == Isolation.PerInstance,
        correlated.isolation == Isolation.Correlated
      )
    },
    test("every capability accessor returns an instance (full-set backend)") {
      for
        control <- ZIO.service[MockControl]
        _       <- control.faults
        _       <- control.scenarios
        _       <- control.stateInspection
        _       <- control.scripting
        _       <- control.proxyRecord
        _       <- control.templating
        _       <- control.intercept
      yield assertCompletes
    },
    test("a WireMock native spec is rejected as InvalidDefinition, not attempted") {
      for
        control <- ZIO.service[MockControl]
        exit    <- control.provisionNative(NativeSpec.WireMock("{}")).exit
      yield assert(exit)(Assertion.fails(Assertion.isSubtype[MockError.InvalidDefinition](Assertion.anything)))
    },
    test("operations on an unknown space fail SpaceNotFound with that space's id") {
      for
        control      <- ZIO.service[MockControl]
        rule          = MockRule(RequestMatch(), ResponseDef())
        addExit      <- control.addRule(unknownSpace, rule).exit
        removeExit   <- control.removeRule(unknownSpace, RuleId("r1")).exit
        destroyExit  <- control.destroy(unknownSpace).exit
        receivedExit <- control.received(unknownSpace).exit
      yield assertTrue(
        List(addExit, removeExit, destroyExit, receivedExit).forall {
          case Exit.Failure(cause) => cause.failureOption.contains(MockError.SpaceNotFound(unknownSpace.id))
          case _                   => false
        }
      )
    },
    test("a malformed raw provision source fails InvalidDefinition before the engine") {
      for
        control <- ZIO.service[MockControl]
        exit    <- control.provision(MockSource.Json("not json")).exit
      yield assert(exit)(Assertion.fails(Assertion.isSubtype[MockError.InvalidDefinition](Assertion.anything)))
    },
    // #285/B5: Intercept must not be advertised for a transport where the listener isn't actually
    // host-reachable (the constructors thread `interceptCapable` in per-transport; here it's forced
    // false to prove the adapter itself honours it, independent of which Rift.* entry point sets it).
    test("interceptCapable=false excludes Intercept from capabilities, and .intercept fails Unsupported") {
      for
        control <- ZIO.service[MockControl].provideLayer(controlOf(RiftMode.PerInstance, interceptCapable = false))
        exit    <- control.intercept.exit
      yield assertTrue(
        control.capabilities == Capability.values.toSet - Capability.Intercept,
        exit.causeOption.flatMap(_.failureOption).contains(Unsupported(Capability.Intercept, "rift"))
      )
    }
  ).provideShared(contractLayer)

  // ── RiftPortPool (pure — the container/connect port-pooling wrinkle, #303) ──────────────────────

  private val portPoolSuite = suite("RiftPortPool")(
    test("acquirePort hands out ports in order and fails typed once exhausted") {
      for
        pool      <- RiftPortPool.fixed(Vector(10, 11))
        a         <- pool.acquirePort
        b         <- pool.acquirePort
        exhausted <- pool.acquirePort.exit
      yield assertTrue(
        a == 10,
        b == 11,
        exhausted.causeOption.flatMap(_.failureOption).exists {
          case MockError.ProvisionFailed(m) => m.contains("exhausted")
          case _                            => false
        }
      )
    },
    test("claimPort removes an in-range port from the free list; releasePort returns it") {
      for
        pool         <- RiftPortPool.fixed(Vector(10, 11))
        claimed      <- pool.claimPort(10)
        outOfRange   <- pool.claimPort(999)
        afterClaim   <- pool.acquirePort // only 11 remains
        _            <- pool.releasePort(10)
        afterRelease <- pool.acquirePort // 10 is back
      yield assertTrue(claimed, !outOfRange, afterClaim == 11, afterRelease == 10)
    },
    test("ensureInRange fails typed (InvalidDefinition, naming the port) for a port the pool never owns") {
      for
        pool       <- RiftPortPool.fixed(Vector(10, 11))
        inRange    <- pool.ensureInRange(10).either
        outOfRange <- pool.ensureInRange(999).either
      yield assertTrue(
        inRange.isRight,
        outOfRange.swap.toOption.exists {
          case MockError.InvalidDefinition(m) => m.contains("999")
          case _                              => false
        }
      )
    }
  )

  // ── live behavior (embedded engine, guarded) ────────────────────────────────────────────────────

  private val http1Client: HttpClient = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  private def get(baseUri: String, path: String, headers: (String, String)*): Task[(Int, String)] =
    ZIO.attemptBlocking {
      val builder = JHttpRequest.newBuilder(URI.create(s"$baseUri$path"))
      headers.foreach((k, v) => builder.header(k, v))
      val res = http1Client.send(builder.GET().build(), JHttpResponse.BodyHandlers.ofString())
      (res.statusCode, res.body)
    }

  private def okRule(path: String, bodyText: String): MockRule =
    MockRule(
      RequestMatch(method = Some(Method.Get), path = PathMatch.Exact(path)),
      ResponseDef(body = Body.Text(bodyText))
    )

  private def withControl[A](mode: Isolation = Isolation.PerInstance)(
    use: MockControl => ZIO[Scope, Any, A]
  ): ZIO[Scope, Any, A] =
    val riftMode = mode match
      case Isolation.PerInstance => RiftMode.PerInstance
      case Isolation.Correlated  => RiftMode.correlated
    EmbeddedRift.layer(riftMode).build.map(_.get[MockControl]).provideSome[Scope](Provisioning.live).flatMap(use)

  private def asRiftT(e: RiftError): Throwable = new RuntimeException(s"RiftError: $e")

  // Probes `n` free OS ports the same way `PortAllocator` does (bind-then-close) — used to build a
  // real `RiftPortPool` for the pool-mechanics tests below (#213/#303), since the embedded transport
  // itself needs no pool (`EmbeddedRift.layer` wires `portPool = None`) and so can't exercise it via
  // `withControl`. A TOCTOU race with something else grabbing the port between probe and imposter
  // bind is possible in principle but accepted here — the same tradeoff `PortAllocatorLive` makes.
  private def freePorts(n: Int): Task[Vector[Int]] =
    ZIO.foreach(Vector.range(0, n)) { _ =>
      ZIO.attemptBlocking {
        val s = new java.net.ServerSocket(0)
        try s.getLocalPort
        finally s.close()
      }
    }

  // A `RiftMockControl` wired against its OWN fresh embedded engine instance (acquiring
  // `SdkRift.embedded` again, rather than reusing `EmbeddedRift.layer`'s, mirrors
  // `EmbeddedRiftCapabilitiesSpec`'s "shared memoizes" test: a plain, non-shared acquisition starts
  // an independent engine) PLUS an explicit `RiftPortPool` over caller-chosen ports — so the
  // container/connect port-pooling contract can be exercised end-to-end through `control.provision`
  // without Docker.
  private def pooledControl(ports: Vector[Int]): ZIO[Scope, Throwable, MockControl] =
    for
      pool <- RiftPortPool.fixed(ports)
      rift <- SdkRift.embedded.build.mapError(asRiftT).map(_.get[SdkRift])
      control <- RiftMockControl
                   .make(rift, Some(pool), RiftMode.PerInstance, InterceptSettings(), interceptCapable = true)
                   .provideSomeLayer[Scope](Provisioning.live)
    yield control

  private def portOf(baseUri: String): Int = URI.create(baseUri).getPort

  private val liveSuite = suite("MockControl contract against a live embedded engine")(
    test("provision serves rules, 404s unmatched, records requests, and destroys exactly one space") {
      ZIO.scoped {
        withControl() { control =>
          for
            provisioned  <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/a", "base")))))
            space         = provisioned.head
            other        <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/b", "other"))))).map(_.head)
            hit          <- get(space.baseUri, "/a")
            miss         <- get(space.baseUri, "/nope")
            received     <- control.received(space)
            _            <- control.destroy(other)
            afterDestroy <- get(space.baseUri, "/a")
            gone         <- control.received(other).exit
          yield assertTrue(
            hit == (200, "base"),
            miss._1 == 404,
            received.exists(r => r.uri == "/a" && r.method == Method.Get),
            afterDestroy == (200, "base"),
            gone.causeOption.flatMap(_.failureOption).contains(MockError.SpaceNotFound(other.id))
          )
        }
      }
    },
    test("overlay rules shadow base rules; removeRule lifts the overlay; replaceRules swaps the set") {
      ZIO.scoped {
        withControl() { control =>
          for
            space       <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/a", "base"))))).map(_.head)
            overlayId   <- control.addRule(space, okRule("/a", "overlay"), Priority.Overlay)
            shadowed    <- get(space.baseUri, "/a")
            _           <- control.removeRule(space, overlayId)
            restored    <- get(space.baseUri, "/a")
            missingExit <- control.removeRule(space, RuleId("no-such")).exit
            _           <- control.replaceRules(space, List(okRule("/b", "replaced")))
            oldGone     <- get(space.baseUri, "/a")
            newServed   <- get(space.baseUri, "/b")
          yield assertTrue(
            shadowed == (200, "overlay"),
            restored == (200, "base"),
            missingExit.causeOption
              .flatMap(_.failureOption)
              .contains(MockError.RuleNotFound(space.id, RuleId("no-such"))),
            oldGone._1 == 404,
            newServed == (200, "replaced")
          )
        }
      }
    },
    test("a connection fault makes the SUT's client observe a transport failure") {
      ZIO.scoped {
        withControl() { control =>
          for
            space     <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/f", "fine"))))).map(_.head)
            faultsApi <- control.faults
            _         <- faultsApi.inject(space, RequestMatch(path = PathMatch.Exact("/f")), FaultKind.ConnectionReset)
            outcome   <- get(space.baseUri, "/f").exit
          yield assertTrue(outcome.isFailure)
        }
      }
    },
    test("stateful scenarios advance, inspect, pin, and reset") {
      ZIO.scoped {
        withControl() { control =>
          val scenario = ScenarioDef(
            "invoice",
            List(
              StatefulRule(
                ScenarioState("Started"),
                RequestMatch(path = PathMatch.Exact("/s")),
                ResponseDef(body = Body.Text("a")),
                Some(ScenarioState("Paid"))
              ),
              StatefulRule(
                ScenarioState("Paid"),
                RequestMatch(path = PathMatch.Exact("/s")),
                ResponseDef(body = Body.Text("b")),
                None
              )
            )
          )
          for
            space      <- control.provision(MockSource.Dsl(MockSpec(Nil))).map(_.head)
            scenarios  <- control.scenarios
            inspection <- control.stateInspection
            _          <- scenarios.define(space, scenario)
            first      <- get(space.baseUri, "/s")
            second     <- get(space.baseUri, "/s")
            state      <- inspection.currentState(space, "invoice")
            _          <- scenarios.reset(space, "invoice")
            afterReset <- get(space.baseUri, "/s")
            _          <- inspection.setState(space, "invoice", ScenarioState("Paid"))
            pinned     <- get(space.baseUri, "/s")
            unknown    <- inspection.currentState(space, "nope").exit
          yield assertTrue(
            first == (200, "a"),
            second == (200, "b"),
            state.value == "Paid",
            afterReset == (200, "a"),
            pinned == (200, "b"),
            unknown.isFailure
          )
        }
      }
    },
    test("templating captures a path segment into the response body") {
      ZIO.scoped {
        withControl() { control =>
          for
            space      <- control.provision(MockSource.Dsl(MockSpec(Nil))).map(_.head)
            templating <- control.templating
            _ <- templating.inject(
                   space,
                   RequestMatch(path = PathMatch.Template("/hello/{name}")),
                   ResponseTemplate(
                     body = "Hello NAME",
                     captures = List(TemplateCapture("NAME", TemplateSource.Path, "[^/]+$"))
                   )
                 )
            greeted <- get(space.baseUri, "/hello/bob")
          yield assertTrue(greeted == (200, "Hello bob"))
        }
      }
    },
    test("a native raw imposter document provisions verbatim and records") {
      ZIO.scoped {
        withControl() { control =>
          val raw =
            """{"protocol":"http","name":"native",
              |"stubs":[{"predicates":[{"equals":{"path":"/n"}}],
              |          "responses":[{"is":{"statusCode":200,"body":"native"}}]}]}""".stripMargin
          for
            spaces   <- control.provisionNative(NativeSpec.Rift(raw))
            space     = spaces.head
            hit      <- get(space.baseUri, "/n")
            received <- control.received(space)
          yield assertTrue(hit == (200, "native"), received.exists(_.uri == "/n"))
        }
      }
    },
    // #285/B6: authored/fixed-port coverage. B2/B3 fixed two silent-regression bugs (a raw
    // `provision()` doc and a `provisionNative` doc both bypassed the port pool); this coverage was
    // ENTIRELY absent before, which is exactly why those regressions shipped invisibly. These must
    // fail if B2/B3 are reverted.
    test("a DSL-authored port is honoured; omitting the port yields a different, auto-assigned one") {
      ZIO.scoped {
        withControl() { control =>
          for
            port <- freePorts(1).map(_.head)
            authored <- control
                          .provision(MockSource.Dsl(MockSpec(List(okRule("/a", "hi")), port = Some(port))))
                          .map(_.head)
            unauthored <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/b", "hi"))))).map(_.head)
            hit        <- get(authored.baseUri, "/a")
          yield assertTrue(
            portOf(authored.baseUri) == port,
            hit == (200, "hi"),
            portOf(unauthored.baseUri) != port,
            portOf(unauthored.baseUri) > 0
          )
        }
      }
    },
    test("a raw NativeSpec.Rift document's top-level port is honoured; omitting it still assigns one (#214)") {
      ZIO.scoped {
        withControl() { control =>
          def rawWithPort(port: Int) =
            s"""{"protocol":"http","port":$port,
               |"stubs":[{"predicates":[{"equals":{"path":"/p"}}],
               |          "responses":[{"is":{"statusCode":200,"body":"pinned"}}]}]}""".stripMargin
          val rawNoPort =
            """{"protocol":"http",
              |"stubs":[{"predicates":[{"equals":{"path":"/q"}}],
              |          "responses":[{"is":{"statusCode":200,"body":"unpinned"}}]}]}""".stripMargin
          for
            port     <- freePorts(1).map(_.head)
            pinned   <- control.provisionNative(NativeSpec.Rift(rawWithPort(port))).map(_.head)
            hit      <- get(pinned.baseUri, "/p")
            unpinned <- control.provisionNative(NativeSpec.Rift(rawNoPort)).map(_.head)
          yield assertTrue(
            portOf(pinned.baseUri) == port,
            hit == (200, "pinned"),
            portOf(unpinned.baseUri) > 0,
            portOf(unpinned.baseUri) != port
          )
        }
      }
    },
    test("an in-pool authored port is claimed, so a later auto-provision cannot collide with it (#213)") {
      ZIO.scoped {
        for
          ports   <- freePorts(2)
          control <- pooledControl(ports)
          authored <- control
                        .provision(MockSource.Dsl(MockSpec(List(okRule("/x", "x")), port = Some(ports(0)))))
                        .map(_.head)
          autoProvisioned <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/y", "y"))))).map(_.head)
        yield assertTrue(
          portOf(authored.baseUri) == ports(0),
          // the only other pool port is `ports(1)`: claiming `ports(0)` for the authored space must
          // force the auto-provision to draw the other one, never collide with the authored port.
          portOf(autoProvisioned.baseUri) == ports(1)
        )
      }
    },
    test("a failed provision releases its claimed port back to the pool (no leak, #213)") {
      ZIO.scoped {
        for
          ports   <- freePorts(1)
          port     = ports.head
          control <- pooledControl(ports)
          // Occupy the port ourselves (outside the pool's/engine's view) so the port is freshly
          // claimed from the pool (`pooled = true`) but `rift.create`'s actual bind fails — the exact
          // path B2/B3 wired `releasePortValue` into.
          blocked <- ZIO.attemptBlocking(new java.net.ServerSocket(port))
          exit    <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/z", "z")), port = Some(port)))).exit
          _       <- ZIO.attemptBlocking(blocked.close())
          reclaimed <- control
                         .provision(MockSource.Dsl(MockSpec(List(okRule("/z2", "z2")), port = Some(port))))
                         .map(_.head)
        yield assertTrue(exit.isFailure, portOf(reclaimed.baseUri) == port)
      }
    },
    test("pool exhaustion surfaces a typed ProvisionFailed THROUGH control.provision") {
      ZIO.scoped {
        for
          ports   <- freePorts(1)
          control <- pooledControl(ports)
          first   <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/e", "e"))))).exit
          second  <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/e2", "e2"))))).exit
        yield assertTrue(
          first.isSuccess,
          second.causeOption.flatMap(_.failureOption).exists {
            case MockError.ProvisionFailed(m) => m.contains("exhausted")
            case _                            => false
          }
        )
      }
    },
    test("scripting serves a Rhai-computed response") {
      ZIO.scoped {
        withControl() { control =>
          for
            space     <- control.provision(MockSource.Dsl(MockSpec(Nil))).map(_.head)
            scripting <- control.scripting
            _ <- scripting.inject(
                   space,
                   RequestMatch(path = PathMatch.Exact("/js")),
                   Script(ScriptEngine.Rhai, """fn respond(ctx) { http(200, #{engine: "rhai"}) }""")
                 )
            scripted <- get(space.baseUri, "/js")
          yield assertTrue(scripted._1 == 200, scripted._2.contains("rhai"))
        }
      }
    },
    test("proxyRecord proxies to an upstream and keeps serving the recorded response") {
      ZIO.scoped {
        withControl() { control =>
          for
            upstream <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/p", "from-upstream"))))).map(_.head)
            front    <- control.provision(MockSource.Dsl(MockSpec(Nil))).map(_.head)
            proxyApi <- control.proxyRecord
            _        <- proxyApi.proxy(front, RequestMatch(path = PathMatch.Exact("/p")), upstream.baseUri)
            proxied  <- get(front.baseUri, "/p")
            _        <- control.destroy(upstream)
            replayed <- get(front.baseUri, "/p")
          yield assertTrue(proxied == (200, "from-upstream"), replayed == (200, "from-upstream"))
        }
      }
    },
    test("intercept starts lazily, memoizes its port, registers rules, and exports a truststore") {
      ZIO.scoped {
        withControl() { control =>
          for
            space        <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/i", "hi"))))).map(_.head)
            interceptApi <- control.intercept
            port1        <- interceptApi.proxyPort
            port2        <- interceptApi.proxyPort
            _ <- interceptApi.add(
                   InterceptRule.Serve("inline.example.com", InterceptStub(status = 203, body = Some("inline")))
                 )
            _           <- interceptApi.add(InterceptRule.Redirect("cdn.example.com", space))
            unknown      = MockSpace("http://localhost:0", identity, SpaceId("ghost-0"))
            unknownExit <- interceptApi.add(InterceptRule.Redirect("x.example.com", unknown)).exit
            store       <- interceptApi.trustStore()
            storeExists <- ZIO.attemptBlocking(java.nio.file.Files.exists(store.path))
          yield assertTrue(
            port1 > 0,
            port1 == port2,
            unknownExit.causeOption.flatMap(_.failureOption).exists {
              case MockError.InvalidDefinition(_) => true
              case _                              => false
            },
            store.password.nonEmpty,
            storeExists,
            // #285/B15: pin the default format to a KNOWN value, not `ts.format.keystoreName` (which
            // is self-referential — it always agrees with itself regardless of what format was used).
            store.format == TrustStoreFormat.Pkcs12
          )
        }
      }
    },
    test("a mid-batch provision failure rolls back the spaces already stood up") {
      // #285/B14: the previous version of this test asserted `afterStill.isLeft || afterStill.isRight`
      // — a tautology for any `Either` that can never fail, regardless of whether rollback actually
      // destroyed anything. Assert the REAL guarantee instead: the imposter count on the engine after
      // the failed batch equals the count before it (the space stood up for `a-valid.json` before
      // `b-broken.json` failed is gone, not leaked) — using the SDK directly (not through
      // `MockControl`, which has no "list spaces" op) on a control built against its own fresh engine
      // so no other suite's imposters can pollute the count. (The enclosing suite is already
      // `EmbeddedRift.available`-gated below, so no separate guard is needed here.)
      ZIO.scoped {
        for
          rift <- SdkRift.embedded.build.mapError(asRiftT).map(_.get[SdkRift])
          control <- RiftMockControl
                       .make(rift, None, RiftMode.PerInstance, InterceptSettings(), interceptCapable = true)
                       .provideSomeLayer[Scope](Provisioning.live)
          dir <- ZIO.attemptBlocking {
                   val d = java.nio.file.Files.createTempDirectory("rift-rollback")
                   java.nio.file.Files.writeString(d.resolve("a-valid.json"), """{"protocol":"http","stubs":[]}""")
                   java.nio.file.Files.writeString(d.resolve("b-broken.json"), "not json")
                   d
                 }
          before       <- rift.imposters.mapError(asRiftT).map(_.size)
          exit         <- control.provision(MockSource.Dir(dir.toString)).exit
          afterFailure <- rift.imposters.mapError(asRiftT).map(_.size)
        yield assertTrue(exit.isFailure, afterFailure == before)
      }
    },
    test("correlated rule mutation rebuilds the flow: overlay shadows, remove restores, gaps are typed") {
      ZIO.scoped {
        withControl(Isolation.Correlated) { control =>
          for
            space      <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/m", "base"))))).map(_.head)
            hdr         = space.inject(HttpRequest(Method.Get, "/m")).headers.entries.map((k, vs) => k -> vs.head)
            _          <- control.addRule(space, okRule("/m2", "appended"), Priority.Base)
            appended   <- get(space.baseUri, "/m2", hdr*)
            overlayId  <- control.addRule(space, okRule("/m", "overlay"), Priority.Overlay)
            shadowed   <- get(space.baseUri, "/m", hdr*)
            _          <- control.removeRule(space, overlayId)
            restored   <- get(space.baseUri, "/m", hdr*)
            missing    <- control.removeRule(space, RuleId("no-such")).exit
            _          <- control.replaceRules(space, List(okRule("/m3", "swapped")))
            swapped    <- get(space.baseUri, "/m3", hdr*)
            oldGone    <- get(space.baseUri, "/m", hdr*)
            inspection <- control.stateInspection
            gap        <- inspection.setState(space, "invoice", ScenarioState("Paid")).exit
          yield assertTrue(
            appended == (200, "appended"),
            shadowed == (200, "overlay"),
            restored == (200, "base"),
            missing.causeOption.flatMap(_.failureOption).contains(MockError.RuleNotFound(space.id, RuleId("no-such"))),
            swapped == (200, "swapped"),
            oldGone._1 == 404,
            gap.causeOption.flatMap(_.failureOption).exists {
              case MockError.InvalidDefinition(reason) => reason.contains("per-flow")
              case _                                   => false
            }
          )
        }
      }
    },
    test("correlated spaces share one imposter but stay header-isolated") {
      ZIO.scoped {
        withControl(Isolation.Correlated) { control =>
          for
            alice      <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/c", "alice"))))).map(_.head)
            bob        <- control.provision(MockSource.Dsl(MockSpec(List(okRule("/c", "bob"))))).map(_.head)
            aliceHeader = alice.inject(HttpRequest(Method.Get, "/c"))
            aliceRes   <- get(alice.baseUri, "/c", aliceHeader.headers.entries.map((k, vs) => k -> vs.head)*)
            bobHeader   = bob.inject(HttpRequest(Method.Get, "/c"))
            bobRes     <- get(bob.baseUri, "/c", bobHeader.headers.entries.map((k, vs) => k -> vs.head)*)
            unmatched  <- get(alice.baseUri, "/c")
            aliceSeen  <- control.received(alice)
            _          <- control.destroy(bob)
            aliceStill <- get(alice.baseUri, "/c", aliceHeader.headers.entries.map((k, vs) => k -> vs.head)*)
          yield assertTrue(
            alice.baseUri == bob.baseUri,
            aliceRes == (200, "alice"),
            bobRes == (200, "bob"),
            unmatched._1 == 404,
            aliceSeen.nonEmpty,
            aliceStill == (200, "alice")
          )
        }
      }
    }
  ) @@ (if EmbeddedRift.available then TestAspect.identity
        else TestAspect.ignore) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  def spec = suite("RiftMockControl")(contractSuite, portPoolSuite, liveSuite)
