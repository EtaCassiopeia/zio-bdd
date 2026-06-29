package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.Client
import zio.test.*

object RiftMockControlSpec extends ZIOSpecDefault:

  private val pingRule = MockRule(
    `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    respond = ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  // A full Rift imposter document with backend-specific `_rift` extensions — the
  // kind of full-fidelity spec the native escape hatch exists for.
  private val nativeImposter =
    """{"port":9999,"protocol":"http",
      | "stubs":[{"predicates":[{"equals":{"path":"/native"}}],
      |           "responses":[{"is":{"statusCode":200,"body":"native!"}}]}],
      | "_rift":{"script":{"engine":"rhai","code":"200"}}}""".stripMargin

  private def portOf(baseUri: String): Int = baseUri.substring(baseUri.lastIndexOf(':') + 1).toInt

  /**
   * Set up a fresh fake admin server + a Rift adapter (with the given imposter
   * port pool) bound to it, in a scope.
   */
  private def withAdapterPool(ports: List[Int], mode: RiftMode = RiftMode.PerInstance)(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    ZIO.scoped {
      for
        adminAndFake <- FakeRift.started
        (admin, fake) = adminAndFake
        endpoint     <- RiftEndpoint.pooled(admin, ports)(p => s"http://localhost:$p")
        control      <- RiftMockControl.make(endpoint, mode)
        result       <- use(control, fake)
      yield result
    }

  private def withAdapter(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    withAdapterPool((4545 until 4600).toList)(use)

  private def withCorrelated(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    withAdapterPool((4545 until 4600).toList, RiftMode.correlated)(use)

  private def withCorrelatedTraceparent(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    withAdapterPool((4545 until 4600).toList, RiftMode.Correlated(Correlation.traceparent))(use)

  def spec = suite("RiftMockControl (adapter vs fake admin API)")(
    test("the Rift adapter's isolation model is PerInstance (own port, identity inject)") {
      withAdapter { (control, _) =>
        ZIO.succeed(assertTrue(control.isolation == Isolation.PerInstance))
      }
    },
    test("provision creates one recording imposter per space; baseUri reflects its port; inject is identity") {
      withAdapter { (control, fake) =>
        for
          spacesE <- control.provision(pingSource).either
          posted  <- fake.posted.get
        yield
          val spaces = spacesE.toOption.toList.flatten
          val space  = spaces.head
          val req    = HttpRequest(Method.Get, "http://sut/")
          assertTrue(
            spacesE.isRight,
            spaces.size == 1,
            posted.size == 1,
            posted.head.contains("\"recordRequests\":true"),
            FakeRift.portOf(posted.head).contains(portOf(space.baseUri)),
            space.baseUri == s"http://localhost:${portOf(space.baseUri)}",
            space.inject(req) == req
          )
      }
    },
    test("destroy(A) deletes only A's imposter — never the global reset — and leaves B intact") {
      withAdapter { (control, fake) =>
        for
          aE       <- control.provision(pingSource).either
          bE       <- control.provision(pingSource).either
          a         = aE.toOption.get.head
          b         = bE.toOption.get.head
          destroyE <- control.destroy(a).either
          deleted  <- fake.deletedPorts.get
          globals  <- fake.globalDeletes.get
          recvBE   <- control.received(b).either
          recvAE   <- control.received(a).either
        yield assertTrue(
          aE.isRight && bE.isRight,
          a.baseUri != b.baseUri,
          destroyE.isRight,
          globals == 0,                        // never DELETE /imposters
          deleted == Chunk(portOf(a.baseUri)), // exactly A's port, once
          !deleted.contains(portOf(b.baseUri)),
          recvBE.isRight,                               // B still addressable
          recvAE == Left(MockError.SpaceNotFound(a.id)) // A is gone from the adapter
        )
      }
    },
    test("received parses the recorded requests from the imposter view") {
      withAdapter { (control, fake) =>
        for
          sE  <- control.provision(pingSource).either
          s    = sE.toOption.get.head
          port = portOf(s.baseUri)
          _ <- fake.setView(
                 port,
                 s"""{"port":$port,"requests":[{"method":"GET","path":"/ping","headers":{},"body":null}]}"""
               )
          recvE <- control.received(s).either
        yield assertTrue(recvE == Right(List(RecordedRequest(Method.Get, "/ping", Map.empty, None))))
      }
    },
    test("addRule/replaceRules/removeRule hit the stubs sub-resource endpoints") {
      withAdapter { (control, fake) =>
        for
          sE    <- control.provision(pingSource).either
          s      = sE.toOption.get.head
          port   = portOf(s.baseUri)
          addE  <- control.addRule(s, pingRule, Priority.Base).either                   // r2 -> index 1
          rmE   <- ZIO.fromEither(addE).flatMap(id => control.removeRule(s, id)).either // remove r2 (index 1)
          _     <- control.replaceRules(s, List(pingRule)).either                       // PUT all stubs
          calls <- fake.stubCalls.get
        yield assertTrue(
          addE == Right(RuleId("r2")), // r1 is the provisioned stub; r2 appended at index 1
          rmE.isRight,
          calls.exists(_.startsWith(s"POST $port")),
          calls.exists(_.startsWith(s"PUT $port")),
          calls.contains(s"DELETE $port/1")
        )
      }
    },
    test("rule ids stay stable: an overlay insert does not misroute a later removal") {
      withAdapter { (control, fake) =>
        for
          sE    <- control.provision(pingSource).either                                  // r1 at index 0
          s      = sE.toOption.get.head
          port   = portOf(s.baseUri)
          baseE <- control.addRule(s, pingRule, Priority.Base).either                    // r2 -> index 1 -> [r1, r2]
          ovE   <- control.addRule(s, pingRule, Priority.Overlay).either                 // r3 -> index 0 -> [r3, r1, r2]
          rmE   <- ZIO.fromEither(baseE).flatMap(id => control.removeRule(s, id)).either // remove r2
          calls <- fake.stubCalls.get
        yield assertTrue(
          baseE == Right(RuleId("r2")),
          ovE == Right(RuleId("r3")),
          rmE.isRight,
          calls.contains(s"DELETE $port/2") // r2 shifted to index 2 by the overlay insert
        )
      }
    },
    test("removeRule of an unknown id fails with RuleNotFound") {
      withAdapter { (control, _) =>
        for
          sE  <- control.provision(pingSource).either
          s    = sE.toOption.get.head
          rmE <- control.removeRule(s, RuleId("ghost")).either
        yield assertTrue(rmE == Left(MockError.RuleNotFound(s.id, RuleId("ghost"))))
      }
    },
    test("a failed provision releases the imposter port — no pool leak") {
      withAdapterPool(List(4545)) { (control, fake) =>
        for
          _    <- fake.failProvision.set(true)
          fail <- control.provision(pingSource).either // POST 400 -> fails; port must be released
          _    <- fake.failProvision.set(false)
          ok   <- control.provision(pingSource).either // would be "pool exhausted" if the port leaked
        yield assertTrue(
          fail.isLeft,
          ok.isRight,
          ok.toOption.get.head.baseUri == "http://localhost:4545"
        )
      }
    },
    test("an exhausted imposter port pool fails with a typed ProvisionFailed") {
      withAdapterPool(List(4545)) { (control, _) =>
        for
          _    <- control.provision(pingSource).either // takes 4545
          next <- control.provision(pingSource).either // pool empty
        yield assertTrue(
          next.swap.toOption.exists {
            case MockError.ProvisionFailed(m) => m.contains("exhausted")
            case _                            => false
          }
        )
      }
    },
    test("advertises no capabilities; every accessor fails fast") {
      withAdapter { (control, _) =>
        for faultsE <- control.faults.either
        yield assertTrue(
          control.capabilities.isEmpty,
          faultsE == Left(Unsupported(Capability.Faults, "rift"))
        )
      }
    },
    test("provisionNative(Rift) stands up a full-fidelity space (stubs + _rift) on our pooled port") {
      withAdapter { (control, fake) =>
        for
          sE     <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either
          posted <- fake.posted.get
        yield
          val space = sE.toOption.get.head
          val body  = posted.headOption.getOrElse("")
          val req   = HttpRequest(Method.Get, "http://sut/")
          assertTrue(
            sE.isRight,
            body.contains("\"_rift\""),                            // full-fidelity: _rift extensions preserved
            body.contains("/native"),                              // the native stub preserved
            body.contains("\"recordRequests\":true"),              // forced on so received() works
            FakeRift.portOf(body).contains(portOf(space.baseUri)), // our pool port, not the spec's 9999
            !body.contains("9999"),
            space.inject(req) == req // isolation: identity inject like a portable space
          )
      }
    },
    test("a natively-provisioned space participates in received and is space-local on destroy") {
      withAdapter { (control, fake) =>
        for
          sE  <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either
          s    = sE.toOption.get.head
          port = portOf(s.baseUri)
          _ <- fake.setView(
                 port,
                 s"""{"port":$port,"requests":[{"method":"GET","path":"/native","headers":{},"body":null}]}"""
               )
          recvE    <- control.received(s).either
          destroyE <- control.destroy(s).either
          deleted  <- fake.deletedPorts.get
          globals  <- fake.globalDeletes.get
        yield assertTrue(
          recvE == Right(List(RecordedRequest(Method.Get, "/native", Map.empty, None))),
          destroyE.isRight,
          deleted == Chunk(port), // space-local: only this imposter
          globals == 0            // never the global reset
        )
      }
    },
    test("native spaces isolate: two get distinct ports; destroy(A) leaves B") {
      withAdapter { (control, _) =>
        for
          aE    <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either
          bE    <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either
          a      = aE.toOption.get.head
          b      = bE.toOption.get.head
          _     <- control.destroy(a).either
          recvB <- control.received(b).either
          recvA <- control.received(a).either
        yield assertTrue(
          a.baseUri != b.baseUri,
          recvB.isRight,
          recvA == Left(MockError.SpaceNotFound(a.id))
        )
      }
    },
    test("provisionNative rejects a non-Rift (WireMock) spec with InvalidDefinition") {
      withAdapter { (control, _) =>
        for e <- control.provisionNative(NativeSpec.WireMock("{}")).either
        yield assertTrue(e.swap.toOption.exists {
          case MockError.InvalidDefinition(_) => true
          case _                              => false
        })
      }
    },
    test("a native space participates in overlays with stable rule ids (native stub is counted)") {
      withAdapter { (control, fake) =>
        for
          sE    <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either       // native stub -> [r1]
          s      = sE.toOption.get.head
          port   = portOf(s.baseUri)
          baseE <- control.addRule(s, pingRule, Priority.Base).either                    // r2 -> index 1 -> [r1, r2]
          ovE   <- control.addRule(s, pingRule, Priority.Overlay).either                 // r3 -> index 0 -> [r3, r1, r2]
          rmE   <- ZIO.fromEither(baseE).flatMap(id => control.removeRule(s, id)).either // remove r2
          calls <- fake.stubCalls.get
        yield assertTrue(
          baseE == Right(RuleId("r2")), // only holds if the native imposter's 1 stub was counted
          ovE == Right(RuleId("r3")),
          rmE.isRight,
          calls.contains(s"DELETE $port/2") // r2 shifted to index 2 by the overlay insert
        )
      }
    },
    test("provisionNative(Rift) with malformed JSON fails with InvalidDefinition") {
      withAdapter { (control, _) =>
        for e <- control.provisionNative(NativeSpec.Rift("not json")).either
        yield assertTrue(e.swap.toOption.exists {
          case MockError.InvalidDefinition(_) => true
          case _                              => false
        })
      }
    },
    test("a raw JSON source is provisioned with our port + recordRequests forced; malformed raw is InvalidDefinition") {
      withAdapter { (control, fake) =>
        val raw =
          """{"port":9999,"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/x"}}],"responses":[{"is":{"statusCode":204}}]}]}"""
        for
          sE        <- control.provision(MockSource.Json(raw)).either
          posted    <- fake.posted.get
          malformed <- control.provision(MockSource.Json("not json")).either
        yield
          val body = posted.headOption.getOrElse("")
          assertTrue(
            sE.isRight,
            body.contains("\"recordRequests\":true"),
            FakeRift.portOf(body).contains(portOf(sE.toOption.get.head.baseUri)),
            !body.contains("9999"), // advisory port overridden by the pool port
            malformed.swap.toOption.exists {
              case MockError.InvalidDefinition(_) => true
              case _                              => false
            }
          )
      }
    },
    test("received surfaces a malformed imposter view as a CommunicationError") {
      withAdapter { (control, fake) =>
        for
          sE   <- control.provision(pingSource).either
          s     = sE.toOption.get.head
          _    <- fake.setView(portOf(s.baseUri), "this is not json")
          recv <- control.received(s).either
        yield assertTrue(recv.swap.toOption.exists {
          case MockError.CommunicationError(_) => true
          case _                               => false
        })
      }
    },
    test("a non-2xx admin response surfaces as a typed CommunicationError, not a swallowed success") {
      withAdapter { (control, fake) =>
        for
          _ <- fake.failProvision.set(true)
          e <- control.provision(pingSource).either
        yield assertTrue(
          e.isLeft,
          e.swap.toOption.exists {
            case MockError.CommunicationError(msg) => msg.contains("400")
            case _                                 => false
          }
        )
      }
    },
    test(
      "Correlated: provision uses ONE shared imposter (flowIdSource header) with space-scoped stubs; inject stamps the header"
    ) {
      withCorrelated { (control, fake) =>
        for
          aE     <- control.provision(pingSource).either
          bE     <- control.provision(pingSource).either
          posted <- fake.posted.get
          stubs  <- fake.spaceStubs.get
        yield
          val a   = aE.toOption.get.head
          val b   = bE.toOption.get.head
          val req = HttpRequest(Method.Get, "http://sut/x")
          assertTrue(
            aE.isRight && bE.isRight,
            control.isolation == Isolation.Correlated,
            posted.size == 1,                                                 // ONE shared imposter for both spaces
            posted.head.contains("\"flowIdSource\":\"header:X-Mock-Space\""), // Rift gates by the header natively
            posted.head.contains("\"_rift\""),                                // flow-state lives under _rift (native ext), not top-level
            a.baseUri == b.baseUri,                                           // shared imposter -> shared baseUri
            stubs.exists(_.contains(s"/${a.id.value} ")),                     // A's stub scoped under A's flowId
            stubs.exists(_.contains(s"/${b.id.value} ")),
            a.inject(req).headers.get("X-Mock-Space").contains(a.id.value), // inject routes the request to A
            a.inject(req).headers.get("X-Mock-Space") != b.inject(req).headers.get("X-Mock-Space")
          )
      }
    },
    test(
      "Correlated: destroy(A) tears down only A's space (DELETE /spaces/:flowId) — never the imposter or a global reset"
    ) {
      withCorrelated { (control, fake) =>
        for
          aE       <- control.provision(pingSource).either
          bE       <- control.provision(pingSource).either
          a         = aE.toOption.get.head
          b         = bE.toOption.get.head
          destroyE <- control.destroy(a).either
          spaceDel <- fake.spaceDeletes.get
          ports    <- fake.deletedPorts.get
          globals  <- fake.globalDeletes.get
          recvB    <- control.received(b).either
          recvA    <- control.received(a).either
        yield assertTrue(
          destroyE.isRight,
          spaceDel.exists(_.endsWith(s"/${a.id.value}")),  // A's space torn down
          !spaceDel.exists(_.endsWith(s"/${b.id.value}")), // B's untouched
          ports.isEmpty,                                   // the shared imposter is NOT deleted
          globals == 0,                                    // never DELETE /imposters
          recvB.isRight,
          recvA == Left(MockError.SpaceNotFound(a.id)) // A is gone from the adapter
        )
      }
    },
    test("Correlated: received(A) reads the header-filtered requests endpoint (rift#201) and parses the array") {
      withCorrelated { (control, fake) =>
        for
          sE      <- control.provision(pingSource).either
          s        = sE.toOption.get.head
          _       <- fake.setFiltered("""[{"method":"GET","path":"/ping","headers":{},"body":null}]""")
          recvE   <- control.received(s).either
          matches <- fake.requestMatches.get
        yield assertTrue(
          recvE == Right(List(RecordedRequest(Method.Get, "/ping", Map.empty, None))),
          matches.exists(_.contains(s"header:X-Mock-Space=${s.id.value}")) // filtered by THIS space's flowId
        )
      }
    },
    test("Correlated: addRule(Base) appends a space-scoped stub") {
      withCorrelated { (control, fake) =>
        for
          sE     <- control.provision(pingSource).either
          s       = sE.toOption.get.head
          before <- fake.spaceStubs.get
          addE   <- control.addRule(s, pingRule, Priority.Base).either
          after  <- fake.spaceStubs.get
        yield assertTrue(addE.isRight, after.size == before.size + 1, after.exists(_.contains(s"/${s.id.value} ")))
      }
    },
    test("Correlated: replaceRules rebuilds the space (DELETE /spaces/:flowId then re-POSTs the new set)") {
      withCorrelated { (control, fake) =>
        for
          sE         <- control.provision(pingSource).either // posts 1 stub under the flowId
          s           = sE.toOption.get.head
          delsBefore <- fake.spaceDeletes.get
          replE      <- control.replaceRules(s, List(pingRule, pingRule)).either
          dels       <- fake.spaceDeletes.get
          stubs      <- fake.spaceStubs.get
        yield assertTrue(
          replE.isRight,
          delsBefore.isEmpty,
          dels.exists(_.endsWith(s"/${s.id.value}")),      // rebuilt via space teardown
          stubs.count(_.contains(s"/${s.id.value} ")) == 3 // 1 (provision) + 2 (re-POST after delete)
        )
      }
    },
    test("Correlated: addRule(Overlay) rebuilds the space so the overlay is first-match") {
      withCorrelated { (control, fake) =>
        for
          sE    <- control.provision(pingSource).either // 1 base stub
          s      = sE.toOption.get.head
          ovE   <- control.addRule(s, pingRule, Priority.Overlay).either
          dels  <- fake.spaceDeletes.get
          stubs <- fake.spaceStubs.get
        yield assertTrue(
          ovE.isRight,
          dels.exists(_.endsWith(s"/${s.id.value}")),      // overlay triggers a rebuild
          stubs.count(_.contains(s"/${s.id.value} ")) == 3 // 1 (provision) + 2 (rebuild: overlay + base)
        )
      }
    },
    test("Correlated: removeRule rebuilds; an unknown id fails RuleNotFound without touching the server") {
      withCorrelated { (control, fake) =>
        for
          sE      <- control.provision(pingSource).either
          s        = sE.toOption.get.head
          addE    <- control.addRule(s, pingRule, Priority.Base).either
          rmE     <- ZIO.fromEither(addE).flatMap(id => control.removeRule(s, id)).either // rebuild
          dels    <- fake.spaceDeletes.get
          ghostE  <- control.removeRule(s, RuleId("ghost")).either
          delsEnd <- fake.spaceDeletes.get
        yield assertTrue(
          addE.isRight,
          rmE.isRight,
          dels.exists(_.endsWith(s"/${s.id.value}")), // removeRule rebuilt the space
          ghostE == Left(MockError.RuleNotFound(s.id, RuleId("ghost"))),
          delsEnd.size == dels.size // a failed removeRule never hit the server
        )
      }
    },
    test(
      "Correlated(traceparent): shared imposter uses flowIdSource header:traceparent; inject stamps a distinct traceparent per space"
    ) {
      withCorrelatedTraceparent { (control, fake) =>
        for
          aE     <- control.provision(pingSource).either
          bE     <- control.provision(pingSource).either
          posted <- fake.posted.get
        yield
          val a   = aE.toOption.get.head
          val b   = bE.toOption.get.head
          val req = HttpRequest(Method.Get, "http://sut/")
          val ta  = a.inject(req).headers.get("traceparent")
          assertTrue(
            aE.isRight && bE.isRight,
            posted.head.contains("\"flowIdSource\":\"header:traceparent\""),
            ta.exists(_.matches("00-[0-9a-f]{32}-[0-9a-f]{16}-01")), // W3C traceparent shape
            ta != b.inject(req).headers.get("traceparent")           // distinct per space
          )
      }
    },
    test("Correlated: a raw (non-portable) source is rejected with InvalidDefinition (use provisionNative)") {
      withCorrelated { (control, _) =>
        for e <- control.provision(MockSource.Json("""{"port":1,"protocol":"http","stubs":[]}""")).either
        yield assertTrue(e.swap.toOption.exists {
          case MockError.InvalidDefinition(_) => true
          case _                              => false
        })
      }
    }
  ).provide(Client.default, Provisioning.live) @@ TestAspect.withLiveClock
