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

  private def portOf(baseUri: String): Int = baseUri.substring(baseUri.lastIndexOf(':') + 1).toInt

  /**
   * Set up a fresh fake admin server + a Rift adapter (with the given imposter
   * port pool) bound to it, in a scope.
   */
  private def withAdapterPool(ports: List[Int])(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    ZIO.scoped {
      for
        adminAndFake <- FakeRift.started
        (admin, fake) = adminAndFake
        endpoint     <- RiftEndpoint.pooled(admin, ports)(p => s"http://localhost:$p")
        control      <- RiftMockControl.make(endpoint)
        result       <- use(control, fake)
      yield result
    }

  private def withAdapter(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    withAdapterPool((4545 until 4600).toList)(use)

  def spec = suite("RiftMockControl (adapter vs fake admin API)")(
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
    test("advertises no capabilities; accessors fail fast; native specs unsupported") {
      withAdapter { (control, _) =>
        for
          faultsE <- control.faults.either
          nativeE <- control.provisionNative(new NativeSpec[Backend] {}).either
        yield assertTrue(
          control.capabilities.isEmpty,
          faultsE == Left(Unsupported(Capability.Faults, "rift")),
          nativeE.swap.toOption.exists {
            case MockError.InvalidDefinition(_) => true
            case _                              => false
          }
        )
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
    }
  ).provide(Client.default, Provisioning.live) @@ TestAspect.withLiveClock
