package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
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

  // Same full-fidelity doc but WITHOUT a top-level port, so each provision gets a
  // distinct auto-assigned pool port (a fixed-port doc would bind every provision
  // on the same authored port — #214).
  private val nativeImposterNoPort =
    """{"protocol":"http",
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
            posted.head.contains("\"defaultResponse\":{\"statusCode\":404}"), // unmatched -> 404 (#165)
            FakeRift.portOf(posted.head).contains(portOf(space.baseUri)),
            space.baseUri == s"http://localhost:${portOf(space.baseUri)}",
            space.inject(req) == req
          )
      }
    },
    test("#211: an authored port binds the imposter on THAT port — baseUri and the create body both use it") {
      val fixed = 9998 // outside withAdapter's 4545..4600 pool, so a pool port can't masquerade as a pass
      withAdapter { (control, fake) =>
        for
          spacesE <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(fixed)))).either
          posted  <- fake.posted.get
        yield
          val space = spacesE.toOption.toList.flatten.head
          assertTrue(
            spacesE.isRight,
            space.baseUri == s"http://localhost:$fixed",
            posted.exists(b => FakeRift.portOf(b).contains(fixed))
          )
      }
    },
    test("#211: omitting the port preserves the default — the imposter gets an auto-assigned pool port") {
      withAdapterPool(List(4600)) { (control, _) =>
        for spacesE <- control.provision(pingSource).either
        yield assertTrue(spacesE.toOption.toList.flatten.head.baseUri == "http://localhost:4600")
      }
    },
    test("#211: a create rejection on the authored port surfaces as a descriptive MockError — no silent fallback") {
      withAdapter { (control, fake) =>
        for
          _      <- fake.failProvision.set(true)
          result <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(9998)))).either
        yield assertTrue(
          result.isLeft,
          result.swap.toOption.exists(_.toString.contains("9998")) // names the port, not a swallowed generic error
        )
      }
    },
    test("#211: destroying an authored-port space does not return the fixed port to the pool") {
      withAdapterPool(List(4600)) { (control, _) => // pool holds exactly one port: 4600
        for
          fixedE <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(9998)))).either
          fixed   = fixedE.toOption.toList.flatten.head
          _      <- control.destroy(fixed).either
          autoE  <- control.provision(pingSource).either // must draw the pool's own 4600, never the freed 9998
        yield assertTrue(
          fixedE.isRight,
          autoE.toOption.toList.flatten.head.baseUri == "http://localhost:4600"
        )
      }
    },
    test("#213: an in-pool-range authored port is claimed so a later auto-provision cannot collide") {
      withAdapterPool(List(4550, 4551)) { (control, _) =>
        for
          authoredE <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(4550)))).either
          autoE     <- control.provision(pingSource).either
        yield
          val authored = authoredE.toOption.toList.flatten.head
          val auto     = autoE.toOption.toList.flatten.head
          assertTrue(
            authored.baseUri == "http://localhost:4550",
            // The authored port was removed from the free-list, so the auto-assigned
            // imposter draws 4551 — never a second imposter on 4550.
            auto.baseUri == "http://localhost:4551",
            auto.baseUri != authored.baseUri
          )
      }
    },
    test("#213: an in-pool-range authored port returns to the pool on destroy (no depletion)") {
      withAdapterPool(List(4550)) { (control, _) => // pool holds exactly the port we author
        for
          authoredE <- control.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(4550)))).either
          authored   = authoredE.toOption.toList.flatten.head
          _         <- control.destroy(authored).either
          autoE     <- control.provision(pingSource).either // 4550 must have come back to the pool
        yield assertTrue(
          authoredE.isRight,
          autoE.toOption.toList.flatten.head.baseUri == "http://localhost:4550"
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
        yield assertTrue(recvE == Right(List(RecordedRequest(Method.Get, "/ping", Headers.empty, None))))
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
    test("advertises all six capabilities and every accessor succeeds (#132)") {
      withAdapter { (control, _) =>
        for
          faultsE   <- control.faults.either
          scenE     <- control.scenarios.either
          siE       <- control.stateInspection.either
          scriptE   <- control.scripting.either
          proxyE    <- control.proxyRecord.either
          templateE <- control.templating.either
        yield assertTrue(
          // The container adapter advertises every capability except Intercept (#219 — embedded-only).
          control.capabilities == Capability.values.toSet - Capability.Intercept,
          faultsE.isRight,
          scenE.isRight,
          siE.isRight,
          scriptE.isRight,
          proxyE.isRight,
          templateE.isRight
        )
      }
    },
    test("faults.inject posts a first-match stub carrying _rift.fault.tcp to the space's imposter") {
      withAdapter { (control, fake) =>
        for
          space  <- control.provision(pingSource).orDieWith(e => new RuntimeException(e.toString)).map(_.head)
          faults <- control.faults.orDieWith(u => new RuntimeException(u.toString))
          ruleId <- faults
                      .inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
                      .orDieWith(e => new RuntimeException(e.toString))
          calls <- fake.stubCalls.get
          post   = calls.find(c => c.startsWith(s"POST ${portOf(space.baseUri)} "))
        yield assertTrue(
          ruleId.value.nonEmpty,
          post.exists(_.contains("\"index\":0")), // first-match — wins over a normal rule
          post.exists(_.contains("CONNECTION_RESET_BY_PEER")),
          post.exists(_.contains("/boom"))
        )
      }
    },
    test("scripting/proxyRecord/templating each post a first-match capability stub to the imposter (#132)") {
      withAdapter { (control, fake) =>
        def asT(e: Any): Throwable = new RuntimeException(e.toString)
        for
          space    <- control.provision(pingSource).orDieWith(asT).map(_.head)
          script   <- control.scripting.orDieWith(asT)
          proxy    <- control.proxyRecord.orDieWith(asT)
          template <- control.templating.orDieWith(asT)
          sid <- script
                   .inject(space, RequestMatch(path = PathMatch.Exact("/s")), Script(ScriptEngine.Rhai, "code"))
                   .orDieWith(asT)
          pid <- proxy.proxy(space, RequestMatch(path = PathMatch.Exact("/p")), "http://up").orDieWith(asT)
          tid <-
            template
              .inject(
                space,
                RequestMatch(path = PathMatch.Exact("/t")),
                ResponseTemplate(body = "x${V}", captures = List(TemplateCapture("${V}", TemplateSource.Body, ".*")))
              )
              .orDieWith(asT)
          calls <- fake.stubCalls.get
          posts  = calls.filter(_.startsWith(s"POST ${portOf(space.baseUri)} "))
          // the capability stubs are tracked in imp.stubs, so removeRule reaches them (not RuleNotFound)
          rmS <- control.removeRule(space, sid).either
          rmT <- control.removeRule(space, tid).either
        yield assertTrue(
          sid.value.nonEmpty && pid.value.nonEmpty && tid.value.nonEmpty,
          posts.forall(_.contains("\"index\":0")), // all first-match
          posts.exists(c => c.contains("_rift") && c.contains("script") && c.contains("rhai")),
          posts.exists(c => c.contains("proxy") && c.contains("proxyOnce") && c.contains("http://up")),
          posts.exists(c => c.contains("copy") && c.contains("${V}")),
          rmS.isRight && rmT.isRight
        )
      }
    },
    test(
      "Correlated scripting/proxy/templating each register a capability space stub ahead of the rules and are removable (#132)"
    ) {
      withCorrelated { (control, fake) =>
        def asT(e: Any): Throwable = new RuntimeException(e.toString)
        for
          space    <- control.provision(pingSource).orDieWith(asT).map(_.head)
          script   <- control.scripting.orDieWith(asT)
          proxy    <- control.proxyRecord.orDieWith(asT)
          template <- control.templating.orDieWith(asT)
          sid <- script
                   .inject(space, RequestMatch(path = PathMatch.Exact("/s")), Script(ScriptEngine.Rhai, "code"))
                   .orDieWith(asT)
          pid <- proxy.proxy(space, RequestMatch(path = PathMatch.Exact("/p")), "http://up").orDieWith(asT)
          tid <-
            template
              .inject(
                space,
                RequestMatch(path = PathMatch.Exact("/t")),
                ResponseTemplate(body = "x${V}", captures = List(TemplateCapture("${V}", TemplateSource.Body, ".*")))
              )
              .orDieWith(asT)
          stubs   <- fake.spaceStubs.get // "$port/$flowId $body" per POST .../spaces/:flow/stubs
          scriptIx = stubs.indexWhere(s => s.contains("script") && s.contains("rhai") && s.contains("/s"))
          // a rebuild re-registers the space's /ping rule AFTER the capability stubs → they win first-match
          rulePast = stubs.zipWithIndex.exists((s, i) => i > scriptIx && s.contains("/ping"))
          rmS     <- control.removeRule(space, sid).either // tracked in cs.extras → removable, not RuleNotFound
          rmP     <- control.removeRule(space, pid).either
          rmT     <- control.removeRule(space, tid).either
        yield assertTrue(
          sid.value.nonEmpty && pid.value.nonEmpty && tid.value.nonEmpty,
          scriptIx >= 0,
          rulePast,
          stubs.exists(s => s.contains("proxyOnce") && s.contains("/p")),
          stubs.exists(s => s.contains("copy") && s.contains("/t")),
          rmS.isRight && rmP.isRight && rmT.isRight
        )
      }
    },
    test("Correlated faults.inject registers a fault space stub ahead of the rules (first-match) and is removable") {
      withCorrelated { (control, fake) =>
        for
          space  <- control.provision(pingSource).orDieWith(e => new RuntimeException(e.toString)).map(_.head)
          faults <- control.faults.orDieWith(u => new RuntimeException(u.toString))
          ruleId <- faults
                      .inject(space, RequestMatch(path = PathMatch.Exact("/boom")), FaultKind.ConnectionReset)
                      .orDieWith(e => new RuntimeException(e.toString))
          stubs <- fake.spaceStubs.get // "$port/$flowId $body" per POST .../spaces/:flow/stubs
          faultIx =
            stubs.indexWhere(s => s.contains("_rift") && s.contains("CONNECTION_RESET_BY_PEER") && s.contains("/boom"))
          // the rebuild re-registers the space's /ping rule AFTER the fault → fault wins first-match
          rulePast = stubs.zipWithIndex.exists((s, i) => i > faultIx && s.contains("/ping"))
          rmE     <- control.removeRule(space, ruleId).either // tracked in cs.faults → removable, not RuleNotFound
        yield assertTrue(
          ruleId.value.nonEmpty,
          faultIx >= 0,
          rulePast,
          rmE.isRight
        )
      }
    },
    // The container proves end-to-end stateful behaviour (RiftContainerSpec / cap-stateful under RIFT_IT);
    // here we assert the adapter issues the right scenario admin calls — no Docker, no wrong-path regression.
    test("define/setState/currentState/reset issue the right Rift scenario admin calls (#131)") {
      val invoice =
        scenario("invoice")
          .when("Started", get("/inv"))
          .respond(ok.text("created"))
          .goTo("Paid")
          .when("Paid", get("/inv"))
          .respond(ok.text("paid"))
          .stay
          .build
      withAdapter { (control, fake) =>
        for
          space <-
            control.provision(MockSource.Dsl(MockSpec(Nil))).orDieWith(e => RuntimeException(e.toString)).map(_.head)
          port    = portOf(space.baseUri)
          ss     <- control.scenarios.orDieWith(u => RuntimeException(u.toString))
          si     <- control.stateInspection.orDieWith(u => RuntimeException(u.toString))
          _      <- ss.define(space, invoice).orDieWith(e => RuntimeException(e.toString))
          stubs  <- fake.stubCalls.get
          _      <- fake.setScenarios(s"""{"flowId":"$port","scenarios":[{"name":"invoice","state":"Paid"}]}""")
          cur    <- si.currentState(space, "invoice").orDieWith(e => RuntimeException(e.toString))
          gets   <- fake.scenarioGets.get
          _      <- si.setState(space, "invoice", ScenarioState("Paid")).orDieWith(e => RuntimeException(e.toString))
          _      <- ss.reset(space, "invoice").orDieWith(e => RuntimeException(e.toString))
          puts   <- fake.scenarioPuts.get
          ghost  <- si.currentState(space, "ghost").either
          ghostR <- ss.reset(space, "ghost").either
        yield assertTrue(
          // define posts both FSM edges with the scenario fields; rule 0 is at index 0 (first-match order)
          stubs.count(_.contains("\"scenarioName\":\"invoice\"")) == 2,
          stubs.exists(s => s.contains("\"index\":0") && s.contains("\"requiredScenarioState\":\"Started\"")),
          stubs.exists(_.contains("\"newScenarioState\":\"Paid\"")), // the advance edge
          // define pins the initial state via PUT /scenarios/invoice/state {state, flowId=port}
          puts.exists(p =>
            p.startsWith(s"$port/invoice ") && p.contains("\"state\":\"Started\"") && p.contains(
              s"\"flowId\":\"$port\""
            )
          ),
          // currentState reads GET /scenarios?flowId=port and parses the view
          cur == ScenarioState("Paid"),
          gets.exists(_ == s"$port?$port"),
          // setState PUTs the new state
          puts.exists(_.contains("\"state\":\"Paid\"")),
          // an unknown scenario fails with InvalidDefinition (Ref guard + absent in the view)
          ghost == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}")),
          ghostR == Left(MockError.InvalidDefinition(s"no scenario 'ghost' on space ${space.id.value}"))
        )
      }
    },
    test("provisionNative(Rift) honours the document's own top-level port (#214)") {
      withAdapter { (control, fake) =>
        for
          sE     <- control.provisionNative(NativeSpec.Rift(nativeImposter)).either // doc declares "port":9999
          posted <- fake.posted.get
        yield
          val space = sE.toOption.get.head
          val body  = posted.headOption.getOrElse("")
          val req   = HttpRequest(Method.Get, "http://sut/")
          assertTrue(
            sE.isRight,
            body.contains("\"_rift\""),               // full-fidelity: _rift extensions preserved
            body.contains("/native"),                 // the native stub preserved
            body.contains("\"recordRequests\":true"), // forced on so received() works
            space.baseUri == "http://localhost:9999", // the document's authored port is honoured (#214)
            FakeRift.portOf(body).contains(9999),     // imposter created on 9999, not an auto pool port
            space.inject(req) == req                  // isolation: identity inject like a portable space
          )
      }
    },
    test("provisionNative(Rift) without a top-level port auto-assigns a pool port (#214)") {
      val noPort = """{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/np"}}],
                     | "responses":[{"is":{"statusCode":200}}]}]}""".stripMargin
      withAdapterPool(List(4600)) { (control, _) =>
        for sE <- control.provisionNative(NativeSpec.Rift(noPort)).either
        yield assertTrue(sE.toOption.get.head.baseUri == "http://localhost:4600")
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
          recvE == Right(List(RecordedRequest(Method.Get, "/native", Headers.empty, None))),
          destroyE.isRight,
          deleted == Chunk(port), // space-local: only this imposter
          globals == 0            // never the global reset
        )
      }
    },
    test("native spaces isolate: two get distinct ports; destroy(A) leaves B") {
      withAdapter { (control, _) =>
        for
          aE    <- control.provisionNative(NativeSpec.Rift(nativeImposterNoPort)).either
          bE    <- control.provisionNative(NativeSpec.Rift(nativeImposterNoPort)).either
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
            posted.head.contains("\"defaultResponse\":{\"statusCode\":404}"), // unmatched -> 404 (#165)
            a.baseUri == b.baseUri,                                           // shared imposter -> shared baseUri
            stubs.exists(_.contains(s"/${a.id.value} ")),                     // A's stub scoped under A's flowId
            stubs.exists(_.contains(s"/${b.id.value} ")),
            a.inject(req).headers.first("X-Mock-Space").contains(a.id.value), // inject routes the request to A
            a.inject(req).headers.first("X-Mock-Space") != b.inject(req).headers.first("X-Mock-Space")
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
          recvE == Right(List(RecordedRequest(Method.Get, "/ping", Headers.empty, None))),
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
          val ta  = a.inject(req).headers.first("traceparent")
          assertTrue(
            aE.isRight && bE.isRight,
            posted.head.contains("\"flowIdSource\":\"header:traceparent\""),
            ta.exists(_.matches("00-[0-9a-f]{32}-[0-9a-f]{16}-01")), // W3C traceparent shape
            ta != b.inject(req).headers.first("traceparent")         // distinct per space
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
