package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.test.*

import java.nio.file.{Files, Path}

/**
 * Exercises the full core port of [[EmbeddedRift]] over the live `librift_ffi`
 * C-ABI: start the engine, provision an imposter on an OS-assigned port, serve
 * + record a real HTTP round-trip, mutate rules (overlay / replace), and tear
 * the space down. It is gated on [[EmbeddedRift.available]] — when the native
 * library is not provisioned (no `-Drift.ffi.lib`), the case is skipped rather
 * than failed, exactly like the Docker-gated container suite.
 */
object EmbeddedRiftFfiSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private val pingSource =
    MockSource.Dsl(
      MockSpec(
        List(
          MockRule(
            RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
            ResponseDef(status = 200, body = Body.Text("pong"))
          )
        )
      )
    )

  private def v2Rule =
    MockRule(RequestMatch(path = PathMatch.Exact("/v2")), ResponseDef(status = 200, body = Body.Text("v2")))

  // A valid raw Rift imposter document (no port — the adapter forces an OS-assigned one).
  private val goodRaw =
    """{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/a"}}],"responses":[{"is":{"statusCode":200,"body":"a"}}]}]}"""

  // A raw native imposter serving GET /native — used to verify native stubs survive rule mutation.
  private val nativeRaw =
    """{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/native"}}],"responses":[{"is":{"statusCode":200,"body":"native!"}}]}]}"""

  // A temp directory with one valid and one malformed imposter file; sorted, the good one provisions
  // first, then the bad one fails — exercising the multi-source rollback. Cleaned up with the scope.
  private def tempDirWithGoodAndBad: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        val dir = Files.createTempDirectory("embedded-rollback")
        Files.writeString(dir.resolve("a-good.json"), goodRaw)
        Files.writeString(dir.resolve("b-bad.json"), "this is not json")
        dir
      }
    )(dir =>
      ZIO.attemptBlocking {
        Files.deleteIfExists(dir.resolve("a-good.json"))
        Files.deleteIfExists(dir.resolve("b-bad.json"))
        Files.deleteIfExists(dir)
      }.ignore
    )

  def spec = suite("EmbeddedRiftFfi")(
    test("core lifecycle over FFM: provision → serve → record → mutate → destroy") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes) // skipped: no native library
      else
        (for
          control <- ZIO.service[MockControl]
          spaces  <- control.provision(pingSource).mapError(asT)
          space    = spaces.head
          resp    <- SutClient.make(space).send(Method.Get, "/ping")
          recv    <- control.received(space).mapError(asT)
          // overlay then replace: both realised as a whole-imposter rift_replace_stubs
          _    <- control.addRule(space, v2Rule, Priority.Overlay).mapError(asT)
          v2   <- SutClient.make(space).send(Method.Get, "/v2")
          _    <- control.replaceRules(space, List(v2Rule)).mapError(asT)
          oldR <- SutClient.make(space).send(Method.Get, "/ping")
          newR <- SutClient.make(space).send(Method.Get, "/v2")
          _    <- control.destroy(space).mapError(asT)
          gone <- control.received(space).either
        yield assertTrue(
          resp.status == 200,
          resp.body == "pong",
          recv.exists(_.uri == "/ping"),
          v2.status == 200 && v2.body == "v2",
          oldR.status == 404, // replaceRules dropped the /ping rule
          newR.status == 200 && newR.body == "v2",
          gone == Left(MockError.SpaceNotFound(space.id)) // ops on a destroyed space fail SpaceNotFound
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("provisionNative: a raw Rift imposter serves, and an overlay reverts to its native stub") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          control <- ZIO.service[MockControl]
          a       <- control.provisionNative(NativeSpec.Rift(nativeRaw)).mapError(asT).map(_.head)
          served  <- SutClient.make(a).send(Method.Get, "/native")
          ovId <- control
                    .addRule(
                      a,
                      MockRule(
                        RequestMatch(path = PathMatch.Exact("/native")),
                        ResponseDef(status = 200, body = Body.Text("overlaid"))
                      ),
                      Priority.Overlay
                    )
                    .mapError(asT)
          overlaid <- SutClient.make(a).send(Method.Get, "/native")
          _        <- control.removeRule(a, ovId).mapError(asT)
          reverted <- SutClient.make(a).send(Method.Get, "/native")
          _        <- control.destroy(a).mapError(asT)
        yield assertTrue(
          served.status == 200 && served.body == "native!",
          overlaid.status == 200 && overlaid.body == "overlaid", // overlay shadows the native stub
          reverted.status == 200 && reverted.body == "native!"   // native stub preserved across mutation
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("proxyRecord: an injected proxy records the upstream response and replays it (#185)") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        // A second embedded imposter is the upstream: the proxy on the target space forwards GET /u
        // to it, records the first response, and replays it on the second call — proving the advertised
        // ProxyRecord capability works end-to-end over the FFI, not just that the adapter emits the JSON.
        val upstreamSource =
          MockSource.Dsl(
            MockSpec(
              List(
                MockRule(
                  RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/u")),
                  ResponseDef(status = 200, body = Body.Text("from-upstream"))
                )
              )
            )
          )
        (for
          control  <- ZIO.service[MockControl]
          upstream <- control.provision(upstreamSource).mapError(asT).map(_.head)
          target   <- control.provision(pingSource).mapError(asT).map(_.head)
          proxy    <- control.proxyRecord.mapError(u => new RuntimeException(s"Unsupported: $u"))
          _        <- proxy.proxy(target, RequestMatch(path = PathMatch.Exact("/u")), upstream.baseUri).mapError(asT)
          first    <- SutClient.make(target).send(Method.Get, "/u")
          replayed <- SutClient.make(target).send(Method.Get, "/u")
          _        <- control.destroy(target).mapError(asT)
          _        <- control.destroy(upstream).mapError(asT)
        yield assertTrue(
          first.status == 200 && first.body == "from-upstream",      // proxied + recorded
          replayed.status == 200 && replayed.body == "from-upstream" // replayed from the recording
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("provisionNative: a WireMock spec is rejected with InvalidDefinition") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          control  <- ZIO.service[MockControl]
          rejected <- control.provisionNative(NativeSpec.WireMock("{}")).either
        yield assertTrue(rejected match
          case Left(_: MockError.InvalidDefinition) => true
          case _                                    => false
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("provision is atomic: a failed source in a multi-source Dir rolls back the served spaces") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO
          .scoped(for
            dir     <- tempDirWithGoodAndBad
            control <- ZIO.service[MockControl]
            // a-good provisions, then b-bad fails → the whole provision fails and a-good is torn down.
            result <- control.provision(MockSource.Dir(dir.toString)).either
          yield assertTrue(result.isLeft))
          .provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("v2: rift_serve_admin yields a loopback admin URL and rift_build_info a non-empty version") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped(
          for
            engine <- EmbeddedRift.startEngine.mapError(asT)
            info   <- engine.serveAdmin("""{"host":"127.0.0.1","port":0}""").mapError(asT)
            build  <- engine.buildInfo.mapError(asT)
          yield assertTrue(
            info.adminUrl.startsWith("http://127.0.0.1:"),
            info.adminPort > 0,
            build.version.nonEmpty
          )
        )
    },
    test("v2: capability set is all six and stateful scenarios round-trip over the in-process admin plane") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        val checkout =
          ScenarioDef(
            "checkout",
            List(
              StatefulRule(
                ScenarioState.Started,
                RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/step")),
                ResponseDef(status = 200, body = Body.Text("one")),
                thenState = Some(ScenarioState("Done"))
              )
            ),
            initial = ScenarioState.Started
          )
        (for
          control <- ZIO.service[MockControl]
          space   <- control.provision(pingSource).mapError(asT).map(_.head)
          ss      <- control.scenarios.mapError(u => new RuntimeException(s"Unsupported: $u"))
          si      <- control.stateInspection.mapError(u => new RuntimeException(s"Unsupported: $u"))
          _       <- ss.define(space, checkout).mapError(asT)
          s0      <- si.currentState(space, "checkout").mapError(asT)
          stepped <- SutClient.make(space).send(Method.Get, "/step")
          s1      <- si.currentState(space, "checkout").mapError(asT)
          _       <- ss.reset(space, "checkout").mapError(asT)
          s2      <- si.currentState(space, "checkout").mapError(asT)
          _       <- control.destroy(space).mapError(asT)
        yield assertTrue(
          control.capabilities == Set(
            Capability.Faults,
            Capability.Scripting,
            Capability.ProxyRecord,
            Capability.Templating,
            Capability.StatefulScenarios,
            Capability.StateInspection
          ),
          s0 == ScenarioState.Started,
          stepped.status == 200 && stepped.body == "one",
          s1 == ScenarioState("Done"), // serving /step advanced the FSM
          s2 == ScenarioState.Started  // reset re-pinned the initial state
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    },
    test("v2: destroy frees the port via rift_delete_imposter — an immediate re-provision serves again") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        (for
          control <- ZIO.service[MockControl]
          first   <- control.provision(pingSource).mapError(asT).map(_.head)
          r1      <- SutClient.make(first).send(Method.Get, "/ping")
          _       <- control.destroy(first).mapError(asT)
          second  <- control.provision(pingSource).mapError(asT).map(_.head)
          r2      <- SutClient.make(second).send(Method.Get, "/ping")
          _       <- control.destroy(second).mapError(asT)
        yield assertTrue(
          r1.status == 200 && r1.body == "pong",
          r2.status == 200 && r2.body == "pong" // re-provision after destroy serves — no port leak
        )).provide(Provisioning.live, EmbeddedRift.layer.mapError(asT))
    }
  ) @@ TestAspect.withLiveClock
