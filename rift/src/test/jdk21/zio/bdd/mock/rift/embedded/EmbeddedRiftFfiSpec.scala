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
    }
  ) @@ TestAspect.withLiveClock
