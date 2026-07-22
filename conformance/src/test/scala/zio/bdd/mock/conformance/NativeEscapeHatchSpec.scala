package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.Rift
import zio.bdd.mock.wiremock.WireMock
import zio.test.*

/**
 * The native-escape-hatch conformance feature (#127/#119): a
 * natively-provisioned space honors the core contract (serve + record +
 * space-local destroy) on every adapter — but each adapter speaks its OWN
 * native dialect, so this is per-adapter with its own snippet (it can't be a
 * portable `MockControl`-only scenario).
 *
 * WireMock runs in-process always; the Rift backend is gated behind `RIFT_IT`
 * (the `rift-it` CI job runs the conformance module under it), so the Rift
 * container isn't built in the default Docker-free lane.
 */
object NativeEscapeHatchSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable      = new RuntimeException(s"MockError: $e")
  private val upWithin: Schedule[Any, Any, Any] = Schedule.recurs(20) && Schedule.spaced(100.millis)

  // Each adapter's own native dialect, both serving GET /native with "native!".
  // No pinned port: since #214 honours a raw doc's own top-level port, pinning one
  // here would bind every provision on the same port (and an out-of-pool port is
  // host-unreachable on the container adapter). Omit it so each provision draws a
  // fresh mapped/auto port — this suite provisions twice and needs distinct ports.
  private val riftNative =
    """{"protocol":"http","stubs":[{"predicates":[{"equals":{"path":"/native"}}],"responses":[{"is":{"statusCode":200,"body":"native!"}}]}]}"""
  private val wmNative =
    """{"request":{"method":"GET","url":"/native"},"response":{"status":201,"body":"native!"}}"""

  private final case class NativeBackend(
    name: String,
    layer: ZLayer[Any, Throwable, MockControl],
    provisionNative: MockControl => IO[MockError, List[MockSpace]],
    status: Int,
    body: String
  )

  private def hatchSuite(b: NativeBackend) =
    suite(b.name)(
      test("a natively-provisioned space serves + records its full-fidelity contract; destroy is space-local") {
        ZIO.scoped {
          for
            control <- ZIO.service[MockControl]
            a <- ZIO.acquireRelease(b.provisionNative(control).mapError(asT).map(_.head))(s =>
                   control.destroy(s).ignoreLogged
                 )
            other <- ZIO.acquireRelease(b.provisionNative(control).mapError(asT).map(_.head))(s =>
                       control.destroy(s).ignoreLogged
                     )
            served <- SutClient.make(a).send(Method.Get, "/native").retry(upWithin)
            recv   <- control.received(a).mapError(asT)
            // a native space honors rule mutation (overlays) like a portable one (NativeSpec contract)
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
            aGone    <- control.received(a).either // ops on a destroyed space
            bAfter   <- SutClient.make(other).send(Method.Get, "/native").retry(upWithin)
          yield assertTrue(
            served.status == b.status, // native serves its own full-fidelity response
            served.body == b.body,
            recv.exists(r => r.method == Method.Get && r.uri == "/native"), // records
            overlaid.status == 200 && overlaid.body == "overlaid",          // overlay shadows the native rule
            reverted.status == b.status && reverted.body == b.body,         // removeRule reverts to the native rule
            aGone == Left(MockError.SpaceNotFound(a.id)),                   // destroyed-space op -> defined MockError
            bAfter.status == b.status                                       // space-local: B unaffected by A's destroy
          )
        }
      }
    ).provideShared(b.layer) @@ TestAspect.sequential

  private val riftEnabled = sys.env.contains("RIFT_IT")

  private val wiremockBackend =
    NativeBackend(
      "wiremock-native",
      Provisioning.live >>> WireMock.perInstance,
      _.provisionNative(NativeSpec.WireMock(wmNative)),
      201,
      "native!"
    )

  // Excluded from the list (not just ignored) when RIFT_IT is unset, so its
  // container is never built in the Docker-free lane.
  private val riftBackend =
    NativeBackend(
      "rift-native",
      Provisioning.live >>> Rift.managed().mapError(asT),
      _.provisionNative(NativeSpec.Rift(riftNative)),
      200,
      "native!"
    )

  private val backends = wiremockBackend :: (if riftEnabled then List(riftBackend) else Nil)

  def spec = suite("NativeEscapeHatch")(backends.map(hatchSuite)*) @@ TestAspect.withLiveClock
