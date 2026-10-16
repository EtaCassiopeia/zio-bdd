package samples.mock

import samples.mock.support.{Backends, MockState, MockSupport}
import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

import java.net.{ServerSocket, URI}

/**
 * 25 — fixed-port pinning via `MockSpec.onPort` (zio-bdd #211, Plane B). The
 * opt-in fixed-port path: a Rift adapter binds exactly the requested port
 * instead of auto-assigning a free one, so a SUT reaches the mock at a known
 * address (and a re-provision after teardown reuses it — the property `@mock`
 * fixtures depend on). WireMock doesn't honour a requested port, so the feature
 * is tagged `@fixedport` and excluded from the wiremock alias (runs on embedded
 * + rift-external). Surfaced by the tmm-tests-zio-bdd audit (#61).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/25-fixed-port.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object FixedPort25Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:
  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

  /** The port a scenario pinned, stashed for the bind assertion. */
  private final case class PinnedPort(port: Int)

  /**
   * A free port to pin. On a pooled backend it must lie in the pool; take the
   * highest free one, so it stays clear of auto-assigned ports drawn from the
   * bottom. Otherwise grab an OS-assigned port and release it. Racy in
   * principle, but the window is tiny and the pinned imposter binds it
   * immediately after — good enough for a conformance run.
   */
  private val freePort: IO[Throwable, Int] =
    ZIO.attemptBlocking {
      def bindable(p: Int) = scala.util.Try(new ServerSocket(p).close()).isSuccess
      Backends.pinnablePorts match
        case Some(pool) =>
          pool.reverse
            .find(bindable)
            .getOrElse(throw new IllegalStateException(s"no free port left in pool $pool"))
        case None =>
          val s = new ServerSocket(0)
          try s.getLocalPort
          finally s.close()
    }

  private def pinnedMock(port: Int, path: String, body: String): MockSource =
    mock(get(path).respondWith(ok.text(body))).onPort(port).source

  private def provisionPinned(source: MockSource): ZIO[MockControl, Throwable, MockSpace] =
    ctl(_.provision(source)).flatMap(spaces =>
      ZIO
        .fromOption(spaces.headOption)
        .orElseFail(new RuntimeException("onPort provision returned no space"))
        .tap(Stage.put(_))
    )

  Given("a mock pinned to a fixed port serving GET " / string / " text " / string) { (path: String, body: String) =>
    for
      port <- freePort
      _    <- provisionPinned(pinnedMock(port, path, body))
      _    <- Stage.put(PinnedPort(port))
    yield ()
  }

  When("the pinned space is destroyed and re-provisioned on the same port serving " / string) { (body: String) =>
    for
      pinned <- Stage.get[PinnedPort].mapError(e => new IllegalStateException(e.message))
      space  <- activeSpace
      _      <- ctl(_.destroy(space))
      _      <- provisionPinned(pinnedMock(pinned.port, "/pinned", body))
    yield ()
  }

  Then("the active space base URI binds the pinned port") {
    for
      space  <- activeSpace
      pinned <- Stage.get[PinnedPort].mapError(e => new IllegalStateException(e.message))
      bound  <- ZIO.attempt(URI.create(space.baseUri).getPort)
      _      <- Assertions.assertEquals(bound, pinned.port, s"bound port of baseUri ${space.baseUri}")
    yield ()
  }

  Then("a GET " / string / " through the SUT returns " / string) { (path: String, body: String) =>
    send(Method.Get, path).flatMap(r => Assertions.assertEquals(r.body, body, "pinned mock body"))
  }
