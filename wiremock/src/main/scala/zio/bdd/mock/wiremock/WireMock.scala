package zio.bdd.mock.wiremock

import zio.*
import zio.bdd.mock.*

import com.github.tomakehurst.wiremock.client.WireMock as WM
import com.github.tomakehurst.wiremock.matching.StringValuePattern

/**
 * How a Correlated space tags its requests and matches them on the shared
 * server. [[value]] is what [[MockSpace.inject]] stamps; [[matcher]] is what
 * the space's stubs require, so only requests carrying this space's tag can
 * match.
 */
final case class Correlation(
  header: String,
  value: SpaceId => String,
  matcher: SpaceId => StringValuePattern
)

object Correlation:
  /** Default: a dedicated `X-Mock-Space: <spaceId>` header, exact-matched. */
  val spaceHeader: Correlation =
    Correlation("X-Mock-Space", id => id.value, id => WM.equalTo(id.value))

  /**
   * Correlate by the W3C trace context the SUT already propagates: `inject`
   * stamps a `traceparent` carrying the space's trace-id, and stubs match that
   * trace-id (the span-id may vary request to request).
   */
  val traceparent: Correlation =
    Correlation(
      "traceparent",
      id => s"00-${traceId(id)}-0000000000000001-01",
      id => WM.matching(s"00-${traceId(id)}-[0-9a-f]{16}-[0-9a-f]{2}")
    )

  // A stable, W3C-shaped 32-hex trace-id derived from the space id. Four
  // salted hashes fill the 32 hex so distinct ids don't collapse to the same
  // repeated chunk (sequential space ids stay distinct).
  private def traceId(id: SpaceId): String =
    (0 until 4).map(i => f"${(id.value + ":" + i).hashCode & 0xffffffffL}%08x").mkString

/**
 * The in-process WireMock provider (#122). Pick an isolation mode and provide a
 * [[Provisioning]]:
 * {{{PortAllocator.layer >>> Provisioning.layer >>> WireMock.correlated()}}}
 * Swapping `WireMock.correlated()` for a Rift layer is the "one-line provider
 * swap" the portable suite relies on.
 */
object WireMock:
  enum Mode:
    case Correlated(correlation: Correlation)
    case PerInstance

  /**
   * Correlated isolation (default): one shared server, header-tagged spaces.
   */
  def correlated(correlation: Correlation = Correlation.spaceHeader): ZLayer[Provisioning, Throwable, MockControl] =
    layer(Mode.Correlated(correlation))

  /**
   * PerInstance isolation: a fresh server per space (use when the cost is
   * acceptable).
   */
  val perInstance: ZLayer[Provisioning, Throwable, MockControl] =
    layer(Mode.PerInstance)

  def layer(mode: Mode): ZLayer[Provisioning, Throwable, MockControl] =
    ZLayer.scoped(WireMockControl.make(mode))
