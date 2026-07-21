package zio.bdd.mock.rift

import java.net.URI

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning, SpaceId}

import rift.bridge.{ConnectConfig, ContainerConfig}
import rift.zio.Rift as SdkRift

/**
 * How a Correlated space tags its requests on the shared imposter. `header` is
 * the correlation header (Rift's `flowIdSource = header:<header>` gates stubs +
 * recorded requests by it natively); `value` is what `MockSpace.inject` stamps
 * on each request — and the `flowId` the adapter scopes stubs/teardown under.
 */
final case class Correlation(header: String, value: SpaceId => String)

object Correlation:
  /** Default: a dedicated `X-Mock-Space: <spaceId>` header. */
  val spaceHeader: Correlation = Correlation("X-Mock-Space", id => id.value)

  /**
   * Correlate by the W3C trace context the SUT already propagates. NOTE: Rift's
   * flow-id is the whole header value, so `inject` stamps a fixed traceparent
   * per space — the SUT cannot vary the span-id (unlike the WireMock adapter,
   * which matches by trace-id).
   */
  val traceparent: Correlation =
    Correlation("traceparent", id => s"00-${traceId(id)}-0000000000000001-01")

  private def traceId(id: SpaceId): String =
    (0 until 4).map(i => f"${(id.value + ":" + i).hashCode & 0xffffffffL}%08x").mkString

/**
 * Rift isolation mode (#156). PerInstance (own port per space) is the default.
 */
enum RiftMode:
  case PerInstance
  case Correlated(correlation: Correlation)

object RiftMode:
  /** Correlated isolation with the default `X-Mock-Space` correlation. */
  val correlated: RiftMode = Correlated(Correlation.spaceHeader)

/**
 * Entry points for the Rift adapter — the default, zero-native-artifact
 * provider of the portable mocking SPI (#113), now driving the engine through
 * the official `rift-scala-zio` SDK (#285) instead of a hand-rolled admin
 * protocol/FFI implementation:
 *
 *   - [[connect]] targets an already-running Rift admin endpoint with a known
 *     imposter port pool (used by tests and when Rift runs out-of-band).
 *   - [[managed]] stands up the published Rift image via the SDK's
 *     testcontainers transport and stops it with the layer's scope finalizer.
 *
 * Both layers require only [[Provisioning]] — the SDK owns the transport, so
 * the `zio-http` `Client` these used to require is gone (an intended source
 * break, #285).
 */
object Rift:

  // The version is `riftVersion` in build.sbt (the single source of truth, #195), surfaced here via
  // the generated RiftBuildInfo so the image tag stays in one place.
  val DefaultImage: String = s"zainalpour/rift-proxy:v${RiftBuildInfo.riftVersion}"

  /**
   * The container's admin port. The SDK's `RiftContainer` hardcodes this (no
   * builder override), so unlike the pre-#285 adapter it can no longer be
   * freely reconfigured — passing a non-default value to [[managed]] fails fast
   * with a typed [[MockError.InvalidDefinition]] rather than silently ignoring
   * it.
   */
  val DefaultAdminPort: Int    = 2525
  val DefaultImposterBase: Int = 4545
  val DefaultPoolSize: Int     = 16

  /**
   * Adapter over an already-running Rift: `adminBase` is the admin URL,
   * `imposterPorts` the pool of ports imposters may bind, and `hostFor` maps an
   * imposter port to its SUT-reachable base URI (the identity under host
   * networking) — wired into the SDK's `ConnectConfig.hostResolver`.
   *
   * Widened from a `URLayer` (pre-#285): the SDK's `connect` performs a real
   * handshake at layer construction (an admin-version check), so a bad
   * `adminBase` or an unreachable engine now surfaces as a typed [[MockError]]
   * there instead of only on first use.
   */
  def connect(
    adminBase: String,
    imposterPorts: List[Int],
    mode: RiftMode = RiftMode.PerInstance,
    interceptProxy: Option[(String, Int)] = None
  )(hostFor: Int => String): ZLayer[Provisioning, MockError, MockControl] =
    parseUri(adminBase) match
      case Left(e) => ZLayer.fail(e)
      case Right(uri) =>
        val config    = ConnectConfig(adminUri = uri, hostResolver = Some(p => URI.create(hostFor(p))))
        val intercept = interceptSettingsOf(interceptProxy)
        ZLayer.makeSome[Provisioning, MockControl](
          SdkRift.connect(config).mapError(RiftModelMapping.toMockError(None)),
          // Intercept is reachable over `connect` only when the caller told us where its listener
          // will be host-reachable (`interceptProxy`) — without it there's no host-side address to
          // report from `proxyPort`, so the capability must not be advertised (#285/B5).
          adapterLayer(mode, Some(imposterPorts.toVector), intercept, interceptCapable = interceptProxy.isDefined)
        )

  /**
   * Start the Rift image via the SDK's testcontainers transport, exposing the
   * admin port plus a pool of imposter ports, and expose a [[MockControl]]
   * bound to it. The container is stopped when the layer's scope closes.
   *
   * Container port-mapping wrinkle: the SDK wires `ImposterHandle.uri` through
   * Docker's port mapping automatically (`RiftContainer`'s own `hostResolver`),
   * so every [[zio.bdd.mock.MockSpace]] `baseUri` is already host-reachable
   * with no adapter-side mapping.
   */
  def managed(
    image: String = DefaultImage,
    poolSize: Int = DefaultPoolSize,
    adminPort: Int = DefaultAdminPort,
    imposterBasePort: Int = DefaultImposterBase,
    mode: RiftMode = RiftMode.PerInstance,
    interceptPort: Option[Int] = None
  ): ZLayer[Provisioning, MockError, MockControl] =
    if adminPort != DefaultAdminPort then
      ZLayer.fail(
        MockError.InvalidDefinition(
          s"Rift.managed's adminPort is fixed at $DefaultAdminPort by the rift-scala SDK's container " +
            s"transport (RiftContainer hardcodes it, no override) and cannot be changed to $adminPort; " +
            s"omit adminPort or pass $DefaultAdminPort"
        )
      )
    else
      val imposterPorts = (0 until poolSize).map(imposterBasePort + _).toVector
      val containerConfig =
        ContainerConfig(image = Some(image), imposterPorts = imposterPorts, interceptPort = interceptPort)
      // The intercept listener binds inside the container: "0.0.0.0" (not loopback) so the
      // container's own network stack accepts the connection Docker's port mapping forwards in.
      val intercept = interceptPort.fold(InterceptSettings())(p => InterceptSettings(bindHost = "0.0.0.0", port = p))
      ZLayer.makeSome[Provisioning, MockControl](
        SdkRift.container(containerConfig).mapError(RiftModelMapping.toMockError(None)),
        // Without `interceptPort`, nothing is published on the container's port mapping: the listener
        // would bind inside the container's netns and `proxyPort` would report an address the SUT can
        // never reach. Advertise Intercept only when the caller actually exposed a port for it (#285/B5).
        adapterLayer(mode, Some(imposterPorts), intercept, interceptCapable = interceptPort.isDefined)
      )

  private def interceptSettingsOf(interceptProxy: Option[(String, Int)]): InterceptSettings =
    interceptProxy.fold(InterceptSettings())((host, port) => InterceptSettings(bindHost = host, port = port))

  private def parseUri(adminBase: String): Either[MockError, URI] =
    try Right(URI.create(adminBase))
    catch
      case e: IllegalArgumentException =>
        Left(MockError.InvalidDefinition(s"invalid admin URL $adminBase: ${e.getMessage}"))

  /**
   * Wires an already-provided SDK `rift.zio.Rift` (from
   * `SdkRift.embedded`/`connect`/`container`) into the portable [[MockControl]]
   * adapter — the shared tail every entry point above funnels into.
   */
  private[rift] def adapterLayer(
    mode: RiftMode,
    portPool: Option[Vector[Int]],
    intercept: InterceptSettings,
    interceptCapable: Boolean
  ): URLayer[SdkRift & Provisioning, MockControl] =
    ZLayer.scoped {
      for
        rift    <- ZIO.service[SdkRift]
        pool    <- ZIO.foreach(portPool)(RiftPortPool.fixed)
        control <- RiftMockControl.make(rift, pool, mode, intercept, interceptCapable)
      yield control
    }
