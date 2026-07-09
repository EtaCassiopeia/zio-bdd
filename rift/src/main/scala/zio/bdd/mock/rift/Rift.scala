package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.{MockControl, MockError, Provisioning, SpaceId}
import zio.http.Client
import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait

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
 * provider of the portable mocking SPI (#113).
 *
 *   - [[connect]] targets an already-running Rift admin endpoint with a known
 *     imposter port pool (used by tests and when Rift runs out-of-band).
 *   - [[managed]] stands up the published Rift image via testcontainers and
 *     stops it with the layer's scope finalizer.
 */
object Rift:

  // The version is `riftVersion` in build.sbt (the single source of truth, #195), surfaced here via
  // the generated RiftBuildInfo so the image tag and the FFI natives version never drift apart.
  // Floor is v0.8.0: the adapter emits the flat `_rift.flowState.flowIdSource` shape
  // (EtaCassiopeia/rift#266, #268), which Rift only reads from v0.8.0 on — earlier images expect the
  // dropped `mountebankStateMapping` wrapper and return 404/empty on the correlated path. v0.8.0 also
  // carries the v0.4.0 correlated-isolation endpoints (#223) and the v0.2.0 keep-alive teardown fix (#207).
  val DefaultImage: String     = s"zainalpour/rift-proxy:v${RiftBuildInfo.riftVersion}"
  val DefaultAdminPort: Int    = 2525
  val DefaultImposterBase: Int = 4545
  val DefaultPoolSize: Int     = 16

  /**
   * Adapter over an already-running Rift: `adminBase` is the admin URL,
   * `imposterPorts` the pool of ports imposters may bind, and `hostFor` maps an
   * imposter port to its SUT-reachable base URI (the identity under host
   * networking).
   */
  def connect(
    adminBase: String,
    imposterPorts: List[Int],
    mode: RiftMode = RiftMode.PerInstance,
    interceptProxy: Option[(String, Int)] = None
  )(hostFor: Int => String): URLayer[Client & Provisioning, MockControl] =
    ZLayer.scoped {
      RiftEndpoint.pooled(adminBase, imposterPorts)(hostFor).flatMap(RiftMockControl.make(_, mode, interceptProxy))
    }

  /**
   * Start the Rift image via testcontainers, exposing the admin port plus a
   * pool of imposter ports, and expose a [[MockControl]] bound to it. The
   * container is stopped when the layer's scope closes.
   *
   * Container port-mapping wrinkle: testcontainers maps each exposed imposter
   * port to an arbitrary host port, so each [[zio.bdd.mock.MockSpace]]
   * `baseUri` is built from the *host-mapped* port. On CI you may instead run
   * Rift with host networking and a pre-mapped port pool, where the mapping is
   * identity.
   */
  def managed(
    image: String = DefaultImage,
    poolSize: Int = DefaultPoolSize,
    adminPort: Int = DefaultAdminPort,
    imposterBasePort: Int = DefaultImposterBase,
    mode: RiftMode = RiftMode.PerInstance,
    interceptPort: Option[Int] = None
  ): ZLayer[Client & Provisioning, MockError, MockControl] =
    ZLayer.scoped {
      val imposterPorts = (0 until poolSize).map(imposterBasePort + _).toList
      // Opt-in intercept (#253): pass the container-internal listener port as a `--intercept-port`
      // command override, and expose it alongside the admin + imposter ports so it maps to a host
      // port. Unlike the embedded adapter's lazy start, the container's listener binds at boot —
      // there is no separate "start" downcall to make on first use.
      // `--allowInjection` enables the script surface (`_rift.script`) the `Scripting` capability
      // drives — Rift ≥ 0.12.0 gates it behind this flag (rift #438). The embedded adapter's FFI
      // engine has no such admin gate.
      val command =
        "--allowInjection" +: interceptPort.fold(Seq.empty[String])(p => Seq("--intercept-port", p.toString))
      for
        container <- ZIO.acquireRelease(
                       ZIO.attemptBlocking {
                         val c = GenericContainer(
                           dockerImage = image,
                           exposedPorts = adminPort :: interceptPort.toList ::: imposterPorts,
                           command = command,
                           waitStrategy = Wait.forHttp("/imposters").forPort(adminPort)
                         )
                         c.start()
                         c
                       }
                         .mapError(t => MockError.ProvisionFailed(s"starting Rift container '$image': ${message(t)}"))
                     )(c => ZIO.attemptBlocking(c.stop()).orDie)
        host           = container.host
        admin          = s"http://$host:${container.mappedPort(adminPort)}"
        endpoint      <- RiftEndpoint.pooled(admin, imposterPorts)(p => s"http://$host:${container.mappedPort(p)}")
        interceptProxy = interceptPort.map(p => (host, container.mappedPort(p)))
        control       <- RiftMockControl.make(endpoint, mode, interceptProxy)
      yield control
    }

  private def message(t: Throwable): String =
    Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
