package samples.mock.support

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.wiremock.WireMock
import zio.bdd.mock.rift.Rift
import zio.bdd.mock.rift.embedded.EmbeddedRift

/**
 * Runtime backend selection for the mock corpus.
 *
 * A suite's `environment` is fixed at compile time, so we pick the backend from
 * the `MOCK_BACKEND` env var inside this shared layer — the one seam every mock
 * suite wires to (`override def environment = Backends.mockControl`).
 * Capability differences between backends are handled by the harness excluding
 * tags per backend (e.g. WireMock runs skip `@scripting`), never by branching
 * here.
 *
 *   - `wiremock` (default) — in-process, zero Docker, Correlated isolation.
 *   - `wiremock-pi` — WireMock in PerInstance isolation.
 *   - `rift` — Rift container via testcontainers (needs Docker; the harness
 *     gates this behind RIFT_IT). Advertises every capability.
 *   - `embedded` — in-process Rift via Panama FFM (needs JDK 22+; the harness
 *     gates this behind that JVM floor). Core port +
 *     Faults/Scripting/ProxyRecord/Templating.
 *   - `rift-external` — a released rift binary spawned as a child process by
 *     [[ExternalRift]] (no Docker, no FFM floor). Advertises (and runs) the
 *     same capability surface as `rift`, minus raw WireMock-only scenarios.
 */
object Backends:

  /** The backend name the harness selected, for diagnostics. */
  val selected: String = sys.env.getOrElse("MOCK_BACKEND", "wiremock")

  private val provisioning: ULayer[Provisioning] = ZLayer(Provisioning.make)

  // Imposter port pool for `rift-external` (46xx). The engine's own admin and metrics ports are
  // OS-assigned ephemeral ports, so they never land in it.
  private val RiftExternalPortBase = 4600
  private val RiftExternalPoolSize = 40

  /**
   * Ports a scenario may pin with `onPort` on the selected backend, or `None`
   * when any free port will do. Since zio-bdd 1.5.0 a pooled adapter
   * (`Rift.connect`) refuses an authored port outside its pool (zio-bdd
   * #213/#303), so a pinned port must come from inside it.
   */
  val pinnablePorts: Option[Range] =
    Option.when(selected == "rift-external")(
      RiftExternalPortBase until RiftExternalPortBase + RiftExternalPoolSize
    )

  val mockControl: ZLayer[Any, Throwable, MockControl] =
    selected match
      case "wiremock"    => provisioning >>> WireMock.correlated()
      case "wiremock-pi" => provisioning >>> WireMock.perInstance
      case "rift" =>
        provisioning >>> Rift
          .managed()
          .mapError(e => new RuntimeException(s"Rift: $e"))
      case "embedded" =>
        provisioning >>> EmbeddedRift.layer.mapError(e => new RuntimeException(s"Embedded Rift: $e"))
      case "rift-external" =>
        // A released rift binary spawned by `ExternalRift` (no Docker, identity host mapping), driven
        // through the MockControl adapter. Imposter port pool 46xx. `Rift.connect` needs
        // `Provisioning`, unrelated to `ExternalRift`, so the engine read and the inner layer's
        // construction run as one scoped effect. The SDK handshakes with the admin API while the
        // layer builds, so an unreachable engine fails here with a `MockError`.
        val ports = (RiftExternalPortBase until RiftExternalPortBase + RiftExternalPoolSize).toList
        val connectLayer: ZLayer[ExternalRift & Provisioning, Throwable, MockControl] =
          ZLayer.scoped {
            for
              engine <- ZIO.service[ExternalRift]
              env <- Rift
                       .connect(engine.adminUrl, ports)(engine.hostFor)
                       .build
                       .mapError(e => new RuntimeException(s"Rift (external): $e"))
            yield env.get[MockControl]
          }
        (ExternalRift.layer ++ provisioning) >>> connectLayer
      case other =>
        ZLayer.fail(
          new IllegalArgumentException(
            s"unknown MOCK_BACKEND='$other' (wiremock|wiremock-pi|rift|embedded|rift-external)"
          )
        )
