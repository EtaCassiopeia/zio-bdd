package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.test.*

import java.nio.file.Path
import org.testcontainers.Testcontainers

/**
 * Outbound TLS trust for proxy stubs on the container backend (#342):
 * `Rift.managed(upstreamTrust = ...)` reaches the containerized engine, so a
 * `proxyRecord` proxy to an HTTPS origin behind a private CA fails by default
 * and succeeds with `CaFile` (the CA is copied into the container) or
 * `SkipVerify`. The origin runs on the host and the container reaches it
 * through the testcontainers host gateway (`host.testcontainers.internal`).
 *
 * The live cases need Docker and run only when `RIFT_IT` is set (`RIFT_IT=1 sbt
 * rift/test`); the missing-`CaFile` check fails before any container starts, so
 * it always runs.
 */
object RiftContainerUpstreamTrustSpec extends ZIOSpecDefault:

  private def build(layer: ZLayer[Provisioning, MockError, MockControl]): UIO[Exit[MockError, Any]] =
    ZIO.scoped(layer.build).provide(Provisioning.live).exit

  // Proxy through a fresh container built with `trust` to the host origin on `port`.
  private def proxiedVia(trust: Option[UpstreamTrust], port: Int): Task[(Int, String)] =
    ZIO.scoped {
      Rift
        .managed(upstreamTrust = trust)
        .build
        .provideSome[Scope](Provisioning.live)
        .mapError(e => new RuntimeException(s"MockError: $e"))
        .flatMap(env =>
          PrivateCaOrigin.proxiedThrough(env.get[MockControl], s"https://host.testcontainers.internal:$port")
        )
    }

  private val hermetic = suite("hermetic")(
    test("a CaFile that does not exist fails Rift.managed with a typed InvalidDefinition, before any container") {
      build(Rift.managed(upstreamTrust = Some(UpstreamTrust.CaFile(Path.of("/nonexistent/zio-bdd-ca.pem")))))
        .map(exit => assertTrue(PrivateCaOrigin.isInvalidDefinition(exit)))
    }
  )

  private val live = suite("real Rift container")(
    test("an image older than Rift 0.18.0 refuses upstreamTrust with a typed InvalidDefinition") {
      build(Rift.managed(image = "zainalpour/rift-proxy:v0.17.0", upstreamTrust = Some(UpstreamTrust.SkipVerify)))
        .map(exit => assertTrue(PrivateCaOrigin.isInvalidDefinition(exit)))
    },
    test("a proxy to a private-CA HTTPS origin fails by default, and succeeds with CaFile or SkipVerify") {
      ZIO.scoped {
        for
          origin    <- PrivateCaOrigin.start("secure-upstream")
          _         <- ZIO.attemptBlocking(Testcontainers.exposeHostPorts(origin.port))
          untrusted <- proxiedVia(None, origin.port)
          viaCa     <- proxiedVia(Some(UpstreamTrust.CaFile(origin.caPem)), origin.port)
          viaSkip   <- proxiedVia(Some(UpstreamTrust.SkipVerify), origin.port)
        yield assertTrue(
          untrusted != (200, "secure-upstream"), // public roots only: the private CA is not trusted
          viaCa == (200, "secure-upstream"),
          viaSkip == (200, "secure-upstream")
        )
      }
    }
  ) @@ TestAspect.sequential

  def spec =
    suite("RiftContainerUpstreamTrust")(
      hermetic,
      if sys.env.contains("RIFT_IT") then live
      else suite("real Rift container (skipped — set RIFT_IT=1)")()
    ) @@ TestAspect.withLiveClock
