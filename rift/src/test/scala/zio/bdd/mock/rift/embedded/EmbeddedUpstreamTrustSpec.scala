package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.{PrivateCaOrigin, RiftMode, UpstreamTrust}
import zio.test.*

import java.nio.file.Path

/**
 * Outbound TLS trust for proxy stubs (engine 0.18.0, rift-scala
 * `upstreamTrust`): a `proxyRecord` proxy dialing an HTTPS origin whose
 * certificate chains to a private CA fails by default, and succeeds once the
 * embedded engine is given that CA (`CaFile`) or told to skip verification
 * (`SkipVerify`). A bad trust setting fails the layer with a typed
 * `InvalidDefinition` before any engine starts, so those cases need no engine.
 */
object EmbeddedUpstreamTrustSpec extends ZIOSpecDefault:

  private def build(trust: UpstreamTrust): UIO[Exit[MockError, Any]] =
    ZIO
      .scoped(EmbeddedRift.layer(RiftMode.PerInstance, EmbeddedRift.InterceptConfig(), Some(trust)).build)
      .provide(Provisioning.live)
      .exit

  // Proxy through an embedded engine built with `trust` to the origin on `port`.
  private def proxiedVia(trust: Option[UpstreamTrust], port: Int): Task[(Int, String)] =
    ZIO.scoped {
      EmbeddedRift
        .layer(RiftMode.PerInstance, EmbeddedRift.InterceptConfig(), trust)
        .build
        .provideSome[Scope](Provisioning.live)
        .mapError(e => new RuntimeException(s"MockError: $e"))
        .flatMap(env => PrivateCaOrigin.proxiedThrough(env.get[MockControl], s"https://localhost:$port"))
    }

  def spec = suite("EmbeddedUpstreamTrust")(
    test("a CaPem with no certificate block fails the layer with a typed InvalidDefinition") {
      build(UpstreamTrust.CaPem("not a pem")).map(exit => assertTrue(PrivateCaOrigin.isInvalidDefinition(exit)))
    },
    test("a CaFile that does not exist fails the layer with a typed InvalidDefinition") {
      build(UpstreamTrust.CaFile(Path.of("/nonexistent/zio-bdd-ca.pem")))
        .map(exit => assertTrue(PrivateCaOrigin.isInvalidDefinition(exit)))
    },
    test("a proxy to a private-CA HTTPS origin fails by default, and succeeds with CaFile or SkipVerify") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped {
          for
            origin    <- PrivateCaOrigin.start("secure-upstream")
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
  ) @@ TestAspect.withLiveClock
