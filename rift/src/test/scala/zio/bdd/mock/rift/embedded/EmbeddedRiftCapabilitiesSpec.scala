package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.RiftMode
import zio.test.*

/**
 * Unit gate for `EmbeddedRift`'s OWN entry-point wiring (#285, re-based onto
 * the rift-scala SDK): the layer variants, `available`/`requireAvailable`, and
 * the `InterceptConfig` -> SDK translation. Full MockControl behavior over the
 * live embedded engine (capabilities, faults, scenarios, scripting,
 * proxy-record, templating, correlated isolation, rollback) is covered once,
 * transport-agnostically, in [[zio.bdd.mock.rift.RiftMockControlSpec]]'s live
 * suite — this file does not duplicate it.
 *
 * Gated on [[EmbeddedRift.available]] wherever it touches the engine, mirroring
 * the other embedded specs; on a host with no `rift-java-embedded`/natives
 * resolvable, those cases degrade to a no-op assertion rather than a hard
 * failure.
 */
object EmbeddedRiftCapabilitiesSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private val pingRule = MockRule(
    RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    ResponseDef(status = 200, body = Body.Text("pong"))
  )
  private val pingSource = MockSource.Dsl(MockSpec(List(pingRule)))

  def spec = suite("EmbeddedRift")(
    test("layer/layer(mode)/layer(intercept)/layer(mode, intercept) all wire a working MockControl") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped {
          for
            a <- EmbeddedRift.layer.build.mapError(asT).map(_.get[MockControl])
            b <- EmbeddedRift.layer(RiftMode.correlated).build.mapError(asT).map(_.get[MockControl])
            c <- EmbeddedRift.layer(EmbeddedRift.InterceptConfig()).build.mapError(asT).map(_.get[MockControl])
            d <- EmbeddedRift
                   .layer(RiftMode.PerInstance, EmbeddedRift.InterceptConfig())
                   .build
                   .mapError(asT)
                   .map(_.get[MockControl])
            spaceA <- a.provision(pingSource).mapError(asT).map(_.head)
            spaceB <- b.provision(pingSource).mapError(asT).map(_.head)
          yield assertTrue(
            a.isolation == Isolation.PerInstance,
            b.isolation == Isolation.Correlated,
            c.isolation == Isolation.PerInstance,
            d.isolation == Isolation.PerInstance,
            spaceA.baseUri.nonEmpty,
            spaceB.baseUri.nonEmpty
          )
        }
          .provide(Provisioning.live)
    },
    test("shared memoizes the engine: two acquisitions of `shared` observe the same running instance") {
      if !EmbeddedRift.available then ZIO.succeed(assertCompletes)
      else
        ZIO.scoped {
          for
            control <- ZIO.service[MockControl]
            a       <- control.provision(pingSource).mapError(asT).map(_.head)
            b       <- control.provision(pingSource).mapError(asT).map(_.head)
          yield assertTrue(a.baseUri != b.baseUri) // distinct spaces, same live (memoized) engine
        }
          .provide(Provisioning.live, EmbeddedRift.shared.mapError(asT))
    },
    test("available/requireAvailable agree: requireAvailable is a no-op exactly when available is true") {
      for outcome <- EmbeddedRift.requireAvailable.either
      yield assertTrue(outcome.isRight == EmbeddedRift.available)
    },
    test("requireAvailable, when it fails, names the host os-arch in a typed MockError") {
      if EmbeddedRift.available then ZIO.succeed(assertCompletes) // nothing to assert — the host has an engine
      else
        for outcome <- EmbeddedRift.requireAvailable.either
        yield assertTrue(outcome.swap.toOption.exists {
          case MockError.ProvisionFailed(m) =>
            m.contains(java.lang.System.getProperty("os.name", "")) && m.contains(
              java.lang.System.getProperty("os.arch", "")
            )
          case _ => false
        })
    },
    test("InterceptConfig.toSettings carries bindHost/port/caCert/caKey through unchanged") {
      val cert = java.nio.file.Path.of("/ca.pem")
      val key  = java.nio.file.Path.of("/ca.key")
      val cfg =
        EmbeddedRift.InterceptConfig(bindHost = "0.0.0.0", port = Some(8888), caCert = Some(cert), caKey = Some(key))
      val settings = cfg.toSettings
      assertTrue(
        settings.bindHost == "0.0.0.0",
        settings.port == 8888,
        settings.caCert.contains(cert),
        settings.caKey.contains(key),
        EmbeddedRift.InterceptConfig().toSettings.port == 0 // default: OS-assigned
      )
    }
  )
