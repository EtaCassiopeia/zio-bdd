package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.Client
import zio.test.*

import java.nio.file.Files

/**
 * Hermetic (no-Docker) coverage for the container Rift's `Intercept` capability
 * (#253) — every other capability is unit-tested against the in-process
 * [[FakeRift]] fake admin server (see [[RiftMockControlSpec]]); this fills the
 * matching gap for the intercept routes. Drives [[RiftMockControl]] against
 * [[FakeRift]] with `interceptProxy` set, asserting the adapter issues the
 * right admin calls and parses the truststore response — the protocol-level
 * behaviour a real container also exercises end-to-end under the RIFT_IT-gated
 * [[RiftInterceptSpec]].
 */
object RiftInterceptHermeticSpec extends ZIOSpecDefault:

  /**
   * Set up a fresh fake admin server + a Rift adapter bound to it, in a scope,
   * with the given `interceptProxy` (mirrors
   * `RiftMockControlSpec.withAdapterPool`).
   */
  private def withControl(interceptProxy: Option[(String, Int)])(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    ZIO.scoped {
      for
        adminAndFake <- FakeRift.started
        (admin, fake) = adminAndFake
        endpoint     <- RiftEndpoint.pooled(admin, (4545 until 4600).toList)(p => s"http://localhost:$p")
        control      <- RiftMockControl.make(endpoint, RiftMode.PerInstance, interceptProxy)
        result       <- use(control, fake)
      yield result
    }

  private def withIntercept(
    use: (MockControl, FakeRift) => UIO[TestResult]
  ): ZIO[Client & Provisioning, Throwable, TestResult] =
    withControl(Some(("localhost", 9999)))(use)

  def spec = suite("RiftMockControl Intercept (adapter vs fake admin API)")(
    test("Intercept is advertised only when the container was started with an intercept port (#253)") {
      withIntercept { (control, _) =>
        ZIO.succeed(assertTrue(control.capabilities.contains(Capability.Intercept)))
      }
    },
    test("add(Redirect) posts a forward rule carrying the target space's port and host to /intercept/rules") {
      withIntercept { (control, fake) =>
        val space = MockSpace("http://localhost:4545", identity, SpaceId("y"))
        for
          ic    <- control.intercept.orDieWith(u => new RuntimeException(u.toString))
          _     <- ic.add(InterceptRule.Redirect("cdn.example.com", space)).orDieWith(e => new RuntimeException(e.toString))
          rules <- fake.interceptRules
        yield assertTrue(
          rules.exists(r => r.contains("\"forward\"") && r.contains("\"port\":4545") && r.contains("cdn.example.com"))
        )
      }
    },
    test("add(Serve) posts a serve rule carrying the status/headers/body and host to /intercept/rules") {
      withIntercept { (control, fake) =>
        val stub = InterceptStub(status = 418, headers = Map("x-mock" -> "1"), body = Some("teapot"))
        for
          ic    <- control.intercept.orDieWith(u => new RuntimeException(u.toString))
          _     <- ic.add(InterceptRule.Serve("api.example.com", stub)).orDieWith(e => new RuntimeException(e.toString))
          rules <- fake.interceptRules
        yield assertTrue(
          rules.exists(r =>
            r.contains("\"serve\"") && r.contains("418") && r.contains("teapot") && r.contains("api.example.com")
          )
        )
      }
    },
    test("trustStore() defaults to PKCS#12, parsing the password header and the exported bytes") {
      withIntercept { (control, _) =>
        for
          ic    <- control.intercept.orDieWith(u => new RuntimeException(u.toString))
          ts    <- ic.trustStore().orDieWith(e => new RuntimeException(e.toString))
          bytes <- ZIO.attemptBlocking(Files.readAllBytes(ts.path)).orDie
        yield assertTrue(
          ts.format == TrustStoreFormat.Pkcs12,
          ts.password == FakeRift.truststorePassword,
          bytes.toVector == FakeRift.truststoreBytes.toVector,
          ts.path.toString.endsWith(".pkcs12")
        )
      }
    },
    test("trustStore(Jks) requests the .jks export and reports the Jks format") {
      withIntercept { (control, _) =>
        for
          ic <- control.intercept.orDieWith(u => new RuntimeException(u.toString))
          ts <- ic.trustStore(TrustStoreFormat.Jks).orDieWith(e => new RuntimeException(e.toString))
        yield assertTrue(ts.format == TrustStoreFormat.Jks, ts.path.toString.endsWith(".jks"))
      }
    },
    test("proxyPort reports the host-mapped intercept port handed to RiftMockControl.make") {
      withIntercept { (control, _) =>
        for
          ic   <- control.intercept.orDieWith(u => new RuntimeException(u.toString))
          port <- ic.proxyPort.orDieWith(e => new RuntimeException(e.toString))
        yield assertTrue(port == 9999)
      }
    },
    test("without an intercept port, Intercept is unsupported (#219 default, unchanged by #253)") {
      withControl(None) { (control, _) =>
        for interceptE <- control.intercept.either
        yield assertTrue(
          !control.capabilities.contains(Capability.Intercept),
          interceptE == Left(Unsupported(Capability.Intercept, control.backendName))
        )
      }
    }
  ).provide(Client.default, Provisioning.live) @@ TestAspect.withLiveClock
