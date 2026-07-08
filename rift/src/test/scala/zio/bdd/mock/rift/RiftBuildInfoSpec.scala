package zio.bdd.mock.rift

import zio.test.*

/**
 * Pins the single-source-of-truth wiring (#195): `Rift.DefaultImage` must be
 * derived from the generated [[RiftBuildInfo.riftVersion]] (which build.sbt
 * generates from its `riftVersion` val, the same val the FFI natives version
 * derives from) — not an independently-maintained literal. A bump to
 * `riftVersion` in build.sbt then flows here automatically; a drift between the
 * image tag and the version constant fails this test.
 */
object RiftBuildInfoSpec extends ZIOSpecDefault:
  def spec = suite("RiftBuildInfo")(
    test("DefaultImage is derived from the single rift-version source of truth") {
      assertTrue(
        RiftBuildInfo.riftVersion == "0.11.3",
        Rift.DefaultImage == s"zainalpour/rift-proxy:v${RiftBuildInfo.riftVersion}",
        Rift.DefaultImage == "zainalpour/rift-proxy:v0.11.3"
      )
    }
  )
