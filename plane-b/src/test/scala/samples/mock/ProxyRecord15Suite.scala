package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 15 — proxy/record (capability ProxyRecord): verify the capability is
 * advertised and a proxy rule installs. The full proxy-to-upstream-then-replay
 * round-trip needs the upstream reachable from inside the Rift container (a
 * shared Docker network), which the library's own ProxyRecordConformanceSpec
 * covers; here we assert the SPI surface a downstream user programs against.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/15-proxy-record.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object ProxyRecord15Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("the backend supports proxy/record") {
    requiring(Capability.ProxyRecord)
  }

  When("a proxy to " / string / " is installed for GET " / string) { (upstream: String, path: String) =>
    for
      space <- activeSpace
      pr <- ZIO
              .serviceWithZIO[MockControl](_.proxyRecord)
              .mapError(u => new RuntimeException(u.message))
      id <- pr.proxy(space, get(path), upstream).mapError(e => new RuntimeException(s"$e"))
      _  <- Assertions.assertTrue(id.value.nonEmpty, "proxy rule id should be non-empty")
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
