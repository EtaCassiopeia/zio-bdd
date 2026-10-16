package samples.mock

import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 20 — in-flight reconfiguration: change how a *live* imposter behaves across a
 * scenario without ever tearing it down and re-provisioning. Exercises
 * `replaceRules` / `addRule` / `removeRule` on the same running space (the
 * Correlated rebuild, Rift `replaceRules`, and embedded `rift_replace_stubs`
 * paths) and verifies each mutation takes effect on the very next request.
 * Portable — runs on WireMock, Rift container, and embedded FFI.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/20-inflight-reconfiguration.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Reconfig20Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a live imposter serving GET " / string / " text " / string) { (path: String, body: String) =>
    stageRule(get(path).respondWith(ok.text(body))) *> ensureProvisioned.unit
  }

  When("the imposter is reconfigured to serve GET " / string / " text " / string) { (path: String, body: String) =>
    for
      space <- activeSpace
      _     <- ctl(_.replaceRules(space, List(get(path).respondWith(ok.text(body)))))
    yield ()
  }

  When("an overlay makes GET " / string / " serve text " / string) { (path: String, body: String) =>
    for
      space <- activeSpace
      id    <- ctl(_.addRule(space, get(path).respondWith(ok.text(body)), Priority.Overlay))
      _     <- Stage.put(LastRuleId(id))
    yield ()
  }

  When("the overlay is reverted") {
    for
      space <- activeSpace
      last  <- Stage.get[LastRuleId].mapError(e => new IllegalStateException(e.message))
      _     <- ctl(_.removeRule(space, last.id))
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
