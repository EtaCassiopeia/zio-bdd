package samples.mock

import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 06 — rule precedence & mutation: Overlay shadows Base;
 * addRule/removeRule/replaceRules.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/06-rule-precedence.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Precedence06Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a base stub GET " / string / " returns text " / string) { (path: String, body: String) =>
    stageRule(get(path).respondWith(ok.text(body)))
  }

  When("an overlay stub GET " / string / " returns text " / string) { (path: String, body: String) =>
    for
      space <- activeSpace
      id    <- ctl(_.addRule(space, get(path).respondWith(ok.text(body)), Priority.Overlay))
      _     <- Stage.put(LastRuleId(id))
    yield ()
  }

  When("the last overlay rule is removed") {
    for
      space <- activeSpace
      last  <- Stage.get[LastRuleId].mapError(e => new IllegalStateException(e.message))
      _     <- ctl(_.removeRule(space, last.id))
    yield ()
  }

  When("all rules are replaced with GET " / string / " returning text " / string) { (path: String, body: String) =>
    for
      space <- activeSpace
      _     <- ctl(_.replaceRules(space, List(get(path).respondWith(ok.text(body)))))
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class LastRuleId(id: RuleId)
