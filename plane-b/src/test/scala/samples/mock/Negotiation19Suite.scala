package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 19 — capability negotiation: `capabilities` is honest, `require` succeeds for
 * advertised capabilities and fails with `Unsupported` (naming the backend) for
 * missing ones — so the harness's per-backend tag exclusions are themselves
 * verified, not assumed.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/19-capability-negotiation.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Negotiation19Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def cap(name: String): Capability = name match
    case "Faults"            => Capability.Faults
    case "StatefulScenarios" => Capability.StatefulScenarios
    case "StateInspection"   => Capability.StateInspection
    case "Scripting"         => Capability.Scripting
    case "ProxyRecord"       => Capability.ProxyRecord
    case "Templating"        => Capability.Templating
    case other               => throw new IllegalArgumentException(s"unknown capability '$other'")

  Then("the backend advertises capability " / string) { (name: String) =>
    ZIO
      .serviceWith[MockControl](_.capabilities.contains(cap(name)))
      .flatMap(has => Assertions.assertTrue(has, s"backend does not advertise $name"))
  }

  Then("requiring " / string / " succeeds") { (name: String) =>
    ZIO
      .serviceWithZIO[MockControl](_.require(cap(name)))
      .mapError(u => new RuntimeException(u.message))
      .unit
  }

  When("requiring " / string / " is attempted") { (name: String) =>
    ZIO
      .serviceWithZIO[MockControl](_.require(cap(name)).either)
      .flatMap {
        case Left(u)  => Stage.put(RequireOutcome(failed = true, u.message))
        case Right(_) => Stage.put(RequireOutcome(failed = false, ""))
      }
  }

  Then("it fails naming the missing capability " / string) { (name: String) =>
    Stage
      .get[RequireOutcome]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(o => Assertions.assertTrue(o.failed && o.message.contains(name), s"outcome=$o"))
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl

final case class RequireOutcome(failed: Boolean, message: String)
