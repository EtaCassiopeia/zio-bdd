package zio.bdd.mock

import zio.*
import zio.test.*

object MockAspectsSpec extends ZIOSpecDefault:

  // A MockControl recording each rule's id, definition, and priority on one
  // space, so a test can read back what an aspect overlaid (and that it reverted).
  private final case class RuleEntry(id: String, rule: MockRule, priority: Priority)
  private final case class TState(nextId: Int, rules: List[RuleEntry])

  private final class TestControl(ref: Ref[TState]) extends MockControl:
    def backendName: String           = "test"
    def capabilities: Set[Capability] = Set.empty

    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      ref.modify { s =>
        val id    = s"r${s.nextId}"
        val entry = RuleEntry(id, rule, priority)
        val rules = priority match
          case Priority.Overlay => entry :: s.rules
          case Priority.Base    => s.rules :+ entry
        (RuleId(id), s.copy(nextId = s.nextId + 1, rules = rules))
      }

    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
      ref.update(s => s.copy(rules = s.rules.filterNot(_.id == id.value)))

    def provision(source: MockSource): IO[MockError, List[MockSpace]] = ZIO.succeed(Nil)
    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.fail(MockError.InvalidDefinition("n/a"))
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] = ZIO.unit
    def destroy(space: MockSpace): IO[MockError, Unit]                             = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]]           = ZIO.succeed(Nil)
    def faults: IO[Unsupported, Faults]                                            = ZIO.fail(Unsupported(Capability.Faults, backendName))
    def scenarios: IO[Unsupported, StatefulScenarios]                              = ZIO.fail(Unsupported(Capability.StatefulScenarios, backendName))
    def stateInspection: IO[Unsupported, StateInspection] =
      ZIO.fail(Unsupported(Capability.StateInspection, backendName))
    def scripting: IO[Unsupported, Scripting]     = ZIO.fail(Unsupported(Capability.Scripting, backendName))
    def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.fail(Unsupported(Capability.ProxyRecord, backendName))
    def templating: IO[Unsupported, Templating]   = ZIO.fail(Unsupported(Capability.Templating, backendName))

  // Shared across a sub-suite (one control + space + the inspectable Ref) so a
  // later test observes whether an earlier aspected test reverted. @@ sequential
  // is load-bearing: without it the tests race on this shared Ref and the
  // revert assertions become meaningless.
  private def sharedFrom(init: TState): ZLayer[Any, Nothing, MockControl & MockSpace & Ref[TState]] =
    ZLayer.fromZIOEnvironment {
      Ref.make(init).map { ref =>
        val space = MockSpace("mock://aspect", identity, SpaceId("aspect"))
        ZEnvironment[MockControl](TestControl(ref)) ++ ZEnvironment(space) ++ ZEnvironment(ref)
      }
    }

  private val rules: ZIO[Ref[TState], Nothing, List[RuleEntry]] = ZIO.serviceWithZIO[Ref[TState]](_.get).map(_.rules)

  private val imposter  = MockRule(RequestMatch(path = PathMatch.Exact("/special")), ResponseDef(status = 503))
  private val imposter2 = MockRule(RequestMatch(path = PathMatch.Exact("/other")), ResponseDef(status = 418))
  private val baseRule  = MockRule(RequestMatch(path = PathMatch.Exact("/orders")), ResponseDef(status = 200))

  def spec = suite("MockAspects")(
    suite("scoped overlay on an empty base")(
      test("withLatency overlays a catch-all delayed rule for the duration of the aspected test") {
        for entries <- rules
        yield assertTrue(
          entries.exists(e =>
            e.priority == Priority.Overlay &&
              e.rule.`match` == RequestMatch() &&       // catch-all
              e.rule.respond.delay.contains(200.millis) // the requested latency
          )
        )
      } @@ MockAspects.withLatency(200.millis),
      test("the latency rule is reverted after the aspected test (no latency leaks to the next)") {
        for entries <- rules
        yield assertTrue(entries.forall(_.rule.respond.delay.isEmpty)) // withLatency above reverted on exit
      },
      test("withImposter overlays all of the given rules for the duration of the aspected test") {
        for entries <- rules
        yield assertTrue(
          entries.count(_.priority == Priority.Overlay) == 2,
          entries.exists(_.rule == imposter),
          entries.exists(_.rule == imposter2)
        )
      } @@ MockAspects.withImposter(imposter, imposter2),
      test("the imposter rules are reverted after the aspected test") {
        for entries <- rules
        yield assertTrue(entries.isEmpty) // every overlay above reverted; back to the empty base
      },
      test("the overlay reverts even when the aspected test FAILS") {
        for entries <- rules
        yield assertTrue(entries.exists(_.rule == imposter)) && assertTrue(false) // deliberately fail under the aspect
      } @@ MockAspects.withImposter(imposter) @@ TestAspect.failing,
      test("a failing aspected test still reverted its overlay (revert is outcome-independent)") {
        for entries <- rules
        yield assertTrue(entries.isEmpty)
      }
    ).provideShared(sharedFrom(TState(0, Nil))) @@ TestAspect.sequential,
    suite("scoped overlay on top of a base rule")(
      test("the overlay shadows the base rule during the aspected test, base still present") {
        for entries <- rules
        yield assertTrue(
          entries.exists(e => e.priority == Priority.Overlay && e.rule.respond.delay.contains(137.millis)),
          entries.exists(_.rule == baseRule) // the declared base is untouched while overlaid
        )
      } @@ MockAspects.withLatency(137.millis),
      test("after the aspected test the overlay is gone and exactly the base rule remains") {
        for entries <- rules
        yield assertTrue(entries.map(_.rule) == List(baseRule)) // reverted TO the base, not to empty
      }
    ).provideShared(sharedFrom(TState(1, List(RuleEntry("base0", baseRule, Priority.Base))))) @@ TestAspect.sequential
  )
