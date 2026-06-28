package zio.bdd.mock.fixtures

import zio.*
import zio.bdd.gherkin.ScenarioMetadata
import zio.bdd.mock.*
import zio.test.*

object MockFixturesSpec extends ZIOSpecDefault:

  // A MockControl that provisions one fresh, uniquely-identified space per source
  // and logs every destroy. It has no bulk delete — so "never a global teardown"
  // is structural; the assertions check the finalizer destroyed exactly its own.
  private final case class TState(nextId: Int, live: Set[String], destroyed: List[String], provisions: Int)
  private object TState:
    val init: TState = TState(0, Set.empty, Nil, 0)

  private final class TestControl(
    ref: Ref[TState],
    failProvision: MockSource => Boolean = _ => false,
    failDestroy: String => Boolean = _ => false
  ) extends MockControl:
    def backendName: String           = "test"
    def capabilities: Set[Capability] = Set.empty

    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      if failProvision(source) then ZIO.fail(MockError.ProvisionFailed("boom"))
      else
        ref.modify { s =>
          val id = s"sp${s.nextId}"
          (
            List(MockSpace(s"http://localhost/$id", identity, SpaceId(id))),
            s.copy(nextId = s.nextId + 1, live = s.live + id, provisions = s.provisions + 1)
          )
        }

    def destroy(space: MockSpace): IO[MockError, Unit] =
      if failDestroy(space.id.value) then ZIO.fail(MockError.CommunicationError("teardown boom"))
      else ref.update(s => s.copy(live = s.live - space.id.value, destroyed = s.destroyed :+ space.id.value))

    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.fail(MockError.InvalidDefinition("n/a"))
    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] = ZIO.succeed(RuleId("r"))
    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]                        = ZIO.unit
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit]           = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]]                     = ZIO.succeed(Nil)
    def faults: IO[Unsupported, Faults]                                                      = ZIO.fail(Unsupported(Capability.Faults, backendName))
    def scenarios: IO[Unsupported, StatefulScenarios]                                        = ZIO.fail(Unsupported(Capability.StatefulScenarios, backendName))
    def stateInspection: IO[Unsupported, StateInspection] =
      ZIO.fail(Unsupported(Capability.StateInspection, backendName))
    def scripting: IO[Unsupported, Scripting]     = ZIO.fail(Unsupported(Capability.Scripting, backendName))
    def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.fail(Unsupported(Capability.ProxyRecord, backendName))
    def templating: IO[Unsupported, Templating]   = ZIO.fail(Unsupported(Capability.Templating, backendName))

  private val srcA = MockSource.Json("""{"a":1}""")
  private val srcB = MockSource.Json("""{"b":2}""")

  private def meta(tags: String*): ScenarioMetadata = ScenarioMetadata("s", tags.toList, None, None)

  private def control: UIO[(MockControl, Ref[TState])] =
    Ref.make(TState.init).map(ref => (TestControl(ref), ref))

  def spec = suite("MockFixtures")(
    test("MockTag parses @mock(...) names and ignores unrelated tags") {
      assertTrue(
        MockTag.parse("mock(a, b)").contains(List("a", "b")),
        MockTag.parse("@mock(x)").contains(List("x")),
        MockTag.parse("mock()").contains(Nil),
        MockTag.parse("flags(k=v)").isEmpty,
        MockTag.parse("smoke").isEmpty,
        MockTag.extract(List("mock(a)", "other", "mock(b, c)")) == List("a", "b", "c")
      )
    },
    test("a feature fixture provisions its sources; the finalizer destroys exactly those spaces (§5.9)") {
      for
        cr        <- control
        (ctl, ref) = cr
        during <- ZIO.scoped {
                    MockFixtures.feature(srcA, srcB).build.map(_.get[MockFixture]).flatMap(fx => ref.get.map((fx, _)))
                  }.provideLayer(ZLayer.succeed(ctl))
        after <- ref.get
      yield
        val (fx, mid) = during
        assertTrue(
          fx.spaces.size == 2,
          mid.provisions == 2,
          mid.live.size == 2,                                      // both live within the scope
          after.live.isEmpty,                                      // both destroyed on release
          after.destroyed.toSet == fx.spaces.map(_.id.value).toSet // exactly its own, nothing else
        )
    },
    test(
      "two scenario fixtures from the same source get independent spaces; one teardown leaves the other live (AC1)"
    ) {
      for
        cr        <- control
        (ctl, ref) = cr
        catalog    = Map("svc" -> srcA)
        res <- ZIO.scoped {
                 MockFixtures.scenario(meta("mock(svc)"), catalog).build.map(_.get[MockFixture]).flatMap { fxA =>
                   ZIO
                     .scoped(MockFixtures.scenario(meta("mock(svc)"), catalog).build.map(_.get[MockFixture]))
                     .flatMap(fxB => ref.get.map((fxA, fxB, _)))
                 }
               }.provideLayer(ZLayer.succeed(ctl))
      yield
        val (fxA, fxB, afterBClosed) = res
        val aIds                     = fxA.spaces.map(_.id.value).toSet
        val bIds                     = fxB.spaces.map(_.id.value).toSet
        assertTrue(
          aIds.nonEmpty && bIds.nonEmpty,
          aIds.intersect(bIds).isEmpty,           // independent spaces
          afterBClosed.destroyed.toSet == bIds,   // only B's teardown ran
          aIds.forall(afterBClosed.live.contains) // A is untouched by B's teardown
        )
    },
    test(
      "a feature fixture provisions once per build; a second build gets fresh, disjoint spaces (isolation between features)"
    ) {
      for
        cr      <- control
        (ctl, _) = cr
        f1      <- ZIO.scoped(MockFixtures.feature(srcA).build.map(_.get[MockFixture])).provideLayer(ZLayer.succeed(ctl))
        f2      <- ZIO.scoped(MockFixtures.feature(srcA).build.map(_.get[MockFixture])).provideLayer(ZLayer.succeed(ctl))
      yield assertTrue(
        f1.spaces.nonEmpty,
        f1.spaces.map(_.id.value).toSet.intersect(f2.spaces.map(_.id.value).toSet).isEmpty
      )
    },
    test("@mock(...) deploys the named catalog entries; no @mock tag provisions nothing") {
      for
        cr      <- control
        (ctl, _) = cr
        catalog  = Map("payments" -> srcA, "inventory" -> srcB)
        tagged <-
          ZIO
            .scoped(MockFixtures.scenario(meta("mock(payments, inventory)"), catalog).build.map(_.get[MockFixture]))
            .provideLayer(ZLayer.succeed(ctl))
        untagged <- ZIO
                      .scoped(MockFixtures.scenario(meta("smoke"), catalog).build.map(_.get[MockFixture]))
                      .provideLayer(ZLayer.succeed(ctl))
      yield assertTrue(tagged.spaces.size == 2, untagged.spaces.isEmpty)
    },
    test("@mock(unknown) referencing a missing catalog entry fails loudly, naming the entry") {
      for
        cr      <- control
        (ctl, _) = cr
        res <- ZIO
                 .scoped(MockFixtures.scenario(meta("mock(ghost)"), Map.empty).build)
                 .provideLayer(ZLayer.succeed(ctl))
                 .either
      yield assertTrue(res.isLeft, res.swap.toOption.exists(_.getMessage.contains("ghost")))
    },
    test("a source that fails mid-provision does not leak the spaces already provisioned") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref, failProvision = _ == srcB) // srcA succeeds, srcB fails
        res <- ZIO
                 .scoped(MockFixtures.feature(srcA, srcB).build)
                 .provideLayer(ZLayer.succeed(ctl))
                 .either
        after <- ref.get
      yield assertTrue(
        res.isLeft,                     // build failed
        after.provisions == 1,          // only srcA got provisioned
        after.destroyed == List("sp0"), // and it was torn down on unwind — not leaked
        after.live.isEmpty
      )
    },
    test("a teardown failure for one space does not strand the others") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref, failDestroy = _ == "sp0") // sp0's destroy fails
        res <- ZIO
                 .scoped(MockFixtures.feature(srcA, srcB).build.unit)
                 .provideLayer(ZLayer.succeed(ctl))
                 .either
        after <- ref.get
      yield assertTrue(
        res.isRight,                    // the failing finalizer is logged, not propagated
        after.destroyed.contains("sp1") // sp1 was still destroyed despite sp0's teardown failing
      )
    }
  )
