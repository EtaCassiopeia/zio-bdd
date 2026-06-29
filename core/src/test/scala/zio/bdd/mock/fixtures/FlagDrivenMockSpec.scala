package zio.bdd.mock.fixtures

import zio.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.*
import zio.bdd.mock.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*

object FlagDrivenMockSpec extends ZIOSpecDefault:

  // A MockControl whose provisioned space is named after the source it deployed,
  // so a test can read back WHICH catalog entry a flag selected. Tracks live and
  // destroyed spaces to prove the fixture reverts.
  private final case class TState(live: Set[String], destroyed: List[String])
  private object TState:
    val init: TState = TState(Set.empty, Nil)

  private final class TestControl(ref: Ref[TState]) extends MockControl:
    def backendName: String           = "test"
    def capabilities: Set[Capability] = Set.empty

    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      source match
        case MockSource.Json(name) =>
          ref.update(s => s.copy(live = s.live + name)).as(List(MockSpace(s"mock://$name", identity, SpaceId(name))))
        case other =>
          ZIO.fail(MockError.InvalidDefinition(s"unsupported source: $other"))

    def destroy(space: MockSpace): IO[MockError, Unit] =
      ref.update(s => s.copy(live = s.live - space.id.value, destroyed = s.destroyed :+ space.id.value))

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

  private val catalog: Map[String, MockSource] =
    Map("stripe" -> MockSource.Json("stripe"), "adyen" -> MockSource.Json("adyen"))

  private def meta(flags: (String, String)*): ScenarioMetadata =
    ScenarioMetadata("scenario", Nil, None, None, flags.toMap)

  // Deploy `reader`-selected fixture, returning (ids live during the scope, the
  // full post-scope state) so a test sees both the selection and the revert.
  private def deploy(
    reader: FlagReader,
    ref: Ref[TState]
  ): ZIO[Any, Throwable, (List[String], TState)] =
    for
      during <- ZIO.scoped {
                  (ZLayer.succeed[MockControl](TestControl(ref)) >>> MockFixtures.byFlag(
                    reader,
                    "payments.provider",
                    catalog
                  )).build
                    .map(_.get[MockFixture].spaces.map(_.id.value))
                }
      after <- ref.get // read AFTER the scope closed, so the revert is visible
    yield (during, after)

  final case class TestState()
  object TestState:
    given Schema[TestState] = DeriveSchema.gen[TestState]

  // A suite whose flag-aware setup deploys the mock the @flags(payments.provider=..)
  // value selects. The hook records (this branch's flag value -> the backend it
  // actually deployed), so the test pins the per-branch pairing — not just that
  // both backends appeared (which a stripe<->adyen swap would still satisfy).
  private class PaymentsSuite(control: MockControl, recorder: Ref[Set[(String, String)]])
      extends ZIOSteps[MockFixture, TestState]:
    override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, MockFixture] =
      ZLayer.succeed(control) >>> MockFixtures.byFlag(FlagReader.fromMetadata(meta), "payments.provider", catalog)

    beforeScenario { (meta: ScenarioMetadata) =>
      ZIO.serviceWithZIO[MockFixture](fx =>
        recorder.update(_ + (meta.flagValues("payments.provider") -> fx.spaces.head.id.value))
      )
    }

    When("the active payments mock is recorded")(ZIO.unit)

  def spec = suite("FlagDrivenMock")(
    test("byFlag deploys the catalog entry the flag value selects, then reverts on scope close") {
      for
        ref         <- Ref.make(TState.init)
        result      <- deploy(FlagReader.fromMetadata(meta("payments.provider" -> "stripe")), ref)
        (during, st) = result
      yield assertTrue(
        during == List("stripe"),      // selected the 'stripe' catalog entry, not 'adyen'
        st.destroyed == List("stripe") // reverted exactly that space on scope close
      )
    },
    test("byFlag reads through any FlagReader (an OpenFeature-style reader), not just metadata") {
      // Stand-in for an OpenFeature getString-backed reader living in zio-openfeature.
      val openFeatureStyle = new FlagReader:
        private val backing                              = Map("payments.provider" -> "adyen")
        def getString(flag: String): UIO[Option[String]] = ZIO.succeed(backing.get(flag))
      for
        ref         <- Ref.make(TState.init)
        result      <- deploy(openFeatureStyle, ref)
        (during, st) = result
      yield assertTrue(during == List("adyen"), st.destroyed == List("adyen"))
    },
    test("byFlag fails when the flag is unset on the scenario") {
      for
        ref <- Ref.make(TState.init)
        res <- deploy(FlagReader.fromMetadata(meta()), ref).either
      yield assert(res.left.toOption.map(_.getMessage).getOrElse(""))(Assertion.containsString("resolved to no value"))
    },
    test("byFlag fails when the flag value names no catalog entry") {
      for
        ref <- Ref.make(TState.init)
        res <- deploy(FlagReader.fromMetadata(meta("payments.provider" -> "paypal")), ref).either
      yield assert(res.left.toOption.map(_.getMessage).getOrElse(""))(Assertion.containsString("no catalog entry"))
    },
    test("a single @flags(payments.provider=stripe)/@flags(...=adyen) scenario mocks each branch correctly") {
      for
        ref      <- Ref.make(TState.init)
        recorder <- Ref.make(Set.empty[(String, String)])
        suite     = new PaymentsSuite(TestControl(ref), recorder)
        scenario = Scenario(
                     "charge a payment",
                     List("flags(payments.provider=stripe)", "flags(payments.provider=adyen)"),
                     List(Step(StepType.WhenStep, "the active payments mock is recorded", None, None, None)),
                     Some("payments.feature"),
                     Some(1)
                   )
        results <- suite
                     .run(List(Feature("Payments", Nil, List(scenario))))
                     .provideEnvironment(ZEnvironment[MockFixture](MockFixture(Nil)))
        deployed  <- recorder.get
        destroyed <- ref.get.map(_.destroyed)
      yield assertTrue(
        results.head.scenarioResults.length == 2,        // the @flags matrix ran both branches
        results.head.scenarioResults.forall(_.isPassed), // each branch passed
        deployed == Set("stripe" -> "stripe", "adyen" -> "adyen"), // each branch deployed ITS OWN flag's backend
        destroyed.sorted == List("adyen", "stripe") // and each reverted its own space
      )
    }
  )
