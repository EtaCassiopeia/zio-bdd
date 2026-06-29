package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.test.*

/**
 * Pure + synthetic tests that lock the runner's classification contract — the
 * FAIL and unjustified-SKIP branches of `conformant` that the live WireMock
 * matrix never exercises (it only ever produces PASS/justified-SKIP). No
 * Docker, no real server.
 */
object ConformanceHarnessSpec extends ZIOSpecDefault:

  // A no-op backend: the predicate/render tests never run its layer; the harness
  // tests run a scenario that ignores the control.
  private val stub: MockControl = new MockControl:
    def backendName: String           = "stub"
    def capabilities: Set[Capability] = Set.empty
    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      ZIO.succeed(List(MockSpace("stub://x", identity, SpaceId("x"))))
    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.fail(MockError.InvalidDefinition("n/a"))
    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] = ZIO.succeed(RuleId("r"))
    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]                        = ZIO.unit
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit]           = ZIO.unit
    def destroy(space: MockSpace): IO[MockError, Unit]                                       = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]]                     = ZIO.succeed(Nil)
    def faults: IO[Unsupported, Faults]                                                      = ZIO.fail(Unsupported(Capability.Faults, "stub"))
    def scenarios: IO[Unsupported, StatefulScenarios]                                        = ZIO.fail(Unsupported(Capability.StatefulScenarios, "stub"))
    def stateInspection: IO[Unsupported, StateInspection]                                    = ZIO.fail(Unsupported(Capability.StateInspection, "stub"))
    def scripting: IO[Unsupported, Scripting]                                                = ZIO.fail(Unsupported(Capability.Scripting, "stub"))
    def proxyRecord: IO[Unsupported, ProxyRecord]                                            = ZIO.fail(Unsupported(Capability.ProxyRecord, "stub"))
    def templating: IO[Unsupported, Templating]                                              = ZIO.fail(Unsupported(Capability.Templating, "stub"))

  // capabilities = empty -> a Faults-requiring scenario is justifiably skipped.
  private val backend  = MockBackendUnderTest("b", ZLayer.succeed(stub), Set.empty, Isolation.PerInstance)
  private val basic    = ConformanceScenario("basic", Set.empty, _ => ZIO.unit) // requires nothing
  private val needsCap = ConformanceScenario("needs-faults", Set(Capability.Faults), _ => ZIO.unit)

  private def matrixOf(scenarios: List[ConformanceScenario], cells: Cell*): Matrix =
    Matrix(List(backend), scenarios, cells.toList)

  def spec = suite("ConformanceHarness")(
    suite("conformant predicate")(
      test("a FAIL cell makes the column non-conformant") {
        assertTrue(!matrixOf(List(basic), Cell("basic", "b", Outcome.Fail)).conformant(backend))
      },
      test("an UNJUSTIFIED skip (a runnable scenario skipped) makes the column non-conformant") {
        // 'basic' requires nothing and the backend is available, so a Skip is unjustified.
        assertTrue(!matrixOf(List(basic), Cell("basic", "b", Outcome.Skip)).conformant(backend))
      },
      test("a JUSTIFIED skip (un-advertised capability) keeps the column conformant") {
        assertTrue(matrixOf(List(needsCap), Cell("needs-faults", "b", Outcome.Skip)).conformant(backend))
      },
      test("an all-PASS column is conformant") {
        assertTrue(matrixOf(List(basic), Cell("basic", "b", Outcome.Pass)).conformant(backend))
      },
      test("an unavailable backend's all-SKIP column is (vacuously) conformant") {
        val down = backend.copy(available = false)
        assertTrue(Matrix(List(down), List(basic), List(Cell("basic", "b", Outcome.Skip))).conformant(down))
      }
    ),
    test("render shows the P/S/F grid with scenario names and outcomes") {
      val m = Matrix(
        List(backend),
        List(basic, needsCap),
        List(Cell("basic", "b", Outcome.Pass), Cell("needs-faults", "b", Outcome.Skip))
      )
      val r = m.render
      assertTrue(
        r.contains("basic"),
        r.contains("needs-faults"),
        r.contains("PASS"),
        r.contains("SKIP"),
        r.contains("b")
      )
    },
    suite("runner classifies failures")(
      test("a failing scenario produces a FAIL cell and a non-conformant column") {
        val boom = ConformanceScenario("boom", Set.empty, _ => ZIO.fail(new RuntimeException("boom")))
        for matrix <- ConformanceHarness.run(List(backend), List(boom))
        yield assertTrue(
          matrix.cell("boom", "b").map(_.outcome).contains(Outcome.Fail),
          !matrix.conformant(backend)
        )
      },
      test("a backend whose layer cannot build fails every scenario") {
        val broken = MockBackendUnderTest(
          "broken",
          ZLayer.fail(new RuntimeException("no backend")),
          Set.empty,
          Isolation.PerInstance
        )
        for matrix <- ConformanceHarness.run(List(broken), List(basic, needsCap))
        yield assertTrue(
          matrix.cell("basic", "broken").map(_.outcome).contains(Outcome.Fail),
          matrix.cell("needs-faults", "broken").map(_.outcome).contains(Outcome.Fail),
          !matrix.conformant(broken)
        )
      }
    )
  )
