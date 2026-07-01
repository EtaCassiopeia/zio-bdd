package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.embedded.EmbeddedRift
import zio.test.*

/**
 * The portable conformance suites run against `MockControl.embedded` (#133) —
 * the in-process Rift provider over `librift_ffi` (Panama FFM) — proving it is
 * a pure backend swap: the exact same scenario catalogues used for
 * WireMock/Rift, with no scenario changes.
 *
 * JDK-21-only (FFM is a preview API on 21), so this spec lives in
 * `src/test/jdk21` and is compiled only on a 21+ JDK; it is gated on the native
 * library being present ([[EmbeddedRift.available]]), mirroring how `RIFT_IT`
 * gates the container backend.
 *
 * `Matrix.conformant(embedded)` is the full acceptance condition for a suite:
 * with the library present, the core scenarios (which require no capability)
 * must PASS, the stub-based capability scenarios (faults/scripting/templating,
 * #185) must PASS against the live engine, and the stateful scenarios
 * SKIP-justified (embedded advertises no StatefulScenarios/StateInspection —
 * the C-ABI has no scenario-state endpoints); with the library absent, the
 * whole column is SKIPPED-unavailable (also conformant). So one assertion gates
 * each catalogue.
 */
object EmbeddedConformanceSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  // The embedded adapter advertises the four stub-based capabilities (#185); StatefulScenarios
  // and StateInspection stay out of scope (no scenario-state endpoints over the C-ABI). This must
  // mirror EmbeddedRiftMockControl.capabilities so the harness runs the supported catalogues live.
  private val embedded =
    MockBackendUnderTest(
      "embedded",
      Provisioning.live >>> EmbeddedRift.layer.mapError(asT),
      Set(Capability.Faults, Capability.Scripting, Capability.ProxyRecord, Capability.Templating),
      Isolation.PerInstance,
      available = EmbeddedRift.available
    )

  private def conforms(label: String, scenarios: List[ConformanceScenario]) =
    test(label) {
      for
        matrix <- ConformanceHarness.run(List(embedded), scenarios)
        _      <- ZIO.logInfo(s"embedded $label:\n${matrix.render}")
        fails =
          scenarios.flatMap(s =>
            matrix
              .cell(s.name, "embedded")
              .filter(_.outcome == Outcome.Fail)
              .map(c => s"${c.scenario}: ${c.detail.getOrElse("")}")
          )
        _ <- ZIO.logInfo(s"embedded $label FAILs: $fails").when(fails.nonEmpty)
      yield assertTrue(scenarios.nonEmpty, matrix.conformant(embedded))
    }

  def spec = suite("EmbeddedConformance")(
    conforms("core (#125)", CoreConformanceScenarios.all),
    conforms("negotiation/error (#127)", NegotiationErrorScenarios.all),
    conforms("cap-stateful (#131)", CapStatefulScenarios.all),
    conforms("faults (#128)", FaultScenarios.all),
    conforms("scripting (#132)", ScriptingScenarios.all),
    conforms("templating (#132)", TemplatingScenarios.all)
  ) @@ TestAspect.withLiveClock
