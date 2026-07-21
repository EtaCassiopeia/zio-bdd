package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.embedded.EmbeddedRift
import zio.test.*

/**
 * The portable conformance suites run against the embedded Rift provider (#133,
 * re-based onto the rift-scala SDK's `RiftMockControl` for #285) — the
 * in-process engine over the stable Panama FFM C-ABI (JDK 22+) — proving it is
 * a pure backend swap: the exact same scenario catalogues used for
 * WireMock/Rift, with no scenario changes.
 *
 * Compiled on every supported JDK (17+); gated at RUNTIME on the native
 * provider being present ([[EmbeddedRift.available]]), mirroring how `RIFT_IT`
 * gates the container backend. [[embeddedAvailabilityGuardSpec]] below is what
 * makes a regression in that gate impossible to miss silently: on a JDK that
 * CAN run the embedded engine (22+), `available` returning `false` is a
 * FAILURE, not a green skip (#285/B1) — this file's own `conforms` suite
 * degrading a missing engine to "conformant via SKIPPED-unavailable" is only a
 * legitimate outcome on an older JDK.
 *
 * `Matrix.conformant(embedded)` is the full acceptance condition for a suite:
 * with the engine present, the core scenarios (which require no capability)
 * must PASS, and every capability catalogue — the stub-based four
 * (faults/scripting/templating, #185), the stateful two (scenarios/state
 * inspection, #193, over the v2 in-process admin plane), and intercept — must
 * PASS against the live engine; with the engine absent, the whole column is
 * SKIPPED-unavailable (also conformant). So one assertion gates each catalogue.
 */
object EmbeddedConformanceSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  // The embedded adapter is capability-complete (all seven) — the in-process admin plane (rift#343)
  // reaches the scenario-state endpoints the C-ABI could not before, and the intercept listener
  // starts in-process so it's always host-reachable (#285/B5). This must mirror
  // RiftMockControl.capabilities so the harness runs every catalogue live.
  private val embedded =
    MockBackendUnderTest(
      "embedded",
      Provisioning.live >>> EmbeddedRift.layer.mapError(asT),
      Set(
        Capability.Faults,
        Capability.Scripting,
        Capability.ProxyRecord,
        Capability.Templating,
        Capability.StatefulScenarios,
        Capability.StateInspection,
        Capability.Intercept
      ),
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

  /**
   * The regression guard (#285/B1): `conformance`'s embedded column silently
   * stopped running once (`conformance.dependsOn(rift % Test)` doesn't export
   * `rift`'s own Test-scoped `rift-java-embedded`/`rift-java-natives`, so no
   * engine resolved) and every scenario read as a justified SKIP rather than
   * the misconfiguration it was. On a JDK that CAN support the embedded engine
   * (22+, what `rift-java-embedded` needs), `available` returning `false` must
   * FAIL this spec — never silently degrade to a skip, which is only legitimate
   * on an unsupported (< 22) JDK.
   */
  private def embeddedAvailabilityGuardSpec =
    test("EmbeddedRift.available is true on a JDK that can run it (>= 22) — never a silent skip") {
      val jdkFeature = java.lang.Runtime.version().feature()
      // Below 22: `rift-java-embedded` can never load here, so `available == false` is the honest
      // answer, not a regression — nothing to assert. 22+: `available` must be true, or this is a
      // build/classpath misconfiguration (missing rift-java-embedded/rift-java-natives on the Test
      // classpath) masquerading as an unsupported platform.
      assertTrue(jdkFeature < 22 || EmbeddedRift.available)
    }

  def spec = suite("EmbeddedConformance")(
    embeddedAvailabilityGuardSpec,
    conforms("core (#125)", CoreConformanceScenarios.all),
    conforms("negotiation/error (#127)", NegotiationErrorScenarios.all),
    conforms("cap-stateful (#131)", CapStatefulScenarios.all),
    conforms("faults (#128)", FaultScenarios.all),
    conforms("scripting (#132)", ScriptingScenarios.all),
    conforms("templating (#132)", TemplatingScenarios.all)
  ) @@ TestAspect.withLiveClock
