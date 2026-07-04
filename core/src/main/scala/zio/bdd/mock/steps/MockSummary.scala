package zio.bdd.mock.steps

/**
 * A static summary of one `@mock(name)` catalog entry, produced by
 * `MockSteps.allMocks`. Consumed by external tooling (LSP server, IntelliJ
 * plugin) to offer completion and unknown-name diagnostics for `@mock(...)`
 * tags without a live mock backend — the catalog counterpart of
 * [[zio.bdd.core.step.StepSummary]].
 *
 * @param name
 *   the catalog key referenced by `@mock(name)`
 * @param sourceKind
 *   the [[zio.bdd.mock.MockSource]] variant backing the entry (e.g. `"Dsl"`,
 *   `"Json"`, `"Resource"`, `"File"`, `"Dir"`)
 */
final case class MockSummary(name: String, sourceKind: String)
