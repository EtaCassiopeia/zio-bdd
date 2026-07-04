# Changelog

All notable changes to zio-bdd are documented here.

## [Unreleased]

## [1.3.0] — 2026-07-04

### Added

- **`allMocks` static catalog discovery** — `MockSteps` now exposes an overridable
  `mockCatalog: Map[String, MockSource]` and a pure, reflectively-callable
  `allMocks: List[MockSummary]` (`name` + `sourceKind`, name-sorted), mirroring
  `ZIOSteps.allDefinitions`. Editor tooling (LSP / IntelliJ) can read a suite's `@mock(name)`
  catalog from a no-arg-constructed instance with no live mock backend, to offer completion and
  unknown-name diagnostics. (#204)
- **rift-embedded Correlated (space-based) isolation** — the embedded FFM provider gains the
  Correlated isolation model (a shared imposter keyed by correlation id), matching the container
  and WireMock adapters. (#203)

### Fixed

- **Core artifacts no longer require Java 22** — the published non-FFM modules (`zio-bdd`,
  `zio-bdd-gherkin`, `zio-bdd-mock`, `zio-bdd-rift`, `zio-bdd-wiremock`) now pin `-release:11`, so
  their bytecode is class 55 and loadable on Java 11+ regardless of the JDK that cuts the release.
  1.2.0 was accidentally published as class-66 (Java 22) bytecode that even JDK 21 could not load.
  The FFM/embedded modules keep their JDK 21/22 split. (#205)

### Changed

- Bumped the pinned Rift release to v0.9.1. (#209)

## [1.2.0] — 2026-07-04

### Added

- **Portable MockControl SPI** — a backend-neutral mocking API (`MockControl`, `MockSource`, the
  canonical request/response model, fail-fast capability negotiation) with a fluent DSL,
  `@mock(name)` / `@flags` scenario fixtures, the `MockSteps[R, S]` mixin, and SUT base-URL
  injection for share-nothing isolation.
- **Adapters** — Rift (container, via testcontainers) and in-process WireMock, plus an embedded
  in-process Rift provider over Panama FFM (`MockControl.embedded`, JDK-21 preview / JDK-22 stable
  variants with bundled native libraries).
- **Capabilities** — Faults, StatefulScenarios + StateInspection, Scripting, ProxyRecord, and
  Templating, verified by a conformance harness + matrix runner across adapters.

## [1.1.0] — 2026-06-26

### Added

- **Property-based testing (Mode P)** — `@property(samples=N, seed=S, shrink=true, ...)` on a
  header-only `Examples:` block runs the scenario against N generated samples instead of literal
  rows. The `HasGen[T]` typeclass resolves a `zio.test.Gen` per column (built-ins for
  `Int`/`Long`/`Double`/`Boolean`/`String`/`UUID`), named overrides via `HasGen.named(name)(gen)`
  and the `| column: generatorName |` header syntax, and `ZIOSteps#columnGenLookup` for
  suite-level column-to-generator routing. Falsifying samples are persisted to
  `.zio-bdd/failures/` and replayed first on the next run; `pretty` and `junitxml` reporters both
  render the counterexample.

### Fixed

- **`scenarioParallelism = 0` ("auto") silently ran sequentially outside `ZIOBDDFramework`** —
  callers that constructed the executor directly (rather than going through the sbt test
  framework entry point) did not get the intended auto-detected parallelism and fell back to
  running scenarios one at a time.
- **A blank line in a `Feature:` description silently dropped the feature** — the Gherkin parser
  treated a blank line inside the description block as the end of the feature, discarding any
  content (including scenarios) that followed.

## [1.0.0] — 2026-06-16

### Added

- **Comprehensive pre-release test suite** covering concurrency, Cause/Exit fidelity,
  JUnit XML reporter, sbt test-interface entry point, background failure semantics,
  step-registry sealing, step timeout × hooks interaction, and LogCollector contention.
- **Realistic example suites**: BankingTransferSpec, RestApiSpec, CsvPipelineSpec,
  SagaWorkflowSpec — exercising layered services, DocStrings, DataTables, GivenS/WhenS/ThenS
  state-injecting variants, withSnapshot, and Assertions.collectAll.
- **Outline edge cases and Unicode tests** (OutlineEdgeCasesSpec) covering multiple
  Examples blocks, pipe-escaped cells, Unicode in step text/tables/DocStrings.
- **Gherkin fuzz and compliance extension tests** (GherkinParserFuzzSpec,
  GherkinParserComplianceExtSpec) covering `# language:` directives, Rule edge cases,
  empty features, interleaved comments, and property-based parser invariants.
- **StressSpec** for 1000+ scenarios under high parallelism (tagged, not run by default).
- `scalacOptions`: `-deprecation -feature -unchecked -Wunused:imports` in all subprojects.
- **JUnit XML formatter and reporter tests** (JUnitXMLFormatterSpec, JUnitXMLReporterSpec)
  validating the emitted XML by parsing it back.
- **ZIOBDDFrameworkSpec** unit-testing the sbt fingerprint, filterFeatures tag inheritance,
  scenarioNameFilter glob, and CompositeReporter delegation.
- **Binary-compatibility checking** via sbt-mima-plugin, with the baseline established at
  1.0.0 (1.0.x / 1.1 are verified against 1.0.0; runs in CI).
- **Strict/lint parser mode** (`parseFeature(..., strict = true)`): flags an unrecognized line
  appearing after a scenario's steps begin as a likely misspelled step keyword.

### Fixed

- `parseScenario` swallowing the next scenario's tag line (commit 4a9cc13).
- Feature-level tags now inherited when filtering scenarios (commit e012d89).
- **JUnitXMLFormatter thread-safety**: `generateXML` used a shared, non-thread-safe
  `PrettyPrinter`; it now uses a fresh instance per call so concurrent report generation
  cannot corrupt the XML output.
- Example `SimpleSpec.scala` typo: `- <-` corrected to `_ <-`.
- `example/README.md` ZIO version reference corrected from `2.1.16` to `2.1.17`.

### Changed

- `featureDir` (singular) is deprecated; use `featureDirs` (array) in `@Suite`.
- **before/afterStep hook failures now fail the step** (fail-loud) instead of aborting the
  entire feature run. A hook that fails or dies marks that step (and scenario) failed with the
  hook's cause, and the run continues; a hook that handles its own errors leaves the step Passed.

### Future Work

- `@retry(n)` annotation: planned for post-v1 to avoid breaking changes.
- Multi-language Gherkin keyword support.

---

## [0.1.0] — Initial release

- Feature/Background/Scenario/Scenario Outline/Rule/DocString/DataTable parser.
- `ZIOSteps[R, S]` trait with `Given`/`When`/`Then`/`And`/`But` DSL.
- `GivenS`/`WhenS`/`ThenS` state-injecting variants.
- Three-tier environment model (`globalLayer` / `featureLayer` / `scenarioLayer`).
- Tag include/exclude filtering; `@ignore`; `@flags(k=v)` matrix expansion.
- Per-step timeout via `@Suite(stepTimeout=N)` or `override def stepTimeout`.
- Lifecycle hooks: `beforeAll/afterAll`, `beforeFeature/afterFeature`,
  `beforeScenario/afterScenario`, `beforeStep/afterStep`.
- `ScenarioContext`, `Stage`, `FeatureContext`, `TypeMap`, `HasLens` state systems.
- `PrettyReporter` (ANSI/plain) and `JUnitXMLReporter` (JUnit 5 / 4 XML output).
- `StreamingReporter` / `LiveProgressReporter`.
- HTTP assertion mixin (`HttpSteps`).
- sbt test-interface integration (`ZIOBDDFramework`).

