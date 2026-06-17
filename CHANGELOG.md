# Changelog

All notable changes to zio-bdd are documented here.

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

