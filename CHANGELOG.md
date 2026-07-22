# Changelog

All notable changes to zio-bdd are documented here.

## [Unreleased]

### Added

- **`zio-bdd-mock-conformance` is now a published artifact** (#329). The portable conformance
  scenario sets (`CoreConformanceScenarios`, `NegotiationErrorScenarios`, `FaultScenarios`,
  `ScriptingScenarios`, `TemplatingScenarios`, `CapStatefulScenarios`) and the
  `ConformanceHarness` already lived in the conformance module's main sources — the module was
  simply publish-skipped. It now publishes with a compile-scope dependency on the SPI
  (`zio-bdd-mock`) alone, so third-party `MockControl` adapters (e.g. rift-scala's) can run the
  official conformance suite in their own CI without pulling in any bundled backend; the
  in-repo Rift/WireMock/embedded matrix runner moved to test scope, unpublished as before.
  Usage is documented in `docs/mock-adapters.md` §6.

### Changed

- **Re-based the Rift `MockControl` adapters onto the official `rift-scala-zio` SDK** (#285).
  `RiftMockControl` now drives Rift entirely through `rift.zio.Rift`, replacing the hand-rolled
  admin-HTTP protocol client and the hand-written Panama FFM bridge to a downloaded
  `librift_ffi` cdylib with the SDK's typed surface — one adapter for every transport (container,
  in-process embedded, or a bare `connect`), where previously the container and embedded
  backends were two separate implementations of the same protocol.
  - **Artifact collapse**: `zio-bdd-rift`, `zio-bdd-rift-embedded`, `zio-bdd-rift-embedded-jdk21`,
    and `zio-bdd-rift-embedded-natives` (four published artifacts) collapse into the single
    `zio-bdd-rift`. There is no more JDK-21-preview-vs-JDK-22-stable split: the embedded engine
    (`rift-java-embedded`) and its native library (`rift-java-natives`, classified per platform)
    are ordinary runtime `ServiceLoader` dependencies an application adds itself, not a bundled
    natives jar this build downloads, checksums, and packages.
  - **JDK floor**: `zio-bdd-rift`'s floor moves 11 -> 17 (its SDK dependency links
    `rift-java-core`, JDK-17 bytecode); every other module (core, gherkin, mock, wiremock,
    conformance) is unaffected and stays on 11. The embedded provider still needs JDK 22+ at
    *runtime* (stable Panama FFM) — `EmbeddedRift.available` reports `false` (not a crash) on an
    older JVM.
  - **Source break**: `Rift.managed`/`Rift.connect` now require only `Provisioning` in their
    environment — the SDK owns the transport, so the `zio-http` `Client` they used to require is
    gone. `Rift.connect` is now fallible (`ZLayer[Provisioning, MockError, MockControl]`, was
    `URLayer`): the SDK's `connect` performs a real admin handshake at layer construction.
    `Rift.managed`'s `adminPort` is effectively fixed at 2525 (the SDK's container transport has
    no override); a non-default value now fails fast with a typed `MockError.InvalidDefinition`
    instead of being silently accepted. The Rift adapter now advertises all seven capabilities
    (including `Intercept`) uniformly on every transport, rather than gating `Intercept` on
    whether the container was started with an `interceptPort`.
  - Update every call site accordingly: drop the `Client` requirement/import, and — for
    `Rift.managed`/`Rift.connect` — drop `Client.default` from the provided environment.

### Fixed

- **`--exclude-tags` no longer provisions an excluded scenario's fixtures** (#337). A scenario
  removed by a tag or name filter (or carrying `@ignore`/`@skip`) is now short-circuited *before*
  its scenario-tier layer (`scenarioLayer`/`flagLayer`) is built and acquired, so no
  `@mock(...)` source is provisioned and no per-scenario resources are set up for it. Previously
  the `isIgnored` short-circuit fired only *inside* the already-acquired layer scope, so an
  excluded scenario carrying a parameterized tag such as `@rift @mock(orders)` still provisioned
  its `orders` source — and, when that source was raw/native under the wrong backend, threw and
  failed the whole run even though the scenario was meant to be skipped.

## [1.4.3] — 2026-07-16

### Changed

- **Bumped Rift to v0.14.0** (from v0.13.1). Realigns the embedded leg — which loads `librift_ffi`
  from `zio-bdd-rift-embedded-natives` — with the container and external legs, which
  `rift-conformance` already moved to 0.14.0. The bump is ABI-safe: Rift v0.14.0 ships no
  C-ABI/FFI change (`abi: v2` unchanged), so the single-source-of-truth `riftVersion` in
  `build.sbt` drives it — the embedded natives, `Rift.DefaultImage`, and the generated
  `RiftBuildInfo` all track 0.14.0. v0.14.0 brings decision-cache key completeness fixes, a
  unified imposter error-response envelope, and `--allowInjection` gating on the runtime intercept
  path, all now exercised through the embedded serve plane. (#324)

### Fixed

- **report**: the `pretty` reporter's feature-level header no longer renders a pending-only
  feature as red `FAILED`. Following #306 (a pending step doesn't redden the build), the
  feature header now reads `PENDING` (orange) when a feature has a pending scenario and no
  hard failure — consistent with the scenario line, the summary, the JUnit XML, and the
  green build. (#319)

## [1.4.2] — 2026-07-10

### Added

- **`EmbeddedRift.shared`** — a safe cross-suite shared embedded Rift instance, so several suites
  reuse one native runtime instead of each starting (and tearing down) their own.

### Changed

- **Bumped Rift to v0.13.1** (from v0.11.3, through the v0.12.0 migration). The v0.12.0 migration
  adopted Rift's **v2 scripting API and removed the Lua injection engine** — mock injection and
  decoration scripts must now be JavaScript. Rift v0.13.0 additionally brought the `_verify`
  conformance endpoint, the C-ABI admin long tail, `allowInjection`, the runtime intercept
  lifecycle, and the packaged sdk-conformance corpus. Rift v0.13.1 is a packaging-only release
  (it renames Rift's internal `rift-core` crate to `rift-mock-core` to clear a crates.io name
  collision); the FFI cdylib, container image, and binary are functionally identical to v0.13.0,
  so the C-ABI (`abi: v2`) and every mock behaviour are unchanged. (#310)
- Marked the `StreamingReporter` / `TestEvent` streaming API `@experimental`.
- Renamed the test-only reset helper to `resetForTest`. (#300)

### Fixed

- **runner**: a suite-level failure now surfaces its full cause instead of a truncated message. (#311)
- **core**: a `pending()` step no longer reddens the sbt build. (#306)
- **core**: `FeatureContext` is synchronized so parallel scenarios don't lose context.
- **core**: `afterAll` is guarded with `.ensuring` so suite teardown always runs.
- **core**: all five log levels are accepted; an unknown level now fails loudly instead of silently.
- **gherkin**: inert `shrink` / `maxShrinks` / `verbose` property settings are warned/rejected.
- **rift**: fail fast on an unreachable authored container port. (#305)
- **rift**: honour the `port` field in a raw `NativeSpec.Rift(json)` imposter document. (#214)
- **rift**: an in-pool-range authored imposter port is claimed to prevent a later auto-provision collision. (#213)
- **rift**: honour an authored `recordRequests` flag in a raw imposter document.
- **rift-embedded**: an actionable error (with remediation docs) when the native library fails to load.

## [1.4.1] — 2026-07-08

### Added

- **Caller-provided persistent CA for the embedded intercept proxy** — `EmbeddedRift.InterceptConfig`
  now accepts `caCert` / `caKey` (PEM file paths, both-or-neither). When set, the built-in TLS-MITM
  proxy loads that committed CA instead of minting a fresh ephemeral one each start, so a long-lived
  containerized SUT can trust a pre-committed truststore at JVM startup — before the test starts the
  proxy. Absent → ephemeral CA, unchanged. (#273)

### Changed

- Bumped Rift to **v0.11.3** — adds `caCertPath` / `caKeyPath` to the intercept FFI. (#273)

## [1.4.0] — 2026-07-07

### Added

- **Built-in HTTPS intercept — a mitmproxy replacement.** The `MockControl.Intercept` capability + DSL:
  a TLS-MITM forward proxy that redirects a hard-coded external host to a mock imposter space, with an
  exported CA truststore for the SUT to trust. (#249)
  - Embedded (no-Docker) intercept driven entirely over the `librift_ffi` FFI. (#246)
  - Configurable intercept bind host + optional fixed port, reachable from another container/host. (#257)
  - Intercept over the containerized Rift adapter (`Rift.managed`). (#253)
  - Merged intercept truststore (intercept CA **+** the JVM default anchors). (#261)
  - `bindHost` validated as an IP literal, with a clear error for a hostname. (#269)
  - Optional caller-specified export path for the intercept truststore. (#270)
  - `EmbeddedRift.requireAvailable` — fail loudly (vs SKIP) when no native resolves. (#272)
- **New assertions & combinators** — `eventually` / `eventuallyAssert` (#221), `during` (#226),
  collection quantifiers (#227), `assertChange` (#228), `assertSatisfies` (#239), `assertRaises` (#240),
  `poll` (#241), `assertApproxEquals` (#243).
- **Scenario control** — `@retry(n)` / `@flaky(n)` / `@nonFlaky(n)` retry tags (#229),
  `@expectedFailure` / `@failing` (#237), and env-aware, suite-overridable parallelism.
- Scenario-aspect states surfaced in the JUnit XML and the streaming reporter. (#248)

### Changed

- Bumped Rift to **v0.11.2** (through 0.10.0 → 0.11.0 → 0.11.1). (#242, #245, #251, #267)
- The rift-natives fetch now works behind a proxy / mirror / offline, with an optional credentialed
  `RIFT_NATIVES_BASE_URL` mirror host. (#260, #266)

### Fixed

- Propagate step/hook interruption instead of retrying it. (#231)
- Guard that the JDK-21 embedded FFM bridge class stays within the method-count limit. (#252)

## [1.3.1] — 2026-07-05

### Added

- **Opt-in fixed imposter port** — `MockSpec.onPort(n)` now binds the Rift container and embedded
  imposters on exactly that port instead of always auto-allocating a random one; the auto-assigned
  free port remains the default. This removes the runtime port-discovery handoff for
  externally-configured clients (proxies, containers, sidecars). A fixed port is never returned to
  the internal port pool. (#211)

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

