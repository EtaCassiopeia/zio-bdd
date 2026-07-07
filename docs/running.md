# Running Tests

This document covers every mechanism for executing zio-bdd test suites: the `@Suite`
annotation, sbt commands, CLI flags, dry-run mode, filtering, parallelism, and IDE
integration.

---

## @Suite annotation

The `@Suite` annotation is placed on the companion object of a `ZIOSteps` subclass. It
tells the sbt test framework where to find feature files and how to execute them.

```java
// zio.bdd.core.Suite (Java annotation)
public @interface Suite {
    String[] featureDirs()  default {"src/test/resources/features"};
    String[] reporters()    default {"pretty"};
    int      parallelism()  default 1;
    String[] includeTags()  default {};
    String[] excludeTags()  default {};
    String   logLevel()     default "info";
    int      stepTimeout()  default 0;
}
```

### Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `featureDirs` | `String[]` | `{"src/test/resources/features"}` | One or more paths to directories or individual `.feature` files. Filesystem paths are resolved relative to the project root. Prefix with `classpath:` to load from the classpath (e.g. from a shared test-jar): `"classpath:features"` or `"classpath:features/my.feature"`. |
| `reporters` | `String[]` | `{"pretty"}` | Reporter names to use. Built-in values: `"pretty"` (coloured console output), `"junitxml"` (JUnit 5 XML in `target/test-results/`). |
| `parallelism` | `int` | `1` | Maximum number of features executed concurrently. `1` means sequential. |
| `includeTags` | `String[]` | `{}` | When non-empty, only scenarios that carry at least one of these tags are executed. |
| `excludeTags` | `String[]` | `{}` | Scenarios carrying any of these tags are skipped. |
| `logLevel` | `String` | `"info"` | Minimum log level captured during step execution: `"debug"`, `"info"`, `"error"`. |
| `stepTimeout` | `int` | `0` | Maximum wall-clock time in **seconds** for a single step. `0` means no timeout. Applies to every step in the suite unless overridden in the suite class — see [Step timeout](#step-timeout) below. |

**Deprecated field:** `featureDir` (singular, `String`) is still accepted for backward
compatibility. When both `featureDirs` and `featureDir` are set, `featureDirs` takes
precedence.

### Examples

```scala
@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[AppEnv, AppState]:
  override def environment = AppEnv.layer
```

```scala
// Multiple feature directories
@Suite(
  featureDirs = Array(
    "src/test/resources/features/provision",
    "src/test/resources/features/post"
  ),
  reporters   = Array("pretty", "junitxml"),
  parallelism = 4,
  excludeTags = Array("slow", "manual"),
  logLevel    = "debug"
)
object FullSuite extends ZIOSteps[AppEnv, AppState]:
  override def environment = AppEnv.layer
```

```scala
// Run only smoke tests in CI
@Suite(
  featureDirs  = Array("src/test/resources/features"),
  includeTags  = Array("smoke"),
  parallelism  = 2
)
object SmokeSuite extends ZIOSteps[AppEnv, AppState]:
  override def environment = AppEnv.layer
```

```scala
// Load feature files from the classpath — useful when .feature files are
// packaged in a shared test-jar from another module
@Suite(
  featureDirs = Array("classpath:features/provision", "classpath:features/post")
)
object SharedFeatureSuite extends ZIOSteps[AppEnv, AppState]:
  override def environment = AppEnv.layer
```

---

## Running with sbt

### Run all test suites

```sh
sbt test
```

Runs every class annotated with `@Suite` that is on the test classpath.

### Run a specific suite

```sh
sbt "testOnly com.example.MySuite"
```

### Run with CLI flags

CLI flags are passed after `--`. They override the corresponding `@Suite` annotation
fields for the duration of that run.

```sh
sbt "testOnly com.example.MySuite -- --dry-run"
sbt "testOnly com.example.MySuite -- --include-tags smoke,regression"
sbt "testOnly com.example.MySuite -- --exclude-tags slow --parallelism 4"
sbt "testOnly com.example.MySuite -- --scenario-name 'Provision*'"
sbt "testOnly com.example.MySuite -- --log-level debug"
```

Multiple flags may be combined on one command:

```sh
sbt "testOnly com.example.MySuite -- \
  --feature-file src/test/resources/features/provision.feature \
  --include-tags smoke \
  --parallelism 2 \
  --log-level debug"
```

---

## CLI flags reference

| Flag | Argument | Description | Example |
|------|----------|-------------|---------|
| `--feature-file` | `<path>` | Path to a specific `.feature` file or directory. Overrides `featureDirs` from the annotation. May be repeated to specify multiple files. | `--feature-file src/test/resources/features/provision.feature` |
| `--include-tags` | `<tag,...>` | Comma-separated list of tags. Only scenarios carrying at least one of these tags run. Merges with `includeTags` from the annotation when the annotation value is non-empty. | `--include-tags smoke,regression` |
| `--exclude-tags` | `<tag,...>` | Comma-separated list of tags. Scenarios carrying any of these tags are skipped. Merges with `excludeTags` from the annotation. | `--exclude-tags slow,manual` |
| `--scenario-name` | `<glob>` | Case-insensitive glob filter on scenario names. `*` matches any sequence of characters. Only scenarios whose name matches the pattern are executed; others are skipped (marked `@ignore`). | `--scenario-name 'Provision*'` |
| `--parallelism` | `<n>` | Maximum number of features executed concurrently. Overrides `parallelism` from the annotation. | `--parallelism 4` |
| `--scenario-parallelism` | `<n>` | Maximum number of scenarios executed concurrently within a single feature. Independent from feature-level parallelism. Default: `1`. | `--scenario-parallelism 2` |
| `--dry-run` | _(none)_ | Enable dry-run mode (see below). | `--dry-run` |
| `--log-level` | `debug\|info\|error` | Minimum log level captured during step execution. Overrides `logLevel` from the annotation. | `--log-level debug` |
| `--reporter` | `pretty\|junitxml` | Reporter to use. May be repeated. Overrides `reporters` from the annotation when specified. | `--reporter junitxml` |

**Precedence:** CLI flags take precedence over `@Suite` annotation fields. When a CLI
flag is not supplied, the annotation field value applies. Additive fields (`--include-tags`,
`--exclude-tags`) override the annotation value entirely rather than merging.

---

## Dry-run mode

Dry-run executes the full test pipeline — feature file discovery, Gherkin parsing, step
matching, tag filtering — but does **not** execute step bodies. Every matched step is
reported as `PASSED`. Unmatched steps are reported as failures.

**When to use it:**

- After writing new feature files, before implementing step definitions: verify that all
  steps have a corresponding registered definition.
- Before a refactor: confirm that renaming a step pattern does not break any feature
  files.
- In CI pre-merge checks: catch missing step definitions without running the full suite.

```sh
sbt "testOnly com.example.MySuite -- --dry-run"
```

Dry-run can also be enabled via the `@Suite` annotation (useful for always-dry
suites in a module that only validates Gherkin syntax):

```scala
@Suite(featureDirs = Array("src/test/resources/features"), /* dryRun not in annotation */ )
```

Note: `dryRun` is not an annotation field — it can only be set via the CLI flag.

---

## Tag filtering

### Include tags

Only scenarios that carry at least one of the listed tags are executed. All other
scenarios are marked as ignored (`@ignore` tag is appended) and are excluded from
results.

```sh
sbt "testOnly com.example.MySuite -- --include-tags smoke"
sbt "testOnly com.example.MySuite -- --include-tags smoke,regression"
```

Feature-file annotation equivalent:

```scala
@Suite(includeTags = Array("smoke"))
```

### Exclude tags

Scenarios carrying any of the listed tags are skipped regardless of include tags.
Exclusion takes precedence over inclusion.

```sh
sbt "testOnly com.example.MySuite -- --exclude-tags slow,flaky"
```

### @ignore tag

Any scenario tagged `@ignore` (case-insensitive) is always skipped, independent of
include/exclude filters. This is the standard way to mark a known-failing or
not-yet-implemented test.

### Tag filtering on features

When all scenarios in a feature are filtered out or ignored, the feature itself is also
treated as ignored and produces no step results.

---

## Scenario name filter

The `--scenario-name` flag accepts a glob pattern matched case-insensitively against
scenario names. `*` matches zero or more characters.

```sh
# Run all scenarios whose name starts with "Provision"
sbt "testOnly com.example.MySuite -- --scenario-name 'Provision*'"

# Run a scenario with an exact name
sbt "testOnly com.example.MySuite -- --scenario-name 'Happy path login'"

# Run all scenarios containing "EOD"
sbt "testOnly com.example.MySuite -- --scenario-name '*EOD*'"
```

Scenarios that do not match the pattern are marked as ignored; they appear in the report
but are not executed.

---

## Parallelism

zio-bdd has two independent parallelism controls.

### Feature-level parallelism (`--parallelism` / `parallelism`)

Controls how many feature files run concurrently. The default is `1` (sequential).
Feature-level parallelism is safe when features are fully independent — different
DynamoDB tables, different accounts, isolated service state.

```sh
sbt "testOnly com.example.MySuite -- --parallelism 4"
```

```scala
@Suite(parallelism = 4)
```

### Scenario-level parallelism (`--scenario-parallelism` / `scenarioParallelism`)

Controls how many scenarios within a single feature run concurrently. Scenario-level
parallelism requires that scenarios within a feature are independent and do not share
mutable state.

```sh
sbt "testOnly com.example.MySuite -- --scenario-parallelism 2"
```

```scala
@Suite(scenarioParallelism = 2)
```

The default is `0`, meaning **auto**: the number of available processors (falling back to
`2` if that can't be determined), so parallel scenario execution is on by default. Pass `1`
explicitly to force fully sequential execution, or `auto`/`0` to make the auto behavior
explicit:

```sh
sbt "testOnly com.example.MySuite -- --scenario-parallelism auto"
```

### Combining both

```sh
sbt "testOnly com.example.MySuite -- --parallelism 4 --scenario-parallelism 2"
```

This runs up to 4 features concurrently, each with up to 2 scenarios concurrently,
for a maximum of 8 concurrent scenario executions.

### Runtime overrides and precedence

A static `@Suite` value can't be right in every environment. A common case: a suite backed by
a single embedded resource (e.g. a local DynamoDB container) must run sequentially locally, but
can use full parallelism in CI where the resource is provisioned to handle load. Because
annotation values are compile-time constants, you can't branch on an env var inside `@Suite`.

Two runtime overrides close that gap. Both `scenarioParallelism` and `featureParallelism`
resolve through this chain, **highest precedence first**:

| # | Source | How to set |
|---|--------|------------|
| 1 | Suite override (Scala) | `override def scenarioParallelism: Option[Int]` in the suite |
| 2 | CLI flag | `--scenario-parallelism <n>` / `--parallelism <n>` |
| 3 | Env var | `ZIO_BDD_SCENARIO_PARALLELISM` / `ZIO_BDD_FEATURE_PARALLELISM` |
| 4 | `@Suite` annotation | `@Suite(scenarioParallelism = n)` (compile-time default) |

The env var accepts an integer or `auto` (equivalent to `0`); an empty or unparseable value is
ignored, deferring to the annotation. It is a JVM-wide default, shared by every suite in the fork.

> **Note on CLI defaults.** The "unset" CLI sentinel is in-band: `--scenario-parallelism` defaults
> to `0` (auto) and `--parallelism` to `1` (sequential). Passing exactly that default value on the
> CLI is indistinguishable from omitting the flag, so it is treated as *unset* and an env var or
> `@Suite` value can still apply. To force sequential scenarios regardless of the environment, use
> the suite override (`override def scenarioParallelism = Some(1)`) rather than `--scenario-parallelism 1`.

The suite override (`def scenarioParallelism: Option[Int] = None` / `def featureParallelism`)
wins over everything when it returns `Some`. Returning `None` (the default) defers to the CLI
flag, env var, and annotation as above. This lets you decide in plain Scala:

```scala
@Suite(featureDirs = Array("src/test/resources/features"))
object LedgerSuite extends ZIOSteps[LedgerEnv, LedgerState]:
  // Sequential locally (CI env var absent), full parallelism (auto) in CI.
  override def scenarioParallelism: Option[Int] =
    Option(System.getenv("CI")).map(_ => 0).orElse(Some(1))
```

Or set it CI-wide with no change to any suite:

```sh
ZIO_BDD_SCENARIO_PARALLELISM=auto sbt test   # CI: full parallelism
sbt test                                      # local: falls back to the @Suite value
```

---

## Step timeout

Integration tests that hit live services can hang indefinitely if a step's HTTP call
stalls. The step timeout mechanism bounds every step body to a configurable wall-clock
duration.

### Suite-level default via annotation

```scala
@Suite(
  featureDirs = Array("src/test/resources/features"),
  stepTimeout = 30   // seconds; 0 = no timeout (default)
)
object MySuite extends ZIOSteps[AppEnv, AppState]:
  override def environment = AppEnv.layer
```

When `stepTimeout > 0`, the timeout is applied to every step in the suite.

### Per-suite override in the class body

Override `def stepTimeout` to set the timeout programmatically, which is useful
when the value comes from configuration rather than a literal:

```scala
object MySuite extends ZIOSteps[AppEnv, AppState]:
  override def stepTimeout: Option[Duration] = Some(Duration.fromSeconds(30))
  override def environment = AppEnv.layer
```

A non-`None` return from `def stepTimeout` takes precedence over the annotation value.

### Per-step local override

A single step can use a longer (or shorter) timeout by wrapping its effect directly:

```scala
When("a very slow operation completes") {
  longRunningEffect
    .timeout(5.minutes)
    .someOrFail(new RuntimeException("operation timed out"))
}
```

### What happens when a step times out

- The step is interrupted.
- The result is reported as `TIMEOUT after Ns` in the pretty reporter — distinct from
  a normal failure.
- The scenario is marked failed; subsequent steps are skipped.
- The `StepStatus.TimedOut(timeout, cause)` variant is available for custom reporters.

---

## Scenario retry tags

For environmentally flaky acceptance tests, a scenario can be re-run automatically. Tag it in
Gherkin with an integer argument, or configure it in code via `scenarioAspects`.

| Tag | Semantics |
|---|---|
| `@retry(n)` | Re-run on failure, up to `n` attempts; pass on the first success. |
| `@flaky(n)` | Run up to `n` attempts; pass if any succeeds (same outcome as `@retry`). |
| `@nonFlaky(n)` | Run up to `n` attempts; pass only if **every** attempt passes (fail fast on the first failure). |

```gherkin
@retry(3)
Scenario: provisions a fresh tenant
  Given a clean environment
  When a tenant is provisioned
  Then the tenant is reachable
```

Or, without touching the feature file, override `scenarioAspects` (keyed by scenario name):

```scala
override def scenarioAspects: Map[String, ScenarioAspect] =
  Map("provisions a fresh tenant" -> ScenarioAspect.Retry(3))
```

**Semantics & interactions**

- Each attempt is fully independent: per-scenario state is reset and the `beforeScenario` /
  `afterScenario` hooks run on every attempt.
- A tag on the scenario takes precedence over the `scenarioAspects` map. `n` is clamped to `≥ 1`.
- `stepTimeout` applies per step *within* each attempt, so a retried scenario's worst-case
  wall-clock time is up to `n ×` the single-attempt time.
- `@ignore` wins over a retry tag: an ignored scenario never runs and is not retried. Include/
  exclude tag filtering is unaffected — the retry tags are not treated as filter labels.
- The pretty reporter appends `(k attempts)` to a scenario that ran more than once; the count is
  also available as `ScenarioResult.attempts` for custom reporters.
- Retry tags are **scenario-level only** — a `@retry(n)` placed above `Feature:` does not apply to
  the feature's scenarios; tag each scenario (or use `scenarioAspects`). An unrecognised or
  malformed argument (e.g. `@retry(abc)`) is ignored and the scenario runs once.

---


## IDE integration

**For a live editing experience** (go-to-definition, hover, autocomplete, real-time
"no matching step" diagnostics with a closest-match hint, and a code lens to run a
scenario/feature) — install the LSP server, VSCode extension, or IntelliJ plugin from
[zio-bdd-tooling](https://github.com/EtaCassiopeia/zio-bdd-tooling). It scans `.scala`/
`.feature` source text directly and needs no build step or registry file.

The two sbt tasks below (`generateStepRegistry`, `zioBddSnippets`) predate that tooling and
serve a narrower, different purpose — read on only if you specifically need one of them.

### generateStepRegistry — interop with *other* Cucumber IDE plugins

> **Scope note:** `generateStepRegistry` is defined by an sbt auto-plugin in *this
> repository's own build* (`project/StepRegistryPlugin.scala`), not by the published
> `io.github.etacassiopeia:zio-bdd` artifact. A project that only adds zio-bdd as a library
> dependency does not get this task — running it fails with "not a valid command". It's
> usable today if you're working inside a clone of this repository, or if you copy
> `StepRegistryPlugin.scala` into your own `project/` directory. See
> [zio-bdd#104](https://github.com/EtaCassiopeia/zio-bdd/issues/104) if you'd find a
> published, `addSbtPlugin`-installable version of this task valuable.

The `generateStepRegistry` sbt task produces a `step-registry.json` file that *generic*
Cucumber-ecosystem IDE plugins (the VS Code Cucumber extension, IntelliJ's built-in Cucumber
support) can use for step navigation — it is not consumed by zio-bdd-tooling's own LSP, which
resolves steps itself from live source text and needs no intermediate file.

```sh
sbt generateStepRegistry
```

Output: `target/zio-bdd/step-registry.json`

The JSON contains one entry per registered step definition:

```json
{
  "version": "1",
  "generator": "zio-bdd-step-registry",
  "steps": [
    {
      "keyword": "Given",
      "text": "a valid provision body",
      "pattern": "a valid provision body",
      "file": "/path/to/ProvisionSteps.scala",
      "line": 42
    },
    {
      "keyword": "When",
      "text": "a post request is sent",
      "pattern": "a post request is sent",
      "file": "/path/to/PostSteps.scala",
      "line": 17
    }
  ]
}
```

**How it works:** the task scans all `.scala` files under the test source directories for
`Given("...")`, `When("...")`, `Then("...")`, `And("...")`, `But("...")` call patterns.
Pattern extraction is best-effort — it reads source text, not compiled bytecode, so
complex chained extractor expressions may not appear in the output.

**IDE configuration:**
- **VS Code Cucumber extension:** set `cucumber.stepDefinitions` in `.vscode/settings.json`
  to `["target/zio-bdd/step-registry.json"]`.
- **IntelliJ IDEA:** configure "Step definition patterns" to use the generated patterns.

Regenerate after adding or renaming step definitions to keep the registry current.

---

## zioBddSnippets task

> **Scope note:** same caveat as `generateStepRegistry` above — `zioBddSnippets` is defined
> in this repository's own build (`project/SnippetGeneratorPlugin.scala`), not published for
> consumers of the library. If you're using zio-bdd-tooling's LSP/VSCode/IntelliJ extension,
> you don't need this task at all: opening a step-definition `.scala` file and typing inside a
> `Given`/`When`/`Then(...)` call already surfaces a completion item for any unmatched step
> text found in your `.feature` files, live, with no command to run.

The `zioBddSnippets` sbt task scans feature files for steps that do not appear to have a
matching step definition and prints skeleton code to stdout.

```sh
sbt zioBddSnippets
sbt "zioBddSnippets src/test/resources/features/my-module"
```

It is advisory — the task always exits successfully and does not fail the build. Use
`--dry-run` at test time for build-breaking validation of missing step definitions.

Example output:

```scala
// ── Generated step skeletons ──────────────────────────────────────────────
// Paste into your step trait and implement each body.

Given("a valid provision body") {
  ZIO.unit // TODO implement
}

When("a post request is sent") {
  ZIO.unit // TODO implement
}

Then("the response status is " / int) { (a: Int) =>
  ZIO.unit // TODO implement
}
```
