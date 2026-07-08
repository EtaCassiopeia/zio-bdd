# zio-bdd Documentation

zio-bdd is a Scala 3 + ZIO 2 BDD testing framework.  Write plain Gherkin `.feature` files;
implement type-safe step definitions; run them through sbt with full ZIO dependency injection.

---

## Getting started

New to zio-bdd?  Read in this order:

1. **[Quick Start](quickstart.md)** — add the dependency, write a feature file and step
   definitions, run the suite, and read the output.  Done in five minutes.
2. **[Concepts](concepts.md)** — understand the mental model: how a `.feature` file becomes
   ZIO effects, what `R` and `S` mean, how state is isolated per scenario.
3. **[Step DSL](step-dsl.md)** — the full extractor reference (`string`, `int`, `table[T]`,
   `docString`, `regex`, `oneOf`, etc.) plus the `StepEffect` / `StepIO` type aliases.
4. **[Property-Based Testing](property-testing.md)** — once you're comfortable with literal
   `Examples:` rows, learn the `@property(...)` tag to sample generated values instead.

---

## Core reference

| Document | What it covers |
|----------|----------------|
| [State Management](state.md) | `ScenarioContext`, `Stage`, `FeatureContext`, `TypeMap`, `HasLens`, `ScenarioLens`, `withSnapshot`, `GivenS`/`WhenS`/`ThenS` |
| [Environment & Layers](layers.md) | `R` type parameter, three-tier model (`globalLayer`, `featureLayer`, `scenarioLayer`), `flagLayer`, `HasService` |
| [Hooks](hooks.md) | `beforeAll`, `afterAll`, `beforeFeature`, `afterFeature`, `beforeScenario`, `afterScenario`, `beforeScenarioTagged`, `beforeStep`, `afterStep` |
| [Step DSL](step-dsl.md) | All extractors, `StepEffect`, `StepIO`, `InlineStepMethods`, `pending`, `withSnapshot`, soft assertions |
| [Gherkin Syntax](gherkin.md) | Complete Gherkin reference — `Feature`, `Background`, `Scenario`, `Scenario Outline`, `Examples`, `Rule`, tags, data tables, doc strings |
| [Property-Based Testing](property-testing.md) | `@property(...)` tag, `HasGen[T]` registry, named generator overrides, failure replay, JUnit XML output |
| [Running Tests](running.md) | `@Suite` annotation, sbt commands, CLI flags, dry-run, tag filtering, parallelism, step timeout, IDE integration |
| [Feature Flag Testing](testing-flags.md) | `@flags(k=v)` matrix expansion, `flagLayer`, `ScenarioMetadata.flagValues`, OpenFeature / Optimizely patterns |
| [Reporters](reporters.md) | `pretty` (console tree), `junitxml` (CI reports), `StreamingReporter`, `LiveProgressReporter`, custom reporters |
| [Performance](performance.md) | Parallelism tuning heuristics, log-capture memory characteristics, state-mechanism cost, startup & discovery |

---

## Mocking

Portable HTTP mocking — write a scenario once and run it across Rift (container **and** no-Docker
embedded FFM) and WireMock, negotiating capabilities per backend.

| Document | What it covers |
|----------|----------------|
| [Mocking Overview](mocking.md) | The portable `MockControl` SPI, `MockSpace` + isolation (PerInstance/Correlated), the request/response model, `MockSource` variants |
| [Mock DSL](mock-dsl.md) | The `zio.bdd.mock.dsl.*` builder — matching requests, building responses, rule precedence, the stateful-scenario builder, raw JSON sources |
| [Mock Adapters](mock-adapters.md) | Rift (`managed`/`connect`), WireMock, and the **embedded FFM** provider (JDK 21/22 matrix + artifacts, LuaJIT); the capability × adapter matrix; choosing one |
| [Mock Gherkin Integration](mock-gherkin.md) | `MockSteps` mixin, the `@mock(name)` tag + `MockFixtures`, the catalog pattern, `Stage` |
| [Mock Advanced](mock-advanced.md) | Capability accessors (faults, scenarios, state inspection, scripting, proxy/record, templating), the `provisionNative` escape hatch, capability negotiation |

---

## Guides

| Document | Use when |
|----------|----------|
| [Cookbook](cookbook.md) | You have a specific task: structuring a multi-module suite, passing data between steps, writing HTTP tests, date-relative parameters, soft assertions, selective tagging |
| [Mock Cookbook](mock-cookbook.md) | You want mocking recipes: stand up a backend as a layer, a portable `MockSteps` scenario, `@mock(...)` fixtures, injecting faults across adapters |
| [Troubleshooting](troubleshooting.md) | Something is wrong: compiler errors, startup failures, state not updating, missing step definitions, timeouts |
| [Migrating from Cucumber](migrating.md) | You are converting a Cucumber-JVM + cucumber-scala suite to zio-bdd |

---

## Quick decisions

**"Which state mechanism do I use?"**
→ See [State Management §6](state.md#6-choosing-the-right-approach) for the decision table.

**"Stage or ScenarioContext?"**
→ [Stage](state.md#7-stage-per-scenario-staging-without-schema) for ephemeral pipeline data
(event payloads, intermediate results).  [ScenarioContext](state.md#2-reading-and-writing-state-in-step-bodies)
for values that `Then` steps need to assert on.

**"GivenS or Given?"**
→ Use `GivenS`/`WhenS`/`ThenS` when the step reads state at the top.  Use `Given`/`When`/`Then`
when the step does not read state at all.  See [cookbook.md §14](cookbook.md#14-using-givens--whens--thens-vs-given--when--then).

**"globalLayer, featureLayer, or scenarioLayer?"**
→ `globalLayer` — shared across the entire run (connection pools, embedded servers).
`featureLayer` — fresh per feature file.
`scenarioLayer(meta)` — fresh per scenario; use when tag-based conditional layers are needed.
See [layers.md §2](layers.md#2-the-three-tier-environment-model).

**"My step trait can't call Given — compile error?"**
→ Add the self-type: `trait MySteps { self: ZIOSteps[R, S] => ... }`.
See [troubleshooting.md](troubleshooting.md#value-given-is-not-a-member-of-mysteptrait).
