# Concepts — how zio-bdd works

This document explains the mental model behind zio-bdd. It covers how Gherkin text becomes
running ZIO effects, what the type parameters mean, how state is isolated per scenario, and
how the environment model controls resource sharing.

---

## 1. The flow from `.feature` to ZIO execution

At a high level, running a suite involves five stages:

```
.feature file
     │
     ▼  parse
 Feature AST (Feature / Scenario / Step nodes)
     │
     ▼  register
 StepRegistry (pattern → ZIO body map)
     │
     ▼  filter
 Filtered scenarios (tag, name-glob, excludeTags)
     │
     ▼  expand
 Expanded scenarios (Scenario Outline rows × @flags matrix)
     │
     ▼  execute
 ZIO effects run, results collected, reporters called
```

### Parse

The Gherkin parser (`GherkinParser`) reads each `.feature` file line by line using a
hand-written tokeniser. It produces a `Feature` value containing a list of `Scenario` values,
each holding a list of `Step` values. No external parser library is involved — the tokeniser
handles all standard Gherkin constructs: Feature, Background, Scenario, Scenario Outline,
Examples, data tables, doc strings, and tags.

### Register

When the sbt test runner loads your suite object, Scala's object initialiser runs all of the
top-level `Given(...)`, `When(...)`, `Then(...)`, `And(...)`, and `But(...)` calls. Each call
compiles a `StepExpression` (the pattern, which can mix literal text and typed extractors) and
records a `StepDef` entry: the step type, the compiled regex pattern, and the body function.

Registration is sealed the first time `getSteps` is called. Any attempt to register a step
after that point throws an `IllegalStateException`. Duplicate patterns for the same step type
are detected at this point and also throw, so ambiguity fails fast at suite startup rather
than mid-run.

### Filter

The framework applies tag filters (`includeTags`, `excludeTags`) and optional scenario-name
glob filters before execution. Filtered-out scenarios are not deleted — they are tagged
`ignore` internally so that the reporter can show them as skipped rather than silently
omitting them.

### Expand

Two expansion mechanisms can multiply scenarios:

- **Scenario Outline** — the parser already expands each `Examples` row into a concrete
  `Scenario` during parsing, substituting `<placeholder>` tokens with row values.
- **Flag matrix** — `@flags(key=value)` tags on a scenario cause `FeatureExecutor` to expand
  it into N runs (one per `@flags(...)` tag), each with a different `Map[String, String]`
  injected into the environment layer. The scenario name is suffixed with `[key=value]` for
  traceability in reports.

These two mechanisms compose: an Outline scenario with two `@flags(...)` tags and three
`Examples` rows produces 2 × 3 = 6 runs.

### Execute

For each scenario, `ScenarioExecutor`:
1. Creates a fresh `FiberRef[S]` initialised to `Default[S].default`.
2. Runs `beforeScenario` hooks.
3. Matches each step text against the `StepRegistry`. Unmatched steps fail with
   `StepLookupError`, which identifies the unmatched text. In dry-run mode, step bodies are
   replaced with `ZIO.unit` so only matching is validated.
4. Executes steps sequentially. On the first failure, all remaining steps in the scenario are
   marked `Skipped` rather than executed.
5. Runs `afterScenario` hooks.

The entire scenario executes inside a `ZIO.scoped` block, so `ZIO.acquireRelease` resources
acquired in step bodies are released when the scenario ends, even if a step failed.

---

## 2. The type signature of a suite: `ZIOSteps[R, S]`

```scala
trait ZIOSteps[R: Tag, S: Tag: Default]
```

### R — the ZIO service environment

`R` is the type of services your step bodies can access. It is threaded into every step body
as part of the ZIO environment. If you have no services, use `Any`.

The `R` type is provided by the layer returned from `environment` (or the three-tier layer
methods). It is built once per the applicable tier and provided to step effects at runtime.

Inside a step body, access services with `ZIO.service[MyService]`. The type `R` flows as the
`R` channel of `ZIO[R & State[S], Throwable, Unit]`.

### S — the scenario state type

`S` holds data that accumulates across steps within a single scenario: parsed request bodies,
received responses, computed IDs, and so on.

Two constraints apply to `S`:

- `Tag[S]` — a `izumi.reflect.Tag` is needed to look up the `State[S]` service at runtime.
  This is derived automatically; you do not write it explicitly.
- `Default[S]` — the framework needs a zero value to initialise state before the first step
  runs. The `Default` typeclass is automatically derived from `Schema[S]` when all fields have
  default values. Alternatively, provide `Default.from(MyState())` explicitly.

`S` is stored in a `FiberRef[S]` so it is isolated per scenario fiber: concurrent scenarios
do not share state.

### Type aliases inside ZIOSteps

Two aliases are defined inside the trait to simplify annotation of helper methods:

```scala
type StepEffect    = ZIO[R & State[S], Throwable, Unit]  // void step helper
type StepIO[+A]    = ZIO[R & State[S], Throwable, A]     // value-producing helper
```

Use these on private methods that are called from step bodies:

```scala
private def callApi(req: Request): StepEffect =
  ZIO.serviceWithZIO[HttpClient](_.post(req)).flatMap(storeResponse)
```

---

## 3. Scenario state lifecycle

```
Scenario begins
  │
  ▼
FiberRef[S] created with Default[S].default
  │
  ├── Step 1 body runs ──→ ScenarioContext.update(...)
  ├── Step 2 body runs ──→ ScenarioContext.get, ScenarioContext.update(...)
  ├── Step 3 body runs ──→ ScenarioContext.get
  │
  ▼
Scenario ends — FiberRef[S] is discarded
```

Each scenario gets its own `FiberRef[S]`. The value starts at `Default[S].default`. Steps
mutate it via `ScenarioContext.update(f: S => S)` and read it via `ScenarioContext.get`.

Because `FiberRef` is fiber-local, parallel scenario fibers cannot accidentally read or write
each other's state. There is no shared mutable variable; there is no explicit lock.

### ScenarioContext in code

```scala
// Read state
ScenarioContext.get                        // ZIO[State[S], Nothing, S]

// Update state
ScenarioContext.update(_.copy(id = newId)) // ZIO[State[S], Nothing, Unit]
```

Both methods require `State[S]` in the ZIO environment. The framework provides this layer
automatically; you do not wire it in your suite.

### Scope in step bodies

Every step body runs inside a `ZIO.scoped` block that covers the entire scenario. This means
step bodies can acquire scoped resources:

```scala
Given("a temporary upload bucket exists") {
  ZIO.acquireRelease(
    S3.createBucket(testBucketName)
  )(_ => S3.deleteBucket(testBucketName).orDie).unit
}
```

The bucket is deleted when the scenario finishes, regardless of whether later steps pass or
fail. `Scope` does not appear in `StepEffect` but it is available in the runtime environment.

---

## 4. The execution phases in detail

```
parse → register → filter → expand → execute
```

| Phase     | Who drives it       | Input                  | Output                     |
|-----------|---------------------|------------------------|----------------------------|
| parse     | ZIOBDDFramework     | `.feature` file paths  | `List[Feature]`            |
| register  | object initialiser  | `Given`/`When`/`Then` calls | `List[StepDef[R,S]]`  |
| filter    | ZIOBDDFramework     | `List[Feature]`, tags  | `List[Feature]` (tagged)   |
| expand    | FeatureExecutor     | `List[Scenario]`       | `List[(Scenario, Flags)]`  |
| execute   | ScenarioExecutor    | scenario + steps + env | `List[ScenarioResult]`     |

The `register` phase happens at JVM class-loading time, before `parse` is called. This means
you cannot conditionally register steps based on runtime data.

---

## 5. How `@Suite` wires everything

`@Suite` is a Java annotation read by the sbt test framework runner (`ZIOBDDTask`) at test
execution time. It tells the runner:

```scala
@Suite(
  featureDirs = Array("src/test/resources/features"), // where to find .feature files
  reporters   = Array("pretty", "junitxml"),           // output formats
  parallelism = 1,                                     // max concurrent features
  excludeTags = Array("ignore"),                       // scenarios to skip
  logLevel    = "info"                                 // step-level log verbosity
)
object MySuite extends ZIOSteps[MyEnv, MyState]
```

The runner:
1. Instantiates the suite object (via `MODULE$` reflection — only `object` is supported, not `class`).
2. Reads `@Suite` values and merges them with any CLI arguments (CLI takes precedence).
3. Discovers `.feature` files from `featureDirs`.
4. Calls `suite.run(features, ...)` which chains `FeatureExecutor.executeFeatures`.
5. Passes results to the reporters and converts them to sbt `Event` values for the build output.

---

## 6. The three-tier environment model

Resources differ in how often they need to be created. A connection pool should be created
once; a per-feature schema should be created per feature; a per-scenario mock should be
fresh each time. The three-tier model maps this directly:

```
┌──────────────────────────────────────────────────────────────────────┐
│ globalLayer         — built once per JVM process                     │
│   e.g. shared connection pool, embedded server                       │
│                                                                      │
│   ┌──────────────────────────────────────────────────────────────┐   │
│   │ featureLayer     — built once per feature file execution     │   │
│   │   e.g. fresh database schema, per-feature mock server setup  │   │
│   │                                                              │   │
│   │   ┌──────────────────────────────────────────────────────┐   │   │
│   │   │ scenarioLayer(meta) — built per scenario             │   │   │
│   │   │   e.g. per-scenario HTTP client, mock overrides      │   │   │
│   │   │                                                      │   │   │
│   │   │   flagLayer(meta, flags) — variant of scenarioLayer  │   │   │
│   │   │   used when @flags(k=v) matrix expansion is active   │   │   │
│   │   └──────────────────────────────────────────────────────┘   │   │
│   └──────────────────────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────────┘
```

### Default behaviour

If you only override `environment`, all three tiers delegate to it:
- `globalLayer` delegates to `featureLayer`
- `featureLayer` delegates to `environment`
- `scenarioLayer` delegates to `featureLayer`
- `flagLayer` delegates to `scenarioLayer`

### Selective override

Override only the tiers you need:

```scala
// Expensive pool: create once for the whole run
override def globalLayer =
  DatabasePool.layer

// Fresh schema per feature: cheaper than pool, but isolated
override def featureLayer =
  globalLayer >>> FreshSchema.layer

// Per-scenario override based on tags
override def scenarioLayer(meta: ScenarioMetadata) =
  if meta.tags.contains("mock-downstream") then featureLayer >>> MockDownstream.layer
  else featureLayer

// Flag-driven configuration override
override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]) =
  featureLayer >>> OpenFeatureOverride.layer(flags)
```

`ScenarioMetadata` contains the scenario name, its tags, the source file, and the line number.
It also carries `flagValues` — the parsed `@flags(...)` map for the current expansion run.

---

## Further reading

- [quickstart.md](quickstart.md) — working example from scratch
- [step-dsl.md](step-dsl.md) — full extractor reference and DSL patterns
- [state.md](state.md) — TypeMap, HasLens, Stage, FeatureContext, withSnapshot
- [layers.md](layers.md) — HasService, flag matrix, three-tier layer model
- [running.md](running.md) — CLI flags, reporters, parallelism options
