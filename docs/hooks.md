# Hooks Reference

Hooks let you run ZIO effects at specific points in the test lifecycle — before and after
suites, features, scenarios, and individual steps.  All hooks are registered by calling
the hook methods inside the body of your `ZIOSteps` object or any mixed-in trait.

---

## Hook methods

| Method | Trigger | Effect type | Context |
|--------|---------|-------------|---------|
| `beforeAll { ... }` | Once before all features start | `URIO[R, Unit]` | none |
| `afterAll { ... }` | Once after all features complete | `URIO[R, Unit]` | none |
| `beforeFeature { ... }` | Before each `Feature` begins | `URIO[R, Unit]` | none |
| `afterFeature { ... }` | After each `Feature` completes | `URIO[R, Unit]` | none |
| `beforeScenario { ... }` | Before each scenario | `URIO[R & State[S], Unit]` | none |
| `beforeScenario { meta => ... }` | Before each scenario (with metadata) | `URIO[R & State[S], Unit]` | `ScenarioMetadata` |
| `afterScenario { ... }` | After each scenario | `URIO[R & State[S], Unit]` | none |
| `afterScenario { meta => ... }` | After each scenario (with metadata) | `URIO[R & State[S], Unit]` | `ScenarioMetadata` |
| `beforeScenarioTagged("tag") { meta => ... }` | Before scenarios carrying `@tag` | `URIO[R & State[S], Unit]` | `ScenarioMetadata` |
| `afterScenarioTagged("tag") { meta => ... }` | After scenarios carrying `@tag` | `URIO[R & State[S], Unit]` | `ScenarioMetadata` |
| `beforeStep { meta => ... }` | Before each step | `URIO[R & State[S], Unit]` | `StepMetadata` |
| `afterStep { meta => ... }` | After each step | `URIO[R & State[S], Unit]` | `StepMetadata` |

Multiple calls to the same hook method **compose** — they do not replace one another.  All
registered hooks run in registration order.

---

## Registering hooks

Hooks are registered in the body of your `ZIOSteps` object, exactly like steps.  They can
also be placed in a separate mixin trait using the self-type constraint:

```scala
// Inline in the suite object
@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[AppEnv, AppState]:

  beforeAll   { ZIO.logInfo("Suite starting") }
  beforeScenario { meta => ZIO.logInfo(s"▶ ${meta.name}") }
  afterScenario  { meta => ZIO.logInfo(s"✓ ${meta.name}") }
  afterAll    { ZIO.logInfo("Suite done") }

  // ... step registrations
```

```scala
// In a shared mixin trait
trait SuiteHooks { self: ZIOSteps[AppEnv, AppState] =>

  beforeAll {
    ZIO.serviceWithZIO[MetricsClient](_.register("test.suite.started"))
  }

  afterAll {
    ZIO.serviceWithZIO[MetricsClient](_.flush())
  }

  beforeScenario { meta =>
    ZIO.logInfo(s"Running: ${meta.name}  tags=${meta.tags.mkString(",")}")
  }
}

object MySuite
    extends ZIOSteps[AppEnv, AppState]
    with SuiteHooks
    with MyStepTraits:
  override def environment = AppEnv.layer
```

---

## `ScenarioMetadata`

Scenario-level hooks that accept a `meta` argument receive a `ScenarioMetadata`:

```scala
case class ScenarioMetadata(
  name:       String,             // Scenario name from the feature file
  tags:       List[String],       // All tags on the scenario (e.g. List("smoke", "regression"))
  file:       Option[String],     // Path to the .feature file
  line:       Option[Int],        // Line number of the Scenario: keyword
  flagValues: Map[String, String] // Populated when @flags(...) expansion is active
)
```

`flagValues` is `Map.empty` for scenarios without `@flags(...)` tags.  When a `@flags`
scenario is expanded into multiple runs, each run's hooks receive the correct flag map for
that run — `meta.flagValues.get("rateLimiting")` gives the current run's value.

---

## `StepMetadata`

Step-level hooks receive a `StepMetadata`:

```scala
case class StepMetadata(
  pattern:  String,    // The step expression text, e.g. "a user named (.+)"
  stepType: StepType,  // GivenStep | WhenStep | ThenStep | AndStep | ButStep
  file:     Option[String],
  line:     Option[Int]
)
```

---

## Accessing scenario state in hooks

`beforeScenario` and `afterScenario` hooks have `State[S]` in their ZIO environment.  This
means they can read and write scenario state using the same `ScenarioContext` API as step bodies:

```scala
beforeScenario { _ =>
  // Initialise a counter in state before the first step runs
  ScenarioContext.update(_.copy(requestCount = 0))
}

afterScenario { meta =>
  // Read state after all steps have run
  ScenarioContext.get.flatMap { s =>
    ZIO.logInfo(s"${meta.name}: ${s.requestCount} requests made")
  }
}
```

`beforeAll` and `afterAll` hooks do **not** have `State[S]` available — they run outside the
scenario lifecycle.  Use `R` services for suite-level initialisation.

---

## Conditional hooks with `beforeScenarioTagged`

Use `beforeScenarioTagged` / `afterScenarioTagged` to run a hook only for scenarios that carry
a specific tag.  This is useful for tag-driven environment setup or cleanup:

```scala
// Only runs for scenarios tagged @integration
beforeScenarioTagged("integration") { meta =>
  ZIO.serviceWithZIO[Database](_.seed(testFixtures))
}

// Only runs for @slow scenarios — log a warning
beforeScenarioTagged("slow") { meta =>
  ZIO.logWarning(s"Slow scenario started: ${meta.name}")
}

afterScenarioTagged("integration") { meta =>
  ZIO.serviceWithZIO[Database](_.reset())
}
```

The tag comparison is case-sensitive and matches the raw tag string without the `@` prefix.

---

## Lifecycle order

The full execution order for a test run is:

```
beforeAll
  ┌─ beforeFeature
  │   ┌─ beforeScenario
  │   │   ┌─ beforeStep
  │   │   │   (step body)
  │   │   └─ afterStep
  │   │   ... (repeat per step)
  │   └─ afterScenario
  │   ... (repeat per scenario)
  └─ afterFeature
afterAll
```

Within each tier, hooks registered earlier run before hooks registered later.

**Key ordering notes:**
- `beforeScenario` fires **before** any step in the scenario runs.  State (`S`) is already
  initialised to `Default[S].default` when `beforeScenario` fires.
- `afterScenario` fires **after** all steps have run, regardless of whether any step failed.
  The state still contains whatever the last step left behind.
- `beforeStep` / `afterStep` fire around **each individual step**, including Background steps
  that are prepended to a scenario.
- `afterAll` runs after all features have completed, provided nothing upstream died. Unlike
  `afterScenario`/`afterFeature`, the `beforeAll → features → afterAll` sequence is a plain
  for-comprehension with no `.ensuring` guard — a defect in `beforeAll` or an unrecovered
  feature-level defect skips `afterAll`.

---

## `afterStep` execution guarantees

`afterStep` always runs after the step body, whether the step passed or failed:

- If the step body **passed** and `afterStep` **fails**, the step is flipped to **failed** —
  `afterStep` can turn a passing step into a failing one.
- If the step body **failed**, the step keeps its **original failure cause** even if
  `afterStep` also fails; `afterStep`'s outcome does not overwrite an existing failure.
- If `beforeStep` fails, neither the step body nor `afterStep` runs — the step is recorded as
  failed from the `beforeStep` cause alone.

## Hook error propagation

Every hook type is a `URIO` (see [`Hooks.scala`](../core/src/main/scala/zio/bdd/core/Hooks.scala))
— hooks have no typed error channel, so a failure only reaches the test run as a **defect**.
How that defect is handled differs by hook:

| Hook | On failure | Guarded by `.ensuring`? |
|------|------------|--------------------------|
| `beforeScenario` | Recorded as the scenario's `setupError`; steps are skipped | No |
| `afterScenario` | Fails the scenario (even if all steps passed) | Yes — always runs |
| `beforeFeature` | Propagates as a defect, skipping the feature's scenarios | No |
| `afterFeature` | Propagates as a defect after teardown | Yes — always runs |
| `beforeAll` | Propagates as a defect, skipping all features and `afterAll` | No |
| `afterAll` | Propagates as a defect | No |

A genuine interruption (fiber cancellation / shutdown), as opposed to a hook failure, is never
treated as a retryable error — an interrupt-only `Cause` always propagates rather than being
recorded as a setup/step failure (see #231).

---

## Common patterns

### Log a progress indicator per scenario

```scala
beforeScenario { meta =>
  ZIO.logInfo(s"▶ ${meta.name}")
}

afterScenario { meta =>
  ZIO.logInfo(s"✓ ${meta.name} (${meta.tags.mkString(",")})")
}
```

### Reset a shared cache before each scenario

```scala
beforeScenario { _ =>
  ZIO.serviceWithZIO[TestCache](_.clear())
}
```

### Seed test data only for tagged scenarios

```scala
beforeScenarioTagged("needs-db-seed") { _ =>
  ZIO.serviceWithZIO[TestDatabase](_.seed(defaultFixtures))
}

afterScenarioTagged("needs-db-seed") { _ =>
  ZIO.serviceWithZIO[TestDatabase](_.truncate())
}
```

### Initialise a resource shared across all scenarios in a feature

`beforeFeature` runs once per feature.  It cannot write to `State[S]` (which is per-scenario),
but it can write to `FeatureContext`:

```scala
beforeFeature {
  for {
    id <- ZIO.serviceWithZIO[AccountService](_.provisionTestAccount())
    _  <- FeatureContext.put(TestAccountId(id))
  } yield ()
}

afterFeature {
  for {
    id <- FeatureContext.getOption[TestAccountId]
    _  <- ZIO.foreachDiscard(id)(a => ZIO.serviceWithZIO[AccountService](_.delete(a.value)))
  } yield ()
}
```

### Access flag values in a hook

```scala
beforeScenario { meta =>
  ZIO.when(meta.flagValues.get("realDownstream").contains("true")) {
    ZIO.serviceWithZIO[DownstreamService](_.warmUp())
  }.unit
}
```

---

## Anti-patterns

### Don't do expensive setup in `beforeScenario` when it only needs to happen once

If a resource takes seconds to create (a connection pool, an embedded server), put it in
`globalLayer` rather than recreating it in `beforeScenario`.  `beforeScenario` runs before
**every** scenario.

### Don't use hooks to pass data between scenarios

Hooks have access to `State[S]` (per-scenario) and `R` (suite-wide).  They do not provide
a mechanism for one scenario to leave data for a later scenario.  If you need shared data
within a feature (not across features), use `FeatureContext` (see [state.md](state.md)).

### Don't swallow errors in `afterScenario`

`afterScenario` hooks should use `ZIO.logWarning` or `.orDie` / `.ignore` on cleanup effects
rather than letting them fail silently.  A hook failure marks the scenario as failed even if
all steps passed:

```scala
// Bad — unhandled failure in afterScenario can fail an otherwise-passing scenario
afterScenario { _ => cleanupResource() }

// Good — log the cleanup failure without propagating
afterScenario { _ => cleanupResource().catchAll(e => ZIO.logWarning(s"cleanup failed: $e")) }
```

### Don't call `pending()` inside a hook

`pending()` returns `ZIO[Any, Throwable, Unit]` and is meant for step bodies only — it fails
with a `PendingException` that the step executor specifically recognizes and renders as
"pending" rather than "failed". Hooks are `URIO[R, Unit]` (no error channel), so they cannot
run a `Task`-typed effect like `pending()` at all; the call won't type-check inside a
`beforeScenario`/`afterStep`/etc. body.
