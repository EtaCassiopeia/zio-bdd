# Troubleshooting

Common compiler errors, startup failures, and runtime issues — with root cause and fix.

---

## Compiler errors

### "value Given is not a member of MyStepTrait"

**Cause:** Step methods (`Given`, `When`, `Then`, etc.) are inherited from `ZIOSteps`.
A step trait that does not declare a self-type cannot call them.

**Fix:** Add the self-type constraint to the trait:

```scala
// Wrong
trait ProvisionSteps {
  Given("a provision request is prepared") { ... }  // compile error
}

// Correct
trait ProvisionSteps { self: ZIOSteps[AppEnv, AppState] =>
  Given("a provision request is prepared") { ... }
}
```

---

### "type mismatch: found ZIO[AppEnv & State[AppState] …], required ZIO[OtherEnv & State[OtherState] …]"

**Cause:** A step trait's self-type uses different `R` or `S` type parameters than the suite
object it is mixed into.

**Fix:** Ensure every step trait uses the same `R` and `S` as the suite:

```scala
// Suite type parameters
object MySuite extends ZIOSteps[AppEnv, AppState]
    with ProvisionSteps    // must use ZIOSteps[AppEnv, AppState]
    with PostSteps         // must use ZIOSteps[AppEnv, AppState]

// Correct trait self-type
trait ProvisionSteps { self: ZIOSteps[AppEnv, AppState] => ... }
```

---

### "Cannot derive Default for `<schema class>`: field 'id' has no default value"

```
java.lang.IllegalStateException: Cannot derive Default for <some Schema-derived class name>: field 'id' has no default value
Hint: add default values to all fields of your state case class, or provide an explicit Default[T] instance.
```

**Cause:** `ZIOSteps[R, S]` requires a `Default[S]` instance.  When `Default[S]` is derived
automatically from `Schema[S]`, every field must have a default value.  The message interpolates
`schema.getClass.getSimpleName` — the runtime class name of the derived `Schema[S]` instance, not
`S`'s own name — so it will **not** literally say `AppState`; grep for `Cannot derive Default for`
instead of the state type name.

**Fix:** Add default values to all fields of `S`:

```scala
// Wrong
case class AppState(id: String, statusCode: Int)

// Correct
case class AppState(id: String = "", statusCode: Int = 0)

given Schema[AppState] = DeriveSchema.gen[AppState]
// Default[AppState] is now derivable automatically
```

Or provide `Default[S]` manually:

```scala
given Default[AppState] = Default.from(AppState(id = "none", statusCode = 0))
```

---

### "No given instance found for HasLens[AppState, CoreState]"

**Cause:** `ScenarioLens.update[AppState, CoreState]` or `ScenarioLens.get[AppState, CoreState]`
requires a `given HasLens[AppState, CoreState]` in implicit scope.

**Fix:** Define the lens in the companion object or in the step trait's scope:

```scala
given HasLens[AppState, CoreState] =
  HasLens(_.core, (s, a) => s.copy(core = a))
```

Place it in the `AppState` companion object so all step traits pick it up without an explicit import.

---

### "Ambiguous implicit values" with `TypeMap`

**Cause:** Two modules declared `given Tag[Data]` where both `Data` types have the same fully
qualified name (e.g. two nested objects both named `Data`).

**Fix:** Give each module's slice type a unique name.  Prefer a meaningful name:

```scala
// Instead of `Data` everywhere:
object ProvisionCtx:
  final case class ProvisionData(arid: String = "")
  given Tag[ProvisionData] = Tag[ProvisionData]

object PostCtx:
  final case class PostData(correlationId: String = "")
  given Tag[PostData] = Tag[PostData]
```

---

### Step body inferred as `ZIO[Any, Nothing, Unit]` but expected `ZIO[R & State[S], …]`

**Cause:** `ZIO.succeed(sideEffect())` was used instead of `ZIO.attempt(sideEffect())`.
`ZIO.succeed` is for pure (already-computed) values; it does not lift side effects.

**Fix:**

```scala
// Wrong — side effect runs eagerly at ZIO.succeed call site
Given("the cache is cleared") { ZIO.succeed(cache.clear()) }

// Correct — side effect deferred inside ZIO.attempt, runs when fiber executes the step
Given("the cache is cleared") { ZIO.attempt(cache.clear()).orDie }
```

---

## Suite startup failures

### "Ambiguous step definitions detected at suite startup"

```
Ambiguous step definitions detected at suite startup.
The following step expressions are registered multiple times:
  GivenStep 'a valid provision request' — registered 2 times
```

**Cause:** Two step definitions with identical text and keyword were registered.  This often
happens when two traits define the same step, or when a step is accidentally registered twice
in the same trait.

**Fix:** Rename or merge the duplicate.  The error is thrown before any scenario runs.

---

### "No matching step found for …"

```
FAILED: Given a valid provision request dated -7 days
  No matching step found for Given 'a valid provision request dated -7 days'.

  Implement it as:

    Given("a valid provision request dated -7 days") { ZIO.unit }
```

**Cause:** The Gherkin step text does not match any registered step expression.  Common reasons:

1. A literal part of the step expression is spelled differently in the feature file.
2. The feature file uses `And` or `But` but the definition is registered under `Given` — this
   should not cause a mismatch (cross-keyword matching is automatic), so check spelling first.
3. The step was registered in a trait that is not mixed into the suite object.
4. The step definition uses an extractor pattern that does not match the actual text.

**Fix:** Run `sbt "testOnly MySuite -- --dry-run"` to see all unmatched steps at once.
If you're working inside this repository (or have vendored `SnippetGeneratorPlugin.scala`
into your own `project/` — see `docs/running.md`'s "zioBddSnippets task" for why it isn't
available otherwise), `sbt zioBddSnippets` prints a skeleton for each unmatched step. With the
[zio-bdd-tooling](https://github.com/EtaCassiopeia/zio-bdd-tooling) LSP/editor extensions
installed, the same skeleton appears live as a completion item the moment you start typing
inside a `Given`/`When`/`Then(...)` call.

---

### Suite object not discovered

```
[info] No tests were executed.
```

**Cause:** The sbt test framework is not registered, the `@Suite` annotation is missing, or
the object is a `class` instead of a Scala `object`.

**Fix:**

```scala
// build.sbt — register the framework
testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")

// Suite must be an `object`, not a class
@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[AppEnv, AppState]   // ← object, not class
```

---

## Runtime failures

### "StagingError.NotFound: No staged value of type Order"

```
Failed: When the order is submitted
  No staged value of type Order. Call Stage.put before Stage.get.
```

**Cause:** `Stage.get[Order]` was called but no prior step called `Stage.put(order)`.  This
can happen when:

1. The `Given` step that was supposed to call `Stage.put` did not run (check tag filters,
   scenario structure, or dry-run output).
2. The type passed to `Stage.put` differs from the type passed to `Stage.get` (e.g. `Stage.put`
   stores an `ProvisionEvent` but `Stage.get[PostEvent]` is called).

`StagingError` is a sealed trait, not a `Throwable` — it cannot surface directly as an exception
from a step body.  A step that wants to fail the scenario on a missing value must map it
explicitly, e.g. `Stage.get[Order].mapError(e => new RuntimeException(e.message))`.

**Fix:** Use `Stage.getOrElse(defaultValue)` if the missing value should fall back gracefully,
or `Stage.getOption[T]` to handle absence explicitly:

```scala
When("the order is submitted") {
  Stage.getOption[Order].flatMap {
    case Some(order) => submitOrder(order)
    case None        => ZIO.fail(new RuntimeException("Expected staged Order, but none was found"))
  }
}
```

---

### State is empty in a `Then` step

```
AssertionError: Expected accountRefId to be non-empty, got ""
```

**Cause:** The `Given` or `When` step that was supposed to write to `ScenarioContext` either:

1. Used `ScenarioContext.update` but called it on a wrong field (e.g. `copy(accountRefId = ...)` on
   a nested type without `HasLens`, so the outer state was not updated).
2. Called `ScenarioContext.update` inside a forked fiber — state changes in forked fibers do
   not propagate back to the parent (this is `FiberRef` semantics).

**Fix for nested copy:** Use `ScenarioLens.update` with a defined `HasLens`:

```scala
// Wrong — updates a copy of `core` but doesn't write it back into `s`
ScenarioContext.update { s =>
  val newCore = s.core.copy(accountRefId = id)
  s  // oops — returning unchanged `s`
}

// Correct — lens writes the updated slice back into the outer state
ScenarioLens.update[AppState, CoreState](_.copy(accountRefId = id))
```

**Fix for forked fibers:** Avoid forking fibers for work that must update state.  If
parallelism is needed, collect results and update state in the main fiber:

```scala
// Correct — update state in the main fiber after parallel work
for {
  results <- ZIO.collectAllPar(requests.map(sendRequest))
  _       <- ScenarioContext.update(_.copy(responses = results))
} yield ()
```

---

### "FeatureContextError.NotFound: No feature-scoped value of type TestAccountId"

```
No feature-scoped value of type TestAccountId. Call FeatureContext.put before FeatureContext.get.
```

**Cause:** `FeatureContext.get[TestAccountId]` was called in a scenario, but no earlier step
called `FeatureContext.put(TestAccountId(...))` in the same feature.

This often happens when the `Given` step that populates `FeatureContext` is in a `Background`
block that runs before each scenario — but the first scenario fails before reaching
`FeatureContext.put`, and later scenarios also fail when they try to read it.

`FeatureContextError` is a sealed trait, not a `Throwable` — it cannot surface directly as an
exception from a step body.  A step that wants to fail the scenario on a missing value must map
it explicitly, e.g. `FeatureContext.get[TestAccountId].mapError(e => new RuntimeException(e.message))`.

**Fix:** Use `FeatureContext.getOrElse` or `FeatureContext.getOption` to handle a missing value,
or ensure the `Background` step populates the context before other steps depend on it.

---

### Steps time out

```
TIMEOUT after 30s: When the request is sent
```

**Cause:** The step body took longer than the configured `stepTimeout`.

**Fix options:**

1. Increase the timeout for the specific step:
   ```scala
   When("the slow operation completes") {
     slowEffect.timeout(5.minutes)
       .someOrFail(new RuntimeException("operation timed out after 5 minutes"))
   }
   ```

2. Increase the suite-level timeout:
   ```scala
   @Suite(stepTimeout = 120)  // 120 seconds
   object MySuite extends ZIOSteps[AppEnv, AppState]
   ```

3. If the service is hanging rather than slow, investigate the underlying call —
   a missing connection timeout on an HTTP client, or a blocking call, is usually the root cause.

---

### Reporter produces no output

**Cause:** The reporter name in `@Suite(reporters = ...)` is misspelled.  Unknown reporter
names fall back to the pretty reporter with a warning — `Unknown reporter '<name>', defaulting
to ConsoleReporter` — but the warning may be lost in build output.

**Fix:** Use exactly `"pretty"` or `"junitxml"`:

```scala
@Suite(reporters = Array("pretty", "junitxml"))
```

---

## Frequently asked questions

### Q: Can I run a single scenario by name?

Yes — use the `--scenario-name` CLI flag with a glob pattern:

```sh
sbt "testOnly MySuite -- --scenario-name 'Provision an account'"
sbt "testOnly MySuite -- --scenario-name 'Provision*'"
```

### Q: Why do I see `[SKIPPED]` on steps after a failure?

This is by design.  When a step in a scenario fails, all subsequent steps in the same
scenario are skipped.  The prior steps still appear in the report; only the failing step
and all subsequent steps are marked.  Skipped steps are **not** failures — they do not
cause the build to fail on their own.

### Q: How do I mark a step as not-yet-implemented without failing the build?

Use `pending(reason)`:

```scala
Given("a complex precondition exists") { pending("TODO: implement in sprint 3") }
```

A pending step is reported as `PENDING` (orange) — distinct from failed or skipped.
A scenario with pending steps is counted separately in the summary and does not fail the build.

### Q: How do I share state between scenarios?

Short answer: don't.  Scenarios are designed to be independent.

If you need a resource provisioned once and used by multiple scenarios within a **feature**
(e.g. one account for all POST tests in a file), use `FeatureContext`.  See
[cookbook.md §7](cookbook.md#7-sharing-setup-across-scenarios-in-a-feature).

For suite-wide shared state (e.g. a shared HTTP client), put it in `R` via `globalLayer`.

### Q: Can I use zio-test assertions inside step bodies?

zio-bdd provides its own assertion helpers in `zio.bdd.core.Assertions`:
`assertTrue`, `assertEquals`, `assertSome`, `assertNone`, `assertThrows`, `collectAll`.

These are plain ZIO effects that throw `AssertionError` on failure — compatible with the
zio-bdd step execution model.  Do not use `zio-test`'s `assertTrue` / `assert` macros
inside step bodies; they integrate with `zio-test`'s `Spec` type, not with zio-bdd's result model.

### Q: Why does the test output not appear until all tests finish?

The `pretty` reporter is a batch reporter — it waits until all features complete, then
renders the full tree.  For live output during CI runs, use `LiveProgressReporter` instead.
See [reporters.md](reporters.md).

### Q: How do I load feature files from multiple directories?

Use `featureDirs` (plural) in `@Suite`:

```scala
@Suite(
  featureDirs = Array(
    "src/test/resources/features/provision",
    "src/test/resources/features/post"
  )
)
object MySuite extends ZIOSteps[AppEnv, AppState]
```

Feature files packaged in a test-jar from another module can be loaded with the `classpath:`
prefix:

```scala
@Suite(featureDirs = Array("classpath:features/provision"))
```
