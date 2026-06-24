# Property-Based Testing

zio-bdd integrates property-based testing directly into Gherkin.  Add a `@property(...)` tag to
a header-only `Examples:` block and the scenario runs N times with sampled values instead of
literal rows — using ZIO Test's `Gen[Any, A]` engine under the hood.

The counterexample output, failure replay, and JUnit XML format are all handled automatically.
Existing step definitions require no changes.

---

## Minimal example

```gherkin
Feature: Account invariants

  Scenario Outline: Withdrawal never produces a negative balance
    Given an account with balance <balance> and limit <limit>
    When I withdraw <amount>
    Then the resulting balance is at least negative <limit>

    @property(samples=500, seed=42, shrink=true)
    Examples:
      | balance | limit | amount |
```

```scala
import zio.bdd.core.property.{ColumnGenLookup, HasGen}
import zio.test.Gen

@Suite(featureDirs = Array("src/test/resources/features"), reporters = Array("pretty", "junitxml"))
object AccountSpec extends ZIOSteps[AccountService, AccountState]:

  // Register generators for the placeholder types. `HasGen` is a plain trait (not a
  // function type), so a `given` instance needs a `with` body — not `= <gen value>`.
  given HasGen[Money] with
    def gen = Gen.double(0, 10_000).map(Money.apply)

  given HasGen[Limit] with
    def gen = Gen.long(0, 5_000).map(Limit.apply)

  // One-time registration makes Money/Limit resolve automatically by type for every
  // column of that type — no per-column columnGenLookup entry needed below.
  HasGen.registerType(HasGen[Money])
  HasGen.registerType(HasGen[Limit])

  Given("an account with balance " / money / " and limit " / limit) { (bal, lim) =>
    ScenarioContext.update(_.copy(balance = bal, limit = lim))
  }
  When("I withdraw " / money) { (amount: Money) => ... }
  Then("the resulting balance is at least negative " / limit) { (lim: Limit) => ... }

  // columnGenLookup is now the override path, not mandatory — every column above already
  // resolves automatically via registerType. Only needed for a narrower/different
  // generator than the automatic choice, e.g. constraining `amount` to smaller values:
  override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
    def byColumn(col: String): Option[HasGen[?]] = col match
      case "amount" => Some(new HasGen[Money] { def gen = Gen.double(0, 100).map(Money.apply) })
      case _        => None
```

On failure the pretty reporter shows:

```
╰─ ✗ Withdrawal never produces a negative balance [seed=42]
      ├─ ⚡ [counterexample] balance=0.01 (HasGen[Money]), limit=0 (HasGen[Limit]), amount=100.0 (HasGen[Money])
      │     ──────────────────────────────────────────────────────────────────
      │   │ balance │ 0.01  (HasGen[Money])
      │   │ limit   │ 0     (HasGen[Limit])
      │   │ amount  │ 100.0 (HasGen[Money])
      │     ──────────────────────────────────────────────────────────────────
      ├─ ✓ Given an account with balance 0.01 and limit 0
      ├─ ✓ When I withdraw 100.0
      ╰─ ✗ Then the resulting balance is at least negative 0
            Expected balance ≥ -0, got -99.99
```

---

## Tag placement

`@property(...)` can be placed on:

1. **The `Examples:` block** (preferred when the property is specific to that block):

   ```gherkin
   Scenario Outline: My invariant
     Given <x>
     Then ok
     @property(samples=100)
     Examples:
       | x |
   ```

2. **The `Scenario Outline:` line** (convenient when there is only one Examples block):

   ```gherkin
   @property(samples=100)
   Scenario Outline: My invariant
     Given <x>
     Then ok
     Examples:
       | x |
   ```

Both placements produce identical behaviour.  `combinedTags` (scenario tags + block tags) is used
for detection, so either location works.

---

## Tag arguments

| Argument | Default | Description |
|---|---|---|
| `samples` | `100` | Number of generated samples to run |
| `seed` | random | Fixed `Long` seed for reproducible runs. Always logged so any failure carries a reproducer. |
| `shrink` | `true` | Walk the ZIO Test shrink tree on failure to find a minimal counterexample |
| `maxShrinks` | `1000` | Cap on shrink-tree steps |
| `maxDiscarded` | `samples × 5` | Reserved for a future `Assume` step; not yet active |
| `verbose` | `false` | Print full shrink path in failure output |
| `replay` | `true` | Consult and update the failure replay file (see [Failure replay](#failure-replay)) |

A bare `@property` with no arguments is valid and equivalent to `@property(samples=100)`.

---

## `HasGen[T]` — generator registry

`HasGen[A]` is a typeclass defined in `zio.bdd.core.property` that provides a
`Gen[Any, A]` for a given type `A`.  It is the bridge between Gherkin column names and
ZIO Test's generator engine.

You do not need to know ZIO Test to use property testing in zio-bdd, but understanding
`Gen` helps you write richer generators.

---

### What is `Gen`?

`Gen[R, A]` (from `zio.test`) is a generator that produces values of type `A` using
environment `R`.  Most generators use `Gen[Any, A]` — no special environment needed.

Common constructors:

```scala
import zio.test.Gen

Gen.int                          // random Int (full range)
Gen.int(1, 100)                  // random Int between 1 and 100 (inclusive)
Gen.long(0L, 1_000_000L)         // random Long in range
Gen.double(0.01, 9_999.99)       // random Double in range
Gen.boolean                      // random Boolean
Gen.alphaNumericString           // random non-empty alphanumeric String
Gen.stringBounded(1, 10)(Gen.alphaChar)  // String of 1–10 alpha chars
Gen.uuid                         // random UUID
Gen.elements("A", "B", "C")      // random pick from a fixed list
Gen.oneOf(gen1, gen2)            // random pick from multiple generators
Gen.option(Gen.int)              // Some(Int) or None
Gen.listOf(Gen.int)              // List of random Ints (variable length)
Gen.const(42)                    // always produces 42
```

Generators compose with `map`, `flatMap`, and for-comprehensions:

```scala
// A generator for a domain type built from multiple fields
val orderGen: Gen[Any, Order] =
  for
    id     <- Gen.uuid
    amount <- Gen.double(0.01, 10_000.0)
    status <- Gen.elements(OrderStatus.Pending, OrderStatus.Completed)
  yield Order(id, amount, status)
```

---

### Built-in `HasGen` instances

`HasGen` instances are already provided for these types — no `given` needed to define
them:

| Type | Generator used |
|---|---|
| `Int` | `Gen.int` |
| `Long` | `Gen.long` |
| `Double` | `Gen.double` |
| `Boolean` | `Gen.boolean` |
| `String` | `Gen.alphaNumericString` |
| `UUID` | `Gen.uuid` |

**A built-in-typed column needs no `columnGenLookup` entry at all.** The executor infers
a column's type from the `TypedExtractor` that governs it in your step definitions (e.g.
`/ int`), then resolves that type automatically against `HasGen`'s built-in registry — no
mechanism existed for this before; now it does. `columnGenLookup` is the *override* path:
reach for it when you want a narrower or different generator than the automatic choice,
or to resolve an ambiguity (see [Wiring column resolution](#wiring-column-resolution-via-columngenlookup)
below). A column with no explicit override and a built-in extractor type just works:

```gherkin
Given a value of <retries>
@property(samples=100)
Examples:
  | retries |
```

```scala
// No columnGenLookup entry needed for `retries` — it resolves automatically from the
// `int` extractor in `Given("a value of " / int)`.
```

---

### Providing `HasGen` for a domain type

For every domain type used as a placeholder, provide a `given HasGen[YourType]` in your
suite object. `HasGen[A]` is a plain trait with one abstract member (`def gen: Gen[Any, A]`),
not a function type — so a `given` instance needs a `with` body, not `given HasGen[X] = <value>`:

```scala
import zio.bdd.core.property.HasGen
import zio.test.Gen

object MySpec extends ZIOSteps[MyService, MyState]:

  // Simple range-bounded value type
  given HasGen[Money] with
    def gen = Gen.double(0.01, 10_000.0).map(Money.apply)

  // Enum — pick uniformly from the valid members
  given HasGen[Currency] with
    def gen = Gen.elements(Currency.GBP, Currency.USD, Currency.EUR)

  // Case class with multiple fields
  given HasGen[Address] with
    def gen =
      for
        street <- Gen.alphaNumericString
        city   <- Gen.elements("London", "New York", "Tokyo")
      yield Address(street, city)
```

The `given` must be in scope at the point where `columnGenLookup` resolves the type.
Placing it directly in the suite object (or in a trait mixed into the suite) is the
idiomatic location.

**Override `label` for readable counterexamples.** The default `label` falls back to
`gen.getClass.getSimpleName`, which is often an uninformative anonymous-class name (e.g.
`HasGen[Gen]`) for a `for`-comprehension or `Gen.elements(...)`-built generator. Override
it explicitly so failure output reads `amount=42 (HasGen[Money])` instead of
`amount=42 (HasGen[Gen])`:

```scala
given HasGen[Money] with
  def gen            = Gen.double(0.01, 10_000.0).map(Money.apply)
  override def label = "HasGen[Money]"
```

#### Using `HasGen.apply` to summon an instance

Once a `given HasGen[A]` is in scope, summon it with `HasGen[A]`. This is the same idiom
used both for an explicit `columnGenLookup` override and for `HasGen.registerType` (the
one-time-per-type registration that makes automatic resolution work — see
[Wiring column resolution](#wiring-column-resolution-via-columngenlookup) below):

```scala
// One-time registration — Money/Currency now resolve automatically by type:
HasGen.registerType(HasGen[Money])
HasGen.registerType(HasGen[Currency])

// Or, as an explicit per-column override in columnGenLookup:
override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
  def byColumn(col: String): Option[HasGen[?]] = col match
    case "amount" | "balance" => Some(HasGen[Money])     // summons the given above
    case "currency"           => Some(HasGen[Currency])
    case _                    => None
```

---

### Named generator overrides

Sometimes you need a different generator for the same type in a specific column — for
example, "small withdrawal amounts" as opposed to the full `HasGen[Money]` range.  Use
`HasGen.named` to register a named generator and reference it in the column header:

**Registration (Scala):**

```scala
// In the suite object initialiser — runs once before tests
HasGen.named("smallAmounts")(Gen.double(0.01, 10.0).map(Money.apply))
HasGen.named("largeLimits")(Gen.long(5_000L, 100_000L).map(Limit.apply))
```

**Reference (Gherkin):**

```gherkin
@property(samples=200)
Examples:
  | balance | limit: largeLimits | amount: smallAmounts |
```

- `balance` resolves via `columnGenLookup` → `HasGen[Money]` (the default generator).
- `amount` resolves via the named registry → `"smallAmounts"` generator.
- `limit` resolves via the named registry → `"largeLimits"` generator.

Named generators are stored in a global registry (`HasGen.namedRegistry`) — they
persist for the JVM lifetime and are shared across suites.  Choose unique, descriptive
names to avoid accidental collision between suites.

---

### Wiring column resolution via `columnGenLookup`

`columnGenLookup` is the *override* path, not a mandatory per-column registry. Most
columns — anything whose extractor produces a built-in type, or a domain type registered
via `HasGen.registerType` (see below) — resolve automatically with no entry here at all.
Override `columnGenLookup` when you want a different generator than the automatic choice
(a narrower range, a domain type that hasn't been registered, or to disambiguate a
conflict):

```scala
import zio.bdd.core.property.{ColumnGenLookup, HasGen}

override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
  def byColumn(col: String): Option[HasGen[?]] = col match
    case "amount" => Some(HasGen[Money])  // Money isn't registered via registerType, so
                                           // it needs an explicit entry (or call registerType instead)
    case _        => None                 // everything else resolves automatically
```

**Resolution order** for a column named `"amount"`:

1. Named override from the column header annotation (`| amount: smallAmounts |`) —
   checked against `HasGen.resolve("smallAmounts")`.
2. `columnGenLookup.byColumn("amount")`.
3. **Automatic type-based lookup:** the executor finds the `Tag[_]` the column's extractor
   produces (from the step definition that governs it) and resolves it via
   `HasGen.resolveByTag` — this is where the six built-ins, and any type registered via
   `HasGen.registerType`, resolve with no further wiring.
4. Setup error — the scenario fails immediately with a clear message naming the column
   and, if a type was inferred, that type: e.g.
   ``Column 'amount' was inferred as Money but no HasGen is registered for that type.
   Register one via `given HasGen[Money] with ...` then `HasGen.registerType(HasGen[Money])`,
   or add an explicit `columnGenLookup` entry for 'amount'.``

**Ambiguity:** if the same column name is used in two different steps and each step's
extractor infers a *different* type, that's reported as a setup error too (not
first-match-wins) — the message names the column and both conflicting types, since no
single generator could be right for both.

---

### Complete setup checklist

For a property scenario with a **built-in-typed** column (`Int`, `String`, `Long`,
`Double`, `Boolean`, `UUID`) to work, there is no checklist — it resolves automatically
from the step's extractor with zero setup.

For a property scenario with a **domain-type** column, pick one of two paths:

- **Register the type once** (works for every column of that type, no per-column wiring):
  - [ ] Define `case class Money(value: Double)` (or whatever your type is).
  - [ ] Add `given HasGen[Money] with { def gen = Gen.double(...).map(Money.apply) }`.
  - [ ] Call `HasGen.registerType(HasGen[Money])` once (e.g. in the suite's object body).
- **Or route it explicitly per column** (useful for a one-off override):
  - [ ] Define the `given HasGen[Money]` as above.
  - [ ] Add `case "amount" => Some(HasGen[Money])` in `columnGenLookup`.

Either way:
- [ ] Ensure the step definition uses the same `TypedExtractor[Money]` it already uses
      for literal scenarios (e.g. `/ money`). No step change is needed.
- [ ] Add `@property(samples=N)` to a header-only `Examples:` block.

---

## Mixed literal + generated Examples

Because `@property` is a per-block tag, one `Scenario Outline` can carry both pinned regression
rows and a generative exploration block:

```gherkin
Scenario Outline: Withdrawal invariants
  Given an account with balance <balance> and limit <limit>
  When I withdraw <amount>
  Then the resulting balance is at least negative <limit>

  # Pinned regression cases — always run, shown as normal Example scenarios
  @regression
  Examples:
    | balance | limit | amount |
    | 0       | 0     | 1      |
    | 100     | 0     | 100    |

  # Generative exploration — 500 samples via HasGen registry
  @property(samples=500)
  Examples:
    | balance | limit | amount |
```

The `@regression` block expands to two literal scenarios.  The `@property` block produces one
property scenario that runs 500 times.  Both share the same step definitions.

**A `@property` block that also has literal data rows is not an error** — it's treated as a
plain literal block instead, rows and all, and no samples are generated. This is a deliberate
"don't lose data" guard against the case where `@property` was added or left on a block whose
rows weren't removed: the `@property` tag stays on `scenario.tags` for inspection, but
`scenario.propertyConfig` is `None`, so it expands one scenario per literal row exactly like an
untagged `Examples:` block. If you intend a property block, leave it header-only (just the
column names, no data rows).

---

## Combining with `@flags(...)`

`@flags(...)` (the [feature-flag matrix](testing-flags.md)) and `@property(...)` compose: a
property scenario carrying one or more `@flags(k=v)` tags runs one **full, independent sample
batch per flag combination** — e.g. verifying an invariant holds with a feature flag both on and
off, across N generated samples each time.

```gherkin
@flags(useNewPricing=true) @flags(useNewPricing=false)
@property(samples=200, seed=7)
Scenario Outline: Total is never negative regardless of pricing engine
  Given a cart with <itemCount> items at <unitPrice> each
  Then the total is never negative

  Examples:
    | itemCount | unitPrice |
```

This produces two scenario results — `... [useNewPricing=true]` and
`... [useNewPricing=false]` — each running its own 200-sample batch. Each sample is executed
with `suite.flagLayer(meta, flags)` (the same override point used for literal `@flags(...)`
scenarios), so the flag values reach the environment the same way they would in a literal
scenario:

```scala
override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]) =
  environment >>> PricingConfig.layer(flags)
```

If you don't override `flagLayer`, it defaults to `scenarioLayer(meta)` — flags are visible in
`meta.flagValues` but not injected into the environment, same as for literal scenarios.

---

## `--dry-run`

`--dry-run` (validates step matching without executing step bodies) is honored by property
scenarios too: instead of running the configured `samples` count for real, it samples exactly
one value per column, substitutes it, and validates that every step pattern matches — without
executing any step body, writing to the failure store, or consulting an existing replay record.
A column with no resolvable `HasGen` still surfaces the usual setup error during a dry run, since
that's a configuration problem independent of whether step bodies execute.

**Use it as a fast CI pre-check.** Column resolution (including the automatic type-based
lookup above) already runs before any sampling on every normal test run, so a missing
`HasGen` fails fast as an ordinary test failure even without `--dry-run`. `--dry-run` goes
one step further and skips real step-body execution too, across every suite, for a
near-instant "is every property column resolvable and does every step pattern match"
smoke check — useful as an early CI stage before the full (slower) test run:

```sh
sbt "Test/testOnly * -- --dry-run"
```

---

## Failure replay

When a property fails, the executor writes the failing seed and counterexample values to
`.zio-bdd/failures/<scenario-slug>.json`.  On the next run, that exact counterexample is
**replayed first**, as a single sample, before generating any new samples:

- **Still falsifies:** the scenario result is reported as failed (with `[replayed from failure
  store]` appended to the failure message) and — exactly as on first failure — *no* fresh
  samples are generated. You get immediate feedback without burning a new sample budget.
- **Now passes** (the bug was fixed): the stale failure file is deleted, and the executor falls
  through to a full fresh batch of `samples` runs with a new seed — the replay is a fast
  pre-check, not a substitute for the regular run.

### Stale records (scenario body changed)

If the scenario's step list changes (the scenario was edited) since a failure was recorded, the
stored counterexample no longer corresponds to the current scenario body. `PropertyFailureStore`
detects this via a `bodyHash` of the step list: on mismatch it logs a `ZIO.logWarning` and
deletes the stale file, then proceeds with a full fresh batch — it never replays a test that no
longer exists in that form.

### File format

```json
{
  "scenarioId":      "account-invariants--withdrawal-never-produces-a-negative-balance",
  "bodyHash":        "a3f1c9",
  "seed":            42,
  "sampleIndex":     12,
  "shrunkValues":    { "balance": "0.01", "limit": "0", "amount": "100.0" },
  "generatorLabels": { "balance": "HasGen[Money]", "limit": "HasGen[Limit]", "amount": "HasGen[Money]" },
  "timestamp":       "2026-06-23T14:32:11Z"
}
```

**Committing the file:** in *your own project* (not in zio-bdd's own repo, which gitignores
`.zio-bdd/` since it's test-run output, not an intentionally pinned regression),
`.zio-bdd/failures/` can be committed to version control if you want a failing seed to travel
with the codebase — CI then replays it on every run until the regression is fixed and the file
is naturally deleted.

**Disabling replay:** `@property(replay=false)` on the tag, or `--no-replay` on the sbt CLI.

---

## JUnit XML output

Property scenarios produce standard `<testcase>` elements that CI tools interpret normally.

**Passing scenario:**

```xml
<testcase name="Withdrawal never produces a negative balance [500 samples passed, seed=42]"
          classname="Account invariants" time="1.234">
  <steps>
    <step keyword="Given"
          name="[property] 500 samples passed, seed=42 — generators: balance (HasGen[Money]), ..."
          status="passed" time="0.000"/>
  </steps>
</testcase>
```

**Failing scenario:**

```xml
<testcase name="Withdrawal never produces a negative balance [seed=42]"
          classname="Account invariants" time="0.043">
  <steps>
    <step keyword="Given" name="[counterexample] balance=0.01 (HasGen[Money]), ..."
          status="failed" time="0.000">
      <message>Falsified after 12 samples (seed=42)&#xa;Minimal counterexample: ...</message>
    </step>
    <step keyword="Given" name="an account with balance 0.01 and limit 0" status="passed" time="0.001"/>
    <step keyword="When"  name="I withdraw 100.0"                          status="passed" time="0.001"/>
    <step keyword="Then"  name="the resulting balance is at least negative 0" status="failed" time="0.001">
      <message>Expected balance ≥ -0, got -99.99</message>
    </step>
  </steps>
  <failure message="Falsified after 12 samples (seed=42)&#xa;Minimal counterexample: ..."
           type="AssertionError"/>
</testcase>
```

The `[counterexample]` step is first in the step list so Jenkins / GitHub Actions step-detail
panels show the sampled values immediately, before the actual failing step.

---

## Worked example

The `example/` module ships a complete working example:

- **Feature file:** `example/src/test/resources/features/greeting_properties.feature`
- **Step suite:** `example/src/test/scala/zio/bdd/example/SimpleSpec.scala`

The feature tests four invariants of `GreetingService`, covering each `HasGen` style and
tag placement described above:

| Scenario | Demonstrates |
|---|---|
| `Greeting always starts with "Hello," and ends with "!"` | Named override (`HasGen.named("name")`), `@property` on the `Examples:` block |
| `Greeting is always longer than 7 characters` | Same named-override generator, a second invariant |
| `Greeting is shorter than 5 characters (intentionally broken)` | Intentionally falsifying scenario, tagged `@negative` (not `@positive`) so it's excluded from the default `--include-tags positive` run — see below |
| `Greeting always contains the name regardless of title` | Two `HasGen` styles in one scenario: named override (`name`) and domain-type `given HasGen[Title]` (`title`); `@property` on the `Scenario Outline:` line instead of the `Examples:` block |

```gherkin
@negative @property(samples=100, seed=7, replay=false)
Scenario Outline: Greeting is shorter than 5 characters (intentionally broken)
  Given a user named <name>
  When the user is greeted
  Then the greeting length is less than 5

  Examples:
    | name |
```

Run the default (passing) set with:

```sh
sbt "example/test"
```

Run the intentionally-broken scenario explicitly to see the counterexample output:

```sh
sbt "example/testOnly zio.bdd.example.SimpleSpec -- --include-tags negative"
```

---

## What is NOT yet supported

| Feature | Status |
|---|---|
| Full ZIO Test shrink-tree walking | v1 records the failing seed; minimal counterexample via shrink traversal is planned for v2 |
| `Assume` / filter step to discard samples | Planned for v2 (`maxDiscarded` arg is reserved) |
| Parallel sample execution | All samples for one property scenario run sequentially; scenario-level parallelism still applies across multiple property scenarios |
| Compile-time detection of missing `HasGen` instances | Not possible — `.feature` files are plain text parsed at runtime, and Scala can't enumerate `given HasGen[_]` instances at compile time either. A missing `HasGen` fails fast as an ordinary test failure (column resolution runs before any sampling); `--dry-run` gives an even faster all-suites pre-check — see [`--dry-run`](#--dry-run) above |
