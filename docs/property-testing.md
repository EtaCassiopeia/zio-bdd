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
import zio.bdd.core.property.HasGen
import zio.test.Gen

@Suite(featureDirs = Array("src/test/resources/features"), reporters = Array("pretty", "junitxml"))
object AccountSpec extends ZIOSteps[AccountService, AccountState]:

  // Register generators for the placeholder types
  given HasGen[Money]  = Gen.double(0, 10_000).map(Money.apply)
  given HasGen[Limit]  = Gen.long(0, 5_000).map(Limit.apply)

  Given("an account with balance " / money / " and limit " / limit) { (bal, lim) =>
    ScenarioContext.update(_.copy(balance = bal, limit = lim))
  }
  When("I withdraw " / money) { (amount: Money) => ... }
  Then("the resulting balance is at least negative " / limit) { (lim: Limit) => ... }
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

These are provided automatically for primitive types that match the built-in
`TypedExtractor` set.  No registration or import needed:

| Type | Generator used |
|---|---|
| `Int` | `Gen.int` |
| `Long` | `Gen.long` |
| `Double` | `Gen.double` |
| `Boolean` | `Gen.boolean` |
| `String` | `Gen.alphaNumericString` |
| `UUID` | `Gen.uuid` |

If your placeholders use only these types and your step extractors already use the
corresponding built-in extractors (`/ int`, `/ string`, etc.), no additional setup is
needed — property mode works out of the box.

---

### Providing `HasGen` for a domain type

For every domain type used as a placeholder, provide a `given HasGen[YourType]` in your
suite object.  The `given` is defined inline using Scala 3's anonymous given syntax:

```scala
import zio.bdd.core.property.HasGen
import zio.test.Gen

object MySpec extends ZIOSteps[MyService, MyState]:

  // Simple range-bounded value type
  given HasGen[Money] = Gen.double(0.01, 10_000.0).map(Money.apply)

  // Enum — pick uniformly from the valid members
  given HasGen[Currency] = Gen.elements(Currency.GBP, Currency.USD, Currency.EUR)

  // Case class with multiple fields
  given HasGen[Address] =
    for
      street <- Gen.alphaNumericString
      city   <- Gen.elements("London", "New York", "Tokyo")
    yield Address(street, city)
```

The `given` must be in scope at the point where `columnGenLookup` resolves the type.
Placing it directly in the suite object (or in a trait mixed into the suite) is the
idiomatic location.

#### Using `HasGen.apply` to summon an instance

Once a `given HasGen[A]` is in scope, summon it with `HasGen[A]`:

```scala
// In columnGenLookup:
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

Override `columnGenLookup` on your suite to tell the executor which `HasGen` to use for
each placeholder column name.  The built-in primitive types are always resolved
automatically; this override is only needed for domain types.

```scala
import zio.bdd.core.property.{ColumnGenLookup, HasGen}

override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
  def byColumn(col: String): Option[HasGen[?]] = col match
    case "balance" | "amount" => Some(HasGen[Money])
    case "limit"              => Some(HasGen[Limit])
    case "currency"           => Some(HasGen[Currency])
    case _                    => None  // falls back to built-ins; fails with a clear error if unresolved
```

**Resolution order** for a column named `"amount"`:

1. Named override from the column header annotation (`| amount: smallAmounts |`) —
   checked against `HasGen.resolve("smallAmounts")`.
2. `columnGenLookup.byColumn("amount")`.
3. Built-in `HasGen` instance for the column's inferred type (for primitive columns
   matched by existing `TypedExtractor` built-ins).
4. Setup error — the scenario fails immediately with a clear message:
   `No HasGen registered for column 'amount'. Register given HasGen[T] for its type.`

---

### Complete setup checklist

For a property scenario with a domain-type column to work:

- [ ] Define `case class Money(value: Double)` (or whatever your type is).
- [ ] Add `given HasGen[Money] = Gen.double(...).map(Money.apply)` in the suite object.
- [ ] Add `case "amount" => Some(HasGen[Money])` in `columnGenLookup`.
- [ ] Ensure the step definition uses the same `TypedExtractor[Money]` it already uses
      for literal scenarios (e.g. `/ money`).  No step change is needed.
- [ ] Add `@property(samples=N)` to a header-only `Examples:` block.

If the column uses a built-in type (`Int`, `String`, `Long`, `Double`, `Boolean`,
`UUID`), steps 1–3 are not required.

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

---

## Failure replay

When a property fails, the executor writes the failing seed and counterexample values to
`.zio-bdd/failures/<scenario-slug>.json`.  On the next run, that seed is **replayed first**
before generating new samples:

```
▶ Replaying 1 known-failing sample for "Withdrawal never produces a negative balance"...
  ✗ Still falsifies (seed=42, sample=12) — counterexample unchanged
  STOP. New samples not generated.
```

If the replay passes (the bug was fixed), the failure file is cleared and the full sample run
proceeds normally.

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

**Body-hash invalidation:** if the scenario's step list changes (the scenario was edited), the
stale failure record is discarded with a warning rather than replaying a test that no longer exists
in the same form.

**Committing the file:** `.zio-bdd/failures/` should be committed to version control.  The failing
seeds are a signal that travels with the codebase — CI will replay them on every run until the
regression is fixed.

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

The feature tests three invariants of `GreetingService`, including one intentionally broken scenario
that demonstrates the failure output:

```gherkin
@positive @property(samples=500, seed=42, shrink=true)
Scenario Outline: Greeting always starts with "Hello," and ends with "!"
  Given a user named <name>
  When the user is greeted
  Then the greeting starts with "Hello,"
  And the greeting ends with "!"
  And the greeting contains the name

  Examples:
    | name |

@positive @property(samples=100, seed=7, replay=false)
Scenario Outline: Greeting is shorter than 5 characters (intentionally broken)
  Given a user named <name>
  When the user is greeted
  Then the greeting length is less than 5

  Examples:
    | name |
```

Run with:

```sh
sbt "example/test"
```

---

## What is NOT yet supported

| Feature | Status |
|---|---|
| Full ZIO Test shrink-tree walking | v1 records the failing seed; minimal counterexample via shrink traversal is planned for v2 |
| `Assume` / filter step to discard samples | Planned for v2 (`maxDiscarded` arg is reserved) |
| Parallel sample execution | All samples for one property scenario run sequentially; scenario-level parallelism still applies across multiple property scenarios |
