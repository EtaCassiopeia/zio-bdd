# Step DSL Reference

This document covers everything you need to define, annotate, and organise step
definitions in zio-bdd.

---

## 1. Registering steps

Steps are registered by calling `Given`, `When`, `Then`, `And`, or `But` inside
the body of a class or object that extends `ZIOSteps[R, S]`.  Each keyword
accepts a **step expression** (described below) followed by a step body.

```scala
object MySuite extends ZIOSteps[MyEnv, MyState]:
  Given("the system is initialised") { ... }
  When("the user sends a request") { ... }
  Then("the response status is " / int) { (status: Int) => ... }
  And("the response body contains " / string) { (body: String) => ... }
  But("no error is logged") { ... }
```

All five keywords are semantically equivalent for matching purposes — the
framework resolves `And` and `But` to the keyword of the preceding step at
runtime.  Use whichever keyword makes the scenario read naturally in English.

### Step expressions

A step expression is built from **literals** and **extractors** joined with the
`/` operator.  Scala's implicit conversion turns a bare `String` into a
literal-only expression, so you can start an expression with a string:

```scala
// Literal only — no extracted values
Given("a clean database") { ... }

// Literal followed by an extractor
Given("a user named " / string) { (name: String) => ... }

// Interleaved literals and extractors
When("transfer " / bigDecimal / " from account " / string / " to " / string) {
  (amount: BigDecimal, from: String, to: String) => ...
}
```

Each extractor in the expression adds one type parameter to the tuple produced
by the match, and one argument to the step body function.  The DSL extension
`/` on `StepExpression[Out]` is provided by `ZIOSteps` itself — no import
needed.

### Splitting steps across traits

Large suites are best split into focused traits.  The self-type constraint is
required because `Given`/`When`/`Then` are inherited from `ZIOSteps`:

```scala
trait CartSteps { self: ZIOSteps[AppEnv, AppState] =>
  Given("a product is added to the cart") { ... }
  When("the order is placed") { ... }
  Then("the response status is 200") { ... }
}

object MySuite
    extends ZIOSteps[AppEnv, AppState]
    with CartSteps
    with PaymentSteps:
  override def environment = AppEnv.layer
```

**Error: "value Given is not a member of CartSteps"** — the self-type
constraint `{ self: ZIOSteps[R, S] => }` is missing from the step trait.

---

## 2. Extractors

Extractors are values in scope via `DefaultTypedExtractor` (mixed into
`ZIOSteps`).  Each one corresponds to one captured group in the generated regex.

### `string`

Matches any text, optionally surrounded by double quotes. The quoted branch is
escape-aware and non-spanning — it stops at its own unescaped closing quote
rather than running greedily to the last quote on the line. Quotes are
stripped from the captured value, and `\"` / `\\` are unescaped to `"` / `\`.

```
pattern: ("(?:\\.|[^"\\])*"|.*)
```

```scala
Given("the account id is " / string) { (id: String) =>
  // id == "abc123"  for step text: the account id is "abc123"
  // id == abc123    for step text: the account id is abc123
  // id == say "hi"  for step text: the account id is "say \"hi\""
}
```

Whitespace is trimmed from unquoted values.

### `word`

Matches a single token with no whitespace.  Use when the parameter is always
one word and you want to prevent accidental multi-word captures.

```
pattern: (\S+)
```

```scala
Given("the status is " / word) { (status: String) =>
  // status == "ACTIVE"  for step text: the status is ACTIVE
}
```

### `int`

Matches a signed integer (`-?\\d+`).

```scala
When("the user deposits " / int / " cents") { (cents: Int) => ... }
```

Fails if the captured text is a decimal or contains non-numeric characters.

### `double`

Matches integer and decimal numbers (`[-+]?[0-9]*\\.?[0-9]+`).

```scala
Then("the rate is " / double / " percent") { (rate: Double) => ... }
```

### `long`

Same regex as `int` (`-?\\d+`) but parses to `Long`.  Use for IDs or counts
that could overflow `Int`.

```scala
Given("sequence number " / long) { (seq: Long) => ... }
```

### `boolean`

Matches `true`, `false`, `True`, `False`, `TRUE`, or `FALSE`.

```scala
When("feature flag enabled is " / boolean) { (enabled: Boolean) => ... }
```

Fails on any other value (e.g. `"yes"` or `"1"`).

### `bigDecimal`

Matches decimal numbers and parses with `java.math.BigDecimal` to preserve
exact precision.  Use this for any financial or monetary value where
floating-point rounding is unacceptable.

```
pattern: ([-+]?[0-9]*\.?[0-9]+)
```

```scala
Then("the balance is " / bigDecimal) { (balance: BigDecimal) => ... }
```

### `uuid`

Matches a UUID in 8-4-4-4-12 hexadecimal format.

```
pattern: ([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})
```

```scala
Given("account " / uuid / " exists") { (id: UUID) => ... }
```

### `rest`

Matches the entire remainder of the step text after the preceding literal,
including spaces.  Useful for free-form messages or SQL fragments.

```
pattern: (.+)
```

```scala
Then("the error message is " / rest) { (msg: String) =>
  // Captures everything after "the error message is "
}
```

`rest` should only be used as the last extractor in an expression; placing
a literal after it will never match.

### `table[T]`

Deserialises a Gherkin data table (the `| col | col |` block beneath a step)
into `List[T]`.  Requires a `zio.schema.Schema[T]` in implicit scope.

By default, column header names must match the Scala field names of `T` exactly.

```scala
case class User(name: String, age: Int)
given Schema[User] = DeriveSchema.gen[User]

Given("the following users exist" / table[User]) { (users: List[User]) =>
  ZIO.foreachDiscard(users)(userRepo.insert)
}
```

Feature file:

```gherkin
Given the following users exist
  | name  | age |
  | Alice | 30  |
  | Bob   | 25  |
```

Supported cell types: `String`, `Int`, `Long`, `Double`, `Boolean`,
`BigDecimal`.  Only fields with `Schema.Primitive` types are supported inside
table cells.

`table[T]` does not consume a regex capture group; it reads from
`StepInput.table` directly.

### `docString`

Extracts the triple-quoted doc string argument of a step as a raw `String`.
The doc string appears on the lines immediately following the step, indented
and fenced with `"""`.

```scala
Given("the request body is" / docString) { (body: String) =>
  // body == "{\n  \"key\": \"value\"\n}"
}
```

Feature file:

```gherkin
Given the request body is
  """
  {
    "key": "value"
  }
  """
```

`docString` does not consume a regex group; it reads from
`StepInput.docString`.

### `oneOf(alternatives*)`

Match any one of the supplied string alternatives and return it as `String`.
Alternatives are sorted longest-first internally to prevent prefix shadowing
(e.g. `"is"` matching before `"is not"`).

```scala
// Replaces two separate step registrations:
Then("the item is " / oneOf("in stock", "out of stock")) { (status: String) =>
  checkInventory(available = status == "in stock")
}

// Multi-word alternatives work too:
When(
  "the payment is processed as a " /
    oneOf("credit card", "debit card", "bank transfer") /
    " transaction"
) { (method: String) => processPayment(method) }
```

### `optional(text)`

Match an optional literal string. Returns `Option[String]` — `Some(text)` when present,
`None` when absent.

```scala
// Matches both "without fork point" and "without fork point with the same simulationId"
Given(
  "a valid SimulationBranch body without fork point" / optional(" with the same simulationId")
) { (suffix: Option[String]) =>
  // suffix.isDefined when the longer variant was used
  ...
}
```

### `regex(pat)`

Embed a raw regex fragment with exactly one top-level capturing group.
Returns `String`; optional groups (`(text)?`) produce `""` when not matched.

```scala
// Optional "express " variant — collapses two step registrations into one:
Given("a valid " / regex("(express )?") / "shipping option") { (prefix: String) =>
  val useExpress = prefix.nonEmpty
  ...
}
```

> **Constraint:** `pat` must contain **exactly one** top-level capturing group. Multiple
> groups shift the group index for subsequent extractors in the same expression, producing
> incorrect results silently. Use `oneOf()` or `optional()` where they fit; reach for
> `regex()` only when neither helper covers the pattern.

---

## 3. Table column mapping

When the feature file uses human-readable header names that differ from Scala
field names, there are two equivalent ways to map them.

### `@ColumnName` annotation

Annotate the case class field with the exact header string as it appears in the
feature file.  The mapping travels with the type.

```scala
import zio.bdd.core.step.ColumnName

case class ProductRow(
  @ColumnName("Product Name") productName: String,
  @ColumnName("Unit Price")   unitPrice: String,
  @ColumnName("SKU")          sku: String
)

given Schema[ProductRow] = DeriveSchema.gen[ProductRow]
```

With this annotation, `table[ProductRow]` resolves the headers
automatically:

```gherkin
| Product Name | Unit Price | SKU      |
| Widget A     | 9.99       | WDG-001  |
```

### `tableWithMapping[T](map)`

Supply a `Map[String, String]` from header text to field name at the call site.
This is useful when you cannot or do not want to annotate the case class.

```scala
val mapping = Map(
  "Product Name" -> "productName",
  "Unit Price"   -> "unitPrice"
)

Given("the catalogue contains" / tableWithMapping[ProductRow](mapping)) {
  (rows: List[ProductRow]) => ...
}
```

**Priority:** `tableWithMapping` explicit map overrides `@ColumnName`, which
overrides the identity (exact field name) mapping.  Headers not listed in the
explicit map fall through to `@ColumnName` and then to the identity mapping.

---

## 4. `StepEffect` and `StepIO[+A]`

These type aliases are defined inside every `ZIOSteps[R, S]` instance:

```scala
type StepEffect    = ZIO[R & State[S], Throwable, Unit]
type StepIO[+A]    = ZIO[R & State[S], Throwable, A]
```

They exist entirely for readability.  `Scope` is present in the ZIO environment
at runtime (injected by the executor), so steps can use `ZIO.acquireRelease`
without adding `Scope` to the type.

**Use `StepEffect` for void helper methods** that are called from step bodies:

```scala
private def placeOrder(item: CartItem): StepEffect =
  for {
    client <- ZIO.service[ShopClient]
    resp   <- client.post("/orders", item)
    _      <- ScenarioContext.update(_.copy(lastResponse = resp))
  } yield ()

Given("an item is added to the order") { placeOrder(CartItem.default) }
```

Without the alias the full type `ZIO[R & State[S], Throwable, Unit]` appears in
error messages, which can be confusing when the type parameters are elaborate.

**Use `StepIO[+A]` for helpers that produce a value** consumed by subsequent
steps in a for-comprehension:

```scala
private def createUser(profile: UserProfile): StepIO[UserId] =
  ZIO.service[UserService]
    .flatMap(_.create(profile))
    .map(_.id)

Given("a registered user exists") {
  for {
    id <- createUser(UserProfile.default)
    _  <- ScenarioContext.update(_.copy(userId = Some(id)))
  } yield ()
}
```

The covariance (`+A`) follows ZIO's own convention and allows `StepIO[Nothing]`
to be assigned where `StepIO[Unit]` is expected.

---

## 5. Empty step bodies

Two idioms are accepted for a step that performs no work:

```scala
// Explicit ZIO.unit
Given("a placeholder step") { ZIO.unit }

// Empty braces — implicit conversion from Unit to ZIO.unit
Given("a no-op step") {}
```

`ZIOSteps` provides `implicit def unitToStepEffect(u: Unit): ZIO[Any, Nothing, Unit] = ZIO.unit`
so `{}` compiles without a warning.  Use `ZIO.unit` explicitly in helper methods
for clarity.

---

## 6. `InlineStepMethods` — the code-gen-free alternative

By default, `ZIOSteps` inherits `Given`/`When`/`Then`/`And`/`But` from the
`GeneratedStepMethods` trait produced by the sbt `StepGeneratorPlugin`.  If you
prefer not to rely on code generation, mix in `InlineStepMethods[R, S]` instead:

```scala
object MySuite extends ZIOSteps[R, S] with InlineStepMethods[R, S]:
  Given("a user " / string / " with age " / int) { (name: String, age: Int) =>
    ScenarioContext.update(s => s.copy(users = s.users + (name -> User(name, age))))
  }
```

`InlineStepMethods` uses the `TupleToFn` Scala 3 match type to resolve the
step body function signature at compile time — no runtime reflection, no sbt
plugin:

```scala
type TupleToFn[R, S, T <: Tuple] <: Any = T match
  case EmptyTuple               => RIO[R & State[S] & Scope, Unit]
  case Tuple1[a]                => a => RIO[R & State[S] & Scope, Unit]
  case (a, b)                   => (a, b) => RIO[R & State[S] & Scope, Unit]
  // ... up to arity 8
```

Steps with arity above 8 produce a compile error rather than silently
truncating captured values.

`InlineStepMethods` and `GeneratedStepMethods` can coexist in the same suite —
mixing them is supported.

---

## 7. The `pending()` helper

Mark a step as intentionally unimplemented.  A pending step is reported
distinctly from a failed step — it does not cause subsequent steps to be
skipped, and it will not fail the suite.

```scala
Given("a complex precondition exists") { pending("not yet implemented") }
When("the edge case is triggered")     { pending() }   // defaults to "TODO"
```

`pending(reason)` returns `ZIO.fail(new PendingException(reason))`.  The
executor recognises `PendingException` and marks the step with a PENDING status
rather than a failure.

---

## 8. `withSnapshot` for before/after assertions

`withSnapshot` captures a slice of the scenario state before a block runs, then
makes it available for comparison afterward.  It is most useful in `Then` steps
that need to verify a delta rather than an absolute value.

```scala
def withSnapshot[A](lens: S => A)(
  body: A => ZIO[R & State[S] & Scope, Throwable, Unit]
): ZIO[R & State[S] & Scope, Throwable, Unit]
```

Usage:

```scala
Then("the balance has increased") {
  withSnapshot(_.balance) { balanceBefore =>
    for {
      _              <- processDeposit(Money(100))
      currentBalance <- ScenarioContext.get.map(_.balance)
      _              <- Assertions.assertTrue(currentBalance > balanceBefore)
    } yield ()
  }
}
```

`withSnapshot` reads the state at the moment it is called (the start of the
`Then` step body), captures the value through the projection function, then
runs the body with that captured value available as `balanceBefore`.

---

## 9. Startup ambiguity detection

When `getSteps` is called for the first time (at suite startup, before any
scenario runs), the framework scans all registered steps for duplicate patterns
registered under the same keyword.

If two or more steps share an identical expression text and keyword,
`IllegalStateException` is thrown immediately:

```
Ambiguous step definitions detected at suite startup.
The following step expressions are registered multiple times:
  GivenStep 'the account is initialised' — registered 2 times
Rename or merge duplicate step definitions.
```

**Important distinctions:**

- The same text registered under different keywords (`Given` vs `When`) is not
  ambiguous — they are distinct lookup keys.
- Duplication across trait mixins (e.g. two traits both defining the same step)
  is caught at startup, not at the point of registration.
- Registration after `getSteps` is sealed (i.e. after the suite has started
  running) throws `IllegalStateException` immediately with a clear message.

This fail-fast design prevents the runtime surprise of an unexpected step
silently matching the wrong definition.

---

## 10. Soft assertions with `Assertions.collectAll`

By default, the first failing assertion inside a step body short-circuits the
step — the ZIO effect fails and remaining checks are never evaluated.  When a
step needs to verify several independent conditions and you want a single report
showing **all** failures, use `Assertions.collectAll`.

```scala
Then("the response is well-formed") {
  ScenarioContext.get.flatMap { s =>
    Assertions.collectAll(
      Assertions.assertEquals(s.http.statusCode, 200),
      Assertions.assertTrue(s.http.body.nonEmpty, "body must not be empty"),
      Assertions.assertTrue(
        s.http.body.contains("orderId"),
        "body must contain orderId"
      )
    )
  }
}
```

Every assertion is always evaluated.  If any fail, a `MultipleAssertionError`
is thrown listing all failures.  The pretty reporter renders each failure on its
own indented line under the step.

### API

```scala
Assertions.collectAll(assertions: ZIO[Any, Throwable, Unit]*): ZIO[Any, Throwable, Unit]
```

- Pass any number of assertion effects — the same ones you already use
  (`assertTrue`, `assertEquals`, `assertSome`, …).
- An empty call (`collectAll()`) always succeeds.
- The resulting effect has no environment requirements and can be used anywhere
  an assertion can.

### `MultipleAssertionError`

```scala
final class MultipleAssertionError(val failures: List[String])
    extends AssertionError(...)
```

`failures` contains one entry per failing sub-assertion, using the `message`
you passed to `assertTrue`/`assertEquals` etc.  The top-level `getMessage`
includes the count and all messages.

---

## 11. State-injecting step variants (`GivenS` / `WhenS` / `ThenS` / `AndS` / `ButS`)

Every step body that reads scenario state must begin with `s <- ScenarioContext.get`.
For steps that only need a snapshot at the start, the `*S` variants inject the
current state as the **first curried argument**, removing that boilerplate:

```scala
// Before — s <- ScenarioContext.get is pure ceremony
Given("an order is prepared") {
  for {
    s     <- ScenarioContext.get
    order <- buildOrder(s.cart.items)
    _     <- ScenarioLens.update[AppState, OrderState](_.copy(current = Some(order)))
  } yield ()
}

// After — state is injected, no ScenarioContext.get needed
GivenS("an order is prepared") { s =>
  for {
    order <- buildOrder(s.cart.items)
    _     <- ScenarioLens.update[AppState, OrderState](_.copy(current = Some(order)))
  } yield ()
}
```

For steps with Gherkin parameters the state comes **first** (curried), then
the extracted parameters:

```scala
// 1 Gherkin param — (s: AppState) => (expected: Int) => effect
ThenS("the response status is " / int) { s => (expected: Int) =>
  assertTrue(s.http.statusCode == expected, s"Expected $expected, got ${s.http.statusCode}")
}

// 2 Gherkin params
WhenS("send " / int / " requests to " / string) { s => (count: Int, url: String) =>
  ZIO.replicateZIODiscard(count)(sendRequest(s.session.userId, url))
}
```

### When to use `*S` variants

| Situation | Recommended |
|-----------|-------------|
| Step reads state once at the top, then performs effects | `GivenS` / `WhenS` |
| Step is a pure assertion on state | `ThenS` / `AndS` |
| Step does not read state at all | `Given` / `When` |
| Step needs state **after** an intermediate update | Keep `ScenarioContext.get` for the second read |

### Implementation

Each `*S` method registers a step whose body is:
`State.get[S].flatMap(s => f(s)(extractedParams...))` — one `FiberRef.get`
per step invocation, equivalent cost to the manual pattern.

Available arities: `GivenS` / `WhenS` / `ThenS` / `AndS` support 0 (no Gherkin
params), 1, 2, and 3 extracted parameters. `ButS` only supports 0 and 1 —
`ButS[A, B]` / `ButS[A, B, C]` are not defined on `ZIOSteps`. A `But` step
needing 2+ params must use the code-gen-free `InlineStepMethods` (§6), whose
`ButS` is arity-unbounded, or fall back to the manual `ScenarioContext.get`
pattern.

For higher arities on `GivenS` / `WhenS` / `ThenS` / `AndS`, use the manual
`ScenarioContext.get` pattern or decompose the step into helper methods.

### Related patterns

- **`ScenarioLens.get`** — read a single sub-state slice without binding the full `S`
  (see [State Management Reference §9.2](state.md#92-scenariolensget-for-read-only-slice-access))
- **`.flatMap` for single reads** — collapse `for { s <- ScenarioContext.get; _ <- f(s) }` to
  `ScenarioContext.get.flatMap(s => f(s))`
  (see [State Management Reference §9.3](state.md#93-flatmap-instead-of-for-comprehension-for-single-read-assertions))
- **`withSnapshot`** — capture a state value before an action, assert after
  (see [§8 above](#8-withsnapshot-for-beforeafter-assertions))

---

## 12. Polling with `Assertions.eventually` / `eventuallyAssert`

When the system under test is *eventually consistent* — feature-flag propagation, async
message-queue consumers, mock reload, distributed state — a step must wait for a condition to
become true. A blind `ZIO.sleep(worstCase)` is both slow and flaky. `eventually` retries an
effect until it succeeds or a time budget elapses, proceeding the moment the condition holds.

```scala
// Before: fixed worst-case margin — slow when fast, flaky when slow
When("flag propagation is observed") {
  ScenarioContext.get.flatMap(_ => ZIO.sleep(5.seconds))
}

// After: proceeds as soon as the flag flips, up to 8s
When("flag propagation is observed") {
  Assertions.eventually(
    getFlagStatus.filterOrFail(_ == "ON")(RuntimeException("flag not ON yet")),
    maxTime = 8.seconds
  )
}
```

`eventuallyAssert` is a convenience overload that probes a value with `fetch`, runs an
assertion against it, and retries the pair:

```scala
Then("the balance is eventually settled") {
  Assertions.eventuallyAssert(fetchBalance)(b => Assertions.assertEquals(b, expected))
}
```

### API

```scala
Assertions.eventually[R, A](
  effect: ZIO[R, Throwable, A],
  maxTime: Duration = 10.seconds,
  schedule: Schedule[Any, Any, Any] = Schedule.exponential(50.millis).jittered && Schedule.spaced(500.millis)
): ZIO[R, Throwable, A]

Assertions.eventuallyAssert[R, A](
  fetch: ZIO[R, Throwable, A]
)(assertion: A => ZIO[Any, Throwable, Unit], maxTime: Duration = 10.seconds): ZIO[R, Throwable, Unit]
```

- **No `zio.Schedule` import needed** for the common case — the defaults live inside
  `Assertions`. Override `schedule` only when you want a different back-off.
- **On timeout the last underlying failure is surfaced** — not a generic "timed out". The
  message you failed with (e.g. `"flag not ON yet"`) is what propagates, so failures stay
  diagnosable.
- **Interruptible.** The retry loop respects the enclosing step timeout: the outer
  `stepTimeout` is a hard cap that interrupts the wait, so keep `maxTime` ≤ `stepTimeout`.
- **Only typed failures are retried.** An exception thrown outside a `ZIO.attempt` boundary
  becomes a defect (`Cause.Die`) and propagates on the first attempt without retrying. Wrap any
  effect that may throw in `ZIO.attempt` so the throw becomes a retryable failure — the `assert*`
  helpers already do this.

---

## 13. Stability with `Assertions.during`

The inverse of `eventually`: assert a condition *stays* true for a window rather than *becomes*
true. Use it for negative/stability properties — "the balance must remain zero for 2s after a
cancelled transaction", "the Rift imposter must record no new requests during a quiesce period". A
`ZIO.sleep(d) *> assertOnce` can't catch a transient violation that recovered mid-sleep; `during`
re-probes continuously and fails the moment the property is breached.

```scala
Then("the balance stays zero after the cancelled transaction") {
  Assertions.during(
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.balance, 0)),
    duration = 2.seconds
  )
}
```

### API

```scala
Assertions.during[R](
  condition: ZIO[R, Throwable, Unit],
  duration: Duration,
  interval: Duration = 200.millis
): ZIO[R, Throwable, Unit]
```

- **Fails fast** on the first probe that fails, surfacing that underlying failure (not a generic
  "stability check failed"). Passes only if `condition` holds on every probe across `duration`.
- **Sampling, not continuous.** A breach that appears and recovers entirely between two probes can
  be missed — choose an `interval` fine enough for the property under test.
- **No `zio.Schedule`/`Clock` import** needed at the call site; the defaults live in `Assertions`.
- **Interruptible**, and respects the enclosing `stepTimeout` — keep `duration` ≤ `stepTimeout`.
  Only typed failures count as a violation (same Fail/Die caveat as `eventually`).

---

## 14. Collection quantifiers

`assertContainsAll` / `assertContainsSubset` check *membership*, but not a *predicate* over the
elements. These quantifiers assert a predicate holds for every / some / no / exactly-n / a range
of elements, and on failure list the offending elements **with their indices** (and the optional
`label`) so you see *which* elements broke the rule, not just a count.

```scala
Then("every settled order has a positive total") {
  ScenarioContext.get.flatMap { s =>
    Assertions.assertForAll(s.orders, label = "orders")(o => Assertions.assertTrue(o.total > 0, s"${o.id} is non-positive"))
  }
}
```

### API

```scala
Assertions.assertForAll[A]     (collection: Iterable[A], label: String = "")(pred: A => ZIO[Any, Throwable, Unit]): ZIO[Any, Throwable, Unit]
Assertions.assertExists[A]     (collection: Iterable[A], label: String = "")(pred: A => ZIO[Any, Throwable, Unit]): ZIO[Any, Throwable, Unit]
Assertions.assertNoneSatisfy[A](collection: Iterable[A], label: String = "")(pred: A => ZIO[Any, Throwable, Unit]): ZIO[Any, Throwable, Unit]
Assertions.assertExactly[A]    (n: Int, collection: Iterable[A], label: String = "")(pred: A => ZIO[Any, Throwable, Unit]): ZIO[Any, Throwable, Unit]
Assertions.assertBetween[A]    (min: Int, max: Int, collection: Iterable[A], label: String = "")(pred: A => ZIO[Any, Throwable, Unit]): ZIO[Any, Throwable, Unit]
```

- **"Satisfies" = the predicate succeeds.** The predicate is any `ZIO[Any, Throwable, Unit]` — the
  existing `assert*` helpers compose directly.
- **`assertForAll`** fails if any element fails (empty collection passes vacuously); **`assertExists`**
  fails if none satisfy (empty fails); **`assertNoneSatisfy`** fails if any satisfies (empty passes) —
  named to avoid a clash with `assertNone[A](Option[A])`, which asserts an `Option` is empty;
  **`assertExactly(n)`** / **`assertBetween(min, max)`** fail when the satisfying count is off /
  outside `[min, max]`.
- **Every element is probed** (soft, `collectAll`-style), so the failure message reports *all*
  offenders with their indices — not just the first.
- **A predicate that fails with a typed error counts as "did not satisfy"** — a genuine error in
  the typed channel is indistinguishable from a legitimate `false`. (A defect — thrown outside a
  `ZIO.attempt` boundary — still propagates, same Fail/Die caveat as `eventually`.) If a predicate
  can raise a typed error you do *not* want treated as an unsatisfied element, handle it first.

---

## 15. Change assertions with `Assertions.assertChange`

A composable, readable upgrade over [`withSnapshot`](#8-withsnapshot-for-beforeafter-assertions)
for "after a deposit of 100, the balance increased by 100." `assertChange(probe)(action)` captures
`probe` before running `action`, re-probes after, and defers the check to a terminal:

```scala
Then("the deposit increases the balance by 100") {
  Assertions.assertChange(ScenarioContext.get.map(_.balance)) {
    When("a deposit of 100 is made") { ... }
  }.by(100)
}
```

### API

```scala
Assertions.assertChange[R, A](probe: ZIO[R, Throwable, A])(action: ZIO[R, Throwable, Unit]): ChangeAssertion[R, A]

final class ChangeAssertion[R, A]:
  def by(delta: A)(using Numeric[A]): ZIO[R, Throwable, Unit]  // changed by exactly delta
  def from(before: A): ChangeAssertion.From[R, A]              // .from(x).to(y): exact before AND after
  def toAnything: ZIO[R, Throwable, Unit]                      // changed at all (before != after)
  def and[B](otherProbe: ZIO[R, Throwable, B]): ComposedChangeAssertion[R]

final class ComposedChangeAssertion[R]:
  def and[B](otherProbe: ZIO[R, Throwable, B]): ComposedChangeAssertion[R]
  def assert: ZIO[R, Throwable, Unit]                          // run the shared action once; every probe must change
```

- **`.by(delta)`** requires a `Numeric[A]` and asserts `after - before == delta` (exact equality —
  for approximate numeric comparison use a dedicated tolerance check).
- **`.from(x).to(y)`** asserts the exact before *and* after values.
- **`.toAnything`** asserts the value changed without pinning the delta.
- **`.and(otherProbe)`** composes several probes over a **single** action — the action runs once
  and every probe must change. It takes a bare *probe* (not a second `assertChange`), so there is
  no way to supply a second action; chain more with `.and` and terminate with `.assert`:
  `assertChange(a)(action).and(b).and(c).assert`.
- Failure messages show the observed **before**, **after**, and **expected** values.

> **Design note.** The environment is threaded as a single `R` (typically containing `State[S]`),
> and `.by` takes `delta: A` directly — a simplification of the issue's sketched
> `R & State[S]` / `.by[B](… A =:= B)` signatures that reads the same at call sites and infers
> reliably.


---

## 16. Named-predicate assertions with `Assertions.assertSatisfies`

A concise shorthand over `assertZIO(value, Assertion.assertion("…")(…))` — assert that a value
satisfies a named predicate, with a failure message that reports both the description and the
offending value.

```scala
Then("the response id is a UUID") {
  ScenarioContext.get.flatMap(s => Assertions.assertSatisfies(s.id, "is a valid UUID")(isUuid))
}
```

### API

```scala
Assertions.assertSatisfies[A](actual: A, description: String)(pred: A => Boolean): ZIO[Any, Throwable, Unit]
Assertions.assertSatisfiesZIO[R, A](actual: A, description: String)(pred: A => ZIO[R, Throwable, Boolean]): ZIO[R, Throwable, Unit]
```

- On failure the message is `expected <actual> to satisfy: <description>`.
- `assertSatisfiesZIO` takes an **effectful** predicate; if the predicate's effect fails, that
  failure propagates unchanged (it is not turned into an assertion failure).

---

## 17. Inspecting a raised exception with `Assertions.assertRaises`

An upgrade over `assertThrows` (which only checks the exception *type*): `assertRaises[E]` catches
the exception raised by an effect and runs an inspection block on it, so you can assert on its
message, cause, or fields.

```scala
Then("charging a declined card raises PaymentError") {
  Assertions.assertRaises[PaymentError](chargeDeclinedCard) { e =>
    Assertions.assertTrue(e.reason == "declined")
  }
}
```

### API

`assertRaises` is a two-stage builder: first fix the exception type (which
supplies its own `ClassTag` via a `using` parameter), then apply it to the
effect to inspect:

```scala
Assertions.assertRaises[E <: Throwable](using ClassTag[E]): AssertRaises[E]

// AssertRaises[E]#apply
def apply[R, A](
  effect: ZIO[R, Throwable, A],
  message: String = ""
)(inspect: E => ZIO[Any, Throwable, Unit]): ZIO[R, Throwable, Unit]
```

- **Passes** when `effect` raises an `E` and `inspect` succeeds. **Fails** when the effect succeeds
  (nothing raised), raises a *different* type, or when `inspect` fails.
- `message` defaults to `""`; when left at the default, the failure text ("Expected `<E>` to be
  raised, but …") is computed from the `ClassTag` at runtime, not from a literal default.
- The exception is caught whether it surfaces as a typed failure or a defect; **interruption** is
  not treated as a raised exception and propagates (cancellation is honored).
- `inspect` is a `ZIO[Any, Throwable, Unit]`, so it composes with the other `assert*` helpers.

---

## 18. Polling a probe with `Assertions.poll`

A companion to [`eventually`](#12-polling-with-assertionseventually--eventuallyassert) that
separates the **probe** (run once per attempt) from the **assertion** (retried) — useful when the
probe is expensive and you want the failure message to foreground the *last polled value*.

```scala
Then("the job eventually reports DONE") {
  Assertions.poll(fetchJobStatus)(status => Assertions.assertEquals(status, "DONE"))
}
```

### API

```scala
Assertions.poll[R, A](
  probe: ZIO[R, Throwable, A],
  maxTime: Duration = 10.seconds,
  interval: Duration = 200.millis
)(assertion: A => ZIO[Any, Throwable, Unit]): ZIO[R, Throwable, Unit]
```

- Re-probes every `interval` until the `assertion` holds or `maxTime` elapses.
- **On timeout the last assertion failure is surfaced**, annotated with the last polled value
  (`poll assertion did not hold (last polled value: …): …`) — not a generic "timed out".
- Configurable `maxTime` and `interval` with sensible defaults. **Interruptible**, and respects the
  enclosing `stepTimeout` — keep `maxTime` ≤ `stepTimeout`. Only typed failures are retried (same
  Fail/Die caveat as `eventually`).
- If the **probe itself** keeps failing (never yields a value), its own error is surfaced on
  timeout — there is no last-polled-value annotation, since nothing was successfully polled.

---

## 19. Approximate numeric equality with `Assertions.assertApproxEquals`

For monetary / floating-point domains where exact `assertEquals` is the wrong tool, assert that a
value is within a tolerance of the expected one. Works for any `Numeric` (`Double`, `BigDecimal`, …).

```scala
Then("the total is about 19.99") {
  ScenarioContext.get.flatMap(s => Assertions.assertApproxEquals(s.total, 19.99, 0.01))
}

// fluent form (import Assertions.shouldBeCloseTo):
Then("the rate is close to 3.5%") {
  ScenarioContext.get.flatMap(s => s.rate.shouldBeCloseTo(0.035, 0.001))
}
```

### API

```scala
Assertions.assertApproxEquals[A](actual: A, expected: A, delta: A, message: String = "")(using Numeric[A]): ZIO[Any, Throwable, Unit]
extension [A: Numeric](actual: A) def shouldBeCloseTo(expected: A, delta: A): ZIO[Any, Throwable, Unit]
```

- Passes when `abs(actual - expected) <= delta` (the boundary is inclusive). On failure the message
  shows the actual, expected, delta, and the observed difference.
