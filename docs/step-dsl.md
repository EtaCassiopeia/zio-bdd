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

Matches any text, optionally surrounded by double quotes.  Quotes are stripped
from the captured value.

```
pattern: (".*"|.*)
```

```scala
Given("the account id is " / string) { (id: String) =>
  // id == "abc123"  for step text: the account id is "abc123"
  // id == abc123    for step text: the account id is abc123
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
  GivenStep "the account is initialised" — registered 2 times
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
WhenS("send " / int / " requests to " / string) { s => (count: Int) => (url: String) =>
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

Available arities: 0 (no Gherkin params), 1, 2, and 3 extracted parameters.
For higher arities, use the manual `ScenarioContext.get` pattern or decompose
the step into helper methods.

### Related patterns

- **`ScenarioLens.get`** — read a single sub-state slice without binding the full `S`
  (see [State Management Reference §9.2](state.md#92-scenariolensget-for-read-only-slice-access))
- **`.flatMap` for single reads** — collapse `for { s <- ScenarioContext.get; _ <- f(s) }` to
  `ScenarioContext.get.flatMap(s => f(s))`
  (see [State Management Reference §9.3](state.md#93-flatmap-instead-of-for-comprehension-for-single-read-assertions))
- **`withSnapshot`** — capture a state value before an action, assert after
  (see [§8 above](#8-withsnapshot-for-beforeafter-assertions))

