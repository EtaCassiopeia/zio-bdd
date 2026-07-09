# State Management Reference

This document explains how zio-bdd stores and evolves per-scenario state, and
how to choose between the three available approaches.

---

## 1. The state type `S`

Every `ZIOSteps[R, S]` suite has a single state type `S` that is scoped to one
scenario.  A fresh instance of `S` (its default value) is created before a
scenario's first step runs.  State mutations made by one step are visible to all
subsequent steps in the same scenario, but are never visible to other scenarios.

State is held in a `FiberRef[S]`, which means:

- The state is fiber-local — no sharing with parallel scenarios.
- `ZIO.acquireRelease` does not interfere with state; the `Scope` and `State[S]`
  services are independent.
- Forking a fiber with `.fork` or `.forkDaemon` shares the current state value
  but changes in the child do not propagate back to the parent.

### Providing the default value with `Default[S]`

`ZIOSteps` requires a `Default[S]` instance in implicit scope.  In the common
case the instance is derived automatically from a `Schema[S]` that has default
values on all fields:

```scala
case class AppState(
  userId: String                  = "",
  lastResponse: Option[HttpResp]  = None,
  balance: BigDecimal             = BigDecimal(0)
)

given Schema[AppState] = DeriveSchema.gen[AppState]
// Default[AppState] is derived automatically from the Schema
```

If the schema has no default value for a field the derivation fails at suite
startup with a clear message:

```
Cannot derive Default for ...: field 'balance' has no default value.
Hint: add default values to all fields of your state case class,
      or provide an explicit Default[T] instance.
```

To provide the default manually (useful when the type is not a case class):

```scala
given Default[AppState] = Default.from(AppState(userId = "unknown"))
```

---

## 2. Reading and writing state in step bodies

Step bodies have `State[S]` in their ZIO environment.  `ZIOSteps` provides the
`ScenarioContext` object as a thin façade:

```scala
object ScenarioContext:
  def get: ZIO[State[S], Nothing, S]
  def update(f: S => S): ZIO[State[S], Nothing, Unit]
```

Example:

```scala
Given("a user named " / string) { (name: String) =>
  ScenarioContext.update(_.copy(userName = name))
}

When("the user is greeted") {
  for {
    ctx <- ScenarioContext.get
    _   <- ZIO.service[GreetingService].flatMap(_.greet(ctx.userName))
    _   <- ScenarioContext.update(_.copy(greeted = true))
  } yield ()
}

Then("the user should be greeted") {
  ScenarioContext.get.flatMap(s =>
    Assertions.assertTrue(s.greeted, "expected greeted = true")
  )
}
```

`ScenarioContext.get` reads the full state.  `ScenarioContext.update` applies a
pure function and replaces the state.  There is no mutable cell exposed — all
updates are via the ZIO `FiberRef` mechanism.

### Under the hood

`State[S]` is a minimal service (`get: UIO[S]`, `update: (S => S) => UIO[Unit]`).
`ZIOSteps[R, S]` mixes in the `StateOps[S]` trait, which is what defines the
`ScenarioContext` object shown above — it is not special-cased per suite, it is
generated once from `S` by `StateOps`. The `State[S]` layer itself is built
per-scenario-attempt from a fresh `FiberRef[S]` via `State.layer(fiberRef)`
(see `ScenarioExecutor`), so `ScenarioContext.get`/`update` always resolve to
that scenario's own `FiberRef`.

---

## 3. `StepIO[+A]` for value-producing helpers

When a helper method needs to return a value to the caller (rather than just
updating state), annotate it with `StepIO[+A]`:

```scala
private def fetchAccountBalance(id: AccountId): StepIO[BigDecimal] =
  ZIO.service[AccountService]
    .flatMap(_.getBalance(id))

Then("the balance is " / bigDecimal) { (expected: BigDecimal) =>
  for {
    ctx     <- ScenarioContext.get
    balance <- fetchAccountBalance(ctx.accountId)
    _       <- Assertions.assertEquals(balance, expected)
  } yield ()
}
```

`StepIO[+A]` is the alias `ZIO[R & State[S], Throwable, A]`.  It cannot be
used outside a step body or a method that is called from within one, because
it requires both the `R` environment and `State[S]` to be present.

---

## 4. `TypeMap`: modular per-module state

### The problem

With a single monolithic case class as `S`, every step module must know the full
shape of the state.  Adding a field requires modifying the shared class and
touching every place that calls `.copy(...)`.  Unrelated modules become coupled
through the shared state type.

### What TypeMap does

`TypeMap` is a type-indexed heterogeneous map.  It allows each step module to
own a private state slice, keyed by the izumi `Tag[A]` of the slice type.
Modules cannot accidentally read or overwrite each other's data.

When `S = TypeMap`, the suite state is a single `TypeMap` value, but each
module interacts only with its own slice.

### Declaring a slice

Each module declares a small case class for its own state:

```scala
object ProvisionCtx:
  final case class Data(
    arid: String            = "",
    transactionId: String   = ""
  )
  // The Tag is required — izumi-reflect derives it automatically
  // for concrete types; an explicit given avoids macro overhead
  given Tag[Data] = Tag[Data]
```

The slice class should have default values on all fields so that
`Default[Data]` is derivable from the schema (or provide one explicitly).

### Reading and writing a slice

Use `TypeMapOps` in step bodies:

```scala
import zio.bdd.core.step.{TypeMap, TypeMapOps}

trait ProvisionSteps { self: ZIOSteps[R, TypeMap] =>

  Given("a provision request is prepared") {
    TypeMapOps.update[ProvisionCtx.Data](_.copy(transactionId = UUID.randomUUID().toString))
  }

  Then("the provision response is stored") {
    for {
      data <- TypeMapOps.getOrDefault[ProvisionCtx.Data]
      _    <- Assertions.assertTrue(data.arid.nonEmpty)
    } yield ()
  }
}
```

The available operations:

| Method | Type | Description |
|--------|------|-------------|
| `TypeMapOps.get[A]` | `ZIO[State[TypeMap], Nothing, Option[A]]` | Returns `None` if the slice has never been set |
| `TypeMapOps.getOrDefault[A]` | `ZIO[State[TypeMap], Nothing, A]` | Returns `Default[A].default` when not set |
| `TypeMapOps.update[A](f)` | `ZIO[State[TypeMap], Nothing, Unit]` | Applies `f` to the current value; uses default when not set |
| `TypeMapOps.set[A](v)` | `ZIO[State[TypeMap], Nothing, Unit]` | Replaces the slice unconditionally |

### Pure TypeMap operations

`TypeMap` itself is also usable as a plain immutable value outside of ZIO
effects:

```scala
val m: TypeMap = TypeMap.empty
  .put(ProvisionCtx.Data(arid = "AR-123"))
  .put(PaymentCtx.Data(amount = BigDecimal("100.00")))

val provData: Option[ProvisionCtx.Data] = m.get[ProvisionCtx.Data]
```

### Setting up `Default[TypeMap]`

`TypeMap.empty` is the correct default — all slices start absent (accessed via
`getOrDefault` which falls back to `Default[A].default` for each slice):

```scala
given Default[TypeMap] = Default.from(TypeMap.empty)
```

This given must be **in scope at the `extends ZIOSteps[R, TypeMap]` site** — the
`Default[S]` context bound is resolved when the suite's superclass is constructed,
*before* the suite body. Put it at the top level of the suite's file, in a shared
object you `import`, or in a base trait — **not** inside the suite object's body,
where it would not yet be visible to the `ZIOSteps[R, TypeMap]` constructor.

---

## 5. `HasLens`: focused updates on a structured state

### The problem

When `S` is a nested case class hierarchy, updating a deeply-nested field
produces verbose `.copy` chains:

```scala
// Before: three levels of copy for one field change
ScenarioContext.update(s => s.copy(
  core = s.core.copy(
    provision = s.core.provision.copy(response = Some(resp))
  )
))
```

`HasLens[S, A]` abstracts the getter/setter pair so step code can work
directly with the sub-state slice `A` without knowing how it is nested inside
`S`.

### Defining a lens

```scala
import zio.bdd.core.step.HasLens

// Inline in the step module, or in a shared companion
given HasLens[AppState, ProvisionState] =
  HasLens(_.provision, (s, a) => s.copy(provision = a))
```

`HasLens.apply` takes two functions: a getter (`S => A`) and a setter
`((S, A) => S)`.  These are plain functions — no macro, no code generation.

For users who already have Monocle lenses, adapt them with:

```scala
given HasLens[AppState, ProvisionState] =
  HasLens.fromMonocleLike(
    getter = _.provision,
    setter = a => s => s.copy(provision = a)
  )
```

> **Dependency-free by design.** zio-bdd does not depend on Monocle at all —
> `fromMonocleLike` only *adapts* an existing `monocle.Lens[S, A]` via plain
> getter/setter functions, for callers whose own project already pulls in
> `monocle-core`. `HasLens.apply` (above) is a complete, standalone
> implementation; Monocle is never required to use `HasLens`.

### `ScenarioLens` ZIO effects

With a `HasLens[S, A]` in implicit scope, the `ScenarioLens` object provides
focused ZIO effects:

```scala
import zio.bdd.core.step.ScenarioLens

// After: clean, one-line focused update
ScenarioLens.update[AppState, ProvisionState](_.copy(response = Some(resp)))
```

The available operations:

| Method | Description |
|--------|-------------|
| `ScenarioLens.get[S, A]` | Read the `A` slice from `State[S]` |
| `ScenarioLens.update[S, A](f)` | Apply `f` to the `A` slice, write back into `S` |
| `ScenarioLens.set[S, A](v)` | Replace the `A` slice unconditionally |

All three require `State[S]` in environment and a `HasLens[S, A]` given.

### Composing lenses with `andThen`

Two lenses can be composed to focus deeper:

```scala
val l1: HasLens[Level1, Level2] = HasLens(_.l2, (s, a) => s.copy(l2 = a))
val l2: HasLens[Level2, Level3] = HasLens(_.l3, (s, a) => s.copy(l3 = a))

val composed: HasLens[Level1, Level3] = l1.andThen(l2)

// Now updates Level3 through Level1 in one call
given HasLens[Level1, Level3] = composed
ScenarioLens.update[Level1, Level3](_.copy(x = 99))
```

`andThen` composition can chain to arbitrary depth.

---

## 6. Choosing the right approach

| Situation | Recommended approach |
|-----------|----------------------|
| Small suite, single module, few shared fields | Monolithic `S` case class with `ScenarioContext` |
| Multi-module suite; modules should not know each other's fields | `TypeMap` with per-module slices |
| Monolithic `S` with deeply nested fields; too many `.copy` layers | `HasLens` + `ScenarioLens` |
| Hybrid: TypeMap for isolation, lens for readability on one slice | `HasLens[TypeMap, A]` (focusing into a TypeMap slice) |
| Pass a value from a `Given` to a `When` without adding it to `S` | `Stage` (no Schema required) |
| Share a value across all scenarios in a feature, reset between features | `FeatureContext` (no Schema required) |

**Monolithic `S`** is the simplest starting point.  It works well when the
number of fields is small and step modules are not expected to evolve
independently.

**`TypeMap`** scales to large suites with many independent step modules.  Each
module declares its own slice; there is no shared case class to maintain.  The
trade-off is that `TypeMapOps.getOrDefault` returns an `Option` (or falls back
to the default) rather than a direct field access.

**`HasLens`** does not replace the state representation — it is a navigation
aid on top of whatever `S` is.  Use it when the state is structured but the
update expressions have become unreadable.

These three approaches can be combined: a `TypeMap`-backed suite can still use
`ScenarioLens` to update a slice elegantly, and a monolithic `S` suite can use
a lens alongside direct `ScenarioContext.update` calls wherever convenient.

---

## 7. `Stage`: per-scenario staging without Schema

`Stage` is a per-scenario typed store for passing transient data between `Given`
and `When` steps.  Unlike `ScenarioContext[S]`, it does **not** require a
`Schema[T]` — types are keyed by `ClassTag` at runtime.  Values staged in one
step are visible to all subsequent steps in the same scenario and are cleared
automatically when the scenario ends.

Use `Stage` for pipeline data that has no business meaning in `S`:
typed domain events, intermediate transformation results, tokens assembled
across several `Given` steps.

```scala
// Stage a domain object in a Given step (no Schema required):
Given("a discount coupon exists") {
  for {
    coupon <- buildCoupon(code = "HALF50", discount = 0.5)
    _      <- Stage.put(coupon)
  } yield ()
}

// Retrieve it in the When step — falls back to a fresh default if no Given ran:
When("the coupon is applied") {
  Stage.getOrElse(Coupon.default).flatMap { coupon =>
    applyDiscount(coupon)
  }
}
```

### Anti-pattern: relay fields in `S`

A common pitfall is adding `payload: String`, `correlationId: String`, etc. to
`S` solely to pass data from a `Given` step to the following `When` step.  These
relay fields grow with every step domain, force every sub-state to duplicate them,
and add fake fields to `Schema[S]`:

```scala
// Bad — relay fields in S, requires Schema
case class AppState(
  orderPayload: String = "",      // relay only — no assertion value
  orderCorrelationId: String = "" // relay only
)

Given("a valid order") {
  for {
    order <- buildOrder(...)
    _ <- ScenarioContext.update(s => s.copy(
      orderPayload = order.payload,
      orderCorrelationId = order.correlationId
    ))
  } yield ()
}

When("the order is submitted") {
  for {
    s <- ScenarioContext.get
    _ <- submit(s.orderPayload, s.orderCorrelationId)
  } yield ()
}
```

Use `Stage` instead — the `Order` type does not need a `Schema` instance:

```scala
// Good — Stage carries the event, S is free of relay fields
Given("a valid order") {
  for {
    order <- buildOrder(...)
    _     <- Stage.put(order)
  } yield ()
}

When("the order is submitted") {
  Stage.getOption[Order].flatMap {
    case Some(order) => submit(order.payload, order.correlationId)
    case None        => submit(Order.default.payload, Order.default.correlationId)
  }
}
```

### API

| Method | Type | Description |
|--------|------|-------------|
| `Stage.put[A: ClassTag](a)` | `UIO[Unit]` | Store `a`. Overwrites any previously staged value of the same type. |
| `Stage.get[A: ClassTag]` | `IO[StagingError, A]` | Retrieve the staged value. Fails with `StagingError.NotFound` if absent. |
| `Stage.getOrElse[A: ClassTag](default)` | `UIO[A]` | Return the staged value, or `default` if not present. |
| `Stage.getOption[A: ClassTag]` | `UIO[Option[A]]` | Return `Some(a)` if staged, `None` otherwise. |
| `Stage.modify[A: ClassTag](f)` | `UIO[Unit]` | Apply `f` to the staged value if present; no-op otherwise. |
| `Stage.remove[A: ClassTag]` | `UIO[Unit]` | Remove the staged value. |
| `Stage.stepLabel` | `UIO[String]` | The Gherkin pattern of the currently executing step (e.g. `"When a provision request is sent"`); `""` outside a step body. |

`StagingError` variants and their `.message` text:

| Variant | `.message` |
|---------|------------|
| `NotFound(typeName)` | `No staged value of type $typeName. Call Stage.put before Stage.get.` |
| `TypeMismatch(typeName, actualType)` | `Staged value for $typeName was actually $actualType.` |

```scala
Stage.get[Coupon].catchAll {
  case StagingError.NotFound(typeName) =>
    ZIO.fail(new RuntimeException(s"expected a staged $typeName"))
  case StagingError.TypeMismatch(typeName, actualType) =>
    ZIO.fail(new RuntimeException(s"staged $typeName was actually $actualType"))
}
```

No environment type is required — all `Stage` methods run in `UIO` or `IO[StagingError, _]`
and are available inside any step body without additional layer wiring.

---

## 8. `FeatureContext`: per-feature staging without Schema

`FeatureContext` fills the gap between scenario-scoped state (`S` or `Stage`) and
suite-scoped environment (`R`).  Values stored in `FeatureContext` persist across
**all scenarios within one feature** and are cleared automatically before each new
feature begins.

Typical use cases: a shared account provisioned once for the feature, a session
token obtained in a `Background` step, a feature-level counter.

```scala
Given("an account is created for the feature") {
  createAccount().flatMap(id => FeatureContext.put(AccountId(id)))
}

// In every subsequent scenario in the same Feature block:
When("a transaction is posted") {
  FeatureContext.get[AccountId].flatMap(id => postTransaction(id))
}
```

### API

`FeatureContext` has an identical API to `Stage`:

| Method | Type | Description |
|--------|------|-------------|
| `FeatureContext.put[A: ClassTag](a)` | `UIO[Unit]` | Store `a` for the remainder of the feature. |
| `FeatureContext.set[A: ClassTag](a)` | `UIO[Unit]` | Alias for `put`. |
| `FeatureContext.get[A: ClassTag]` | `IO[FeatureContextError, A]` | Retrieve stored value; fails if absent. |
| `FeatureContext.getOrElse[A: ClassTag](default)` | `UIO[A]` | Return stored value, or `default`. |
| `FeatureContext.getOption[A: ClassTag]` | `UIO[Option[A]]` | Return `Some(a)` or `None`. |
| `FeatureContext.modify[A: ClassTag](f)` | `UIO[Unit]` | Update in place if present. |
| `FeatureContext.remove[A: ClassTag]` | `UIO[Unit]` | Remove the stored value. |

`FeatureContextError` variants and their `.message` text:

| Variant | `.message` |
|---------|------------|
| `NotFound(typeName)` | `No feature-scoped value of type $typeName. Call FeatureContext.put before FeatureContext.get.` |
| `TypeMismatch(typeName, actualType)` | `Feature-scoped value for $typeName was actually $actualType.` |

```scala
FeatureContext.get[AccountId].catchAll {
  case FeatureContextError.NotFound(typeName) =>
    ZIO.fail(new RuntimeException(s"expected a feature-scoped $typeName"))
  case FeatureContextError.TypeMismatch(typeName, actualType) =>
    ZIO.fail(new RuntimeException(s"feature-scoped $typeName was actually $actualType"))
}
```

### Lifetime summary

| Store | Reset when | Visible to |
|-------|-----------|------------|
| `Stage` | Start of each scenario | Steps in the same scenario |
| `FeatureContext` | Start of each feature | All scenarios in the same feature |
| `ScenarioContext[S]` | Start of each scenario | Steps in the same scenario |
| ZLayer (`R`) | Once per suite run | All steps in all features |

### Implementation note: how clearing actually happens

Both stores are cleared via `Ref.locallyScoped(Map.empty)`, not by an explicit
"clear at the end" call:

- `ScenarioExecutor` scopes `Stage.ref` (and `Stage.currentStepLabel`) with
  `Stage.ref.locallyScoped(Map.empty)` at the start of each scenario *attempt*,
  **before** `beforeScenarioHook` runs.
- `FeatureExecutor` installs a fresh per-feature store with
  `FeatureContext.freshScope` at the start of each feature, **before**
  `beforeFeatureHook` runs.

`locallyScoped` sets the `FiberRef` for the lifetime of the enclosing
`ZIO.scoped` block and restores the previous value when that scope closes — this
is what guarantees a retried scenario attempt, or the next scenario/feature,
starts from a clean store even if the previous attempt was interrupted
mid-step. `FeatureContext.freshScope` allocates a new synchronized cell and
`locallyScoped`s *that*, so each feature — including parallel features — is
isolated while the scenarios within one feature share a single cell.

The `private[bdd] def reset` method on both `Stage` and `FeatureContext` is
**test-only** — it exists for unit-testing the stores in isolation and is
never called by `ScenarioExecutor` or `FeatureExecutor`. Do not rely on it to
understand production clearing behavior; the `locallyScoped` calls above are
the actual production clearing path.

### Concurrency: `FeatureContext` under `scenarioParallelism > 1`

`FeatureContext.ref` is a `FiberRef[Ref.Synchronized[Map[String, Any]]]` — the
`FiberRef` holds a *reference* to a per-feature synchronized cell, not the map
itself. When a suite runs with `scenarioParallelism > 1`, `FeatureExecutor`
executes sibling scenarios of the same feature as **true parallel fibers**
(`ZIO.foreachExec` with `ExecutionStrategy.ParallelN`), all forked from the same
parent fiber.

Because the fibers share the same underlying `Ref.Synchronized` instance (fork
copies the reference, not the cell), `FeatureContext.put`/`modify` from different
scenario fibers are **synchronized and compose** rather than clobbering each
other. Accumulation patterns — e.g. a shared counter incremented by every
scenario in the feature — work correctly under parallel execution.

Cross-feature isolation is preserved: `FeatureContext.freshScope` installs a
*new* cell per feature, so parallel features never share state.

---

## 9. Ergonomic patterns: reducing `ScenarioContext.get` noise

### 9.1 State-injecting step variants (`GivenS` / `WhenS` / `ThenS` / `AndS` / `ButS`)

The most common friction in step bodies is the mandatory `s <- ScenarioContext.get`
required before any state read, even when the step only needs a snapshot at the
start and never re-reads.

`GivenS` / `WhenS` / `ThenS` / `AndS` / `ButS` inject the current state as the
**first curried argument**, removing that boilerplate:

```scala
// Before — ScenarioContext.get is pure ceremony when it's the first line
Given("an order is placed") {
  for {
    s     <- ScenarioContext.get
    order <- buildOrder(s.cart.items)
    _     <- ScenarioLens.update[AppState, OrderState](_.copy(lastOrder = Some(order)))
  } yield ()
}

// After — s is injected, no explicit get needed
GivenS("an order is placed") { s =>
  for {
    order <- buildOrder(s.cart.items)
    _     <- ScenarioLens.update[AppState, OrderState](_.copy(lastOrder = Some(order)))
  } yield ()
}
```

For steps with Gherkin parameters, the state comes **first** (curried), then
the extracted parameters:

```scala
// 1 Gherkin param
ThenS("the response status is " / int) { s => (expected: Int) =>
  assertTrue(s.http.statusCode == expected, s"Expected $expected, got ${s.http.statusCode}")
}

// 2 Gherkin params
WhenS("send " / int / " requests to " / string) { s => (count: Int, url: String) =>
  ZIO.replicateZIODiscard(count)(sendRequest(s.session.userId, url))
}
```

**When to use `GivenS` / `WhenS` / `ThenS`:**
- The step body reads state at the top AND performs effects — most `Given`/`When` steps.
- The step body is a pure assertion on state — all `Then`/`And` steps.

**When to keep `Given` / `When` / `Then`:**
- The step body does NOT read state at all: `Given("a fresh session") { ZIO.unit }`.
- You need to re-read state **after** an intermediate update within the step body:
  use an explicit `ScenarioContext.get` at the point where the updated state is needed.

### 9.2 `ScenarioLens.get` for read-only slice access

Instead of reading the full `ScenarioState` when only one sub-state is needed:

```scala
// Verbose — reads full state, uses only .http
for {
  s <- ScenarioContext.get
  _ <- assertTrue(s.http.statusCode == 200, ...)
} yield ()

// Concise — reads only the response slice
ScenarioLens.get[AppState, HttpState].flatMap { http =>
  assertTrue(http.statusCode == 200, ...)
}
```

Requires a `given HasLens[AppState, HttpState]` in scope (see section 5).

### 9.3 `.flatMap` instead of for-comprehension for single-read assertions

A for-comprehension where `ScenarioContext.get` is the only binding and the body
is a single assertion can be collapsed to a one-liner:

```scala
// Instead of:
for {
  s <- ScenarioContext.get
  _ <- assertTrue(s.http.body.nonEmpty, "expected non-empty body")
} yield ()

// Write:
ScenarioContext.get.flatMap(s => assertTrue(s.http.body.nonEmpty, "expected non-empty body"))
```

### 9.4 `withSnapshot` for before/after comparisons

When a step captures a value before an action and asserts it changed, use
`withSnapshot` to avoid manual state threading:

```scala
// Verbose — manually saves before, reads after
When("the order is confirmed") {
  for {
    before <- ScenarioContext.get.map(_.cart.itemCount)
    _      <- confirmOrder(...)
    after  <- ScenarioContext.get.map(_.cart.itemCount)
    _      <- assertTrue(after == 0, "cart should be empty after confirm")
  } yield ()
}

// Cleaner — withSnapshot captures the lens value before the body runs
When("the order is confirmed") {
  withSnapshot(_.cart.itemCount) { before =>
    for {
      _     <- confirmOrder(...)
      after <- ScenarioContext.get.map(_.cart.itemCount)
      _     <- assertTrue(after < before, "cart should shrink after confirm")
    } yield ()
  }
}
```

`withSnapshot(lens)(body)` is defined in `ZIOSteps` — it reads `lens(currentState)`
once before `body` executes and passes the snapshot to `body`.

```scala
def withSnapshot[A](lens: S => A)(
  body: A => ZIO[R & State[S] & Scope, Throwable, Unit]
): ZIO[R & State[S] & Scope, Throwable, Unit]
```

Note the `Scope` requirement in both the `body` parameter and the return type.
Inside an ordinary step body this is satisfied automatically — `ScenarioExecutor`
already runs each scenario attempt inside a `ZIO.scoped` block — so no extra
wiring is needed when `withSnapshot` is called from `Given`/`When`/`Then`.
Calling `withSnapshot` from outside that scoped context (e.g. invoking a step
helper directly from a unit test) requires providing `Scope` explicitly, such
as by wrapping the call in `ZIO.scoped { ... }`; otherwise compilation will
fail with an unsatisfied `Scope` requirement in the environment.
