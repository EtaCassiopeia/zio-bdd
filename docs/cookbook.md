# Cookbook: Patterns and Best Practices

This page answers the questions that come up most often when writing real-world test suites
with zio-bdd.  Each section is a self-contained recipe.

---

## Table of contents

1. [Structuring a multi-module suite](#1-structuring-a-multi-module-suite)
2. [Choosing a state strategy](#2-choosing-a-state-strategy)
3. [Passing data from Given to When (Stage vs ScenarioContext)](#3-passing-data-from-given-to-when)
4. [Adding a new step to an existing suite](#4-adding-a-new-step-to-an-existing-suite)
5. [Testing HTTP APIs](#5-testing-http-apis)
6. [Asserting on response status and body](#6-asserting-on-response-status-and-body)
7. [Sharing setup across scenarios in a feature](#7-sharing-setup-across-scenarios-in-a-feature)
8. [Date-relative step parameters](#8-date-relative-step-parameters)
9. [Optional step parameters](#9-optional-step-parameters)
10. [Combining optional and required parameters](#10-combining-optional-and-required-parameters)
11. [Steps that produce values for later steps](#11-steps-that-produce-values-for-later-steps)
12. [Writing soft assertions](#12-writing-soft-assertions)
13. [Tagging scenarios for selective runs](#13-tagging-scenarios-for-selective-runs)
14. [Using GivenS / WhenS / ThenS vs Given / When / Then](#14-using-givens--whens--thens-vs-given--when--then)

---

## 1. Structuring a multi-module suite

For suites with many step definitions, split them into focused traits mixed into one object.

```scala
// One trait per domain area
trait ProvisionSteps { self: ZIOSteps[AppEnv, AppState] =>
  GivenS("a valid provision request is prepared") { s =>
    for {
      event <- ProvisionGenerator.build(s.core.accountRefId)
      _     <- Stage.put(event)
    } yield ()
  }
}

trait PostSteps { self: ZIOSteps[AppEnv, AppState] =>
  WhenS("a post request is sent") { s =>
    for {
      event <- Stage.getOrElse(PostEvent.default)
      resp  <- ZIO.service[ApiClient].flatMap(_.post(event))
      _     <- ScenarioLens.update[AppState, HttpState](_.copy(
                 statusCode = resp.status, body = resp.body
               ))
    } yield ()
  }
}

// Suite assembles the traits
@Suite(featureDirs = Array("src/test/resources/features"))
object ComponentSuite
    extends ZIOSteps[AppEnv, AppState]
    with SuiteHooks
    with ProvisionSteps
    with PostSteps
    with AssertionSteps:

  override def globalLayer: ZLayer[Any, Throwable, AppEnv] =
    AppEnv.layer
```

**Rules:**
- Every step trait must have the self-type `{ self: ZIOSteps[R, S] => }`.
- All traits mixed into the same suite must share the same `R` and `S` types.
- Put lifecycle hooks in a separate `SuiteHooks` trait to keep step traits focused on steps.
- Put `globalLayer` / `featureLayer` / `scenarioLayer` in the suite object, not in traits.

---

## 2. Choosing a state strategy

Use this table to pick the right approach before writing any step code:

| Situation | Recommended |
|-----------|-------------|
| Small suite with few shared fields; one developer maintains it | Monolithic `case class AppState(...)` with `ScenarioContext` |
| Multiple independent step modules; modules should not import each other's types | `TypeMap` with per-module slices |
| Monolithic state with deeply nested sub-states (3+ levels of `.copy`) | `HasLens` / `ScenarioLens` over the monolithic type |
| You need to pass a generated event payload from a `Given` to a `When` | `Stage.put` / `Stage.getOption` |
| You need a value set once for all scenarios in a feature (e.g. a shared account) | `FeatureContext.put` / `FeatureContext.get` |
| All scenarios are independent and can run in any order | Any of the above — they all isolate per-scenario by default |

### Decision rule for `Stage` vs `ScenarioContext`

> **Stage** — use for *pipeline data*: generated events, assembled payloads, request objects.
> They are ephemeral, exist only to flow from `Given` to `When`, and have no business
> meaning in the scenario's final state.  `Schema[T]` is not required.

> **ScenarioContext** — use for *decision flags and assertions*: values that `Then` steps
> need to verify, counters, status codes, IDs that drive assertions.  Requires `Schema[S]`
> (usually derived automatically from a case class with default values).

A field in `ScenarioContext` that exists purely to pass data from a `Given` to a `When`
and is never read by a `Then` step is a relay field — move it to `Stage`.

---

## 3. Passing data from Given to When

### Pattern A — `Stage.put` (recommended for event payloads)

```scala
// Given step generates and stages the event
GivenS("a valid order is prepared") { s =>
  for {
    order <- OrderGenerator.build(s.core.accountRefId, s.core.openDate)
    _     <- Stage.put(order)   // Stage does not require Schema[Order]
  } yield ()
}

// When step retrieves it
When("the order is submitted") {
  for {
    order <- Stage.getOrElse(Order.default)
    resp  <- ZIO.service[ApiClient].flatMap(_.submit(order))
    _     <- ScenarioLens.update[AppState, HttpState](_.copy(
               statusCode = resp.status, body = resp.body
             ))
  } yield ()
}
```

`Stage.put[A]` overwrites any previously staged value of the same type.  If you need to stage
two distinct events of the same class, wrap them in newtype wrappers:

```scala
case class ProvisionStaged(event: ProvisionEvent)
case class PostStaged(event: PostEvent)

Stage.put(ProvisionStaged(provEvent))
Stage.put(PostStaged(postEvent))
```

### Pattern B — `ScenarioContext.update` (for flags and IDs that drive later assertions)

```scala
GivenS("a provision request is sent") { _ =>
  for {
    resp  <- ZIO.service[ApiClient].flatMap(_.provision(ProvisionEvent.default))
    id     = resp.body("accountReferenceId")
    _     <- ScenarioLens.update[AppState, CoreState](_.copy(accountRefId = id))
    _     <- Stage.put(ProvisionStaged(resp))   // also stage for inspection if needed
  } yield ()
}

ThenS("the account reference id is not empty") { s =>
  Assertions.assertTrue(s.core.accountRefId.nonEmpty, "accountRefId should not be empty")
}
```

---

## 4. Adding a new step to an existing suite

1. Find the trait that covers the domain area (or create one if none fits).
2. Decide whether the step reads state (`GivenS`/`WhenS`/`ThenS`) or doesn't (`Given`/`When`/`Then`).
3. Decide what to extract from the Gherkin text and what to read from state.
4. Register the step; run `sbt test` to confirm it matches and passes.

```scala
// Adding a new step to PostSteps
trait PostSteps { self: ZIOSteps[AppEnv, AppState] =>

  // Existing steps ...

  // New step: configures the posting date relative to today
  GivenS("a post dated " / int / " days relative to current date") { s => (offset: Int) =>
    val date = LocalDate.now().plusDays(offset).toString
    ScenarioLens.update[AppState, CoreState](_.copy(postDate = date))
  }
}
```

Feature file:
```gherkin
Given a post dated -7 days relative to current date
```

If the step text already exists in the feature file, the
[zio-bdd-tooling](https://github.com/EtaCassiopeia/zio-bdd-tooling) LSP/editor extensions
surface a skeleton live as a completion item the moment you start typing inside a
`Given`/`When`/`Then(...)` call. `sbt zioBddSnippets` does the same from the command line, but
only inside this repository's own build (see `docs/running.md`). Run
`sbt "testOnly MySuite -- --dry-run"` to confirm all feature file steps match registered
definitions without executing them.

---

## 5. Testing HTTP APIs

Put the HTTP client in `R` and provide it via `globalLayer`.  The suite below uses a
generic `ApiClient` type — replace it with your actual HTTP backend.

```scala
trait ApiClient:
  def post(path: String, body: String): Task[HttpResponse]
  def get(path: String): Task[HttpResponse]

case class HttpResponse(status: Int, body: String)

@Suite(featureDirs = Array("src/test/resources/features"))
object ApiSuite extends ZIOSteps[ApiClient, HttpState]:

  override def globalLayer: ZLayer[Any, Throwable, ApiClient] =
    // Replace with your HTTP client layer, e.g.:
    // AsyncHttpClientZioBackend().map(LiveApiClient.apply)
    ZLayer.fromZIO(ZIO.succeed(new ApiClient { ... }))

  When("a POST request is sent to " / string) { (path: String) =>
    for {
      client <- ZIO.service[ApiClient]
      resp   <- client.post(path, "")
      _      <- ScenarioContext.update(_.copy(statusCode = resp.status, body = resp.body))
    } yield ()
  }

  Then("the response status is " / int) { (expected: Int) =>
    ScenarioContext.get.flatMap { s =>
      Assertions.assertEquals(s.statusCode, expected,
        s"Expected HTTP $expected, got ${s.statusCode}")
    }
  }
```

---

## 6. Asserting on response status and body

Keep HTTP response state in a dedicated sub-state slice:

```scala
case class HttpState(
  statusCode: Int      = 0,
  body:       String   = "",
  headers:    Map[String, String] = Map.empty
)

// Given lens defined once:
given HasLens[AppState, HttpState] = HasLens(_.http, (s, a) => s.copy(http = a))
```

Then assertion steps are one-liners:

```scala
// Assert only the status
ThenS("the API returns a " / int / " status code") { s => (expected: Int) =>
  Assertions.assertEquals(s.http.statusCode, expected,
    s"Expected $expected, got ${s.http.statusCode}")
}

// Assert status and body substring
ThenS("the API returns a " / int / " status code and " / string / " in the body") {
  s => (expectedStatus: Int, bodyFragment: String) =>
    Assertions.collectAll(
      Assertions.assertEquals(s.http.statusCode, expectedStatus,
        s"Expected status $expectedStatus, got ${s.http.statusCode}"),
      Assertions.assertTrue(s.http.body.contains(bodyFragment),
        s"Expected body to contain '$bodyFragment', got: ${s.http.body}")
    )
}
```

---

## 7. Sharing setup across scenarios in a feature

For a value that should be created once and shared across all scenarios in a feature
(e.g. an account provisioned in a `Background` step), use `FeatureContext`:

```scala
// Background step — runs once per scenario but FeatureContext.put is idempotent:
Given("an account exists for this feature") {
  FeatureContext.getOption[TestAccountId].flatMap {
    case Some(_) => ZIO.unit  // already provisioned in a prior scenario
    case None    =>
      for {
        id <- ZIO.service[AccountService].flatMap(_.provision())
        _  <- FeatureContext.put(TestAccountId(id))
      } yield ()
  }
}

// Any step in any scenario in the same feature can read it:
WhenS("a transaction is posted") { s =>
  for {
    accountId <- FeatureContext.get[TestAccountId]
    _         <- ZIO.service[ApiClient].flatMap(_.post(accountId.value, s.post.payload))
  } yield ()
}
```

`FeatureContext` is cleared before each new feature begins.  It does **not** persist across
features.  For suite-wide shared state, use a service in `R` via `globalLayer`.

---

## 8. Date-relative step parameters

Use `int` to capture a day offset and derive the actual date in the step body:

```gherkin
Given a post dated -7 days relative to current date
Given a post dated 0 days relative to current date
Given a post dated 30 days relative to current date
```

```scala
GivenS("a post dated " / int / " days relative to current date") { s => (offset: Int) =>
  val date = LocalDate.now().plusDays(offset).toString
  ScenarioLens.update[AppState, CoreState](_.copy(postDate = date))
}
```

A single step covers all offsets — no per-date duplicate registrations.

---

## 9. Optional step parameters

Use `optional(text)` when a step has a variant with an extra phrase:

```gherkin
Given a valid provision request
Given a valid provision request with the same simulationId
```

```scala
GivenS("a valid provision request" / optional(" with the same simulationId")) {
  s => (suffix: Option[String]) =>
    val reuseId = suffix.isDefined
    for {
      event <- ProvisionGenerator.build(s.core.accountRefId, reuseSimulationId = reuseId)
      _     <- Stage.put(event)
    } yield ()
}
```

`optional(text)` returns `Some(text)` when the phrase is present, `None` when absent.
Both Gherkin variants match the same step definition.

---

## 10. Combining optional and required parameters

Extractors are chained with `/` and compose freely.  An optional extractor can appear
anywhere in the chain:

```gherkin
Given a valid deposit body
Given a valid deposit body with the same simulationId
Given a valid deposit body with simulationId "abc-123"
```

```scala
// Variant A — optional literal phrase
GivenS("a valid deposit body" / optional(" with the same simulationId")) {
  s => (reuseId: Option[String]) =>
    for {
      event <- PostGenerator.build(s.core.accountRefId, reuseSimId = reuseId.isDefined)
      _     <- Stage.put(event)
    } yield ()
}

// Variant B — optional explicit ID
GivenS("a valid deposit body" / optional(" with simulationId " / string)) {
  // Note: optional with extractors requires regex() — use two separate steps instead
  // when the optional part itself captures a value.  See below.
}
```

When the optional part **captures a value** (like an ID string), it is cleaner to register
two separate steps rather than a single step with `optional()`:

```scala
// Two steps — clearer than one complex optional with a nested extractor
GivenS("a valid deposit body") { s =>
  for {
    event <- PostGenerator.build(s.core.accountRefId, simulationId = None)
    _     <- Stage.put(event)
  } yield ()
}

GivenS("a valid deposit body with simulationId " / string) { s => (simId: String) =>
  for {
    event <- PostGenerator.build(s.core.accountRefId, simulationId = Some(simId))
    _     <- Stage.put(event)
  } yield ()
}
```

---

## 11. Steps that produce values for later steps

When a `Given` or `When` step creates an entity and a `Then` step needs to verify it,
store the result in `ScenarioContext`:

```scala
WhenS("an account is provisioned") { _ =>
  for {
    resp <- ZIO.service[ApiClient].flatMap(_.provision(ProvisionEvent.default))
    id    = resp.body("accountReferenceId")
    _    <- ScenarioLens.update[AppState, CoreState](_.copy(
               accountRefId = id, statusCode = resp.status
             ))
  } yield ()
}

ThenS("the provisioned account id is not empty") { s =>
  Assertions.assertTrue(s.core.accountRefId.nonEmpty,
    s"Expected non-empty accountReferenceId, got empty")
}
```

For values that are only needed by the immediately following step (not by `Then` steps),
use `Stage.put` to avoid polluting `ScenarioContext`.

---

## 12. Writing soft assertions

When a step must verify several independent conditions, use `Assertions.collectAll` so all
failures are reported at once instead of stopping at the first:

```scala
ThenS("the response is well-formed") { s =>
  Assertions.collectAll(
    Assertions.assertEquals(s.http.statusCode, 200,
      s"Expected 200, got ${s.http.statusCode}"),
    Assertions.assertTrue(s.http.body.nonEmpty,
      "Response body must not be empty"),
    Assertions.assertTrue(s.http.body.contains("accountReferenceId"),
      "Response body must contain accountReferenceId")
  )
}
```

Without `collectAll`, a failing status check would hide the missing-field failures.
`collectAll` always evaluates every assertion before failing.

---

## 13. Tagging scenarios for selective runs

Use tags to categorise scenarios and control which ones run in CI vs locally:

```gherkin
@smoke
Scenario: Provision an account
  Given a valid provision request
  When the request is sent
  Then the response status is 200

@regression @slow
Scenario: Full lifecycle with EOD
  Given a provisioned account
  When EOD runs
  Then the balance is updated
```

Run only smoke tests:
```sh
sbt "testOnly MySuite -- --include-tags smoke"
```

Skip slow tests:
```sh
sbt "testOnly MySuite -- --exclude-tags slow"
```

Run a specific scenario by name:
```sh
sbt "testOnly MySuite -- --scenario-name 'Provision*'"
```

Annotation-level default for CI:
```scala
@Suite(
  featureDirs  = Array("src/test/resources/features"),
  includeTags  = Array("smoke"),
  parallelism  = 4
)
object CiSuite extends ZIOSteps[AppEnv, AppState]
```

Mark work-in-progress with `@ignore` to keep the scenario visible in reports without
failing the build:

```gherkin
@ignore
Scenario: Not yet implemented
  Given a step that is not ready yet
```

---

## 14. Using `GivenS` / `WhenS` / `ThenS` vs `Given` / `When` / `Then`

The `*S` variants inject the current scenario state as the **first curried argument**,
eliminating the `s <- ScenarioContext.get` boilerplate.

**Use `GivenS` / `WhenS` / `ThenS` when:**
- The step reads state at the start before doing any work (most `Given` and `When` steps).
- The step body is a pure assertion that reads state (most `Then` and `And` steps).

**Use `Given` / `When` / `Then` when:**
- The step does not read state at all: `Given("a fresh context") { ZIO.unit }`.
- The step needs to read state **after** an intermediate update (use explicit `ScenarioContext.get`
  at that point).

```scala
// Reads state at entry → use GivenS
GivenS("a post event is prepared") { s =>
  for {
    event <- PostGenerator.build(s.core.accountRefId)
    _     <- Stage.put(event)
  } yield ()
}

// Does not read state → use Given
Given("the feature flag is disabled") {
  ZIO.serviceWithZIO[FlagConfig](_.set("rateLimiting", false))
}

// Assertion on state → use ThenS
ThenS("the response status is " / int) { s => (expected: Int) =>
  Assertions.assertEquals(s.http.statusCode, expected,
    s"Expected $expected, got ${s.http.statusCode}")
}

// Needs state AFTER an update → use explicit ScenarioContext.get
When("an account is provisioned and the id is captured") {
  for {
    resp <- ZIO.service[ApiClient].flatMap(_.provision())
    _    <- ScenarioContext.update(_.copy(accountRefId = resp.id))
    s2   <- ScenarioContext.get   // read the updated state
    _    <- ZIO.logInfo(s"Provisioned: ${s2.accountRefId}")
  } yield ()
}
```

For `*S` steps with Gherkin parameters, state is curried first:

```scala
// 1 param: s => (param) => effect
ThenS("the account balance is " / bigDecimal) { s => (expected: BigDecimal) =>
  Assertions.assertEquals(s.account.balance, expected)
}

// 2 params: s => (p1, p2) => effect
WhenS("transfer " / bigDecimal / " from " / string) { s => (amount: BigDecimal, target: String) =>
  ZIO.service[AccountService].flatMap(_.transfer(s.core.accountRefId, target, amount))
}
```

---

## Quick reference: which API does what

| Need | Use |
|------|-----|
| Read scenario state in a step body | `ScenarioContext.get` or `GivenS`/`WhenS`/`ThenS` injection |
| Update scenario state | `ScenarioContext.update(_.copy(...))` or `ScenarioLens.update[S, A](...)` |
| Pass event payload from Given to When | `Stage.put(event)` / `Stage.getOrElse(Event.default)` |
| Share a value across scenarios in a feature | `FeatureContext.put(v)` / `FeatureContext.get[T]` |
| Access a service inside a step body | `ZIO.service[MyService]` |
| Acquire a resource that auto-releases at scenario end | `ZIO.acquireRelease(acquire)(release).unit` |
| Mark a step as not yet implemented | `pending("reason")` |
| Verify several assertions at once | `Assertions.collectAll(a1, a2, a3)` |
| Capture state before an operation for comparison | `withSnapshot(_.field) { before => ... }` |
| Run setup before every scenario | `beforeScenario { _ => ... }` |
| Run setup only for tagged scenarios | `beforeScenarioTagged("tag") { meta => ... }` |
