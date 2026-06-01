# Environment and Layers Reference

This document explains how to wire services into zio-bdd step bodies using
the `R` type parameter, the three-tier layer model, per-scenario overrides,
resource scopes, flag-matrix layers, and the `HasService` typeclass for
reusable step libraries.

---

## 1. The `R` type parameter

`ZIOSteps[R, S]` has two type parameters:

- `S` — the per-scenario state type (covered in `state.md`)
- `R` — the ZIO environment type whose services are available inside every step
  body

`R` is declared once per suite and is typically a type intersection of all the
services the suite's steps need:

```scala
type AppEnv = HttpClient & AccountService & DynamoDB

object MySuite extends ZIOSteps[AppEnv, AppState]:
  Given("an account exists") {
    ZIO.service[AccountService].flatMap(_.create(AccountId.random))
  }
```

Step bodies can access services via `ZIO.service[A]` when `A` is part of `R`.
Services are not injected via constructor parameters into step methods — they
are looked up from the environment inside each step body as needed.

The type `R` flows through all three tiers of the layer model described below.
Everything you provide via `environment`, `featureLayer`, or `scenarioLayer`
must collectively satisfy `R`.

---

## 2. The three-tier environment model

The framework builds an environment in three distinct tiers.  Override the
methods that apply to your use case; leave the rest at their defaults.

```
globalLayer        built once per JVM process
  featureLayer     rebuilt once per Feature file
    scenarioLayer  rebuilt once per Scenario
```

Each tier delegates to the one above it by default, so overriding only
`environment` (the legacy single-tier entry point) continues to work unchanged.

### Tier 1: `globalLayer`

Built once when the suite first starts and shared for the entire JVM lifetime.
Use it for expensive shared resources — connection pools, embedded databases,
in-process gRPC servers — that should start once and be reused across all
features and scenarios.

```scala
override def globalLayer: ZLayer[Any, Throwable, R] =
  sharedConnectionPool
```

Default: delegates to `featureLayer`.

### Tier 2: `featureLayer`

Built once per `Feature` execution.  Suitable for resources that should be
fresh per feature but shared across the scenarios within that feature.

```scala
override def featureLayer: ZLayer[Any, Throwable, R] =
  sharedConnectionPool >>> freshSchemaPerFeature
```

Default: delegates to `environment` (the legacy single-tier entry point).

### Tier 3: `scenarioLayer`

Built once per `Scenario`.  The `ScenarioMetadata` argument carries the
scenario name, tags, file, line number, and (if flag expansion is active) the
current flag values.  Use this for conditional layer selection based on tags:

```scala
override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, R] =
  if (meta.tags.contains("use-mock")) mockHttpLayer
  else realHttpLayer
```

Default: delegates to `featureLayer`.

### `environment` — the legacy single-tier entry point

Suites that do not need tier differentiation can override `environment` alone:

```scala
override def environment: ZLayer[Any, Throwable, R] =
  ZLayer.succeed(Config("Hello")) >>> GreetingService.live
```

All three tier methods default to delegating through to `environment`, so
existing suites need no changes.

**Rule:** Override `environment` OR the three-tier methods.  Do not mix both —
overriding `featureLayer` without also overriding `environment` means the two
are decoupled, which causes confusing behaviour where the `environment` override
is ignored.

---

## 3. Per-scenario layer overrides

Any conditional logic that reads `ScenarioMetadata` belongs in `scenarioLayer`.
Common patterns:

```scala
override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, R] =
  // Switch between mock and real by tag
  if (meta.tags.contains("integration"))
    realDependencyLayer
  else
    mockDependencyLayer

// Or: switch by scenario name fragment
override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, R] =
  if (meta.name.contains("error path"))
    brokenDependencyLayer
  else
    happyPathLayer
```

`ScenarioMetadata` fields:

```scala
case class ScenarioMetadata(
  name:       String,
  tags:       List[String],
  file:       Option[String],
  line:       Option[Int],
  flagValues: Map[String, String]   // populated by @flags expansion
)
```

---

## 4. Per-scenario `Scope`: resource lifecycle in steps

The framework wraps every scenario in a `ZIO.scoped` block.  This means a
`Scope` is always present in the ZIO environment when step bodies run — even
though `Scope` is not part of the declared `R` type.

Step bodies can acquire and release resources that are automatically finalised
at the end of the scenario, even if a later step fails:

```scala
Given("a temporary DynamoDB table exists") {
  ZIO.acquireRelease(
    DynamoDB.createTable(testTableName).orDie
  )(
    _ => DynamoDB.deleteTable(testTableName).orDie
  ).unit
  // Table is deleted when the scenario ends, pass or fail
}
```

This is the standard ZIO resource pattern: `ZIO.acquireRelease` registers
both the acquire and the release action.  The release runs when the scenario's
`Scope` closes, in reverse-acquisition order.

No changes to `R` or `S` are required to use scoped resources.  The `Scope`
service is injected by the executor automatically.

---

## 5. `flagLayer`: injecting `@flags` values into the environment

The `@flags(key=value)` tag syntax expands one scenario into multiple runs,
one per `@flags(...)` annotation.  The framework calls `flagLayer` for each
expansion, passing the parsed key/value pairs:

```scala
override def flagLayer(
  meta: ScenarioMetadata,
  flags: Map[String, String]
): ZLayer[Any, Throwable, R] =
  environment >>> FeatureFlagOverride.layer(flags)
```

When the `@flags` tags are:

```gherkin
@flags(rateLimiting=true)
@flags(rateLimiting=false)
Scenario: payment processing under rate limiting
```

`flagLayer` is called twice — once with `Map("rateLimiting" -> "true")` and
once with `Map("rateLimiting" -> "false")`.  The scenario name in the report is
suffixed automatically: `payment processing under rate limiting [rateLimiting=true]`.

`flagLayer` is only called for scenarios that have at least one `@flags(...)`
tag.  Scenarios without `@flags(...)` tags go through `scenarioLayer` as
normal.

The default implementation ignores `flags` and delegates to `scenarioLayer(meta)`,
which means flag values appear in `meta.flagValues` but do not affect the
environment unless you override `flagLayer`.

### Boolean shorthand

`@flags(rateLimiting)` is equivalent to `@flags(rateLimiting=true)`.  The
parsed map will contain `"rateLimiting" -> "true"`.

### Outline × flags matrix

A Scenario Outline combined with `@flags` produces a full cross-product.  Two
`Examples` rows × two `@flags` tags = four scenario runs.

---

## 6. `HasService[A, R]`: reusable step libraries

### The problem

A step module that calls `ZIO.service[HttpClient]` inside its steps has an
implicit dependency on `HttpClient` being part of `R`.  Without any annotation
this requirement is invisible.  When the step module is reused in a different
suite, the compiler may fail with a confusing environment mismatch rather than
a clear "HttpClient is missing" message.

### What `HasService` does

`HasService[A, R]` is a typeclass that expresses *"`R` contains service `A`"*.
It is derived automatically by the compiler from intersection types:

```scala
sealed trait HasService[A, R]

object HasService:
  given identity[A]: HasService[A, A]                              = ...
  given leftIntersection[A, B]: HasService[A, A & B]               = ...
  given intersection[A, B, R](using HasService[A, R]): HasService[A, B & R] = ...
```

So `HasService[HttpClient, HttpClient & DynamoDB]` is automatically available
because `HttpClient` is present in the intersection.

### Using `HasService` to constrain step modules

Declare the constraint on the step module's type parameter:

```scala
// This module requires HttpClient to be present in R
trait HttpSteps[R](using HasService[HttpClient, R])
    { self: ZIOSteps[R, ?] =>

  Given("GET " / string / " returns " / int) { (url: String, expected: Int) =>
    for {
      client <- ZIO.service[HttpClient]
      status <- client.get(url).map(_.status)
      _      <- Assertions.assertEquals(status, expected)
    } yield ()
  }
}
```

Mixing this trait into a suite automatically satisfies the constraint when `R`
includes `HttpClient`:

```scala
object MySuite
    extends ZIOSteps[HttpClient & DynamoDB, AppState]
    with HttpSteps[HttpClient & DynamoDB]    // compiles: HasService[HttpClient, HttpClient & DynamoDB]
    with DynamoSteps[HttpClient & DynamoDB]: // compiles: HasService[DynamoDB, HttpClient & DynamoDB]
  override def environment = ...
```

If `HttpClient` were absent from `R`, the `using HasService[HttpClient, R]`
clause would produce a clear compile error at the `with HttpSteps[...]` line,
naming the missing service.

### `HasServiceOps.withService[A]`

A convenience wrapper for accessing a service directly.  Equivalent to
`ZIO.service[A]`:

```scala
import zio.bdd.core.step.HasServiceOps

HasServiceOps.withService[HttpClient].flatMap(_.get(url))
```

This is a documentation aid — it signals that the calling code depends on `A`.
In practice `ZIO.service[A]` is shorter and equally correct.

---

## 7. `globalLayer`: sharing expensive resources across the JVM

`globalLayer` is the top tier and is built at most once per JVM process.  It is
the right home for:

- Connection pools (`HikariCP`, AWS SDK clients)
- Embedded or shared databases (LocalStack, embedded Kafka)
- In-process test servers that take seconds to start

```scala
override def globalLayer: ZLayer[Any, Throwable, R] =
  ZLayer.make[R](
    SharedConnectionPool.layer,
    LocalStackContainer.layer,
    AccountService.layer,
    // ...
  )
```

**Note:** If `globalLayer` fails to build, the entire test run fails.  Keep it
focused on truly global resources and push per-feature or per-scenario
isolation to the lower tiers.

By default `globalLayer` delegates to `featureLayer`.  If you override neither,
both tiers use whatever `environment` returns, which is the same single layer
evaluated once per feature (the pre-3.0 behaviour).
