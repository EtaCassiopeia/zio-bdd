# Testing with Feature Flags: @flags(k=v)

The `@flags(k=v)` tag is a built-in zio-bdd mechanism for running the same scenario
against multiple feature flag combinations without duplicating Gherkin or step code.

---

## The problem it solves

Feature flags create a combinatorial testing problem. A service with three boolean flags
has eight possible states. Writing eight separate scenarios — all structurally identical
except for which flags are enabled — leads to massive duplication in both feature files
and step definitions.

The `@flags` tag solves this by treating each tag as a separate run of the same scenario,
with the flag values injected into the test environment as a `Map[String, String]`. The
Gherkin and the step definitions remain unchanged. Only the layer wiring differs between
runs.

This also provides exact traceability: each run appears in the report as a distinct named
scenario — `Base scenario [rateLimiting=true]`, `Base scenario [rateLimiting=false]` —
making it clear which flag combination passed or failed.

---

## The @flags(k=v) syntax

The tag is written as `@flags(` followed by one or more `key=value` pairs separated by
commas, then `)`.

```gherkin
@flags(rateLimiting=true)
Scenario: Provision under rate limiting
  Given a provision request is sent
  Then the response status is 429
```

**Boolean shorthand:** a key without `=value` defaults to `true`:

```gherkin
@flags(rateLimiting)
# equivalent to @flags(rateLimiting=true)
```

**Multiple key-value pairs in one tag:**

```gherkin
@flags(rateLimiting=false, pricingAlgo=v2)
Scenario: Provision with new algorithm
  Given a provision request is sent
  Then the response status is 200
  And the response body contains v2 pricing
```

Values are always strings. Step definitions receive the raw string and perform their own
type conversion.

---

## Multiple @flags tags = multiple runs

When a scenario has more than one `@flags(...)` tag, the framework expands it into
multiple independent runs — one per tag. Each run receives only the flag map from its
own tag.

```gherkin
@flags(rateLimiting=true)
@flags(rateLimiting=false)
Scenario: Provision with rate limiting toggle
  Given a provision request is sent
  Then the response status is 200
```

This single Gherkin block produces **two** scenario runs:

1. `Provision with rate limiting toggle [rateLimiting=true]`
2. `Provision with rate limiting toggle [rateLimiting=false]`

The first run receives `Map("rateLimiting" -> "true")`. The second receives
`Map("rateLimiting" -> "false")`.

**Three or more tags** produce three or more runs:

```gherkin
@flags(algo=v1)
@flags(algo=v2)
@flags(algo=v3)
Scenario: Provision with algorithm variants
  Given a provision request is sent
  Then the response completes
```

Three runs: `[algo=v1]`, `[algo=v2]`, `[algo=v3]`.

**Expansion semantics:** the tag list is `[flags(algo=v1), flags(algo=v2), flags(algo=v3)]`.
`FlagsTag.extractAll(tags)` returns one map per `@flags(...)` tag in the order they
appear. Each map produces one scenario run. There is no cross-product of separate tags —
for a cross-product (e.g., two independent boolean flags), use one multi-key tag per
combination:

```gherkin
@flags(rateLimiting=true,  algo=v1)
@flags(rateLimiting=true,  algo=v2)
@flags(rateLimiting=false, algo=v1)
@flags(rateLimiting=false, algo=v2)
Scenario: Full matrix
  Given a provision request is sent
  Then the response completes
```

Four runs covering all four combinations.

---

## Expanded scenario names

The framework appends the flag values to the scenario name in brackets. Keys are sorted
alphabetically within a tag:

```
Original name:  Provision with rate limiting toggle
Tag:            @flags(rateLimiting=true)
Expanded name:  Provision with rate limiting toggle [rateLimiting=true]
```

For multi-key tags:

```
Tag:            @flags(rateLimiting=false, pricingAlgo=v2)
Expanded name:  Provision with flag matrix [pricingAlgo=v2, rateLimiting=false]
```

The bracket suffix appears in the pretty-printer output, JUnit XML reports, and in
`ScenarioResult.scenario.name`. You can pattern-match on names in assertions:

```scala
val runOn  = results.find(_.scenario.name.contains("rateLimiting=true")).get
val runOff = results.find(_.scenario.name.contains("rateLimiting=false")).get
```

**Stable contract:** the suffix format — `[k=v, k2=v2]`, keys sorted alphabetically
(`flags.toList.sortBy(_._1)` in `FeatureExecutor.expandFlagScenarios`) — is guaranteed
stable across runs for a given flag map. Pattern-matching on names (as above) is safe to
rely on.

**No collision detection:** the framework does not check whether two distinct `@flags(...)`
tags on the same scenario render to the same suffix (e.g. a duplicated tag, or two tags
whose maps happen to be equal). If that happens, both runs produce an identically-named
scenario in the report, and there is no way to distinguish them by name alone.

---

## flagLayer — injecting flags into the environment

By default, `@flags(...)` tags are only visible through `ScenarioMetadata.flagValues`.
To make the flag values affect service behaviour, override `flagLayer` in your suite:

```scala
override def flagLayer(
    meta: ScenarioMetadata,
    flags: Map[String, String]
): ZLayer[Any, Throwable, R] =
  // Return an R-producing layer that encodes the flag values
  environment >>> MyFlagService.withFlags(flags)
```

`flagLayer` is called once per expanded run (once per `@flags(...)` tag). The `meta`
argument carries `ScenarioMetadata` for the current run, including `meta.flagValues`
which contains the same map as `flags`.

For scenarios **without** any `@flags(...)` tags, `flagLayer` is **not** called.
`scenarioLayer(meta)` is called instead (the normal per-scenario layer). This is a
backward-compatible boundary: existing tests that have no `@flags` tags are unaffected
by any `flagLayer` override.

---

## ScenarioMetadata.flagValues in hooks

Every lifecycle hook receives a `ScenarioMetadata` argument. Its `flagValues` field
contains the flag map for the current run.

```scala
beforeScenario { meta =>
  ZIO.logInfo(s"Running scenario '${meta.name}' with flags: ${meta.flagValues}")
}
```

When there are no `@flags(...)` tags, `meta.flagValues` is an empty map (`Map.empty`).

Use `flagValues` in hooks to:
- Record which flag combination a run belongs to in structured logs.
- Skip setup that is incompatible with a specific flag state.
- Route to different external services based on flag values.

```scala
beforeScenario { meta =>
  val useRealService = meta.flagValues.get("realDownstream").contains("true")
  ZIO.when(useRealService)(warmUpRealServiceConnection).unit
}
```

---

## Outline x flags: multiplicative expansion

`Scenario Outline` expansion and `@flags` expansion are independent and multiplicative.
An outline with N example rows tagged with M `@flags` tags produces N × M scenario runs.

```gherkin
@flags(mode=fast)
@flags(mode=slow)
Scenario Outline: login <user>
  Given user <user> is authenticated

Examples:
  | user  |
  | Alice |
  | Bob   |
```

**2 example rows × 2 flags tags = 4 scenario runs:**

1. `login <user> - Example 1 [mode=fast]` — Alice, fast
2. `login <user> - Example 1 [mode=slow]` — Alice, slow
3. `login <user> - Example 2 [mode=fast]` — Bob, fast
4. `login <user> - Example 2 [mode=slow]` — Bob, slow

The outline expansion happens at parse time (in `GherkinParser`). The flags expansion
happens at execution time (in `FeatureExecutor.expandFlagScenarios`). Each parsed outline
scenario is independently flags-expanded.

---

## Interactions with other tags

`expandFlagScenarios` runs *before* scenario execution: it turns one `Scenario` into N
independent `(Scenario, flags)` pairs, each carrying the original tag list unchanged
(only `name` is rewritten with the `[k=v]` suffix). Execution-affecting tags
(`@retry`/`@flaky`/`@nonFlaky`/`@expectedFailure`/`@ignore`) are resolved per expanded
copy, not once for the whole scenario — so each flag combination is retried,
inverted, or skipped independently of the others.

| Tag | Applies to | Execution-count implication |
|-----|------------|------------------------------|
| `@retry(n)` / `@flaky(n)` | each expanded copy independently | Each of the N flag runs gets its own retry loop (up to n attempts, stops at first pass). Worst case: N × n total attempts. |
| `@nonFlaky(n)` | each expanded copy independently | Each of the N flag runs gets its own fail-fast loop (up to n attempts, stops at first failure). Worst case: N × n total attempts. |
| `@expectedFailure` | each expanded copy independently | Each of the N flag runs executes exactly once — retry aspects are ignored (`@expectedFailure` wins over `@retry`/`@flaky`/`@nonFlaky`) — and its outcome is inverted. |
| `@ignore` | the scenario as a whole, but expansion still happens | `expandFlagScenarios` does not special-case `@ignore`; a scenario with N `@flags(...)` tags is still expanded into N named copies, each of which is then reported as ignored (no steps executed). The report shows N ignored entries, not one. |

---

## Pattern 1: FlagConfig service (config-file style)

This pattern mirrors how a service reads feature flags from a configuration file
overridable by environment variables. The flag map from the `@flags(...)` tag replaces
the config values for that scenario run.

**Service definition:**

```scala
trait FlagConfig:
  def get(key: String): UIO[Option[String]]
  def getOrDefault(key: String, default: String): UIO[String] =
    get(key).map(_.getOrElse(default))
  def isEnabled(key: String): UIO[Boolean] =
    getOrDefault(key, "false").map(_.equalsIgnoreCase("true"))

object FlagConfig:
  def fromMap(m: Map[String, String]): FlagConfig = new FlagConfig:
    def get(key: String): UIO[Option[String]] = ZIO.succeed(m.get(key))

  val default: ZLayer[Any, Nothing, FlagConfig] =
    ZLayer.succeed(fromMap(Map.empty))

  def withFlags(flags: Map[String, String]): ZLayer[Any, Nothing, FlagConfig] =
    ZLayer.succeed(fromMap(flags))
```

**Suite wiring:**

```scala
@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[FlagConfig, AppState]:

  override def environment: ZLayer[Any, Throwable, FlagConfig] =
    FlagConfig.default.asInstanceOf[ZLayer[Any, Throwable, FlagConfig]]

  // Called once per @flags(...) tag — provide flag values for this run
  override def flagLayer(
      meta: ScenarioMetadata,
      flags: Map[String, String]
  ): ZLayer[Any, Throwable, FlagConfig] =
    FlagConfig.withFlags(flags).asInstanceOf[ZLayer[Any, Throwable, FlagConfig]]

  Given("a provision request is sent") {
    for {
      flagCfg      <- ZIO.service[FlagConfig]
      rateLimiting <- flagCfg.isEnabled("rateLimiting")
      response      = if (rateLimiting) Response(429, "rate limited")
                      else              Response(200, "success")
      _            <- ScenarioContext.update(_.copy(response = Some(response)))
    } yield ()
  }

  Then("the response status is " / int) { (expected: Int) =>
    ScenarioContext.get.flatMap { s =>
      Assertions.assertEquals(s.response.map(_.status).getOrElse(-1), expected)
    }
  }
```

**Feature file:**

```gherkin
Feature: Provision rate limiting

  @flags(rateLimiting=true)
  @flags(rateLimiting=false)
  Scenario: Provision with rate limiting toggle
    Given a provision request is sent
    Then the response status is 200
```

The `rateLimiting=true` run fails (429 ≠ 200). The `rateLimiting=false` run passes. The
report clearly names both. No step code duplication; no second scenario block.

---

## Pattern 2: zio-openfeature style with in-memory provider

This pattern uses the OpenFeature API shape so the same step definitions work against
both an in-memory test provider and a real Optimizely provider in production.

**Service definition (mirrors zio-openfeature's FeatureFlags API):**

```scala
trait FeatureFlags:
  def getBooleanFlag(key: String, default: Boolean): UIO[Boolean]
  def getStringFlag(key: String,  default: String):  UIO[String]

object FeatureFlags:
  // In-memory provider: used in tests via flagLayer
  def withFlags(flags: Map[String, String]): ZLayer[Any, Nothing, FeatureFlags] =
    ZLayer.succeed(new FeatureFlags:
      def getBooleanFlag(key: String, default: Boolean): UIO[Boolean] =
        ZIO.succeed(flags.get(key).map(_.equalsIgnoreCase("true")).getOrElse(default))
      def getStringFlag(key: String, default: String): UIO[String] =
        ZIO.succeed(flags.getOrElse(key, default))
    )
```

**Suite wiring:**

```scala
@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[FeatureFlags, AppState]:

  override def environment: ZLayer[Any, Throwable, FeatureFlags] =
    FeatureFlags.withFlags(Map.empty).asInstanceOf[ZLayer[Any, Throwable, FeatureFlags]]

  override def flagLayer(
      meta: ScenarioMetadata,
      flags: Map[String, String]
  ): ZLayer[Any, Throwable, FeatureFlags] =
    FeatureFlags.withFlags(flags).asInstanceOf[ZLayer[Any, Throwable, FeatureFlags]]

  Given("a provision request is sent") {
    for {
      ff           <- ZIO.service[FeatureFlags]
      rateLimiting <- ff.getBooleanFlag("rateLimiting", default = false)
      pricingAlgo  <- ff.getStringFlag("pricingAlgo", default = "v1")
      response      = (rateLimiting, pricingAlgo) match
                        case (true,  _)    => Response(429, "rate limited")
                        case (false, "v2") => Response(200, "success via v2 pricing")
                        case _             => Response(200, "success via v1 pricing")
      _            <- ScenarioContext.update(_.copy(response = Some(response)))
    } yield ()
  }

  Then("the response status is " / int) { (expected: Int) =>
    ScenarioContext.get.flatMap(s =>
      Assertions.assertEquals(s.response.map(_.status).getOrElse(-1), expected)
    )
  }

  Then("the response body contains " / string) { (expected: String) =>
    ScenarioContext.get.flatMap { s =>
      val body = s.response.map(_.body).getOrElse("")
      Assertions.assertTrue(body.contains(expected))
    }
  }
```

**Feature file:**

```gherkin
Feature: Pricing and rate limiting

  @flags(rateLimiting=false, pricingAlgo=v1)
  @flags(rateLimiting=false, pricingAlgo=v2)
  @flags(rateLimiting=true,  pricingAlgo=v1)
  @flags(rateLimiting=true,  pricingAlgo=v2)
  Scenario: Provision under all flag combinations
    Given a provision request is sent
    Then the response status is 200
```

Four runs, four named scenarios in the report.

---

## Swapping to real Optimizely

When the in-memory provider has been validated, replacing it with the real Optimizely
provider requires changes only to the ZLayer construction. Step definitions are unchanged.

**What changes:**
1. Add the `zio-openfeature` and Optimizely SDK dependencies.
2. Replace `FeatureFlags.withFlags(flags)` in `flagLayer` with:
   ```scala
   OpenFeatureFlags.live(
     OptimizelyProvider(sdkKey, userId, attributes = flags)
   )
   ```
3. Replace `FeatureFlags.withFlags(Map.empty)` in `environment` with the production
   Optimizely layer.

**What does not change:**
- Every `@flags(...)` tag in every feature file.
- Every step definition that calls `ff.getBooleanFlag(...)` or `ff.getStringFlag(...)`.
- Every `flagLayer` override signature.
- Every `ScenarioMetadata.flagValues` usage in hooks.

The `@flags(...)` tags become declarative documentation of which Optimizely flag
combinations the test explicitly covers.

---

## FlagsTag API reference

`FlagsTag` is the parser for `@flags(...)` tags. It lives in `zio.bdd.gherkin`.

```scala
object FlagsTag:
  /** Parse a single tag string. Returns None for non-flags tags. */
  def parse(tag: String): Option[Map[String, String]]

  /**
   * Extract all flag maps from a tag list.
   * Returns empty list when no @flags(...) tags are present.
   * The scenario runs exactly once in that case (no expansion).
   */
  def extractAll(tags: List[String]): List[Map[String, String]]
```

**`FlagsTag.parse` examples:**

| Input | Result |
|-------|--------|
| `"flags(a=true)"` | `Some(Map("a" -> "true"))` |
| `"@flags(a=true)"` | `Some(Map("a" -> "true"))` — leading `@` stripped |
| `"flags(a=true, b=false)"` | `Some(Map("a" -> "true", "b" -> "false"))` |
| `"flags(rateLimiting)"` | `Some(Map("rateLimiting" -> "true"))` — boolean shorthand |
| `"smoke"` | `None` — not a flags tag |
| `"ignore"` | `None` — not a flags tag |
| `""` | `None` |

**`FlagsTag.parse` edge cases:**

| Input | Result | Why |
|-------|--------|-----|
| `"flags(a=)"` | `Some(Map("a" -> ""))` | empty value is kept as `""`, not treated as missing/boolean |
| `"flags( =v)"` | `Some(Map())` | blank key is dropped (falls through to `case _ => None` for that pair) |
| `"flags(key=a=b)"` | `Some(Map("key" -> "a=b"))` | split on `=` with `limit = 2` — only the first `=` splits |
| `"flags( a = b )"` | `Some(Map("a" -> "b"))` | key and value are trimmed of surrounding whitespace |
| `"flags(a=\"x\")"` | `Some(Map("a" -> "\"x\""))` | quotes are **not** stripped — `.trim` only removes whitespace |
| `"flags()"` | `None` | `tagPattern` requires 1+ chars inside the parens (`[^)]+`); empty parens don't match at all |

> **Duplicate keys within one tag last-wins.** `"flags(a=1, a=2)"` parses to
> `Some(Map("a" -> "2"))` — the trailing pair overwrites the earlier one because the
> parsed pairs are folded into a `Map` via `.toMap`, which keeps the last occurrence of
> a repeated key.

**`FlagsTag.extractAll` examples:**

```scala
FlagsTag.extractAll(List("flags(a=true)", "flags(a=false)", "smoke"))
// → List(Map("a" -> "true"), Map("a" -> "false"))
// "smoke" is ignored; two entries → two runs

FlagsTag.extractAll(List("smoke", "regression"))
// → List()
// No @flags tags → scenario runs once (no expansion)
```

---

## Non-flags tags on expanded scenarios

Non-flags tags are preserved on all expanded scenarios. A scenario tagged with both
`@smoke` and `@flags(x=1)` / `@flags(x=2)` produces two runs, both carrying `@smoke`.
This means `--include-tags smoke` will include both runs, and `--exclude-tags smoke`
will exclude both runs.

```gherkin
@smoke
@flags(x=1)
@flags(x=2)
Scenario: s
  Given a step
```

Both `s [x=1]` and `s [x=2]` carry the `smoke` tag.

---

## Backward compatibility

The flags expansion is strictly additive. Scenarios without any `@flags(...)` tag are
unaffected:
- They run exactly once.
- `flagLayer` is not called for them.
- `meta.flagValues` is `Map.empty` in their hooks.

Existing test suites that do not override `flagLayer` and have no `@flags(...)` tags in
their feature files behave identically to before the feature was added.
