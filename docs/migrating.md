# Migrating from Cucumber-Scala to zio-bdd

This guide is for teams moving from Cucumber-JVM with the `cucumber-scala` DSL to zio-bdd.
It assumes Scala 3 and ZIO 2.

---

## Why migrate

| Cucumber-Scala | zio-bdd |
|---|---|
| Reflection-based step matching | Compile-time typed extractors |
| Mutable `World` state per scenario | Immutable `ScenarioContext` via `FiberRef[S]` |
| Step bodies are plain Scala (no effect type) | Step bodies return `ZIO` effects |
| `@Before`/`@After` JUnit annotations | `beforeScenario` / `afterScenario` DSL |
| DataTable → `List[List[String]]` then manual map | `table[T]` with ZIO Schema derivation |
| Regex patterns everywhere | Typed extractors — regex only where needed |
| Global `ObjectFactory` for DI | `ZLayer` wired per scenario/feature/suite |
| Separate runner class (`@RunWith(Cucumber.class)`) | Single `@Suite` annotation on the steps object |

The core benefit is that step arguments, state, and dependencies become part of the Scala
type system. Mismatches between feature files and step definitions produce compile errors
or clear startup failures rather than silent misbehavior at runtime.

---

## Dependency changes

Remove:

```scala
"io.cucumber" %% "cucumber-scala"   % cucumberVersion,
"io.cucumber"  % "cucumber-junit"   % cucumberVersion,
"io.cucumber"  % "cucumber-picocontainer" % cucumberVersion  // or other DI plugin
```

Add:

```scala
"io.github.etacassiopeia" %% "zio-bdd" % "1.4.4"
```

The `zio-bdd` artifact includes the sbt test framework integration. You do not need a
separate runner or test framework dependency.

In `build.sbt`, register the framework:

```scala
testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")
```

---

## Step definition translation

### No-argument step

**Cucumber:**
```scala
Given("a clean database") { () =>
  db.truncateAll()
}
```

**zio-bdd:**
```scala
Given("a clean database") {
  ZIO.serviceWithZIO[Database](_.truncateAll())
}
```

### Single string argument

**Cucumber:**
```scala
Given("""a user named "([^"]+)"""") { (name: String) =>
  world.userName = name
}
```

**zio-bdd:**
```scala
Given("a user named " / string) { (name: String) =>
  ScenarioContext.update(_.copy(userName = name))
}
```

The `string` extractor matches an optional double-quoted value or an unquoted word sequence.
No regex is required for this common pattern.

### Integer argument

**Cucumber:**
```scala
When("""the user adds (\d+) items""") { (count: Int) =>
  world.addItems(count)
}
```

**zio-bdd:**
```scala
When("the user adds " / int / " items") { (count: Int) =>
  ScenarioContext.update(s => s.copy(itemCount = s.itemCount + count))
}
```

### Multiple arguments

**Cucumber:**
```scala
Then("""the balance of "([^"]+)" should be (\d+\.\d+)""") {
  (account: String, balance: Double) =>
    assert(ledger.balance(account) == balance)
}
```

**zio-bdd:**
```scala
import zio.bdd.core.Assertions.assertTrue

Then("the balance of " / string / " should be " / double) {
  (account: String, balance: Double) =>
    for {
      actual <- ZIO.serviceWithZIO[Ledger](_.balance(account))
      _      <- assertTrue(actual == balance)
    } yield ()
}
```

Step expression parts are chained with `/`. Literal string segments and typed extractors
alternate. The compiler infers the tuple type and produces a matching function signature.

### Available extractors

| Extractor | Matches | Scala type |
|---|---|---|
| `string` | Any text, strips optional double quotes | `String` |
| `word` | Single whitespace-free token | `String` |
| `rest` | Remainder of step text | `String` |
| `int` | Integer (negative allowed) | `Int` |
| `long` | Long integer | `Long` |
| `double` | Decimal number | `Double` |
| `bigDecimal` | Exact-precision decimal | `BigDecimal` |
| `boolean` | `true`/`false` (case-insensitive) | `Boolean` |
| `uuid` | UUID string | `java.util.UUID` |
| `table[T]` | Gherkin data table | `List[T]` |
| `docString` | Triple-quoted doc string | `String` |

For patterns that genuinely require a regular expression, or for alternation between two
literal words, register separate step definitions — one per variant:

```scala
// Cucumber: @Given("the (cat|dog) is present")
// zio-bdd: two separate steps
Given("the cat is present") { ... }
Given("the dog is present") { ... }
```

---

## World / state: ScenarioContext

In Cucumber-Scala the `World` object is a mutable class instantiated per scenario, shared
across step definitions via dependency injection:

```scala
class World {
  var userName: String = ""
  var lastResponse: Response = null
}

class RegistrationSteps(world: World) {
  Given("a user named Alice") { () => world.userName = "Alice" }
  Then("the user is registered") { () => assert(world.lastResponse.status == 200) }
}
```

In zio-bdd the scenario state `S` is an immutable case class stored in a `FiberRef[S]`
created fresh for each scenario. The `ScenarioContext` object (generated via the `StateOps`
mixin) provides `get` and `update`:

```scala
import zio.bdd.core.Assertions.assertTrue

case class AppState(userName: String = "", lastResponse: Option[Response] = None)

object AppState:
  given Default[AppState] = Default.from(AppState())

@Suite(featureDirs = Array("src/test/resources/features"))
object MySuite extends ZIOSteps[AppEnv, AppState]:

  Given("a user named Alice") {
    ScenarioContext.update(_.copy(userName = "Alice"))
  }

  Then("the user is registered") {
    ScenarioContext.get.flatMap { state =>
      assertTrue(state.lastResponse.exists(_.status == 200))
    }
  }
```

Key differences:

- `ScenarioContext.get` returns `ZIO[State[S], Nothing, S]` — it reads the current state.
- `ScenarioContext.update(f)` applies `f` to the current state atomically.
- State is never shared between scenarios — each scenario starts with `Default[S].default`.
- State is never shared between features unless you explicitly put it in the global layer.

### Large state: TypeMap

When your suite has many step modules each owning disjoint state, use `TypeMap` instead of
a single monolithic case class:

```scala
object CartCtx:
  case class Data(sessionId: String = "", itemCount: Int = 0)
  given Default[Data] = Default.from(Data())

// S = TypeMap, each module reads/writes only its own slice
trait CartSteps { self: ZIOSteps[R, TypeMap] =>
  Given("items are added to the cart") {
    TypeMapOps.update[CartCtx.Data](_.copy(itemCount = 3))
  }
}
```

### State relay anti-pattern → `Stage`

A common Cucumber migration pitfall is carrying raw event payloads through `ScenarioState`
so the `Given` step can pass them to the `When` step:

```scala
// Anti-pattern: relay fields in state just to pass data Given→When
case class AppState(
  // ... real state ...
  orderPayload: String = "",       // relay — only used Given→When
  orderCorrelationId: String = "", // relay
  orderTimestamp: String = ""      // relay
)

Given("a valid order is prepared") {
  for {
    order <- buildOrder(...)
    _ <- ScenarioContext.update(s => s.copy(
      orderPayload = order.toJson,
      orderCorrelationId = order.correlationId,
      orderTimestamp = order.createdAt
    ))
  } yield ()
}

When("the order is submitted") {
  for {
    s <- ScenarioContext.get
    request = buildRequest(s.orderPayload, s.orderCorrelationId, s.orderTimestamp)
    ...
  } yield ()
}
```

The relay fields exist only because `Order` has no `Schema` instance and
cannot be stored in `S` directly. This adds fields per step domain that `ScenarioState`
has no business owning.

**Use `Stage` instead — see [state.md §7](state.md#7-stage-per-scenario-staging-without-schema) for the full pattern.**

```scala
// Clean: typed event stored by ClassTag, no Schema required, zero relay fields
Given("a valid order is prepared") {
  for {
    order <- buildOrder(...)
    _     <- Stage.put(order)
  } yield ()
}

When("the order is submitted") {
  for {
    staged  <- Stage.getOption[Order]
    s       <- ScenarioContext.get
    request <- staged match {
                 case Some(o) => ZIO.succeed(buildRequest(o.toJson, o.correlationId))
                 case None    => buildDefaultRequest(s.session.userId)
               }
    ...
  } yield ()
}
```

`Stage` is cleared automatically between scenarios by the framework. Types are keyed by
`ClassTag` — only one value of each type per scenario. Use a wrapper type if you need to
stage two distinct values of the same class.

---

## DataTable

**Cucumber:**
```scala
When("the following products exist:") { (table: DataTable) =>
  val products = table.asMaps().asScala.map { row =>
    Product(row.get("name"), row.get("price").toDouble, row.get("sku"))
  }
  catalog.seed(products)
}
```

**zio-bdd:**
```scala
case class Product(name: String, price: Double, sku: String)
  derives Schema

When("the following products exist:" / table[Product]) { (products: List[Product]) =>
  ZIO.serviceWithZIO[Catalog](_.seed(products))
}
```

`table[T]` uses ZIO Schema to map the header row to case class fields. The header names
must match the Scala field names exactly unless you annotate them.

### Column name mapping

When the feature file uses display names that differ from Scala field names, annotate the
case class fields with `@ColumnName`:

```scala
import zio.bdd.core.step.ColumnName

case class ProvisionRow(
  @ColumnName("Account Open Date") accountOpenDate: String,
  @ColumnName("Instrument Class")  instrumentClass: String,
  @ColumnName("Source Timestamp")  sourceTimestamp: String
) derives Schema
```

Alternatively, supply a `Map[String, String]` at the call site using `tableWithMapping[T]`:

```scala
When("the following products exist:" / tableWithMapping[Product](
  Map("Product Name" -> "name", "Unit Price" -> "price", "SKU Code" -> "sku")
)) { (products: List[Product]) =>
  ...
}
```

---

## Lifecycle hooks

### @Before / @After → beforeScenario / afterScenario

**Cucumber:**
```scala
class Hooks(world: World) {
  @Before
  def setUp(): Unit = {
    world.db = createTestDatabase()
  }

  @After
  def tearDown(): Unit = {
    world.db.close()
  }
}
```

**zio-bdd:**
```scala
beforeScenario {
  ZIO.serviceWithZIO[Database](_.reset())
}

afterScenario { meta =>
  ZIO.logInfo(s"Scenario '${meta.name}' finished")
}
```

All hook variants:

| Hook | Trigger |
|---|---|
| `beforeAll { ... }` | Once before the entire suite run |
| `afterAll { ... }` | Once after the entire suite run |
| `beforeFeature { ... }` | Before each feature |
| `afterFeature { ... }` | After each feature |
| `beforeScenario { ... }` | Before each scenario (unconditional) |
| `beforeScenario { meta => ... }` | Before each scenario (receives metadata) |
| `beforeScenarioTagged("tag") { meta => ... }` | Before scenarios with `@tag` |
| `afterScenario { ... }` | After each scenario |
| `afterScenario { meta => ... }` | After each scenario (receives metadata) |
| `afterScenarioTagged("tag") { meta => ... }` | After scenarios with `@tag` |
| `beforeStep { meta => ... }` | Before each step |
| `afterStep { meta => ... }` | After each step |

`ScenarioMetadata` carries: `name: String`, `tags: List[String]`,
`file: Option[String]`, `line: Option[Int]`, `flagValues: Map[String, String] = Map.empty`.

Multiple calls to the same hook method compose — they do not replace each other:

```scala
beforeScenario { ZIO.serviceWithZIO[Cache](_.clear()) }
beforeScenario { ZIO.serviceWithZIO[Metrics](_.reset()) }
// both run before every scenario
```

---

## Resources: ZIO.acquireRelease replaces manual cleanup

**Cucumber:**
```scala
class Hooks(world: World) {
  @Before
  def createTable(): Unit = {
    world.tableName = s"test_${UUID.randomUUID()}"
    dynamo.createTable(world.tableName)
  }

  @After
  def deleteTable(): Unit = {
    dynamo.deleteTable(world.tableName)
  }
}
```

**zio-bdd:**

Resources acquired in a step body are automatically released when the scenario ends,
even if later steps fail. Wrap acquisition with `ZIO.acquireRelease`:

```scala
Given("a temporary DynamoDB table exists") {
  ZIO.acquireRelease(
    ZIO.serviceWithZIO[DynamoDB](_.createTable(s"test_${UUID.randomUUID()}"))
      .tap(name => ScenarioContext.update(_.copy(tableName = name)))
  )(
    name => ZIO.serviceWithZIO[DynamoDB](_.deleteTable(name)).orDie
  ).unit
}
```

`Scope` is automatically provided by the scenario executor — you do not add it to the step
environment type.

---

## Regex steps and alternation

Cucumber step patterns are raw regular expressions. zio-bdd step text is literal, with typed
extractors handling the variable parts.

### Alternation — `oneOf()`

When a Cucumber regex alternation group captures a value used in the step body, use `oneOf()`:

**Cucumber:**
```scala
Given("""^the connection (is|is not) active$""") { (condition: String) =>
  checkConnectionState(condition == "is")
}
```

**zio-bdd:**
```scala
// oneOf sorts alternatives longest-first so "is not" matches before "is"
Given("the connection " / oneOf("is not", "is") / " active") { (condition: String) =>
  checkConnectionState(condition == "is")
}
```

For multi-word alternatives (`"withdraw Post"`, `"deposit Post"`, etc.) the same pattern applies:
```scala
When(
  "remove for previous " /
    oneOf("withdraw Post", "deposit Post", "Remove", "EOD", "Patch Provision") /
    " FinancialCoreCommandId and Source"
) { (txKind: String) => ... }
```

`oneOf(alternatives*)` generates `(alt0|alt1|...)` with alternatives sorted longest-first,
preventing shorter prefixes (e.g. `"is"`) from shadowing longer ones (e.g. `"is not"`).

### Optional literal — `optional()`

When a Cucumber regex has an optional group `(text)?`, use `optional(text)`:

**Cucumber:**
```scala
Given("""^a valid SimulationBranch body without fork point(?: with the same simulationId)?$""") { ... }
```

**zio-bdd:**
```scala
Given(
  "a valid SimulationBranch body without fork point" / optional(" with the same simulationId")
) { (suffix: Option[String]) =>
  // suffix.isDefined when the variant was present
  ...
}
```

`optional(text)` returns `Option[String]` — `Some(text)` when present, `None` when absent.

### Optional prefix — `regex()`

When a Cucumber regex has an optional prefix (e.g. `(express |)`), use `regex()` with a
manually-written capturing group:

**Cucumber:**
```scala
Given("""^a valid (express |)shipping option is selected$""") { (prefix: String) =>
  val useExpress = prefix.nonEmpty
}
```

**zio-bdd:**
```scala
Given("a valid " / regex("(express )?") / "shipping option is selected" / table[T]) {
  (prefix: String, rows: List[T]) =>
    val useExpress = prefix.nonEmpty
    ...
}
```

`regex(pat)` emits `pat` verbatim into the compiled regex. The caller must ensure `pat`
contains **exactly one** top-level capturing group. For optional groups, the captured value
is `""` (empty string) when the group doesn't match.

> **Tip:** `regex()` is the escape hatch for patterns that don't fit `oneOf()` or `optional()`.
> The more specific helpers are preferred when they fit because they prevent the one-group
> constraint from being silently violated.

### When separate step registrations are still appropriate

If the alternatives have **completely different semantics** (different state fields accessed,
different generators called) and do not share a dispatch helper, separate registrations are
clearer than a single step with a string parameter:

```scala
Given("the credit card is valid")   { setCard("credit", "valid")   }
Given("the credit card is expired") { setCard("credit", "expired") }
```

This is the right choice when combining them would make the body a complex nested match.
Use `oneOf()` / `regex()` when the body logic is parametric; use separate steps when the
bodies diverge significantly.

---

## Common compiler errors and fixes

### "value Given is not a member of ProvisionSteps"

Step methods (`Given`, `When`, `Then`, etc.) are members of `ZIOSteps`. Step traits must
declare a self-type to access them:

```scala
// Wrong
trait CartSteps {
  Given("an item is added") { ... }  // compile error
}

// Correct
trait CartSteps { self: ZIOSteps[AppEnv, AppState] =>
  Given("an item is added") { ... }
}
```

### "type mismatch: found ZIO[AppEnv & State[AppState] & Scope, ...], required ZIO[OtherEnv & State[OtherState], ...]"

The `R` or `S` type on a step trait does not match the suite object. All mixed-in step
traits must share exactly the same `R` and `S` as the suite:

```scala
// Suite uses ZIOSteps[AppEnv, AppState]
trait ProvisionSteps { self: ZIOSteps[AppEnv, AppState] => ... }  // correct
trait ProvisionSteps { self: ZIOSteps[OtherEnv, OtherState] => ... }  // mismatch
```

### "Ambiguous step definitions detected at suite startup"

Two step definitions with identical text were registered — usually from two mixin traits both
defining the same step:

```
Ambiguous step definitions detected at suite startup. The following step expressions are
registered multiple times:
  GivenStep 'a valid user' — registered 2 times
```

Rename or merge the duplicate step definition. The error is thrown at suite startup, not
mid-test.

### "ZIO.succeed wrapping side-effecting code"

Step bodies must return ZIO effects. Wrapping a side-effecting expression with `ZIO.succeed`
is wrong because `ZIO.succeed` is only for pure expressions:

```scala
// Wrong — side effect in ZIO.succeed
Given("the cache is cleared") {
  ZIO.succeed(cache.clear())  // cache.clear() runs eagerly, not inside the fiber
}

// Correct
Given("the cache is cleared") {
  ZIO.attempt(cache.clear()).orDie
}
```

---

## Worked example

### Before (Cucumber-Scala)

`features/order.feature`:
```gherkin
Feature: Order placement

  Scenario: Place a valid order
    Given a product "Widget A" costs 9.99
    When the order is placed
    Then the order total should be 9.99
```

`steps/OrderSteps.scala`:
```scala
class World {
  var productName: String = ""
  var orderId: String = ""
}

class OrderSteps(world: World) {
  val client = new HttpClient("http://localhost:8080")

  @Given("""a product "([^"]+)" costs (\d+\.\d+)""")
  def givenProduct(name: String, price: Double): Unit = {
    world.productName = name
  }

  @When("the order is placed")
  def whenOrderPlaced(): Unit = {
    val resp = client.post("/orders", Map("product" -> world.productName))
    world.orderId = resp.body("orderId")
  }

  @Then("""the order total should be (\d+\.\d+)""")
  def thenTotalShouldBe(expected: Double): Unit = {
    val total = client.get(s"/orders/${world.orderId}").body("total").toDouble
    assert(total == expected)
  }
}
```

### After (zio-bdd)

The feature file is unchanged.

`steps/OrderSpec.scala`:
```scala
import zio.bdd.core.Assertions.assertTrue

case class OrderState(productName: String = "", orderId: String = "")

object OrderState:
  given Default[OrderState] = Default.from(OrderState())

@Suite(featureDirs = Array("src/test/resources/features"))
object OrderSpec extends ZIOSteps[HttpClient, OrderState]:

  GivenS("a product " / string / " costs " / double) { s => (name: String, _: Double) =>
    ScenarioContext.update(_.copy(productName = name))
  }

  WhenS("the order is placed") { s =>
    for {
      client <- ZIO.service[HttpClient]
      resp   <- client.post("/orders", Map("product" -> s.productName))
      id      = resp.body("orderId")
      _      <- ScenarioContext.update(_.copy(orderId = id))
    } yield ()
  }

  ThenS("the order total should be " / double) { s => (expected: Double) =>
    for {
      client <- ZIO.service[HttpClient]
      total  <- client.get(s"/orders/${s.orderId}").map(_.body("total").toDouble)
      _      <- assertTrue(total == expected, s"Expected $expected, got $total")
    } yield ()
  }

  override def environment: ZLayer[Any, Throwable, HttpClient] =
    HttpClient.live("http://localhost:8080")
```

Key changes:

- `World` replaced by `OrderState` with `Default` instance.
- Mutable `var` fields replaced by `ScenarioContext.update`.
- Step arguments are typed at compile time — no regex, no `String.toDouble`.
- `GivenS`/`WhenS`/`ThenS` inject state as the first curried argument — no `ScenarioContext.get` boilerplate.
- HTTP client injected via ZLayer instead of instantiated in the step class.
- No runner class, no `@RunWith`, no PicoContainer.
