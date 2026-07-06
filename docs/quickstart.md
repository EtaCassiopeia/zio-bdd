# Quickstart — zero to running test in 5 minutes

This guide walks through adding zio-bdd to an existing sbt project, writing a feature file,
implementing the step definitions, and interpreting the output. We use a shopping cart domain
as a concrete example throughout.

## 1. Add the dependency

In `build.sbt`:

```scala
libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "1.3.1" % Test

Test / testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")
```

The transitive dependencies (`zio`, `zio-schema`, `zio-schema-derivation`) are pulled in
automatically. No other configuration is needed.

## 2. Write a feature file

Create `src/test/resources/features/cart.feature`:

```gherkin
Feature: Shopping cart

  Scenario: Adding an item increases the total
    Given an empty cart
    When the user adds 2 units of "Widget" at price 9.99
    Then the cart total should be 19.98
```

Gherkin is plain English. Each line beginning with a keyword (`Given`, `When`, `Then`, `And`,
`But`) becomes a step. The framework matches each step to a step definition in your Scala suite.

## 3. Write the ZIOSteps suite

Create `src/test/scala/example/CartSpec.scala`:

```scala
package example

import zio.*
import zio.bdd.core.{Assertions, Suite}
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

// ── Domain ─────────────────────────────────────────────────────────────────

case class CartItem(name: String, quantity: Int, unitPrice: Double)

// ── Scenario state ─────────────────────────────────────────────────────────
// S must have default values for every field so the framework can
// reset it to a known-empty state at the start of each scenario.

case class CartState(items: List[CartItem] = Nil):
  def total: Double =
    BigDecimal(items.map(i => i.quantity * i.unitPrice).sum)
      .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      .toDouble

object CartState:
  given Schema[CartState] = DeriveSchema.gen[CartState]

// ── Service ─────────────────────────────────────────────────────────────────
// The suite's R type is CartService.  Every step body can call
// ZIO.service[CartService] to reach it.

trait CartService:
  def addItem(name: String, qty: Int, price: Double): UIO[CartItem]

object CartService:
  val live: ULayer[CartService] = ZLayer.succeed:
    (name, qty, price) => ZIO.succeed(CartItem(name, qty, price))

// ── Suite ────────────────────────────────────────────────────────────────────

@Suite(
  featureDirs = Array("src/test/resources/features"),
  reporters   = Array("pretty")
)
object CartSpec extends ZIOSteps[CartService, CartState]:

  override def environment: ZLayer[Any, Throwable, CartService] = CartService.live

  Given("an empty cart") {
    ScenarioContext.update(_ => CartState())
  }

  When("the user adds " / int / " units of " / string / " at price " / double) {
    (qty: Int, name: String, price: Double) =>
      for
        svc  <- ZIO.service[CartService]
        item <- svc.addItem(name, qty, price)
        _    <- ScenarioContext.update(s => s.copy(items = s.items :+ item))
      yield ()
  }

  Then("the cart total should be " / double) { (expected: Double) =>
    ScenarioContext.get.flatMap: state =>
      Assertions.assertEquals(state.total, expected, s"Expected $expected, got ${state.total}")
  }
```

Key points:

- `ZIOSteps[R, S]` is the base trait. `R` is your ZIO service environment; `S` is the
  per-scenario state type.
- `@Suite` tells the framework where to find feature files and which reporters to use.
- `override def environment` returns the `ZLayer` that builds `R`.
- Step bodies are ZIO effects of type `ZIO[R & State[S], Throwable, Unit]`. The `StepEffect`
  type alias inside `ZIOSteps` captures this.
- `ScenarioContext.get` and `ScenarioContext.update` read and write the scenario state.
- Typed extractors (`int`, `string`, `double`) in the step expression are separated by `/`.
  The extracted values arrive as typed parameters to the body function.

## 4. Run the test

```
sbt test
```

Or to run just this suite:

```
sbt "testOnly example.CartSpec"
```

## 5. Interpret the output

The `pretty` reporter prints a colour-coded tree. A passing run looks like this:

```
◉ Feature: Shopping cart
  ◑ Scenario: Adding an item increases the total
    • Given an empty cart
    • When the user adds 2 units of "Widget" at price 9.99
    • Then the cart total should be 19.98

Summary: 1 feature, 1 scenario passed  (47ms)
```

Icons and colours at a glance:

| Icon | Colour  | Meaning         |
|------|---------|-----------------|
| `◉`  | green   | feature passed  |
| `◉`  | red     | feature failed  |
| `◑`  | yellow  | scenario passed |
| `◑`  | red     | scenario failed |
| `◑`  | grey    | scenario skipped / ignored |
| `•`  | blue    | step passed     |
| `•`  | red     | step failed     |
| `•`  | grey    | step skipped (earlier step in scenario failed) |
| `•`  | orange  | step pending    |

When a step fails the tree shows the exception message inline:

```
◉ Feature: Shopping cart
  ◑ Scenario: Adding an item increases the total
    • Given an empty cart
    • When the user adds 2 units of "Widget" at price 9.99
    • Then the cart total should be 19.98
        AssertionError: Expected 19.98, got 19.97
```

## 6. Add a second scenario and a Background

Extend `cart.feature` to share common setup and add a second scenario:

```gherkin
Feature: Shopping cart

  Background:
    Given an empty cart

  Scenario: Adding one item
    When the user adds 2 units of "Widget" at price 9.99
    Then the cart total should be 19.98

  Scenario: Adding two different items
    When the user adds 1 units of "Widget" at price 9.99
    And the user adds 3 units of "Gadget" at price 4.50
    Then the cart total should be 23.49
```

A `Background` block runs before every scenario in the feature. It uses the same step
definitions — no extra Scala code is needed. The `And` keyword is interchangeable with
`When` at the matching level; it is purely cosmetic in the feature file.

Running `sbt test` now executes both scenarios. The background step appears in the pretty
output as a separate group before each scenario's own steps:

```
◉ Feature: Shopping cart
  ◑ Scenario: Adding one item
    • Given an empty cart         (Background)
    • When the user adds 2 units of "Widget" at price 9.99
    • Then the cart total should be 19.98

  ◑ Scenario: Adding two different items
    • Given an empty cart         (Background)
    • When the user adds 1 units of "Widget" at price 9.99
    • And the user adds 3 units of "Gadget" at price 4.50
    • Then the cart total should be 23.49

Summary: 1 feature, 2 scenarios passed  (62ms)
```

From here, explore the [concepts document](concepts.md) to understand the execution model, or
jump to [step-dsl.md](step-dsl.md) for the full extractor reference.
