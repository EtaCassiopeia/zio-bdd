# zio-bdd

A BDD testing framework for Scala 3 + ZIO 2 that connects Gherkin `.feature` files to type-safe ZIO step definitions.

## Minimal example

```gherkin
# src/test/resources/features/account.feature
Feature: Account balance
  Scenario: Deposit increases balance
    Given an account with balance 100
    When a deposit of 50 is made
    Then the balance should be 150
```

```scala
import zio.*
import zio.bdd.core.{Assertions, Suite}
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

case class AccountState(balance: Int = 0)
object AccountState:
  given Schema[AccountState] = DeriveSchema.gen[AccountState]

@Suite(featureDirs = Array("src/test/resources/features"), reporters = Array("pretty"))
object AccountSpec extends ZIOSteps[Any, AccountState]:
  Given("an account with balance " / int) { (initial: Int) =>
    ScenarioContext.update(_.copy(balance = initial))
  }

  When("a deposit of " / int / " is made") { (amount: Int) =>
    ScenarioContext.update(s => s.copy(balance = s.balance + amount))
  }

  Then("the balance should be " / int) { (expected: Int) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.balance, expected))
  }
```

## Features

- **Type-safe step DSL** — `"text " / string / " more " / int` builds typed extractors; the step body receives `(String, Int)` parameters checked at compile time
- **ZIO environment injection** — suites declare a `ZLayer`-based environment; services are available in every step body via `ZIO.service[T]`
- **Scenario state** — each scenario starts with a fresh `S` instance; steps read/write it through `ScenarioContext.get` / `ScenarioContext.update`
- **Three-tier environment** — `globalLayer` (once per JVM), `featureLayer` (once per feature), `scenarioLayer(meta)` (once per scenario) for fine-grained resource sharing
- **Lifecycle hooks** — `beforeAll`, `afterAll`, `beforeFeature`, `afterFeature`, `beforeScenario`, `afterScenario`, `beforeStep`, `afterStep`
- **Full Gherkin support** — Background, Scenario Outline (Examples tables), data tables, doc strings, tags
- **Flag matrix** — `@flags(key=value)` tags expand a scenario into N runs with different environment values
- **Tag filtering** — `includeTags` / `excludeTags` in `@Suite` or via `--include-tags` / `--exclude-tags` CLI
- **Parallel execution** — feature-level via `@Suite(parallelism = N)`, scenario-level via `--scenario-parallelism N`
- **Dry-run mode** — `--dry-run` validates step matching without executing step bodies
- **Built-in reporters** — `pretty` (ANSI colour tree) and `junitxml` (JUnit 5 compatible XML)
- **Modular state** — `TypeMap` for independent per-module state slices; `HasLens` for nested state without `.copy` chains
- **Step reuse** — `HasService[A, R]` typeclass lets step traits express service requirements without coupling to a concrete `R`
- **Typed table extraction** — `table[T]` reads Gherkin data tables into `List[T]` using ZIO Schema
- **Scalar extractors** — `string`, `word`, `int`, `long`, `double`, `boolean`, `bigDecimal`, `uuid`, `docString`

## Installation

```scala
// build.sbt
libraryDependencies += "local.zio-bdd" %% "zio-bdd" % "0.1.0-pfc-SNAPSHOT" % Test

Test / testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")
```

Requires Scala 3.3.4 and ZIO 2.1.17. `zio-schema` and `zio-schema-derivation` are pulled in
transitively and are needed to derive `Schema[S]` for your state type.

## Documentation

| Document | Description |
|---|---|
| [docs/quickstart.md](docs/quickstart.md) | Zero to running test in 5 minutes |
| [docs/concepts.md](docs/concepts.md) | Mental model: how the framework works |
| [docs/step-definitions.md](docs/step-definitions.md) | Step DSL, extractors, and patterns |
| [docs/gherkin-reference.md](docs/gherkin-reference.md) | Supported Gherkin syntax |
| [docs/advanced-features.md](docs/advanced-features.md) | Flags, TypeMap, HasLens, HasService |
| [docs/running-tests.md](docs/running-tests.md) | CLI flags, reporters, parallelism |
| [docs/examples.md](docs/examples.md) | Annotated end-to-end examples |

## License

Apache 2.0 — see [LICENSE](LICENSE).
