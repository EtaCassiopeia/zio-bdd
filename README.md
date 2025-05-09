# zio-bdd

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/EtaCassiopeia/zio-bdd)

**zio-bdd** is a modern Behavior-Driven Development (BDD) testing framework for Scala 3, tailored for ZIO-based applications. It integrates Gherkin-style feature files with ZIO’s effect system, offering compile-time safety, fiber-based concurrency, and type-safe dependency injection via `ZLayer`. This framework enables developers to write expressive, business-aligned tests with robust error handling and modular service integration.

## Key Features
- **Type-Safe Step Definitions**: Define steps with typed parameters, ensuring compile-time safety and reducing runtime errors.
- **Typed Parameter Extraction**: Supports extractors (`string`, `int`, `double`, `table[T]`) for safe parameter parsing, integrated with ZIO Schema for complex data.
- **State Management**: Uses `ScenarioContext` to manage test state across steps, enabling seamless data sharing.
- **Fiber-Based Concurrency**: Executes scenarios concurrently using ZIO fibers for high-performance testing.
- **ZIO Service Integration**: Utilizes `ZLayer` for modular, type-safe dependency injection in test environments.
- **Dynamic Feature Loading**: Automatically discovers `.feature` files from a specified directory, with tag-based filtering.
- **Flexible Reporting**: Includes `PrettyReporter` for console output and JUnit XML support, with extensibility for custom formats.
- **Rich Gherkin Support**: Handles `Background`, `Scenario Outline`, data tables, and tags (e.g., `@ignore`, `@positive`).

## Setup

**Add Dependency**:
```scala
libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "0.1.0" % Test,
libraryDependencies += "dev.zio" %% "zio" % "2.1.17" % Test,
libraryDependencies += "dev.zio" %% "zio-test" % "2.1.17" % Test,
```

**Enable zio-bdd in sbt**:
```scala
Test / testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")
```

## Usage Example

The following example tests a greeting service, demonstrating how to define steps, manage state with `ScenarioContext`, and integrate ZIO services using a Gherkin feature file.

### 1. Define Domain and Steps

Create `example/src/test/scala/zio/bdd/example/GreetingSpec.scala`:

```scala
package zio.bdd.example

import zio.*
import zio.bdd.core.{Assertions, Suite}
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

// Domain and state
case class Context(userName: String, greeting: String)
object Context {
  implicit val schema: Schema[Context] = DeriveSchema.gen[Context]
}

// Service
trait GreetingService {
  def greet(name: String): ZIO[Any, Nothing, String]
}
case class GreetingServiceImpl(prefix: String) extends GreetingService {
  def greet(name: String): ZIO[Any, Nothing, String] = ZIO.succeed(s"$prefix, $name!")
}
object GreetingService {
  val live: ZLayer[String, Nothing, GreetingService] =
    ZLayer.fromFunction(GreetingServiceImpl(_))
}

// Test specification
@Suite(
  featureDir = "example/src/test/resources/features",
  reporters = Array("pretty", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive"),
  logLevel = "debug"
)
object GreetingSpec extends ZIOSteps[GreetingService, Context] {
  Given("a user named " / string) { (name: String) =>
    ScenarioContext.update(_.copy(userName = name))
  }

  When("the user is greeted") {
    for {
      ctx <- ScenarioContext.get
      service <- ZIO.service[GreetingService]
      greeting <- service.greet(ctx.userName)
      _ <- ScenarioContext.update(_.copy(greeting = greeting))
    } yield ()
  }

  Then("the greeting should be " / string) { (expectedGreeting: String) =>
    ScenarioContext.get.map(_.greeting).map { actualGreeting =>
      Assertions.assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', got '$actualGreeting'")
    }
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    GreetingService.live
}
```

### 2. Create a Feature File

Create `example/src/test/resources/features/greeting.feature`:

```gherkin
@core @greeting
Feature: User Greeting
  Scenario: Greet a positive user
    Given a user named "Alice"
    When the user is greeted
    Then the greeting should be "Hello, Alice!"

  @positive
  Scenario: Greet another positive user
    Given a user named "Bob"
    When the user is greeted
    Then the greeting should be "Hello, Bob!"
```

- **Explanation**:
  - **Scenarios**: Test greeting different users, with one scenario tagged `@positive` to match the `@Suite` filter.
  - **Steps**: Use typed extractors (`string`) to parse user names and expected greetings.
  - **Tags**: `@core`, `@greeting`, and `@positive` enable filtering.

### 3. Run Tests

**Run All Tests**:
```bash
sbt "example/test"
```
- Executes all scenarios, with `PrettyReporter` output.

**Run with Tag Filter**:
```bash
sbt "testOnly zio.bdd.example.GreetingSpec -- --include-tags positive"
```
- Runs only the `@positive` scenario ("Greet another positive user").

**Run Specific Feature**:
```bash
sbt "testOnly zio.bdd.example.GreetingSpec -- --feature-file example/src/test/resources/features/greeting.feature"
```

**Debug Mode**:
```bash
sbt "example/testOnly zio.bdd.example.GreetingSpec -- -DlogLevel=debug"
```
- Enables detailed logging for step execution.

## Contributions

`zio-bdd` is an open-source project aimed at advancing BDD testing in the ZIO ecosystem. We welcome contributions to enhance Gherkin support, reporting, or performance. Submit issues or pull requests on GitHub to join the community.

## License

Licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.
