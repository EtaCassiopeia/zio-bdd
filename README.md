# zio-bdd

**zio-bdd** is an open-source Behavior-Driven Development (BDD) testing framework for Scala 3, designed specifically for ZIO-based applications. It blends Gherkin-style testing with ZIO’s effect system, fiber-based concurrency, and compile-time safety, addressing the limitations of existing BDD tools in the ZIO ecosystem.

 :warning: **This project is under heavy development and is not yet production ready.**

## Motivation
ZIO’s fiber-based concurrency and effect management are powerful for building scalable Scala applications, but traditional BDD frameworks struggle to integrate seamlessly:
- **Scala-Cucumber**: Uses runtime reflection for step matching, lacks fiber support, and poorly integrates with ZIO effects and ZLayer, often requiring blocking calls like unsafeRun.
- **Specs2/ScalaTest**: Provide limited or no native Gherkin-style BDD, rely on thread-based execution, and miss ZIO-specific features like TestClock or ZLayer.

These gaps result in fragile tests, limited concurrency, and cumbersome setups. **zio-bdd** was created to offer a ZIO-native BDD solution with type-safe steps, parallel execution, and effortless dependency management.

## Key Features
- **Compile-Time Step Resolution**: Leverages Scala 3 macros to match Gherkin steps at compile time, avoiding runtime reflection errors seen in Scala-Cucumber.
- **Fiber-Based Parallelism**: Runs scenarios concurrently using ZIO fibers, outperforming thread-based frameworks like ScalaTest or Scala-Cucumber.
- **ZIO Effect Integration**: Natively supports ZIO effects and ZLayer for type-safe dependency injection, eliminating blocking calls.
- **Dynamic Feature Discovery**: Automatically scans the module’s `src/test/resources/features/` directory for `.feature` files, configurable via `@ZIOBDDTest`.
- **Customizable Reporting**: Includes console, JUnit XML, and extensible reporting for CI integration.
- **Extending Standard Gherkin**: Adds testing aspects like `flaky`, `repeat`, and `retry` annotations.

## Feature Checklist

- [X] Gherkin parser
- [X] Step definition and macro expansion
- [X] ScenarioRunner
- [X] SBT test interface for framework integration
- [X] Feature discovery
- [X] Extended Gherkin with test aspects: Flaky, Repeat, and Retry
- [X] Enhance output value propagation between steps using OutputStack
- [X] Implement JUnit XML reporting for CI integration
- [X] Add hook support for before/after steps, scenarios, and features
- [X] Improve feature discovery and runner configurations
- [ ] Extend Gherkin syntax to support ZIO test generators in examples and scenario outlines
- [ ] Publish artifacts on Sonatype
- [ ] Implement plugins for navigate between feature files and step definitions (e.g., IntelliJ, VSCode)
- [ ] Add support for linting and code formatting
- [ ] Introduce E2E testing with test flow support
- [ ] Expand Gherkin to handle event-driven scenarios with event and trigger mechanisms
- [ ] Improve test coverage

## Setup

**Add Dependency**:
```scala
libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "0.1.0" % Test // Not yet published, use local build
libraryDependencies += "dev.zio" %% "zio" % "2.1.16" // Required for ZIO effects

// Enable zio-bdd in sbt
Test / testFrameworks += new TestFramework("zio.bdd.core.ZIOBDDFramework")
```

## Usage Example
### 1. Define a Test Spec
Create `example/src/test/scala/zio/bdd/example/SimpleSpec.scala`:
```scala
package zio.bdd.example

import zio.*
import zio.bdd.core.{ZIOSteps, Suite}

@Suite(featureDir = "example/src/test/resources/features", reporters = Array("console", "junitxml"), parallelism = 2)
object SimpleSpec extends ZIOSteps.Default[GreetingService] {
  Given[String, String]("a user named {string}") { name =>
    ZIO.succeed(name)
  }

  When[String, String]("the user is greeted") { name =>
    ZIO.serviceWithZIO[GreetingService](_.greet(name))
  }

  Then[String, Unit]("the greeting should be {string}") { expectedGreeting =>
    for {
      actualGreeting <- ZIO.serviceWithZIO[GreetingService](_.greet("World"))
      _              <- ZIO.succeed(assert(actualGreeting == expectedGreeting))
    } yield ()
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}

trait GreetingService {
  def greet(name: String): ZIO[Any, Nothing, String]
}

object GreetingService {
  val live: ZLayer[Config, Nothing, GreetingService] = ZLayer.fromFunction { (config: Config) =>
    new GreetingService {
      override def greet(name: String): ZIO[Any, Nothing, String] =
        ZIO.succeed(s"${config.greetingPrefix}, $name!")
    }
  }
}

case class Config(greetingPrefix: String)
```

### 2. Create a Feature File
Create `example/src/test/resources/features/simple.feature`:

```gherkin
Feature: Simple Greeting
  Scenario: Greet a user
    Given a user named World
    When the user is greeted
    Then the greeting should be Hello, World!
```

### 3. Run Tests
- **Run All Tests in example Module**:
  ```bash
  sbt "example/test"
  ```

- **Run Specific Test with Explicit File**:
  ```bash
  sbt "testOnly zio.bdd.example.SimpleSpec -- --feature-file example/src/test/resources/features/simple.feature"
  ```

- **Debug Mode (Verbose Output)**:
  ```bash
  sbt --debug "example/test"
  ```

## Contributions
zio-bdd is an open-source project! We welcome contributions to enhance features like parallel execution, custom reporters, or Gherkin parsing. Submit issues or pull requests on GitHub to join the community effort.
