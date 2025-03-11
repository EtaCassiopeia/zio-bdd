# zio-bdd

**zio-bdd** is an open-source Behavior-Driven Development (BDD) testing framework for Scala 3, tailored for ZIO-based applications. It integrates Gherkin-style testing with ZIO’s effect system, fiber-based concurrency, and compile-time safety, providing a powerful and modern testing solution for the ZIO ecosystem.

:warning: **This project is under heavy development and is not yet production ready.**

## Key Features
- **Compile-Time Step Resolution**: Uses Scala 3 macros to match Gherkin steps at compile time, ensuring type safety and eliminating runtime reflection errors.
- **Fiber-Based Parallelism**: Executes scenarios concurrently with ZIO fibers for efficient, lightweight concurrency.
- **ZIO Effect Integration**: Seamlessly supports ZIO effects and ZLayer for type-safe dependency injection without blocking calls.
- **Dynamic Feature Discovery**: Automatically scans the module’s `src/test/resources/features/` directory for `.feature` files, customizable via the `@Suite` annotation.
- **Customizable Reporting**: Offers built-in console and JUnit XML reporters, with extensibility for custom formats.
- **Enhanced Gherkin**: Extends standard Gherkin with testing aspects like `@flaky`, `@repeat`, and `@retry`, plus support for advanced features like `Background` and `Scenario Outline`.

## Feature Checklist

- [X] Gherkin parser
  - [X] Feature: Defines a feature with a name and optional tags
  - [X] Background: Specifies steps that run before each scenario in a feature
  - [X] Scenario: Defines a single test scenario with steps
  - [X] Scenario Outline: Supports parameterized scenarios with placeholders
  - [X] Examples: Provides data tables for Scenario Outlines
  - [X] Step: Supports `Given`, `When`, `Then`, and `And` keywords with text patterns
  - [X] Tags: Allows `@tag` annotations for filtering and test aspects (e.g., `@flaky`, `@retry(n)`)
  - [ ] Rule: Groups scenarios under a rule
  - [ ] Doc String: Multi-line string arguments for steps
- [X] Step definition and macro expansion
- [X] Feature and Scenario Runner
- [X] SBT test interface for framework integration
- [X] Feature discovery
- [X] Extended Gherkin with test aspects: Flaky, Repeat, and Retry
- [X] Enhance output value propagation between steps using OutputStack
- [X] Implement JUnit XML reporting for CI integration
- [X] Add hook support for before/after steps, scenarios, and features
- [X] Improve feature discovery and runner configurations
- [X] Tag-based filtering for features and scenarios
- [ ] Extend Gherkin syntax to support ZIO test generators in examples and scenario outlines
- [ ] Publish artifacts on Sonatype
- [ ] Implement plugins for navigation between feature files and step definitions (e.g., IntelliJ, VSCode)
- [ ] Add support for linting and code formatting
- [ ] Introduce E2E testing with test flow support
- [ ] Expand Gherkin to handle event-driven scenarios with event and trigger mechanisms
- [ ] Create comprehensive documentation

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
Create `example/src/test/scala/zio/bdd/example/UserSpec.scala`:
```scala
package zio.bdd.example

import zio.*
import zio.bdd.core.{ZIOSteps, Suite}

@Suite(featureDir = "example/src/test/resources/features", reporters = Array("console", "junitxml"), parallelism = 2)
object UserSpec extends ZIOSteps.Default[UserService] {
  Given[String, User]("a user named {string}") { name =>
    ZIO.serviceWithZIO[UserService](_.createUser(name))
  }

  Given[User, Unit]("the user has an account") { user =>
    ZIO.serviceWithZIO[UserService](_.activateAccount(user))
  }

  When[User, String]("the user requests a greeting") { user =>
    ZIO.serviceWithZIO[UserService](_.greet(user))
  }

  Then[String, Unit]("the greeting should be {string}") { expectedGreeting =>
    for {
      actualGreeting <- ZIO.serviceWithZIO[UserService](_.greet(User("World")))
      _              <- ZIO.succeed(assert(actualGreeting == expectedGreeting))
    } yield ()
  }

  override def environment: ZLayer[Any, Any, UserService] =
    ZLayer.succeed(UserServiceImpl())
}

case class User(name: String, active: Boolean = false)

trait UserService {
  def createUser(name: String): ZIO[Any, Nothing, User]
  def activateAccount(user: User): ZIO[Any, Nothing, Unit]
  def greet(user: User): ZIO[Any, Nothing, String]
}

case class UserServiceImpl() extends UserService {
  override def createUser(name: String): ZIO[Any, Nothing, User] = ZIO.succeed(User(name))
  override def activateAccount(user: User): ZIO[Any, Nothing, Unit] = ZIO.unit
  override def greet(user: User): ZIO[Any, Nothing, String] = ZIO.succeed(s"Hello, ${user.name}!")
}
```

### 2. Create an Advanced Feature File
Create `example/src/test/resources/features/user.feature`:
```gherkin
@core @user
Feature: User Greeting Management
  Background:
    Given a user named "DefaultUser"
    And the user has an account

  @positive @repeat(2) @flaky
  Scenario: Greet an active user
    When the user requests a greeting
    Then the greeting should be "Hello, DefaultUser!"
    
  Scenario Outline: Greet users with different names
    Given a user named "<name>"
    When the user requests a greeting
    Then the greeting should be "<greeting>"
    Examples:
      | name   | greeting         |
      | Alice  | Hello, Alice!    |
      | Bob    | Hello, Bob!      |
```

- **Explanation**:
  - **Background**: Sets up common steps (`a user named "DefaultUser"` and `the user has an account`) for all scenarios.
  - **Scenario**: A simple test with tags `@positive`, running twice (`@repeat(2)`) and marked as flaky (`@flaky`).
  - **Scenario Outline**: Parameterized test with `<name>` and `<greeting>`.
  - **Examples**: Defines test data for the outline.

### 3. Run Tests
- **Run All Tests**:
  ```bash
  sbt "example/test"
  ```
  - Executes all scenarios, including the repeated outline runs.

- **Filter by Tags (Example)**:
  ```bash
  sbt "testOnly zio.bdd.example.UserSpec -- --include-tags positive"
  ```
  - Runs only the `Greet an active user` scenario due to `@positive`.

- **Run with Specific File**:
  ```bash
  sbt "testOnly zio.bdd.example.UserSpec -- --feature-file example/src/test/resources/features/user.feature"
  ```

- **Debug Mode**:
  ```bash
  sbt --debug "example/test"
  ```

### Example Output
Running with default settings:
```
[info] Loaded 1 features with 3 scenarios
[info] * Feature: User Greeting Management
[info]   ◉ Scenario: Greet an active user
[info]     ├─◑ [PASSED] Given a user named "DefaultUser"
[info]     ├─◑ [PASSED] And the user has an account
[info]     ├─◑ [PASSED] When the user requests a greeting
[info]     ├─◑ [PASSED] Then the greeting should be "Hello, DefaultUser!"
[info]   ◉ Scenario Outline: Greet users with different names (iteration 1)
[info]     ├─◑ [PASSED] Given a user named "Alice"
[info]     ├─◑ [PASSED] When the user requests a greeting
[info]     ├─◑ [PASSED] Then the greeting should be "Hello, Alice!"
[info]   ◉ Scenario Outline: Greet users with different names (iteration 2)
[info]     ├─◑ [PASSED] Given a user named "Bob"
[info]     ├─◑ [PASSED] When the user requests a greeting
[info]     ├─◑ [PASSED] Then the greeting should be "Hello, Bob!"
```

## Contributions
`zio-bdd` is an open-source project aimed at advancing BDD testing in the ZIO ecosystem! We welcome contributions to enhance features like concurrency, reporting, or Gherkin extensions. Submit issues or pull requests on GitHub to join the community effort.
