# ZIOBDDFramework

**ZIOBDDFramework** is an open-source Behavior-Driven Development (BDD) testing framework for Scala 3, designed specifically for ZIO-based applications. It blends Gherkin-style testing with ZIO’s effect system, fiber-based concurrency, and compile-time safety, addressing the limitations of existing BDD tools in the ZIO ecosystem.

## Motivation
ZIO’s fiber-based concurrency and effect management are powerful for building scalable Scala applications, but traditional BDD frameworks struggle to integrate seamlessly:
- **Scala-Cucumber**: Uses runtime reflection for step matching, lacks fiber support, and poorly integrates with ZIO effects and `ZLayer`, often requiring blocking calls like `unsafeRun`.
- **Specs2/ScalaTest**: Provide limited or no native Gherkin-style BDD, rely on thread-based execution, and miss ZIO-specific features like `TestClock` or `ZLayer`.

These gaps result in fragile tests, limited concurrency, and cumbersome setups. **ZIOBDDFramework** was created to offer a ZIO-native BDD solution with type-safe steps, parallel execution, and effortless dependency management.

## Key Features
- **Compile-Time Step Resolution**: Leverages Scala 3 macros to match Gherkin steps at compile time, avoiding runtime reflection errors seen in Scala-Cucumber.
- **Fiber-Based Parallelism**: Runs scenarios concurrently using ZIO fibers, outperforming thread-based frameworks like ScalaTest or Scala-Cucumber.
- **ZIO Effect Integration**: Natively supports ZIO effects and `ZLayer` for type-safe dependency injection, eliminating blocking calls.
- **Dynamic Feature Discovery**: Automatically scans the module’s `src/test/resources/features/` directory for `.feature` files, configurable via `@ZIOBDDTest`.
- **Customizable Reporting**: Includes console, JUnit XML, and extensible reporting for CI integration.

## How It Differs
- **From Scala-Cucumber**: Replaces reflection with compile-time checks, uses ZIO fibers instead of threads, and integrates `ZLayer` for dependency management, reducing runtime errors and boilerplate.
- **From Specs2/ScalaTest**: Offers a Gherkin-based BDD DSL with ZIO-first design, leveraging fibers and `ZLayer` for concurrency and setup, unlike their thread-based, code-centric approaches.

## Setup

**Add Dependency**:
   ```scala
   libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "0.1.0" % Test
   libraryDependencies += "dev.zio" %% "zio" % "2.1.16" // Required for ZIO effects

   // Enable ZIOBDDFramework in sbt
   Test / testFrameworks += new TestFramework("zio.bdd.core.ZIOBDDFramework")
   ```

## Usage Example
### 1. Define a Test Spec
Create `example/src/test/scala/zio/bdd/example/SimpleSpec.scala`:
```scala
package zio.bdd.example

import zio.*
import zio.bdd.core.{ZIOSteps, ZIOBDDTest}

@ZIOBDDTest(featureDir = "example/src/test/resources/features")
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

  // Provide the environment layer using ZLayer composition
  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed(Config("Hello")) >>> GreetingService.live
}

// Example service
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
```
Feature: Simple Greeting
  Scenario: Greet a user
    Given a user named World
    When the user is greeted
    Then the greeting should be Hello, World!
```

### 3. Run Tests
- **Run All Tests in `example` Module**:
  ```bash
  sbt "example/test"
  ```
  This scans `example/src/test/resources/features/` for all `.feature` files automatically.

- **Run Specific Test with Explicit File**:
  ```bash
  sbt "testOnly zio.bdd.example.SimpleSpec -- --feature-file example/src/test/resources/features/simple.feature"
  ```

- **Debug Mode (Verbose Output)**:
  ```bash
  sbt --debug "example/test"
  ```

### Expected Output
```
* Feature: Simple Greeting
  ◉ Scenario: a user named World
the user is greeted
the greeting should be Hello, World!
    ├─◑ a user named World
    ├─◑ [PASSED] a user named World
      ╰─ [2025-03-03T04:40:07.799662385Z] Executing: a user named World with input: World
      ╰─ [2025-03-03T04:40:07.784392948Z] Selected Input for a user named World: World
      ╰─ [2025-03-03T04:40:07.783501740Z] Step: a user named World, Context: None, PreviousOutput: (), Params: List(World)
    ├─◑ the user is greeted
    ├─◑ [PASSED] the user is greeted
      ╰─ [2025-03-03T04:40:07.813150260Z] Executing: the user is greeted with input: World
      ╰─ [2025-03-03T04:40:07.812827291Z] Selected Input for the user is greeted: World
      ╰─ [2025-03-03T04:40:07.812790208Z] Step: the user is greeted, Context: Some(World), PreviousOutput: World, Params: List()
      ╰─ [2025-03-03T04:40:07.812573373Z] After a user named World, NewContext: Some(World), NewPreviousOutput: World
    ├─◑ the greeting should be Hello, World!
    ├─◑ [PASSED] the greeting should be Hello, World!
      ╰─ [2025-03-03T04:40:07.814245483Z] Executing: the greeting should be Hello, World! with input: Hello, World!
      ╰─ [2025-03-03T04:40:07.814045655Z] Selected Input for the greeting should be Hello, World!: Hello, World!
      ╰─ [2025-03-03T04:40:07.814034174Z] Step: the greeting should be Hello, World!, Context: Some(Hello, World!), PreviousOutput: Hello, World!, Params: List(Hello, World!)
      ╰─ [2025-03-03T04:40:07.813943412Z] After the user is greeted, NewContext: Some(Hello, World!), NewPreviousOutput: Hello, World!
  ◉ Results: 3 passed, 0 failed
* Finished Feature: Simple Greeting - 3 passed, 0 failed
```

## Contributions
ZIOBDDFramework is an open-source project! We welcome contributions to enhance features like parallel execution, custom reporters, or Gherkin parsing. Submit issues or pull requests on GitHub to join the community effort.
