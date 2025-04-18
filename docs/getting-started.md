# Getting Started with zio-bdd

This guide will help you set up and run your first test using `zio-bdd`, a Behavior-Driven Development (BDD) testing framework for Scala 3 and ZIO. By the end, you'll have a simple test suite up and running, demonstrating the core concepts of `zio-bdd`.

## Prerequisites

- **Scala 3**: Ensure you have Scala 3 installed.
- **SBT**: Use SBT 1.9.x or later.
- **Basic ZIO Knowledge**: Familiarity with ZIO's effect system is helpful but not required.

## Step 1: Add Dependencies

In your `build.sbt`, add the following dependencies:

```scala
libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "0.1.0" % Test
libraryDependencies += "dev.zio" %% "zio" % "2.1.17" % Test
libraryDependencies += "dev.zio" %% "zio-test" % "2.1.17" % Test
```

Then, enable the `zio-bdd` test framework:

```scala
Test / testFrameworks += new TestFramework("zio.bdd.core.ZIOBDDFramework")
```

## Step 2: Create a Feature File

Feature files describe the behavior of your application in Gherkin syntax. Create a directory for your feature files, e.g., `src/test/resources/features`, and add a file named `greeting.feature`:

```gherkin
Feature: User Greeting
  Scenario: Greet a user
    Given a user named "Alice"
    When the user is greeted
    Then the greeting should be "Hello, Alice!"
```

This feature file defines a simple scenario where a user is greeted by name.

## Step 3: Define Step Definitions

Step definitions connect the Gherkin steps to executable Scala code. Create a Scala file, e.g., `GreetingSpec.scala`, in your test directory:

```scala
package zio.bdd.example

import zio.*
import zio.bdd.core.{Assertions, Suite}
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

case class Context(userName: String, greeting: String)
object Context {
  implicit val schema: Schema[Context] = DeriveSchema.gen[Context]
}

@Suite(
  featureDir = "src/test/resources/features",
  reporters = Array("pretty"),
  parallelism = 1
)
object GreetingSpec extends ZIOSteps[Any, Context] {
  Given("a user named " / string) { (name: String) =>
    ScenarioContext.update(_.copy(userName = name))
  }

  When("the user is greeted") {
    for {
      ctx <- ScenarioContext.get
      _   <- ScenarioContext.update(_.copy(greeting = s"Hello, ${ctx.userName}!"))
    } yield ()
  }

  Then("the greeting should be " / string) { (expectedGreeting: String) =>
    ScenarioContext.get.map(_.greeting).map { actualGreeting =>
      Assertions.assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', got '$actualGreeting'")
    }
  }
}
```

### Explanation

- **Context**: A case class to hold state across steps, with an implicit ZIO Schema for serialization.
- **@Suite**: Annotation to configure the test suite, specifying the feature directory and reporters.
- **ZIOSteps**: Extend this trait to define step definitions using `Given`, `When`, and `Then`.
- **Step Definitions**: Use patterns like `"a user named " / string` to match Gherkin steps and extract parameters.
- **ScenarioContext**: Manage state across steps with `get` and `update`.

## Step 4: Run the Tests

To run the tests, use SBT:

```bash
sbt test
```

You should see output indicating that the scenario passed:

```
[info] Feature: User Greeting
[info]   Scenario: Greet a user
[info]     Given a user named "Alice"                                  [PASSED]
[info]     When the user is greeted                                    [PASSED]
[info]     Then the greeting should be "Hello, Alice!"                 [PASSED]
```

## Next Steps

- Explore more Gherkin syntax in [Gherkin Syntax Reference](gherkin-reference.md).
- Learn about advanced step definitions in [Step Definitions](step-definitions.md).
- Discover how to run tests with different configurations in [Running Tests](running-tests.md).
