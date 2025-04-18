# Step Definitions in zio-bdd

This page explains how to define step definitions in `zio-bdd` to match the steps in your Gherkin feature files. Step definitions are the executable code that runs when a step in a scenario is encountered.

## Overview

In `zio-bdd`, step definitions are created using the `ZIOSteps` trait. This trait provides methods like `Given`, `When`, and `Then` to register steps with specific patterns and corresponding ZIO effects that execute the step's logic.

## Defining Steps

### Basic Step Definition

To define a step, use `Given`, `When`, or `Then`, specifying a pattern to match the step text and a ZIO effect to run when the step is matched.

```scala
Given("a user named " / string) { (name: String) =>
  ZIO.succeed(println(s"Setting up user: $name"))
}
```

- **Pattern**: Combines literal text (e.g., `"a user named "`) with extractors (e.g., `string`) to capture parameters.
- **Effect**: A ZIO effect that executes the step’s action, using the extracted values like `name`.

### Type Extractors

`zio-bdd` provides type extractors to safely parse parameters from step text:

- `string`: Extracts a string, handling quotes automatically.
- `int`: Extracts an integer.
- `double`: Extracts a double.
- `long`: Extracts a long.
- `table[T]`: Extracts a data table into a list of type `T` (requires a ZIO `Schema[T]`).

Example with multiple extractors:

```scala
Given("a user named " / string / " with age " / int) { (name: String, age: Int) =>
  ZIO.succeed(println(s"User: $name, Age: $age"))
}
```

### Data Tables

For steps with Gherkin data tables, use the `table[T]` extractor. Define a case class for the table rows and provide an implicit `Schema[T]`.

```scala
case class User(name: String, age: Int)
implicit val userSchema: Schema[User] = DeriveSchema.gen[User]

Given("the following users:" / table[User]) { (users: List[User]) =>
  ZIO.foreach(users) { user =>
    ZIO.succeed(println(s"Adding user: ${user.name}, ${user.age}"))
  }
}
```

- The table’s columns must match the case class fields (`name` and `age` in this case).

## Managing State with ScenarioContext

`ScenarioContext` is a mutable state container that persists across steps within a scenario, allowing you to share data between steps.

### Accessing and Updating Context

- **Get**: Use `ScenarioContext.get` to retrieve the current state.
- **Update**: Use `ScenarioContext.update` to modify the state.

Example:

```scala
case class Context(userName: String, greeting: String)
implicit val schema: Schema[Context] = DeriveSchema.gen[Context]

Given("a user named " / string) { (name: String) =>
  ScenarioContext.update(_.copy(userName = name))
}

Then("the greeting should be " / string) { (expectedGreeting: String) =>
  ScenarioContext.get.map(_.greeting).map { actualGreeting =>
    Assertions.assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', got '$actualGreeting'")
  }
}
```

- Define a case class for the context and provide an implicit `Schema`.

## Advanced Step Definitions

### Multiple Parameter Types

Combine extractors for complex steps:

```scala
When("the user adds " / int / " units of product " / string) { (quantity: Int, productId: String) =>
  // Add to cart logic
}
```

### Handling Scenario Outlines

Scenario outlines in Gherkin are automatically expanded into multiple scenarios by `zio-bdd`. Define steps with placeholders as you would for regular steps, and `zio-bdd` handles the rest.

### Using ZIO Services

Inject ZIO services into your steps for dependency management:

```scala
When("the user is greeted") {
  for {
    service <- ZIO.service[GreetingService]
    ctx <- ScenarioContext.get
    greeting <- service.greet(ctx.userName)
    _ <- ScenarioContext.update(_.copy(greeting = greeting))
  } yield ()
}
```

- Provide the service via the `environment` method in your `ZIOSteps` implementation.

## Best Practices

- **Keep Steps Atomic**: Each step should focus on one clear action.
- **Reuse Steps**: Write generic steps usable across multiple scenarios.
- **Leverage Type Extractors**: Use extractors for type safety and to avoid parsing errors.
- **Manage State Wisely**: Use `ScenarioContext` for data that needs to persist across steps.

## Next Steps

- Learn how to execute your tests in [Running Tests](running-tests.md).
- Dive into additional features in [Advanced Features](advanced.md).
