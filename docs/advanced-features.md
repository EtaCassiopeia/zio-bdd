
# Advanced Features in zio-bdd

This page delves into advanced capabilities of `zio-bdd`, including integrating ZIO services, managing complex state, using `ScenarioContext` effectively, and leveraging custom reporters. These features enable you to write sophisticated tests for complex applications.

## Integrating ZIO Services

`zio-bdd` seamlessly integrates with ZIO's dependency injection system via `ZLayer`. This allows you to provide services to your step definitions, making your tests modular and easy to manage.

### Providing Services

In your `ZIOSteps` subclass, override the `environment` method to provide the required services:

```scala
override def environment: ZLayer[Any, Any, Env] =
  ZLayer.make[Env](
    Service1.live,
    Service2.live
  )
```

- **Env**: The type of the environment required by your steps (e.g., `Service1 with Service2`).

### Using Services in Steps

Access services in your step definitions using `ZIO.service`:

```scala
When("some action is performed") {
  for {
    service <- ZIO.service[Service1]
    result  <- service.doSomething()
    // ...
  } yield ()
}
```

This approach keeps your tests clean and decoupled from service implementations.

## Managing Complex State

For tests requiring intricate state management, `ScenarioContext` can hold complex data structures. Define a case class to represent your state and provide an implicit `Schema` for serialization.

### Example

```scala
case class ComplexState(users: Map[String, User], currentUser: Option[String])
implicit val schema: Schema[ComplexState] = DeriveSchema.gen[ComplexState]

Given("a user named " / string) { (name: String) =>
  for {
    state <- ScenarioContext.get
    updatedUsers = state.users + (name -> User(name))
    _ <- ScenarioContext.update(_.copy(users = updatedUsers))
  } yield ()
}
```

- Use `ScenarioContext.update` to modify the state immutably.

## Custom Reporters

While `zio-bdd` provides built-in reporters, you can create custom reporters for specialized output formats.

### Implementing a Custom Reporter

Extend the `Reporter` trait:

```scala
class CustomReporter extends Reporter {
  override def report(results: List[FeatureResult]): ZIO[Any, Nothing, Unit] = {
    // Custom reporting logic
    ZIO.succeed(())
  }
}
```

- Register your reporter in the `@Suite` annotation:

```scala
@Suite(reporters = Array("custom"))
object MySpec extends ZIOSteps[Env, State] {
  // ...
}
```

- You'll need to map `"custom"` to your `CustomReporter` instance in the test framework configuration.

## Scenario Outlines and Examples

`zio-bdd` fully supports Gherkin’s `Scenario Outline` and `Examples` for data-driven testing.

### Defining Steps for Outlines

Write step definitions as usual, using extractors for placeholders:

```scala
Given("a user named " / string) { (name: String) =>
  // ...
}
```

- `zio-bdd` automatically handles the expansion of outlines into multiple scenarios.

## Background Steps

Gherkin’s `Background` section runs before each scenario in a feature. Define steps for background as you would for any other step.

### Example

In your feature file:

```gherkin
Feature: User Management
  Background:
    Given a system is running
  Scenario: Create user
    When a user is created
    Then the user exists
```

Define the background step:

```scala
Given("a system is running") {
  // Setup system
}
```

- Background steps are executed before each scenario, ensuring a consistent starting state.

## Best Practices

- **Service Mocking**: Use test doubles or mocks for services to isolate tests.
- **State Reset**: Ensure each scenario starts with a clean state, especially when using shared resources.
- **Custom Extractors**: For complex parameter types, define custom extractors extending `TypedExtractor`.

## Next Steps

- Explore practical applications in [Examples](examples.md).
- Contribute to `zio-bdd` by checking the [Contribution Guide](contributing.md).
