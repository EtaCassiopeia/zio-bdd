
# Examples

This page provides practical examples to demonstrate how to use `zio-bdd` in real-world scenarios. Each example builds on the concepts covered in previous sections, showcasing different features and best practices.

## Example 1: Simple Greeting Service

This example tests a basic greeting service, illustrating step definitions, state management, and service injection.

### Feature File

```gherkin
Feature: User Greeting
  Scenario: Greet a user
    Given a user named "Alice"
    When the user is greeted
    Then the greeting should be "Hello, Alice!"
```

### Step Definitions

```scala
case class Context(userName: String, greeting: String)
object Context {
  implicit val schema: Schema[Context] = DeriveSchema.gen[Context]
}

trait GreetingService {
  def greet(name: String): ZIO[Any, Nothing, String]
}

object GreetingService {
  val live: ZLayer[String, Nothing, GreetingService] =
    ZLayer.fromFunction(prefix => (name: String) => ZIO.succeed(s"$prefix, $name!"))
}

@Suite(featureDir = "features", reporters = Array("pretty"))
object GreetingSpec extends ZIOSteps[GreetingService, Context] {
  Given("a user named " / string) { (name: String) =>
    ScenarioContext.update(_.copy(userName = name))
  }

  When("the user is greeted") {
    for {
      service <- ZIO.service[GreetingService]
      ctx <- ScenarioContext.get
      greeting <- service.greet(ctx.userName)
      _ <- ScenarioContext.update(_.copy(greeting = greeting))
    } yield ()
  }

  Then("the greeting should be " / string) { (expected: String) =>
    ScenarioContext.get.map(_.greeting).map(actual =>
      Assertions.assertTrue(actual == expected, s"Expected '$expected', got '$actual'")
    )
  }

  override def environment: ZLayer[Any, Any, GreetingService] =
    ZLayer.succeed("Hello") >>> GreetingService.live
}
```

- **Context**: Holds the user’s name and greeting.
- **Service**: `GreetingService` is provided via `ZLayer`.
- **Steps**: Use extractors and manage state with `ScenarioContext`.

## Example 2: Data-Driven Testing with Scenario Outline

This example demonstrates data-driven testing using a `Scenario Outline` and `Examples`.

### Feature File

```gherkin
Feature: User Login
  Scenario Outline: Validate login credentials
    Given a user with username "<username>" and password "<password>"
    When the user attempts to login
    Then the login should "<result>"
  Examples:
    | username | password | result  |
    | admin    | admin123 | succeed |
    | user     | wrong    | fail    |
```

### Step Definitions

```scala
case class LoginContext(username: String, password: String, result: Option[String])
object LoginContext {
  implicit val schema: Schema[LoginContext] = DeriveSchema.gen[LoginContext]
}

trait AuthService {
  def login(username: String, password: String): ZIO[Any, Nothing, Boolean]
}

object AuthService {
  val live: ZLayer[Any, Nothing, AuthService] =
    ZLayer.succeed((username: String, password: String) =>
      ZIO.succeed(username == "admin" && password == "admin123")
    )
}

@Suite(featureDir = "features", reporters = Array("pretty"))
object LoginSpec extends ZIOSteps[AuthService, LoginContext] {
  Given("a user with username " / string / " and password " / string) { (username: String, password: String) =>
    ScenarioContext.update(_.copy(username = username, password = password))
  }

  When("the user attempts to login") {
    for {
      service <- ZIO.service[AuthService]
      ctx <- ScenarioContext.get
      success <- service.login(ctx.username, ctx.password)
      _ <- ScenarioContext.update(_.copy(result = Some(if (success) "succeed" else "fail")))
    } yield ()
  }

  Then("the login should " / string) { (expected: String) =>
    ScenarioContext.get.map(_.result).map {
      case Some(actual) => Assertions.assertTrue(actual == expected, s"Expected '$expected', got '$actual'")
      case None => ZIO.fail("Login result not set")
    }
  }

  override def environment: ZLayer[Any, Any, AuthService] = AuthService.live
}
```

- **Scenario Outline**: Runs the scenario for each row in `Examples`.
- **Steps**: Use extractors for placeholders and manage login state.

## Example 3: Using Data Tables

This example shows how to use data tables to pass structured data to steps.

### Feature File

```gherkin
Feature: Batch User Creation
  Scenario: Create multiple users
    Given the following users:
      | name  | age |
      | Alice | 30  |
      | Bob   | 25  |
    When the users are added to the system
    Then the system should have 2 users
```

### Step Definitions

```scala
case class User(name: String, age: Int)
implicit val userSchema: Schema[User] = DeriveSchema.gen[User]

case class SystemState(users: List[User])
object SystemState {
  implicit val schema: Schema[SystemState] = DeriveSchema.gen[SystemState]
}

@Suite(featureDir = "features", reporters = Array("pretty"))
object UserSpec extends ZIOSteps[Any, SystemState] {
  Given("the following users:" / table[User]) { (users: List[User]) =>
    ScenarioContext.update(state => state.copy(users = users))
  }

  When("the users are added to the system") {
    // In a real scenario, this might interact with a service
    ZIO.succeed(())
  }

  Then("the system should have " / int / " users") { (expectedCount: Int) =>
    ScenarioContext.get.map(_.users.size).map { actualCount =>
      Assertions.assertTrue(actualCount == expectedCount, s"Expected $expectedCount users, got $actualCount")
    }
  }
}
```

- **Data Table**: Passed as `List[User]` using `table[User]`.
- **State**: `SystemState` holds the list of users.
