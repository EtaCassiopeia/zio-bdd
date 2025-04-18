# Gherkin Syntax Reference

This page provides a comprehensive reference for writing feature files in `zio-bdd` using Gherkin syntax. Gherkin is a human-readable, domain-specific language that describes application behavior in a structured format, making it ideal for Behavior-Driven Development (BDD).

## Overview

Gherkin files (`.feature`) consist of keywords like `Feature`, `Scenario`, `Given`, `When`, `Then`, `And`, and `But`. These keywords help define the behavior of your application in a way that’s understandable to both technical and non-technical stakeholders.

## Structure of a Feature File

### Feature
The `Feature` keyword defines the high-level functionality or behavior being described. It’s followed by a name and an optional description.

```gherkin
Feature: User Greeting
  As a system
  I want to greet users by name
  So that they feel welcomed
```

- **Name**: A short, descriptive title (e.g., "User Greeting").
- **Description**: Optional free-text explanation, often in "As a... I want... So that..." format.

### Scenario
A `Scenario` outlines a specific example or test case for the feature. It’s a sequence of steps that describe a single path of behavior.

```gherkin
Scenario: Greet a user
  Given a user named "Alice"
  When the user is greeted
  Then the greeting should be "Hello, Alice!"
```

- Each scenario is independent and should be executable on its own.

### Steps
Steps define the actions and assertions in a scenario. They use the following keywords:

- **Given**: Sets up the initial context or preconditions.
- **When**: Describes an action or event.
- **Then**: Specifies the expected outcome or assertion.
- **And**: Continues a previous step type (e.g., multiple `Given` conditions).
- **But**: Adds a negative condition or exception (less common).

Example with multiple steps:
```gherkin
Scenario: Greet a user with a title
  Given a user named "Alice"
  And the user has the title "Dr."
  When the user is greeted
  Then the greeting should be "Hello, Dr. Alice!"
```

## Parameterization

Steps can include parameters (e.g., strings, numbers) enclosed in quotes. These are extracted and passed to step definitions in `zio-bdd`.

```gherkin
Given a user named "Alice"
Then the greeting should be "Hello, Alice!"
```

- Use `"..."` for strings.
- Parameters must match the step definition’s type extractors (e.g., `string`, `int`).

## Data Tables

Data tables provide structured input or expected output for steps, written with `|` delimiters.

```gherkin
Scenario: Greet multiple users
  Given the following users:
    | name  | title |
    | Alice | Dr.   |
    | Bob   | Mr.   |
  When the users are greeted
  Then the greetings should be:
    | greeting         |
    | Hello, Dr. Alice!|
    | Hello, Mr. Bob!  |
```

- Tables are passed as `List[User]` to step definitions.

## Scenario Outlines

A `Scenario Outline` allows running the same scenario with multiple examples, using placeholders (`<placeholder>`) and an `Examples` table.

```gherkin
Scenario Outline: Greet a user with different names
  Given a user named "<name>"
  When the user is greeted
  Then the greeting should be "<greeting>"
  Examples:
    | name  | greeting      |
    | Alice | Hello, Alice! |
    | Bob   | Hello, Bob!   |
```

- Placeholders are replaced with values from the `Examples` table for each row.

## Tags

Tags (e.g., `@smoke`, `@wip`) filter or categorize scenarios. Add them above `Feature` or `Scenario`.

```gherkin
@smoke
Feature: User Greeting

@wip
Scenario: Greet a user
  Given a user named "Alice"
  When the user is greeted
  Then the greeting should be "Hello, Alice!"
```

- Use tags to control test execution in `zio-bdd` (e.g., run only `@smoke` tests).

## Comments

Comments start with `#` and are ignored during execution.

```gherkin
# This is a comment
Feature: User Greeting
```

## Best Practices

- **Keep it Simple**: Write concise, focused scenarios.
- **Be Declarative**: Focus on "what" not "how" (e.g., "Given a user is logged in" vs. "Given I click the login button").
- **Reuse Steps**: Define generic steps for reuse across scenarios.
- **Use Examples**: Leverage `Scenario Outline` for data-driven tests.

## Next Steps

- Learn how to implement these in [Step Definitions](step-definitions.md).
- Explore test execution options in [Running Tests](running-tests.md).
