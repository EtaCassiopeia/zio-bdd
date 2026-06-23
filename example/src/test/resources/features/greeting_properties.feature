@core @property-testing
Feature: Greeting invariants

  # Property test: any non-empty name always produces a greeting of the
  # expected form.  500 generated names are sampled; no literal rows needed.
  @positive @property(samples=500, seed=42, shrink=true)
  Scenario Outline: Greeting always starts with "Hello," and ends with "!"
    Given a user named <name>
    When the user is greeted
    Then the greeting starts with "Hello,"
    And the greeting ends with "!"
    And the greeting contains the name

    Examples:
      | name |

  # Property test: the greeting length is always > 7 characters (length of
  # "Hello, " + at least one character + "!").
  @positive @property(samples=200, seed=99)
  Scenario Outline: Greeting is always longer than 7 characters
    Given a user named <name>
    When the user is greeted
    Then the greeting length is greater than 7

    Examples:
      | name |

  # Intentionally failing property: greetings are never shorter than 5 chars,
  # so this invariant is always falsified on the very first sample.
  @negative @property(samples=100, seed=7, replay=false)
  Scenario Outline: Greeting is shorter than 5 characters (intentionally broken)
    Given a user named <name>
    When the user is greeted
    Then the greeting length is less than 5

    Examples:
      | name |
