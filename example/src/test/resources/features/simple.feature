@core @greeting
Feature: Simple Greeting
  @positive @smoke
  Scenario: Greet a user
    Given a user named World
    When the user is greeted
    Then the greeting should be Hello, World!

  # This scenario is ignored
  @ignore
  Scenario: Greet an empty user
    Given a user named ""
    When the user is greeted
    Then the greeting should be Hello, !

  @negative
  Scenario: Greet a different user
    Given a user named Alice
    When the user is greeted
    Then the greeting should be Hello, Alice!
