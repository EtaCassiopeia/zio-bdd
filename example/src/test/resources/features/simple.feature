Feature: Simple Greeting
  Scenario: Greet a user
    Given a user named World
    When the user is greeted
    Then the greeting should be Hello, World!
