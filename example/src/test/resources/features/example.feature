Feature: User Password Reset
  Background:
    Given a user exists with name "Default"
  Scenario: Successful password reset with logging
    When the user requests a password reset
    And the reset email is logged
    Then an email should be sent to "default@example.com"