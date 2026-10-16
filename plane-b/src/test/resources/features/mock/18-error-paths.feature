@mock @errors
Feature: Error paths — typed MockError variants surface as values

  Scenario: removeRule with an unknown id fails with RuleNotFound
    Given a provisioned space
    When removeRule is called with an unknown id
    Then the error is a "RuleNotFound"

  Scenario: operating on a destroyed space fails with SpaceNotFound
    Given a provisioned space
    When the space is destroyed and then queried
    Then the error is a "SpaceNotFound"

  Scenario: a malformed native document fails with InvalidDefinition
    Given a provisioned space
    When a malformed native document is provisioned
    Then the error is a "InvalidDefinition"
