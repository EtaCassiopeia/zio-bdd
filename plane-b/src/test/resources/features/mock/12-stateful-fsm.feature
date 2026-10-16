@mock @stateful
Feature: Stateful FSM — fail twice then succeed, and reset

  Scenario: The FSM advances through its states
    Given the backend supports stateful scenarios
    And a fail-twice-then-succeed scenario for GET "/resource"
    When the client sends "GET" "/resource"
    Then the response status is 503
    When the client sends "GET" "/resource"
    Then the response status is 503
    When the client sends "GET" "/resource"
    Then the response status is 200
    And the response body is "ok"

  Scenario: reset returns the FSM to its initial state
    Given the backend supports stateful scenarios
    And a fail-twice-then-succeed scenario for GET "/resource"
    When the client sends "GET" "/resource"
    Then the response status is 503
    When the scenario "retry" is reset
    And the client sends "GET" "/resource"
    Then the response status is 503
