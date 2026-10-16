@mock @stateinspection
Feature: State inspection — read and force scenario state

  Scenario: currentState reflects the FSM's initial state
    Given the backend supports state inspection
    And a two-state scenario "toggle" for GET "/x"
    Then the current state of "toggle" is "Started"

  Scenario: setState forces a transition, currentState reflects it
    Given the backend supports state inspection
    And a two-state scenario "toggle" for GET "/x"
    When the state of "toggle" is forced to "Next"
    Then the current state of "toggle" is "Next"

  Scenario: a forced state changes what the next request serves
    Given the backend supports state inspection
    And a two-state scenario "toggle" for GET "/x"
    When the state of "toggle" is forced to "Next"
    And the client sends "GET" "/x"
    Then the response body is "second"
