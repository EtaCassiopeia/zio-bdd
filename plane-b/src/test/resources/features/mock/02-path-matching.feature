@mock @portable
Feature: Path matching — Exact, Regex, Template, Any

  Scenario: Exact path matches only that path
    Given a stub with exact path "/users/42" returns text "exact"
    When the client sends "GET" "/users/42"
    Then the response status is 200
    And the response body is "exact"

  Scenario: Exact path does not match a different path
    Given a stub with exact path "/users/42" returns text "exact"
    When the client sends "GET" "/users/99"
    Then the response status is 404

  Scenario: Regex path matches any numeric id
    Given a stub with regex path "/users/[0-9]+" returns text "byRegex"
    When the client sends "GET" "/users/12345"
    Then the response status is 200
    And the response body is "byRegex"

  Scenario: Template path matches a path variable
    Given a stub with template path "/orders/{id}" returns text "byTemplate"
    When the client sends "GET" "/orders/abc-1"
    Then the response status is 200
    And the response body is "byTemplate"

  Scenario: Any path matches everything
    Given a stub matching any path returns text "catchAll"
    When the client sends "GET" "/literally/anything"
    Then the response status is 200
    And the response body is "catchAll"
