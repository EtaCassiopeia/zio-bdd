@mock @portable
Feature: Header and query value matching — Equals, Contains, Matches

  Scenario: Header equals matches when the value is exact
    Given a stub GET "/secure" requiring header "X-Api-Key" equal to "secret" returns text "authorized"
    When the request header "X-Api-Key" is "secret"
    And the client sends "GET" "/secure"
    Then the response status is 200
    And the response body is "authorized"

  Scenario: Header equals does not match a different value
    Given a stub GET "/secure" requiring header "X-Api-Key" equal to "secret" returns text "authorized"
    When the request header "X-Api-Key" is "wrong"
    And the client sends "GET" "/secure"
    Then the response status is 404

  Scenario: Header contains matches a substring
    Given a stub GET "/accept" requiring header "Accept" containing "json" returns text "ok"
    When the request header "Accept" is "application/json; charset=utf-8"
    And the client sends "GET" "/accept"
    Then the response status is 200
    And the response body is "ok"

  Scenario: Query matches a regex
    Given a stub GET "/search" requiring query "q" matching "[a-z]+" returns text "found"
    When the client sends "GET" "/search?q=abc"
    Then the response status is 200
    And the response body is "found"
