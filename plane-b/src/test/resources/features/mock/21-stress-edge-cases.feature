@mock @portable @stress
Feature: Stress & edge cases — unicode, large bodies, special chars, churn

  Scenario: A unicode body round-trips intact
    Given a stub GET "/u" returns text "héllo-wörld-日本語-🎉-Ω"
    When the client sends "GET" "/u"
    Then the response status is 200
    And the response body is "héllo-wörld-日本語-🎉-Ω"

  Scenario: A large body round-trips intact
    Given a stub GET "/big" returns a body of 100000 chars
    When the client sends "GET" "/big"
    Then the response status is 200
    And the response body has length 100000

  Scenario: A body with quotes, newlines and JSON survives
    Given a stub GET "/special" returns text "{\"k\":\"a\\nb \\\"q\\\" c\"}"
    When the client sends "GET" "/special"
    Then the response status is 200
    And the response body is "{\"k\":\"a\\nb \\\"q\\\" c\"}"

  Scenario: The root path is matched
    Given a stub GET "/" returns text "root"
    When the client sends "GET" "/"
    Then the response status is 200
    And the response body is "root"

  Scenario: Multiple query parameters must all match
    Given a two-query stub at "/q" needing "a" = "1" and "b" = "2" serving "matched"
    When the client sends "GET" "/q?a=1&b=2"
    Then the response status is 200
    And the response body is "matched"

  Scenario: Rapid space churn — provision, serve, destroy in a loop
    When churn 10 spaces serving GET "/churn"
    Then every churned space served correctly
