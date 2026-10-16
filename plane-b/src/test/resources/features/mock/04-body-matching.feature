@mock @portable
Feature: Request body matching — Equals, Contains, JsonPath

  Scenario: Body equals matches an exact payload
    Given a stub POST "/echo" requiring body equal to "ping" returns text "pong"
    When the request body is "ping"
    And the client sends "POST" "/echo"
    Then the response status is 200
    And the response body is "pong"

  Scenario: Body equals does not match a different payload
    Given a stub POST "/echo" requiring body equal to "ping" returns text "pong"
    When the request body is "nope"
    And the client sends "POST" "/echo"
    Then the response status is 404

  Scenario: Body contains matches a substring
    Given a stub POST "/log" requiring body containing "error" returns text "captured"
    When the request body is "level=error msg=boom"
    And the client sends "POST" "/log"
    Then the response status is 200
    And the response body is "captured"

  Scenario: JsonPath equals matches a field value
    Given a stub POST "/orders" requiring jsonpath "$.status" equal to "paid" returns text "accepted"
    When the request body is "{\"status\":\"paid\"}"
    And the client sends "POST" "/orders"
    Then the response status is 200
    And the response body is "accepted"
