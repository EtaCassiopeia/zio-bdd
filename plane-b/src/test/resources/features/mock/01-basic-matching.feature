@mock @portable
Feature: Basic request/response matching — method, exact path, status, body kinds

  @smoke
  Scenario: Exact GET is matched; text body + status are served and recorded
    Given a stub "GET" "/hello" returns 200 text "world"
    When the client sends "GET" "/hello"
    Then the response status is 200
    And the response body is "world"
    And the mock recorded 1 requests
    And a "GET" to "/hello" was recorded

  Scenario: A JSON body is served verbatim
    Given a stub "GET" "/widgets/1" returns 200 json "{\"id\":1,\"name\":\"gizmo\"}"
    When the client sends "GET" "/widgets/1"
    Then the response status is 200
    And the response body is "{\"id\":1,\"name\":\"gizmo\"}"

  Scenario: A 201 Created with a body round-trips (regression for zio-bdd#183)
    Given a stub "POST" "/widgets" returns 201 json "{\"id\":1}"
    When the client sends "POST" "/widgets"
    Then the response status is 201
    And the response body contains "\"id\":1"

  Scenario: An empty-body response carries only the status
    Given a stub "DELETE" "/widgets/1" returns 204
    When the client sends "DELETE" "/widgets/1"
    Then the response status is 204
    And the response body is empty

  Scenario: Method is matched — a mismatched method falls through to 404
    Given a stub "GET" "/only-get" returns 200 text "ok"
    When the client sends "POST" "/only-get"
    Then the response status is 404
