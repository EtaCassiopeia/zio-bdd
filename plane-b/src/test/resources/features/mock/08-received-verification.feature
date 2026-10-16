@mock @portable
Feature: Request verification via received() — count, method/path, body

  Scenario: Recorded count reflects every request the space served
    Given a stub matching any request returns 200
    When the client sends "GET" "/a"
    And the client sends "GET" "/b"
    And the client sends "GET" "/a"
    Then the mock recorded 3 requests
    And a "GET" to "/b" was recorded

  Scenario: A recorded request carries its method, path and body
    Given a stub matching any request returns 200
    When the request body is "hello=world"
    And the client sends "POST" "/submit"
    Then the mock recorded 1 requests
    And the recorded "POST" to "/submit" has body "hello=world"
