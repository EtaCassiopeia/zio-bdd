@mock @scripting
Feature: Scripting — a backend script computes the response (Rift)

  Scenario: A Rhai script echoes the request method into the body
    Given the backend supports scripting
    And a "Rhai" script echoing the method for GET "/scripted"
    When the client sends "GET" "/scripted"
    Then the response status is 200
    And the response body is "scripted-GET"
