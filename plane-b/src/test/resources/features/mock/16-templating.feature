@mock @templating
Feature: Templating — response body pulls captures from the request (Rift)

  Scenario: A path segment is templated into the response
    Given the backend supports templating
    And a template greeting the last path segment at "^/greet/[^/]+$"
    When the client sends "GET" "/greet/World"
    Then the response status is 200
    And the response body is "Hello World"

  Scenario: A request body value is echoed via a template
    Given the backend supports templating
    And a body-echo template for POST "/echo"
    When the request body is "Alice"
    And the client sends "POST" "/echo"
    Then the response status is 200
    And the response body is "echo:Alice"
