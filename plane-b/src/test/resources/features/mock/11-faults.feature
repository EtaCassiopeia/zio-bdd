@mock @faults
Feature: Fault injection — connection faults and latency spikes

  Scenario: A connection reset aborts the client connection
    Given the backend supports faults
    And a "ConnectionReset" fault is injected for GET "/boom"
    When GET "/boom" is attempted
    Then the request fails at the transport level

  Scenario: An empty response aborts the client connection
    Given the backend supports faults
    And a "EmptyResponse" fault is injected for GET "/boom"
    When GET "/boom" is attempted
    Then the request fails at the transport level

  Scenario: A malformed chunk aborts the client connection
    Given the backend supports faults
    And a "MalformedChunk" fault is injected for GET "/boom"
    When GET "/boom" is attempted
    Then the request fails at the transport level

  Scenario: Random-data-then-close aborts the client connection
    Given the backend supports faults
    And a "RandomThenClose" fault is injected for GET "/boom"
    When GET "/boom" is attempted
    Then the request fails at the transport level

  @slow
  Scenario: A latency spike delays an otherwise normal response
    Given the backend supports faults
    And a latency fault of 300 millis is injected for GET "/slow"
    When GET "/slow" is attempted
    Then the request succeeds after at least 300 millis
