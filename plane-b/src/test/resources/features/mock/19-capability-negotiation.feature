@mock @negotiation
Feature: Capability negotiation — capabilities are honest and require fails fast

  Scenario: Faults is advertised and requirable on every mocking backend
    Then the backend advertises capability "Faults"
    And requiring "Faults" succeeds

  @wiremock
  Scenario: WireMock does not advertise Scripting and require names the gap
    When requiring "Scripting" is attempted
    Then it fails naming the missing capability "Scripting"

  @rift
  Scenario: Rift advertises Scripting
    Then the backend advertises capability "Scripting"
    And requiring "Scripting" succeeds
