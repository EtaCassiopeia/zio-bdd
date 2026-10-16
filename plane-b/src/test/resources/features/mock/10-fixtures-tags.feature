@mock @fixtures @portable
Feature: Scenario-tier mock fixtures — @mock(name) deploys catalog entries

  @mock(greeting)
  Scenario: The greeting fixture is deployed for the tagged scenario
    Then the fixture provisioned 1 spaces
    When the client sends "GET" "/hello"
    Then the response body is "hi"

  @mock(greeting, farewell)
  Scenario: Two named fixtures are deployed together
    Then the fixture provisioned 2 spaces

  # The catalog mixes Dsl entries (greeting/farewell) with a raw-file Resource entry (orders); this
  # proves @mock resolves and deploys a file-backed catalog entry, not just DSL ones (#64). Rift-only
  # (raw imposter), so @rift excludes it from the wiremock + embedded backends.
  @rift @mock(orders)
  Scenario: A file-backed (Resource) catalog entry is deployed and served
    Then the fixture provisioned 1 spaces
    When the client sends "GET" "/from-resource"
    Then the response body is "resource-body"
