@mock @native
Feature: Native escape hatch — provision from a raw backend document

  @wiremock
  Scenario: A raw WireMock stub mapping is served
    Given a native WireMock stub for GET "/native" returning "native-wm"
    When the client sends "GET" "/native"
    Then the response status is 200
    And the response body is "native-wm"

  @rift
  Scenario: A raw Rift imposter is served
    Given a native Rift imposter for GET "/native" returning "native-rift"
    When the client sends "GET" "/native"
    Then the response status is 200
    And the response body is "native-rift"
