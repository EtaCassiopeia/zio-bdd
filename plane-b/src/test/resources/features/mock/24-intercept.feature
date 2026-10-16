@intercept
Feature: Intercept / TLS-MITM forward-proxy (MockControl Intercept capability, #219) — Plane B

  # The portable MockControl `Intercept` capability: an intercept-capable backend (embedded FFM here)
  # runs a TLS-MITM forward proxy so a SUT's call to a hard-coded external HTTPS host is redirected to
  # a mock. Driven with a JDK HttpClient trusting the exported truststore and proxying HTTPS through
  # the intercept port — the mitmproxy-container replacement. Only backends advertising `Intercept`
  # run this (wiremock + rift-external don't → the @intercept tag is excluded from their aliases).

  Scenario: respondWith answers an intercepted host inline from a CA-trusting SUT
    Given the mock intercepts "api.example.com" answering 418 "inline-teapot"
    When the SUT calls "https://api.example.com/anything" through the intercept proxy
    Then the intercepted status is 418
    And the intercepted body is "inline-teapot"

  Scenario: redirectTo forwards an intercepted host to a mock imposter
    Given a mock stub "GET" "/config.json" returns 200 text "cdn-mocked"
    And the mock intercepts "cdn.example.com" redirecting to the mock
    When the SUT calls "https://cdn.example.com/config.json" through the intercept proxy
    Then the intercepted status is 200
    And the intercepted body is "cdn-mocked"
