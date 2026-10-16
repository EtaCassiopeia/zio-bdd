@mock @portable @reconfig
Feature: In-flight reconfiguration — one live imposter, changing behaviour

  Scenario: replaceRules redirects the same live imposter without a restart
    Given a live imposter serving GET "/config" text "v1"
    When the client sends "GET" "/config"
    Then the response body is "v1"
    When the imposter is reconfigured to serve GET "/config" text "v2"
    And the client sends "GET" "/config"
    Then the response body is "v2"
    When the imposter is reconfigured to serve GET "/config" text "v3"
    And the client sends "GET" "/config"
    Then the response body is "v3"

  Scenario: an overlay changes behaviour then reverts, live
    Given a live imposter serving GET "/flag" text "base"
    When the client sends "GET" "/flag"
    Then the response body is "base"
    When an overlay makes GET "/flag" serve text "overridden"
    And the client sends "GET" "/flag"
    Then the response body is "overridden"
    When the overlay is reverted
    And the client sends "GET" "/flag"
    Then the response body is "base"

  Scenario: rapid alternation stays consistent on the live imposter
    Given a live imposter serving GET "/toggle" text "A"
    When the client sends "GET" "/toggle"
    Then the response body is "A"
    When the imposter is reconfigured to serve GET "/toggle" text "B"
    And the client sends "GET" "/toggle"
    Then the response body is "B"
    When the imposter is reconfigured to serve GET "/toggle" text "A"
    And the client sends "GET" "/toggle"
    Then the response body is "A"
    When the imposter is reconfigured to serve GET "/toggle" text "B"
    And the client sends "GET" "/toggle"
    Then the response body is "B"
