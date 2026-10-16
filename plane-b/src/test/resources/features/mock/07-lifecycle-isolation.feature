@mock @portable
Feature: Lifecycle & isolation — per-space destroy and space-local recording

  Scenario: Two spaces serve independently
    Given a space "A" serving GET "/ping" text "from-A"
    And a space "B" serving GET "/ping" text "from-B"
    When GET "/ping" is sent to space "A"
    Then the response body is "from-A"
    When GET "/ping" is sent to space "B"
    Then the response body is "from-B"

  Scenario: Destroying one space leaves the other intact (never a global reset)
    Given a space "A" serving GET "/ping" text "from-A"
    And a space "B" serving GET "/ping" text "from-B"
    When space "A" is destroyed
    And GET "/ping" is sent to space "B"
    Then the response status is 200
    And the response body is "from-B"

  Scenario: received() is space-local
    Given a space "A" serving GET "/ping" text "from-A"
    And a space "B" serving GET "/ping" text "from-B"
    When GET "/ping" is sent to space "A"
    Then space "A" recorded 1 requests
    And space "B" recorded 0 requests
