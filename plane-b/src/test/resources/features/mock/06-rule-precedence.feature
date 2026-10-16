@mock @portable
Feature: Rule precedence & mutation — Overlay shadows Base; add/remove/replace

  Scenario: An overlay rule shadows the base rule
    Given a base stub GET "/p" returns text "base"
    When an overlay stub GET "/p" returns text "overlay"
    And the client sends "GET" "/p"
    Then the response status is 200
    And the response body is "overlay"

  Scenario: Removing the overlay reverts to the base rule
    Given a base stub GET "/p" returns text "base"
    When an overlay stub GET "/p" returns text "overlay"
    And the last overlay rule is removed
    And the client sends "GET" "/p"
    Then the response status is 200
    And the response body is "base"

  Scenario: replaceRules swaps the entire rule set
    Given a base stub GET "/old" returns text "old"
    When all rules are replaced with GET "/new" returning text "new"
    And the client sends "GET" "/new"
    Then the response status is 200
    And the response body is "new"
