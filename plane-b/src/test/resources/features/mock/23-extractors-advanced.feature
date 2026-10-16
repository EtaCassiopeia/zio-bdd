@mock @portable
Feature: Advanced step extractors — word/rest, bigDecimal, uuid, long, regex, oneOf, optional

  Scenario: word captures a single token and rest captures to end of line
    When I split first and the remaining tokens are two three four
    Then the captured word is "first"
    And the captured rest is "two three four"

  Scenario: bigDecimal keeps exact precision a Double would lose
    When I read the amount 3.14159265358979323846
    Then the captured amount is exactly "3.14159265358979323846"

  Scenario: uuid and a max-bound long round-trip
    When I read id 550e8400-e29b-41d4-a716-446655440000 and seq 9223372036854775807
    Then the captured uuid is "550e8400-e29b-41d4-a716-446655440000"
    And the captured seq is 9223372036854775807

  Scenario: regex captures an alternation group
    When the account status is active
    Then the captured status is "active"

  Scenario: oneOf matches the longest alternative first
    When the account is not provisioned
    Then the captured condition is "is not"

  Scenario: an optional literal present yields Some
    When a deploy runs with rollback
    Then the optional clause was present
    And the captured clause is " with rollback"

  Scenario: an optional literal absent yields None
    When a deploy runs
    Then the optional clause was absent
