@mock @portable
Feature: Overlapping step definitions resolve to the most specific (zio-bdd#186)

  # Two step definitions share the "order <x>" prefix: a short one ("order {string}")
  # and a strict extension ("order {string} rush {string}"). Because the unquoted
  # {string} fallback is greedy (.*), the short definition ALSO fully matches the
  # longer line — so both are candidates. Before #186 the fewest-extractors rule
  # bound the shorter (wrong) definition, silently mis-capturing the args. The
  # mostSpecificWinner tie-break (most literal characters wins, extractor count only
  # breaks a literal-length tie) now binds the longer, more-qualified definition.

  Scenario: The longer, more-literal overlapping definition wins the tie
    When order widget rush yes
    Then the matched step was "long"
    And the first captured arg was "widget"
    And the second captured arg was "yes"

  Scenario: The shorter definition still binds when the extension literal is absent
    When order gadget
    Then the matched step was "short"
    And the first captured arg was "gadget"
