@fixedport
Feature: Fixed-port pinning — MockSpec.onPort binds an exact imposter port (#211, Plane B)

  # onPort is the opt-in fixed-port path (#211): the Rift adapters bind exactly the requested port
  # instead of auto-assigning a free one, so a SUT can reach the mock at a known address (this is
  # what real consumers pin a downstream to). WireMock doesn't honour a requested port → tagged
  # @fixedport and excluded from the wiremock alias; runs on embedded + rift-external (both real
  # Rift adapters). Surfaced by the tmm-tests-zio-bdd consumer audit (#61).

  Scenario: a mock pinned with onPort binds exactly that port and serves there
    Given a mock pinned to a fixed port serving GET "/pinned" text "pinned-ok"
    Then the active space base URI binds the pinned port
    And a GET "/pinned" through the SUT returns "pinned-ok"

  Scenario: re-provisioning onPort after teardown reuses the same port deterministically
    Given a mock pinned to a fixed port serving GET "/pinned" text "first"
    When the pinned space is destroyed and re-provisioned on the same port serving "second"
    Then the active space base URI binds the pinned port
    And a GET "/pinned" through the SUT returns "second"
