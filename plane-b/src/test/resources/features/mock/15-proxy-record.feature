@mock @proxyrecord
Feature: Proxy/record — capability surface (Rift)

  Scenario: A proxy rule installs against an upstream
    Given the backend supports proxy/record
    When a proxy to "http://upstream.invalid:5678" is installed for GET "/echo"
    Then the mock recorded 0 requests
