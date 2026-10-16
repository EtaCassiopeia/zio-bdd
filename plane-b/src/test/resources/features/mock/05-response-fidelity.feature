@mock @portable
Feature: Response fidelity — base64 body, multi-value headers, latency

  Scenario: A base64 body is decoded and served
    Given a stub GET "/blob" returns 200 base64 "aGVsbG8="
    When the client sends "GET" "/blob"
    Then the response status is 200
    And the response body is "hello"

  Scenario: Multi-value response headers survive the round trip (#162)
    Given a stub GET "/cookies" returns 200 with header "Set-Cookie" values "a=1" and "b=2"
    When the client sends "GET" "/cookies"
    Then the response status is 200
    And the response header "Set-Cookie" has values "a=1,b=2"

  @slow
  Scenario: Latency is applied to the response
    Given a stub GET "/slow" returns 200 text "ok" after 300 millis
    When the client sends "GET" "/slow"
    Then the response status is 200
    And the response body is "ok"
    And the response took at least 300 millis
