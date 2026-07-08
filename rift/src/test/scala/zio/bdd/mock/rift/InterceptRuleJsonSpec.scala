package zio.bdd.mock.rift

import zio.bdd.mock.*
import zio.test.*

/**
 * Gate for issue #253 — the backend-neutral intercept-rule JSON builder shared
 * by the container ([[RiftMockControl]]) and embedded adapters. Pure; runs on
 * JDK 11 (the container-adapter toolchain), so it is the local gate for the
 * rule shape while the full HTTPS-intercept path is exercised by the
 * RIFT_IT-gated container e2e in CI.
 */
object InterceptRuleJsonSpec extends ZIOSpecDefault:

  def spec = suite("InterceptRuleJson")(
    test("Redirect builds a forward action carrying the target space's port and the host") {
      val space = MockSpace("http://localhost:4545", identity, SpaceId("y"))
      InterceptRuleJson.ruleJson(InterceptRule.Redirect("cdn.example.com", space)) match
        case Right(j) =>
          assertTrue(j.contains("\"forward\""), j.contains("\"port\":4545"), j.contains("cdn.example.com"))
        case Left(_) => assertTrue(false)
    },
    test("Redirect whose target space has no port in its baseUri fails with InvalidDefinition") {
      val portless = MockSpace("localhost-without-a-port", identity, SpaceId("x"))
      InterceptRuleJson.ruleJson(InterceptRule.Redirect("cdn.example.com", portless)) match
        case Left(_: MockError.InvalidDefinition) => assertTrue(true)
        case other                                => assertTrue(false)
    },
    test("Serve builds a serve action with the status, headers and body for the host") {
      val stub = InterceptStub(status = 418, headers = Map("x-mock" -> "1"), body = Some("teapot"))
      InterceptRuleJson.ruleJson(InterceptRule.Serve("api.example.com", stub)) match
        case Right(j) =>
          assertTrue(
            j.contains("\"serve\""),
            j.contains("418"),
            j.contains("teapot"),
            j.contains("x-mock"),
            j.contains("api.example.com")
          )
        case Left(_) => assertTrue(false)
    }
  )
