package zio.bdd.mock

import zio.test.*

/**
 * The portable Intercept DSL + value types (#219, AC1/AC2): pure builders that
 * produce the neutral [[InterceptRule]] a suite applies via the [[Intercept]]
 * capability. No backend needed.
 */
object InterceptDslSpec extends ZIOSpecDefault:

  import dsl.*

  private val space = MockSpace("http://localhost:4545", identity, SpaceId("s1"))

  def spec = suite("Intercept DSL (#219)")(
    test("intercept(host).redirectTo(space) builds a Redirect rule") {
      assertTrue(
        intercept("cdn.example.com").redirectTo(space) == InterceptRule.Redirect("cdn.example.com", space)
      )
    },
    test("intercept(host).respondWith(stub) builds a Serve rule") {
      val stub = InterceptStub(status = 418, headers = Map("X-Teapot" -> "1"), body = Some("teapot"))
      assertTrue(
        intercept("cdn.example.com").respondWith(stub) == InterceptRule.Serve("cdn.example.com", stub)
      )
    },
    test("TrustStoreFormat.wire is the backend token") {
      assertTrue(TrustStoreFormat.Pkcs12.wire == "pkcs12", TrustStoreFormat.Jks.wire == "jks")
    }
  )
