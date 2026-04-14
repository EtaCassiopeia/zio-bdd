package zio.bdd.http

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.step.{HasLens, ZIOSteps}

/**
 * Mixin providing pre-built HTTP assertion step definitions.
 *
 * Mix into any `ZIOSteps` suite whose scenario state `S` contains an
 * `HttpState`. Requires a `HasLens[S, HttpState]` given instance to locate the
 * `HttpState` within `S`.
 *
 * ===Example===
 *
 * {{{
 *   import zio.bdd.core.step.HasLens
 *   import zio.bdd.http.{HttpState, HttpSteps}
 *
 *   final case class ScenarioState(http: HttpState = HttpState(), ...)
 *   object ScenarioState:
 *     given HasLens[ScenarioState, HttpState] =
 *       HasLens(_.http, (s, h) => s.copy(http = h))
 *
 *   object MySuite
 *       extends ZIOSteps[MyR, ScenarioState]
 *       with HttpSteps[MyR, ScenarioState]:
 *     given HasLens[ScenarioState, HttpState] = ScenarioState.given_HasLens_ScenarioState_HttpState
 * }}}
 *
 * ===Registered steps===
 *
 *   - `Then the response status is {int}` — asserts `HttpState.statusCode`
 *   - `Then the response body contains {string}` — asserts `HttpState.body`
 *     contains substring
 *   - `Then the response body is {string}` — asserts `HttpState.body` equals
 *     string
 *   - `Then the response body is not empty` — asserts `HttpState.body.nonEmpty`
 *   - `Then the response header {string} equals {string}` — asserts lowercased
 *     header value
 *   - `Then the response content type is {string}` — asserts
 *     `HttpState.contentType` contains substring
 *
 * @tparam R
 *   The ZIO environment type of the enclosing suite
 * @tparam S
 *   The scenario state type of the enclosing suite (must contain an `HttpState`
 *   slice)
 */
trait HttpSteps[R, S] { self: ZIOSteps[R, S] =>

  /**
   * Locates the `HttpState` slice within `S`. Provided by the user as a
   * `given`.
   */
  given httpLens: HasLens[S, HttpState]

  Then("the response status is " / int) { (expectedStatus: Int) =>
    ScenarioContext.get.flatMap { s =>
      val actual = httpLens.get(s).statusCode
      Assertions.assertEquals(actual, expectedStatus, s"HTTP status mismatch: expected $expectedStatus, got $actual")
    }
  }

  Then("the response body contains " / string) { (expected: String) =>
    ScenarioContext.get.flatMap { s =>
      val body = httpLens.get(s).body
      Assertions.assertTrue(body.contains(expected), s"Response body does not contain '$expected'. Body was: '$body'")
    }
  }

  Then("the response body is " / string) { (expected: String) =>
    ScenarioContext.get.flatMap { s =>
      val body = httpLens.get(s).body
      Assertions.assertEquals(body, expected, s"Response body mismatch")
    }
  }

  Then("the response body is not empty") {
    ScenarioContext.get.flatMap { s =>
      Assertions.assertTrue(httpLens.get(s).body.nonEmpty, "Expected non-empty response body")
    }
  }

  Then("the response header " / string / " equals " / string) { (header: String, expected: String) =>
    ScenarioContext.get.flatMap { s =>
      val headers = httpLens.get(s).headers
      headers.get(header.toLowerCase) match {
        case Some(actual) =>
          Assertions.assertEquals(actual, expected, s"Header '$header' mismatch")
        case None =>
          ZIO.fail(
            new AssertionError(
              s"Header '$header' not present in response. Available headers: ${headers.keys.mkString(", ")}"
            )
          )
      }
    }
  }

  Then("the response content type is " / string) { (expected: String) =>
    ScenarioContext.get.flatMap { s =>
      val ct = httpLens.get(s).contentType
      Assertions.assertTrue(ct.contains(expected), s"Content-Type '$ct' does not contain '$expected'")
    }
  }
}
