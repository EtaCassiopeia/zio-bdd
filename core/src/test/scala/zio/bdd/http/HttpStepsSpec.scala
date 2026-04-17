package zio.bdd.http

import zio.*
import zio.bdd.core.step.{HasLens, ZIOSteps}
import zio.bdd.core.{FeatureExecutor, FeatureResult}
import zio.bdd.gherkin.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for HttpSteps — the pre-built HTTP assertion step mixin.
 *
 * Verifies:
 *   - "the response status is N" passes when statusCode matches and fails when
 *     it doesn't
 *   - "the response body contains X" passes/fails correctly
 *   - "the response body is X" passes/fails correctly
 *   - "the response body is not empty" passes/fails correctly
 *   - "the response header X equals Y" passes when header present and correct,
 *     fails otherwise
 *   - "the response content type is X" passes/fails correctly
 */
object HttpStepsSpec extends ZIOSpecDefault {

  // ── Scenario state ──────────────────────────────────────────────────────────

  final case class TestState(http: HttpState = HttpState())

  object TestState:
    given Schema[TestState] = DeriveSchema.gen[TestState]
    given HasLens[TestState, HttpState] =
      HasLens(_.http, (s, h) => s.copy(http = h))

  // ── Step suite ──────────────────────────────────────────────────────────────

  /**
   * A minimal suite that seeds `HttpState` via `Given` steps and then exercises
   * the assertions provided by `HttpSteps`.
   *
   * Each test creates a fresh instance to avoid step-registration pollution.
   */
  class HttpSuite extends ZIOSteps[Any, TestState] with HttpSteps[Any, TestState]:

    override given httpLens: HasLens[TestState, HttpState] =
      HasLens(_.http, (s, h) => s.copy(http = h))

    // Seed steps — set the HttpState directly in scenario context
    Given("the response has status " / int) { (code: Int) =>
      ScenarioContext.update(s => s.copy(http = s.http.copy(statusCode = code)))
    }

    Given("the response body is set to " / string) { (body: String) =>
      ScenarioContext.update(s => s.copy(http = s.http.copy(body = body)))
    }

    Given("the response has header " / string / " with value " / string) { (name: String, value: String) =>
      ScenarioContext.update(s => s.copy(http = s.http.copy(headers = s.http.headers + (name.toLowerCase -> value))))
    }

    Given("the response content type is set to " / string) { (ct: String) =>
      ScenarioContext.update(s => s.copy(http = s.http.copy(contentType = ct)))
    }

  // ── Test helpers ────────────────────────────────────────────────────────────

  private val F = "http-test.feature"

  private def run(suite: HttpSuite, scenarioSteps: List[Step]): ZIO[Any, Throwable, FeatureResult] =
    suite
      .run(
        List(Feature("HTTP", Nil, List(Scenario("s", Nil, scenarioSteps, Some(F), Some(1)))))
      )
      .map(_.head)

  private def step(t: StepType, p: String) = Step(t, p, None, None, None)

  // ── Status assertion ────────────────────────────────────────────────────────

  private val statusTests = suite("the response status is {int}")(
    test("passes when statusCode matches") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, "the response has status 200"),
          step(StepType.ThenStep, "the response status is 200")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when statusCode does not match") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, "the response has status 404"),
          step(StepType.ThenStep, "the response status is 200")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── Body contains ──────────────────────────────────────────────────────────

  private val bodyContainsTests = suite("the response body contains {string}")(
    test("passes when body contains the expected substring") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response body is set to "Hello, World!""""),
          step(StepType.ThenStep, """the response body contains "World"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when body does not contain the expected substring") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response body is set to "Hello""""),
          step(StepType.ThenStep, """the response body contains "World"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── Body equals ────────────────────────────────────────────────────────────

  private val bodyEqualsTests = suite("the response body is {string}")(
    test("passes when body matches exactly") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response body is set to "exact""""),
          step(StepType.ThenStep, """the response body is "exact"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when body does not match exactly") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response body is set to "actual""""),
          step(StepType.ThenStep, """the response body is "expected"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── Body not empty ─────────────────────────────────────────────────────────

  private val bodyNotEmptyTests = suite("the response body is not empty")(
    test("passes when body is non-empty") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response body is set to "some content""""),
          step(StepType.ThenStep, "the response body is not empty")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when body is empty") {
      val s = new HttpSuite {}
      // Default HttpState has body = "" so we just assert without setting
      run(
        s,
        List(
          step(StepType.ThenStep, "the response body is not empty")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── Header assertion ───────────────────────────────────────────────────────

  private val headerTests = suite("the response header {string} equals {string}")(
    test("passes when the header is present and matches") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response has header "Content-Type" with value "application/json""""),
          step(StepType.ThenStep, """the response header "content-type" equals "application/json"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when the header is present but value does not match") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response has header "Content-Type" with value "text/plain""""),
          step(StepType.ThenStep, """the response header "content-type" equals "application/json"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    },
    test("fails when the header is absent") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.ThenStep, """the response header "x-custom" equals "value"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── Content type ───────────────────────────────────────────────────────────

  private val contentTypeTests = suite("the response content type is {string}")(
    test("passes when contentType contains the expected substring") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response content type is set to "application/json; charset=utf-8""""),
          step(StepType.ThenStep, """the response content type is "application/json"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when contentType does not contain the expected substring") {
      val s = new HttpSuite {}
      run(
        s,
        List(
          step(StepType.GivenStep, """the response content type is set to "text/html""""),
          step(StepType.ThenStep, """the response content type is "application/json"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  // ── HttpState defaults ─────────────────────────────────────────────────────

  private val httpStateDefaults = suite("HttpState defaults")(
    test("default HttpState has statusCode 0") {
      val s = HttpState()
      assertTrue(s.statusCode == 0)
    },
    test("default HttpState has empty body") {
      val s = HttpState()
      assertTrue(s.body == "")
    },
    test("default HttpState has empty headers") {
      val s = HttpState()
      assertTrue(s.headers.isEmpty)
    },
    test("default HttpState has empty contentType") {
      val s = HttpState()
      assertTrue(s.contentType == "")
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("HttpSteps")(
    httpStateDefaults,
    statusTests,
    bodyContainsTests,
    bodyEqualsTests,
    bodyNotEmptyTests,
    headerTests,
    contentTypeTests
  )
}
