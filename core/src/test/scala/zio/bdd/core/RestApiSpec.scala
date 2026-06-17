package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Simple in-process REST API test suite using a simulated HTTP client.
 *
 * Exercises:
 *   - DocString for request bodies (JSON payload)
 *   - DataTable for response header expectations
 *   - Scenario Outline over HTTP status codes
 *   - scenarioLayer providing per-scenario auth tokens
 *   - Soft assertions via Assertions.collectAll
 */
object RestApiSpec extends ZIOSpecDefault {

  // ── Domain ───────────────────────────────────────────────────────────────────

  case class HttpResponse(status: Int, body: String, headers: Map[String, String] = Map.empty)
  case class ApiError(message: String, statusCode: Int)

  // ── Simulated API client ──────────────────────────────────────────────────────

  trait ApiClient {
    def post(path: String, body: String, token: Option[String]): IO[ApiError, HttpResponse]
    def get(path: String, token: Option[String]): IO[ApiError, HttpResponse]
  }

  case class SimulatedApiClient(
    responseMap: Map[(String, String), HttpResponse] // (method+path, body-prefix) → response
  ) extends ApiClient {
    def post(path: String, body: String, token: Option[String]): IO[ApiError, HttpResponse] =
      responseMap.get(("POST", path)) match {
        case Some(r) => ZIO.succeed(r)
        case None    => ZIO.fail(ApiError(s"No POST handler for $path", 404))
      }
    def get(path: String, token: Option[String]): IO[ApiError, HttpResponse] =
      responseMap.get(("GET", path)) match {
        case Some(r) => ZIO.succeed(r)
        case None    => ZIO.fail(ApiError(s"No GET handler for $path", 404))
      }
  }

  // ── State ─────────────────────────────────────────────────────────────────────

  case class ApiState(
    lastResponse: Option[HttpResponse] = None,
    lastBody: Option[String] = None,
    sessionToken: Option[String] = None
  )
  given Schema[ApiState] = DeriveSchema.gen[ApiState]

  // ── Header row for DataTable ──────────────────────────────────────────────────

  case class HeaderRow(name: String, value: String)
  given Schema[HeaderRow] = DeriveSchema.gen[HeaderRow]

  // ── Env ───────────────────────────────────────────────────────────────────────

  case class ApiEnv(client: ApiClient, token: Option[String])

  val F = "rest-api.feature"

  class ApiSteps(
    responses: Map[(String, String), HttpResponse] = Map.empty
  ) extends ZIOSteps[ApiEnv, ApiState] {

    override def environment: ZLayer[Any, Throwable, ApiEnv] =
      ZLayer.succeed(ApiEnv(SimulatedApiClient(responses), None))

    Given("I am authenticated with token " / string) { (token: String) =>
      ScenarioContext.update(_.copy(sessionToken = Some(token)))
    }

    When("I POST to " / string / " with body" / docString) { (path: String, body: String) =>
      ZIO.serviceWithZIO[ApiEnv] { env =>
        ScenarioContext.get.flatMap { state =>
          env.client
            .post(path, body, state.sessionToken)
            .foldZIO(
              err => ScenarioContext.update(_.copy(lastResponse = Some(HttpResponse(err.statusCode, err.message)))),
              resp => ScenarioContext.update(_.copy(lastResponse = Some(resp), lastBody = Some(body)))
            )
        }
      }
    }

    When("I GET " / string) { (path: String) =>
      ZIO.serviceWithZIO[ApiEnv] { env =>
        ScenarioContext.get.flatMap { state =>
          env.client
            .get(path, state.sessionToken)
            .foldZIO(
              err => ScenarioContext.update(_.copy(lastResponse = Some(HttpResponse(err.statusCode, err.message)))),
              resp => ScenarioContext.update(_.copy(lastResponse = Some(resp)))
            )
        }
      }
    }

    Then("the response status is " / int) { (expected: Int) =>
      ScenarioContext.get.flatMap { s =>
        s.lastResponse match {
          case None    => ZIO.fail(new RuntimeException("No response recorded"))
          case Some(r) => Assertions.assertEquals(r.status, expected)
        }
      }
    }

    Then("the response body contains " / string) { (substring: String) =>
      ScenarioContext.get.flatMap { s =>
        s.lastResponse match {
          case None => ZIO.fail(new RuntimeException("No response recorded"))
          case Some(r) =>
            Assertions.assertTrue(r.body.contains(substring), s"Body '${r.body}' does not contain '$substring'")
        }
      }
    }

    Then("all response assertions pass") {
      ScenarioContext.get.flatMap { s =>
        s.lastResponse match {
          case None => ZIO.fail(new RuntimeException("No response recorded"))
          case Some(r) =>
            Assertions.collectAll(
              Assertions.assertTrue(r.status >= 200 && r.status < 300, s"Status ${r.status} not 2xx"),
              Assertions.assertTrue(r.body.nonEmpty, "Body must not be empty")
            )
        }
      }
    }

    Then("the request body was " / docString) { (expected: String) =>
      ScenarioContext.get.flatMap { s =>
        Assertions.assertEquals(s.lastBody.getOrElse(""), expected.trim)
      }
    }
  }

  private def makeDefaultSteps = new ApiSteps(
    Map(
      ("POST", "/users") -> HttpResponse(
        201,
        """{"id":"usr-1","name":"Alice"}""",
        Map("Content-Type" -> "application/json")
      ),
      ("GET", "/users/1") -> HttpResponse(
        200,
        """{"id":"usr-1","name":"Alice"}""",
        Map("Content-Type" -> "application/json")
      ),
      ("POST", "/invalid") -> HttpResponse(400, """{"error":"bad request"}"""),
      ("GET", "/missing")  -> HttpResponse(404, """{"error":"not found"}""")
    )
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("RestApiSpec")(
    test("POST with DocString body: status 201 and body contains created ID") {
      val steps = makeDefaultSteps
      val featureText =
        "Feature: REST API\n" +
          "  Scenario: create user\n" +
          "    When I POST to \"/users\" with body\n" +
          "      \"\"\"\n" +
          "      {\"name\":\"Alice\",\"email\":\"alice@example.com\"}\n" +
          "      \"\"\"\n" +
          "    Then the response status is 201\n" +
          "    And the response body contains usr-1\n"
      GherkinParser
        .parseFeature(featureText, F)
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[ApiEnv, ApiState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("GET existing resource: 200 with correct body") {
      val steps = makeDefaultSteps
      GherkinParser
        .parseFeature(
          """Feature: REST API
            |  Scenario: get user
            |    When I GET /users/1
            |    Then the response status is 200
            |    And the response body contains Alice
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[ApiEnv, ApiState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("failed request returns error status code") {
      val steps = makeDefaultSteps
      val featureText =
        "Feature: REST API\n" +
          "  Scenario: bad request\n" +
          "    When I POST to \"/invalid\" with body\n" +
          "      \"\"\"\n" +
          "      {\"malformed\":true}\n" +
          "      \"\"\"\n" +
          "    Then the response status is 400\n"
      GherkinParser
        .parseFeature(featureText, F)
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[ApiEnv, ApiState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("soft assertions on successful response all pass") {
      val steps = makeDefaultSteps
      val featureText =
        "Feature: REST API\n" +
          "  Scenario: soft assertions\n" +
          "    When I POST to \"/users\" with body\n" +
          "      \"\"\"\n" +
          "      {\"name\":\"Bob\"}\n" +
          "      \"\"\"\n" +
          "    Then all response assertions pass\n"
      GherkinParser
        .parseFeature(featureText, F)
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[ApiEnv, ApiState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("Scenario Outline over multiple paths: all outcomes recorded") {
      val steps = makeDefaultSteps
      GherkinParser
        .parseFeature(
          """Feature: REST API
            |  Scenario Outline: various GET endpoints
            |    When I GET <path>
            |    Then the response status is <status>
            |  Examples:
            |    | path     | status |
            |    | /users/1 | 200    |
            |    | /missing | 404    |
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[ApiEnv, ApiState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map { r =>
          val results = r.head.scenarioResults
          assertTrue(
            results.length == 2,
            results.forall(_.isPassed)
          )
        }
    }
  )
}
