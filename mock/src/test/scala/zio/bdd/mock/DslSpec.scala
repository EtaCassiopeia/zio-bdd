package zio.bdd.mock

import zio.*
import zio.bdd.mock.dsl.*
import zio.test.*

/**
 * AC2 gate: the DSL is a pure convenience over the canonical model. Every
 * builder expression must equal the hand-written case class it stands for, and
 * a full DSL spec must normalise through the real [[Provisioning]] path to
 * exactly the canonical rules — i.e. DSL output IS the wire contract an adapter
 * consumes. (Adapter-level "identical JSON behaviour" is verified by the
 * conformance suite in #113/#122; no adapter parses JSON in this module.)
 */
object DslSpec extends ZIOSpecDefault:

  def spec = suite("mock DSL builders")(
    suite("request matchers equal the canonical RequestMatch")(
      test("get/post/... set method + exact path") {
        assertTrue(
          get("/x") == RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/x")),
          post("/x") == RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/x")),
          put("/x") == RequestMatch(method = Some(Method.Put), path = PathMatch.Exact("/x")),
          delete("/x") == RequestMatch(method = Some(Method.Delete), path = PathMatch.Exact("/x")),
          patch("/x") == RequestMatch(method = Some(Method.Patch), path = PathMatch.Exact("/x")),
          head("/x") == RequestMatch(method = Some(Method.Head), path = PathMatch.Exact("/x")),
          options("/x") == RequestMatch(method = Some(Method.Options), path = PathMatch.Exact("/x")),
          anyRequest == RequestMatch()
        )
      },
      test("withPath replaces the path matcher with regex/template") {
        assertTrue(
          get("/ignored").withPath(PathMatch.Template("/users/{id}")) ==
            RequestMatch(method = Some(Method.Get), path = PathMatch.Template("/users/{id}")),
          get("/ignored").withPath(PathMatch.Regex("/u/.*")) ==
            RequestMatch(method = Some(Method.Get), path = PathMatch.Regex("/u/.*"))
        )
      },
      test("header(...).{equalTo,contains,matches} add a header value matcher") {
        assertTrue(
          get("/x").where(header("Authorization").matches("Bearer .*")) ==
            RequestMatch(
              method = Some(Method.Get),
              path = PathMatch.Exact("/x"),
              headers = Map("Authorization" -> ValueMatch.Matches("Bearer .*"))
            ),
          get("/x").where(header("X").equalTo("1")) ==
            RequestMatch(Some(Method.Get), PathMatch.Exact("/x"), headers = Map("X" -> ValueMatch.Equals("1"))),
          get("/x").where(header("X").contains("ab")) ==
            RequestMatch(Some(Method.Get), PathMatch.Exact("/x"), headers = Map("X" -> ValueMatch.Contains("ab")))
        )
      },
      test("query(...).{equalTo,contains,matches} add a query value matcher") {
        assertTrue(
          get("/x").where(query("page").equalTo("1")) ==
            RequestMatch(Some(Method.Get), PathMatch.Exact("/x"), query = Map("page" -> ValueMatch.Equals("1"))),
          get("/x").where(query("page").contains("1")).query == Map("page" -> ValueMatch.Contains("1")),
          get("/x").where(query("page").matches("\\d+")).query == Map("page" -> ValueMatch.Matches("\\d+"))
        )
      },
      test("jsonPath(...).exists / .equalTo set a JsonPath body matcher") {
        assertTrue(
          post("/x").where(jsonPath("$.a").exists).body.contains(BodyMatch.JsonPath("$.a", None)),
          post("/x").where(jsonPath("$.a").equalTo("v")).body.contains(BodyMatch.JsonPath("$.a", Some("v")))
        )
      },
      test("body matchers (equals/contains/matches/xPath) set the body matcher") {
        assertTrue(
          post("/x").where(bodyEquals("hi")).body.contains(BodyMatch.Equals("hi")),
          post("/x").where(bodyContains("h")).body.contains(BodyMatch.Contains("h")),
          post("/x").where(bodyMatches("h.")).body.contains(BodyMatch.Matches("h.")),
          post("/x").where(xPath("/a/b")).body.contains(BodyMatch.XPath("/a/b", None))
        )
      },
      test("clauses targeting the same slot are last-wins") {
        assertTrue(
          get("/x").where(bodyEquals("a"), bodyContains("b")).body.contains(BodyMatch.Contains("b")),
          get("/x").where(header("X").equalTo("1"), header("X").matches("2")).headers ==
            Map("X" -> ValueMatch.Matches("2"))
        )
      },
      test("where folds multiple clauses onto one match") {
        val rm = get("/x").where(
          header("Authorization").matches("Bearer .*"),
          query("page").equalTo("1"),
          jsonPath("$.id").exists
        )
        assertTrue(
          rm == RequestMatch(
            method = Some(Method.Get),
            path = PathMatch.Exact("/x"),
            query = Map("page" -> ValueMatch.Equals("1")),
            headers = Map("Authorization" -> ValueMatch.Matches("Bearer .*")),
            body = Some(BodyMatch.JsonPath("$.id", None))
          )
        )
      }
    ),
    suite("response builders equal the canonical ResponseDef")(
      test("ok / status set the status with an empty body") {
        assertTrue(
          ok == ResponseDef(status = 200),
          status(402) == ResponseDef(status = 402)
        )
      },
      test("json/text/base64/withBody set the body") {
        assertTrue(
          ok.json("{}") == ResponseDef(status = 200, body = Body.Json("{}")),
          ok.text("hi") == ResponseDef(status = 200, body = Body.Text("hi")),
          ok.base64("YQ==") == ResponseDef(status = 200, body = Body.Base64("YQ==")),
          status(402).withBody(Body.Text("nope")) == ResponseDef(status = 402, body = Body.Text("nope"))
        )
      },
      test("withHeader / withStatus / withLatency refine the response") {
        assertTrue(
          ok.withHeader("Content-Type", "application/json") ==
            ResponseDef(status = 200, headers = Headers("Content-Type" -> "application/json")),
          ok.withStatus(204) == ResponseDef(status = 204),
          ok.json("{}").withLatency(50.millis) ==
            ResponseDef(status = 200, body = Body.Json("{}"), delay = Some(50.millis))
        )
      }
    ),
    suite("rule + spec assembly")(
      test("respondWith pairs a match and a response into a MockRule") {
        assertTrue(
          get("/ping").respondWith(ok.text("pong")) ==
            MockRule(
              `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
              respond = ResponseDef(status = 200, body = Body.Text("pong"))
            )
        )
      },
      test("withId stamps a rule id") {
        assertTrue(
          get("/ping").respondWith(ok).withId("r1").id.contains(RuleId("r1"))
        )
      },
      test("a full DSL rule equals the hand-built canonical rule") {
        val built = get("/ignored")
          .withPath(PathMatch.Template("/users/{id}"))
          .where(header("Authorization").matches("Bearer .*"), jsonPath("$.name").exists)
          .respondWith(status(201).json("""{"ok":true}""").withHeader("Location", "/users/1").withLatency(10.millis))
          .withId("create-user")

        val canonical = MockRule(
          `match` = RequestMatch(
            method = Some(Method.Get),
            path = PathMatch.Template("/users/{id}"),
            headers = Map("Authorization" -> ValueMatch.Matches("Bearer .*")),
            body = Some(BodyMatch.JsonPath("$.name", None))
          ),
          respond = ResponseDef(
            status = 201,
            headers = Headers("Location" -> "/users/1"),
            body = Body.Json("""{"ok":true}"""),
            delay = Some(10.millis)
          ),
          id = Some(RuleId("create-user"))
        )
        assertTrue(built == canonical)
      },
      test("mock / onPort / source assemble a spec and a Dsl source") {
        val r1 = get("/a").respondWith(ok)
        val r2 = post("/b").respondWith(status(201))
        assertTrue(
          mock(r1, r2) == MockSpec(List(r1, r2)),
          mock(r1).onPort(8080) == MockSpec(List(r1), port = Some(8080)),
          mock(r1, r2).source == MockSource.Dsl(MockSpec(List(r1, r2))),
          dslSource(r1, r2) == MockSource.Dsl(MockSpec(List(r1, r2)))
        )
      }
    ),
    test("a DSL spec normalizes through Provisioning to exactly its canonical rules") {
      // Ties DSL output to the real wire contract: the same Provisioning path the
      // raw sources use yields SourcePayload.Rules(handBuiltRules) for the DSL.
      val r1 = get("/ping").respondWith(ok.text("pong"))
      val r2 = post("/echo").where(jsonPath("$.msg").exists).respondWith(status(201))
      for
        prov <- Provisioning.make
        norm <- prov.normalize(dslSource(r1, r2))
      yield assertTrue(
        norm.size == 1,
        norm.head.payload == SourcePayload.Rules(List(r1, r2))
      )
    }
  )
