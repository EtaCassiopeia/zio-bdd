package zio.bdd.mock.rift

import zio.bdd.mock.*
import zio.json.ast.Json
import zio.test.*

object RiftProtocolSpec extends ZIOSpecDefault:

  private def field(j: Json, k: String): Option[Json] = j match
    case o: Json.Obj => o.fields.toMap.get(k)
    case _           => None

  extension (j: Json) private def /(k: String): Json = field(j, k).getOrElse(Json.Null)
  private def arr(j: Json): List[Json] = j match
    case Json.Arr(es) => es.toList
    case _            => Nil

  private val pingRule = MockRule(
    `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
    respond = ResponseDef(status = 200, headers = Map("Content-Type" -> "text/plain"), body = Body.Text("pong"))
  )

  def spec = suite("RiftProtocol (canonical <-> Mountebank)")(
    test("imposter carries port, http protocol, recordRequests, and one stub per rule") {
      val j = RiftProtocol.imposter(4545, "ping", List(pingRule))
      assertTrue(
        j / "port" == Json.Num(4545),
        j / "protocol" == Json.Str("http"),
        j / "recordRequests" == Json.Bool(true),
        arr(j / "stubs").size == 1
      )
    },
    test("an exact path + method rule becomes equals predicates and an `is` response") {
      val stub       = RiftProtocol.stub(pingRule)
      val preds      = arr(stub / "predicates")
      val methodPred = preds.find(p => (p / "equals" / "method") != Json.Null)
      val pathPred   = preds.find(p => (p / "equals" / "path") != Json.Null)
      val isResp     = arr(stub / "responses").head / "is"
      assertTrue(
        methodPred.exists(_ / "equals" / "method" == Json.Str("GET")),
        pathPred.exists(_ / "equals" / "path" == Json.Str("/ping")),
        isResp / "statusCode" == Json.Num(200),
        isResp / "body" == Json.Str("pong"),
        isResp / "headers" / "Content-Type" == Json.Str("text/plain")
      )
    },
    test("a regex path becomes a matches predicate; a template path is anchored regex") {
      val regex = RiftProtocol.stub(pingRule.copy(`match` = RequestMatch(path = PathMatch.Regex("/u/.*"))))
      val tmpl  = RiftProtocol.stub(pingRule.copy(`match` = RequestMatch(path = PathMatch.Template("/users/{id}"))))
      assertTrue(
        arr(regex / "predicates").head / "matches" / "path" == Json.Str("/u/.*"),
        arr(tmpl / "predicates").head / "matches" / "path" == Json.Str("^/users/[^/]+$")
      )
    },
    test("a response delay becomes a _behaviors.wait in milliseconds") {
      import zio.*
      val withDelay = RiftProtocol.response(ResponseDef(status = 201, delay = Some(50.millis)))
      assertTrue(
        withDelay / "is" / "statusCode" == Json.Num(201),
        withDelay / "_behaviors" / "wait" == Json.Num(50)
      )
    },
    test("PathMatch.Any emits no path predicate") {
      val stub = RiftProtocol.stub(pingRule.copy(`match` = RequestMatch(method = Some(Method.Post))))
      val keys =
        arr(stub / "predicates").map(p => p match { case o: Json.Obj => o.fields.map(_._1).toSet; case _ => Set.empty })
      assertTrue(arr(stub / "predicates").size == 1, keys.head == Set("equals"))
    },
    test("parseRecorded reads the requests[] of a GET imposter view") {
      val view =
        """{"port":4545,"requests":[
          |  {"method":"GET","path":"/ping","headers":{"Host":"x"},"body":null},
          |  {"method":"POST","path":"/echo","body":"hi"}
          |]}""".stripMargin
      val parsed = RiftProtocol.parseRecorded(view)
      assertTrue(
        parsed == Right(
          List(
            RecordedRequest(Method.Get, "/ping", Map("Host" -> "x"), None),
            RecordedRequest(Method.Post, "/echo", Map.empty, Some("hi"))
          )
        )
      )
    },
    test("response body kinds: Empty omits body; Json sets body; Base64 sets _mode binary") {
      val empty = RiftProtocol.response(ResponseDef(body = Body.Empty))
      val json  = RiftProtocol.response(ResponseDef(body = Body.Json("""{"a":1}""")))
      val b64   = RiftProtocol.response(ResponseDef(body = Body.Base64("YQ==")))
      assertTrue(
        field(empty / "is", "body").isEmpty,
        json / "is" / "body" == Json.Str("""{"a":1}"""),
        b64 / "is" / "body" == Json.Str("YQ=="),
        b64 / "is" / "_mode" == Json.Str("binary")
      )
    },
    test("query/header/body matchers map to their Mountebank operators") {
      val rule = MockRule(
        `match` = RequestMatch(
          query = Map("q" -> ValueMatch.Equals("1")),
          headers = Map("H" -> ValueMatch.Contains("x")),
          body = Some(BodyMatch.Matches("ab.*"))
        ),
        respond = ResponseDef()
      )
      val preds = arr(RiftProtocol.stub(rule) / "predicates")
      assertTrue(
        preds.exists(p => p / "equals" / "query" / "q" == Json.Str("1")),
        preds.exists(p => p / "contains" / "headers" / "H" == Json.Str("x")),
        preds.exists(p => p / "matches" / "body" == Json.Str("ab.*"))
      )
    },
    test("a JsonPath body matcher emits a jsonpath selector with an equals body") {
      val rule = MockRule(RequestMatch(body = Some(BodyMatch.JsonPath("$.id", Some("7")))), ResponseDef())
      val pred = arr(RiftProtocol.stub(rule) / "predicates").head
      assertTrue(
        pred / "jsonpath" / "selector" == Json.Str("$.id"),
        pred / "equals" / "body" == Json.Str("7")
      )
    },
    test("parseRecorded fails on malformed JSON; imposterFromRaw rejects a non-object document") {
      assertTrue(
        RiftProtocol.parseRecorded("not json").isLeft,
        RiftProtocol.imposterFromRaw(1, "x", "[1,2,3]").isLeft
      )
    },
    test("imposterFromRaw forces our port and recordRequests while keeping stubs") {
      val raw           = """{"port":9999,"protocol":"http","stubs":[{"predicates":[],"responses":[]}]}"""
      val Right((j, n)) = RiftProtocol.imposterFromRaw(4600, "json", raw): @unchecked
      assertTrue(
        j / "port" == Json.Num(4600),
        j / "recordRequests" == Json.Bool(true),
        n == 1
      )
    }
  )
