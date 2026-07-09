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
    respond = ResponseDef(status = 200, headers = Headers("Content-Type" -> "text/plain"), body = Body.Text("pong"))
  )

  def spec = suite("RiftProtocol (canonical <-> Mountebank)")(
    test("statefulStub carries the scenario FSM fields; stay omits newScenarioState (#131)") {
      val invRule = (path: String, body: String) =>
        MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status = 200, body = Body.Text(body)))
      val advance = RiftProtocol.statefulStub(
        "invoice",
        ScenarioState("Started"),
        Some(ScenarioState("Paid")),
        invRule("/inv", "created")
      )
      val stay = RiftProtocol.statefulStub("invoice", ScenarioState("Paid"), None, invRule("/inv", "paid"))
      assertTrue(
        advance / "scenarioName" == Json.Str("invoice"),
        advance / "requiredScenarioState" == Json.Str("Started"),
        advance / "newScenarioState" == Json.Str("Paid"),
        arr(advance / "responses").head / "is" / "body" == Json.Str("created"),
        stay / "requiredScenarioState" == Json.Str("Paid"),
        stay / "newScenarioState" == Json.Null // stay -> no transition field
      )
    },
    test("parseScenarioState reads a scenario's state from the admin view; None when absent (#131)") {
      val view =
        """{"flowId":"4545","scenarios":[{"name":"invoice","state":"Paid"},{"name":"other","state":"Started"}]}"""
      assertTrue(
        RiftProtocol.parseScenarioState(view, "invoice") == Right(Some("Paid")),
        RiftProtocol.parseScenarioState(view, "missing") == Right(None),
        RiftProtocol.parseScenarioState("not json", "invoice").isLeft // malformed -> Left (becomes CommunicationError)
      )
    },
    test("imposter carries port, http protocol, recordRequests, a stub per rule, and a flow store (#131)") {
      val j = RiftProtocol.imposter(4545, "ping", List(pingRule))
      assertTrue(
        j / "port" == Json.Num(4545),
        j / "protocol" == Json.Str("http"),
        j / "recordRequests" == Json.Bool(true),
        arr(j / "stubs").size == 1,
        // an in-memory flow store keyed by port, so scenario stubs added later actually advance
        j / "_rift" / "flowState" / "backend" == Json.Str("inmemory"),
        // flowIdSource sits directly under flowState; the mountebankStateMapping wrapper is gone (rift#266)
        j / "_rift" / "flowState" / "flowIdSource" == Json.Str("imposter_port"),
        field(j / "_rift" / "flowState", "mountebankStateMapping").isEmpty
      )
    },
    test("correlatedImposter emits flowIdSource header:<h> directly under flowState; no wrapper (rift#266)") {
      val j = RiftProtocol.correlatedImposter(4600, "correlated", "X-Mock-Space")
      assertTrue(
        j / "_rift" / "flowState" / "flowIdSource" == Json.Str("header:X-Mock-Space"),
        field(j / "_rift" / "flowState", "mountebankStateMapping").isEmpty
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
        isResp / "headers" / "content-type" == Json.Str("text/plain")
      )
    },
    test("a multi-value response header emits a JSON array; a single value stays a bare string (rift#241)") {
      val multi  = RiftProtocol.response(ResponseDef(status = 200, headers = Headers.multi("X-Multi" -> List("a", "b"))))
      val single = RiftProtocol.response(ResponseDef(status = 200, headers = Headers("X-One" -> "v")))
      assertTrue(
        arr(multi / "is" / "headers" / "x-multi") == List(Json.Str("a"), Json.Str("b")),
        single / "is" / "headers" / "x-one" == Json.Str("v") // single value is not wrapped in an array
      )
    },
    test("parseRecorded decodes a multi-value header array into multiple values; keys lower-case (rift#241)") {
      val view   = """{"requests":[{"method":"GET","path":"/p","headers":{"X-Multi":["a","b"],"Host":"x"}}]}"""
      val parsed = RiftProtocol.parseRecorded(view)
      assertTrue(
        parsed.map(_.head.headers.values("x-multi")) == Right(List("a", "b")),
        parsed.map(_.head.headers.first("host")) == Right(Some("x")) // mixed-case wire key surfaces lower-cased
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
    test("a connection FaultKind becomes a stub response with _rift.fault.tcp and a 200 carrier `is`") {
      def tcp(fault: FaultKind): Json = RiftProtocol.faultResponse(fault) / "_rift" / "fault" / "tcp"
      assertTrue(
        tcp(FaultKind.ConnectionReset) == Json.Str("CONNECTION_RESET_BY_PEER"),
        tcp(FaultKind.EmptyResponse) == Json.Str("EMPTY_RESPONSE"),
        tcp(FaultKind.MalformedChunk) == Json.Str("MALFORMED_RESPONSE_CHUNK"),
        tcp(FaultKind.RandomThenClose) == Json.Str("RANDOM_DATA_THEN_CLOSE"),
        RiftProtocol.faultResponse(FaultKind.ConnectionReset) / "is" / "statusCode" == Json.Num(200)
      )
    },
    test("a LatencySpike becomes _rift.fault.latency with probability 1 and ms; no tcp") {
      import zio.*
      val resp    = RiftProtocol.faultResponse(FaultKind.LatencySpike(1.second))
      val latency = resp / "_rift" / "fault" / "latency"
      assertTrue(
        latency / "ms" == Json.Num(1000),
        latency / "probability" == Json.Num(1),
        field(resp / "_rift" / "fault", "tcp").isEmpty,
        resp / "is" / "statusCode" == Json.Num(200)
      )
    },
    test("faultStub carries the request predicates and the fault response under the given id") {
      val m     = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/boom"))
      val stub  = RiftProtocol.faultStub(m, FaultKind.ConnectionReset, RuleId("f1"))
      val preds = arr(stub / "predicates")
      assertTrue(
        stub / "id" == Json.Str("f1"),
        preds.exists(p => p / "equals" / "path" == Json.Str("/boom")),
        arr(stub / "responses").head / "_rift" / "fault" / "tcp" == Json.Str("CONNECTION_RESET_BY_PEER")
      )
    },
    test("scriptStub carries predicates and a _rift.script response (engine + code) (#132)") {
      val m = RequestMatch(path = PathMatch.Exact("/s"))
      val stub =
        RiftProtocol.scriptStub(m, Script(ScriptEngine.Rhai, "fn should_inject(r,f){#{inject:false}}"), RuleId("s1"))
      val resp = arr(stub / "responses").head
      assertTrue(
        stub / "id" == Json.Str("s1"),
        arr(stub / "predicates").exists(p => p / "equals" / "path" == Json.Str("/s")),
        resp / "_rift" / "script" / "engine" == Json.Str("rhai"),
        resp / "_rift" / "script" / "code" == Json.Str("fn should_inject(r,f){#{inject:false}}")
      )
    },
    test("script engine tokens map to rhai/lua/javascript (#132)") {
      assertTrue(
        RiftProtocol.scriptEngineName(ScriptEngine.Rhai) == "rhai",
        RiftProtocol.scriptEngineName(ScriptEngine.Lua) == "lua",
        RiftProtocol.scriptEngineName(ScriptEngine.JavaScript) == "javascript"
      )
    },
    test(
      "proxyStub emits a proxy response to the upstream in proxyOnce mode with method+path predicate generators (#132)"
    ) {
      val stub = RiftProtocol.proxyStub(RequestMatch(path = PathMatch.Exact("/p")), "http://up:8080", RuleId("p1"))
      val resp = arr(stub / "responses").head
      val gen  = arr(resp / "proxy" / "predicateGenerators").head
      assertTrue(
        stub / "id" == Json.Str("p1"),
        resp / "proxy" / "to" == Json.Str("http://up:8080"),
        resp / "proxy" / "mode" == Json.Str("proxyOnce"),
        gen / "matches" / "method" == Json.Bool(true),
        gen / "matches" / "path" == Json.Bool(true)
      )
    },
    test("templateStub emits an `is` body plus a _behaviors.copy per capture; path/body sources map (#132)") {
      val template = ResponseTemplate(
        body = "Hello ${NAME} ${WHO}",
        captures = List(
          TemplateCapture("${NAME}", TemplateSource.Path, "[^/]+$"),
          TemplateCapture("${WHO}", TemplateSource.Body, "\\w+")
        )
      )
      val stub     = RiftProtocol.templateStub(RequestMatch(path = PathMatch.Exact("/greet")), template, RuleId("t1"))
      val resp     = arr(stub / "responses").head
      val copies   = arr(resp / "_behaviors" / "copy")
      val pathCopy = copies.find(_ / "into" == Json.Str("${NAME}")).getOrElse(Json.Null)
      val bodyCopy = copies.find(_ / "into" == Json.Str("${WHO}")).getOrElse(Json.Null)
      assertTrue(
        resp / "is" / "body" == Json.Str("Hello ${NAME} ${WHO}"),
        pathCopy / "from" == Json.Str("path"),
        pathCopy / "using" / "method" == Json.Str("regex"),
        pathCopy / "using" / "selector" == Json.Str("[^/]+$"),
        bodyCopy / "from" == Json.Str("body") // TemplateSource.Body -> "body"
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
            RecordedRequest(Method.Get, "/ping", Headers("Host" -> "x"), None),
            RecordedRequest(Method.Post, "/echo", Headers.empty, Some("hi"))
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
    test("imposterFromRaw forces our port and defaults recordRequests on when the doc omits it") {
      val raw           = """{"port":9999,"protocol":"http","stubs":[{"predicates":[],"responses":[]}]}"""
      val Right((j, n)) = RiftProtocol.imposterFromRaw(4600, "json", raw): @unchecked
      assertTrue(
        j / "port" == Json.Num(4600),
        j / "recordRequests" == Json.Bool(true),
        n == 1
      )
    },
    test("imposterFromRaw honors an authored recordRequests, still forcing our port (#294)") {
      val off              = """{"port":9999,"recordRequests":false,"stubs":[]}"""
      val on               = """{"port":9999,"recordRequests":true,"stubs":[]}"""
      val Right((jOff, _)) = RiftProtocol.imposterFromRaw(4600, "json", off): @unchecked
      val Right((jOn, _))  = RiftProtocol.imposterFromRaw(4600, "json", on): @unchecked
      assertTrue(
        jOff / "recordRequests" == Json.Bool(false), // authored false preserved, not overridden
        jOff / "port" == Json.Num(4600),             // port still forced
        jOn / "recordRequests" == Json.Bool(true),
        jOn / "port" == Json.Num(4600)
      )
    },
    test("topLevelPort reads a document's own numeric port (#214), else None") {
      assertTrue(
        RiftProtocol.topLevelPort("""{"port":9999,"protocol":"http","stubs":[]}""").contains(9999),
        RiftProtocol.topLevelPort("""{"protocol":"http","stubs":[]}""").isEmpty, // no port
        RiftProtocol.topLevelPort("""{"port":0,"stubs":[]}""").isEmpty,          // 0 = auto-assign convention
        RiftProtocol.topLevelPort("""{"port":"nope","stubs":[]}""").isEmpty,     // non-numeric
        RiftProtocol.topLevelPort("[1,2,3]").isEmpty,                            // not an object
        RiftProtocol.topLevelPort("not json").isEmpty                            // malformed
      )
    }
  )
