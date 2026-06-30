package zio.bdd.mock.rift

import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json

/**
 * Pure translation between the canonical mock model and Rift's
 * Mountebank-compatible wire JSON. No effects, no I/O — every function is a
 * total mapping over value types, so the protocol is unit-testable without a
 * backend.
 */
private[rift] object RiftProtocol:

  // ---------------------------------------------------------------------------
  // Canonical model -> Mountebank wire JSON
  // ---------------------------------------------------------------------------

  // Rift/Mountebank serve 200-empty for an unmatched request unless an imposter
  // `defaultResponse` is set; pin it to 404 so the portable adapter matches the
  // cross-adapter contract (WireMock's unmatched default) — #165.
  private val unmatched404: Json = Json.Obj("statusCode" -> Json.Num(404))

  /**
   * The `_rift.flowState` extension that backs declarative scenarios
   * (rift#190). Rift only attaches a real (in-memory) flow store at
   * imposter-creation when either scenario stubs are present OR this config is
   * — otherwise stubs added later via `POST /stubs` hit a NoOp store and their
   * state never advances. We provision empty then add scenario stubs in
   * `define`, so every imposter carries it. `flowIdSource` partitions state:
   * the imposter port (PerInstance) or a correlation header (Correlated).
   */
  private def flowStateExt(flowIdSource: String): (String, Json) =
    "_rift" -> Json.Obj(
      "flowState" -> Json.Obj(
        "backend"                -> Json.Str("inmemory"),
        "ttlSeconds"             -> Json.Num(300),
        "mountebankStateMapping" -> Json.Obj("flowIdSource" -> Json.Str(flowIdSource))
      )
    )

  /**
   * A full `POST /imposters` body for one space: one imposter, recording on.
   */
  def imposter(port: Int, name: String, rules: List[MockRule]): Json =
    Json.Obj(
      "port"            -> Json.Num(port),
      "protocol"        -> Json.Str("http"),
      "name"            -> Json.Str(name),
      "recordRequests"  -> Json.Bool(true),
      "defaultResponse" -> unmatched404,
      "stubs"           -> Json.Arr(rules.map(stub)*),
      flowStateExt("imposter_port")
    )

  /**
   * The shared imposter for Correlated isolation (#156): empty stubs (added per
   * space via `POST /imposters/:port/spaces/:flowId/stubs`) and a flow-id
   * source of `header:<correlationHeader>`, so Rift resolves each request's
   * flow + gates stubs/recorded requests by that header natively (rift#223).
   * The flow-state config is a backend-native extension, so it lives under
   * `_rift.flowState`
   * (`imposter.config.rift.flowState.mountebankStateMapping.flowIdSource`).
   */
  def correlatedImposter(port: Int, name: String, correlationHeader: String): Json =
    Json.Obj(
      "port"            -> Json.Num(port),
      "protocol"        -> Json.Str("http"),
      "name"            -> Json.Str(name),
      "recordRequests"  -> Json.Bool(true),
      "defaultResponse" -> unmatched404,
      "stubs"           -> Json.Arr(),
      flowStateExt(s"header:$correlationHeader")
    )

  /** A single Mountebank stub: predicates (ANDed) + one response. */
  def stub(rule: MockRule): Json =
    val idField = rule.id.map(id => "id" -> Json.Str(id.value)).toList
    Json.Obj(
      (idField ++ List(
        "predicates" -> Json.Arr(predicates(rule.`match`)*),
        "responses"  -> Json.Arr(response(rule.respond))
      ))*
    )

  /**
   * One edge of a scenario FSM (#131): a normal stub carrying Rift's
   * declarative scenario fields (rift#190). The stub is eligible only while
   * `(flow_id, scenarioName)` state equals `whenState`, and serves `respond`
   * then transitions to `thenState` (`None` = stay — `newScenarioState` is
   * omitted). State is partitioned by `flow_id`, so two spaces sharing a
   * scenario name keep independent state natively (no name-namespacing needed).
   */
  def statefulStub(
    scenarioName: String,
    whenState: ScenarioState,
    thenState: Option[ScenarioState],
    rule: MockRule
  ): Json =
    val transition = thenState.map(s => "newScenarioState" -> Json.Str(s.value)).toList
    Json.Obj(
      (List(
        "scenarioName"          -> Json.Str(scenarioName),
        "requiredScenarioState" -> Json.Str(whenState.value)
      ) ++ transition ++ List(
        "predicates" -> Json.Arr(predicates(rule.`match`)*),
        "responses"  -> Json.Arr(response(rule.respond))
      ))*
    )

  /** The `{stubs: [...]}` body for `PUT /imposters/:port/stubs`. */
  def replaceStubsBody(rules: List[MockRule]): Json =
    Json.Obj("stubs" -> Json.Arr(rules.map(stub)*))

  /** The `{index, stub}` body for `POST /imposters/:port/stubs`. */
  def addStubBody(index: Int, rule: MockRule): Json =
    Json.Obj("index" -> Json.Num(index), "stub" -> stub(rule))

  // ---------------------------------------------------------------------------
  // Fault injection (#128) — the `_rift.fault` response extension (rift#239)
  // ---------------------------------------------------------------------------

  /**
   * A stub response carrying a `_rift.fault` extension. The connection kinds
   * set `_rift.fault.tcp` (Rift aborts the connection at the transport layer,
   * so the SUT observes a real reset/empty/malformed/random failure — the `is`
   * below is never sent); [[FaultKind.LatencySpike]] sets `_rift.fault.latency`
   * (Rift sleeps, then serves this `is` 200). `probability` is pinned to 1 so
   * the fault always fires.
   */
  def faultResponse(fault: FaultKind): Json =
    val cfg = fault match
      case FaultKind.LatencySpike(d) =>
        Json.Obj("latency" -> Json.Obj("probability" -> Json.Num(1), "ms" -> Json.Num(d.toMillis.toInt)))
      case FaultKind.ConnectionReset => tcpFault("CONNECTION_RESET_BY_PEER")
      case FaultKind.EmptyResponse   => tcpFault("EMPTY_RESPONSE")
      case FaultKind.MalformedChunk  => tcpFault("MALFORMED_RESPONSE_CHUNK")
      case FaultKind.RandomThenClose => tcpFault("RANDOM_DATA_THEN_CLOSE")
    Json.Obj("is" -> Json.Obj("statusCode" -> Json.Num(200)), "_rift" -> Json.Obj("fault" -> cfg))

  private def tcpFault(token: String): Json = Json.Obj("tcp" -> Json.Str(token))

  /**
   * A Mountebank stub whose response injects `fault` for requests matching `m`.
   */
  def faultStub(m: RequestMatch, fault: FaultKind, id: RuleId): Json =
    Json.Obj(
      "id"         -> Json.Str(id.value),
      "predicates" -> Json.Arr(predicates(m)*),
      "responses"  -> Json.Arr(faultResponse(fault))
    )

  /**
   * The `{index, stub}` body for `POST /imposters/:port/stubs` of a fault stub.
   */
  def addFaultStubBody(index: Int, m: RequestMatch, fault: FaultKind, id: RuleId): Json =
    Json.Obj("index" -> Json.Num(index), "stub" -> faultStub(m, fault, id))

  /**
   * The `{index, stub}` body for a pre-built stub JSON (e.g. a
   * [[statefulStub]]).
   */
  def addStubBodyOf(index: Int, stub: Json): Json =
    Json.Obj("index" -> Json.Num(index), "stub" -> stub)

  // ===== Optional capabilities (#132): script / proxy / templating ===============

  /** A Mountebank stub for `m` carrying a single pre-built response object. */
  private def capStub(m: RequestMatch, id: RuleId, response: Json): Json =
    Json.Obj(
      "id"         -> Json.Str(id.value),
      "predicates" -> Json.Arr(predicates(m)*),
      "responses"  -> Json.Arr(response)
    )

  /** The Rift script-engine token. */
  def scriptEngineName(engine: ScriptEngine): String = engine match
    case ScriptEngine.Rhai       => "rhai"
    case ScriptEngine.Lua        => "lua"
    case ScriptEngine.JavaScript => "javascript"

  /**
   * A stub whose response is computed by `script` via the `_rift.script`
   * extension — the script's `should_inject` decides the response.
   */
  def scriptStub(m: RequestMatch, script: Script, id: RuleId): Json =
    capStub(
      m,
      id,
      Json.Obj(
        "_rift" -> Json.Obj(
          "script" -> Json.Obj(
            "engine" -> Json.Str(scriptEngineName(script.engine)),
            "code"   -> Json.Str(script.code)
          )
        )
      )
    )

  /**
   * A stub that proxies `m` to `upstream` in `proxyOnce` mode — Rift records
   * the first upstream response as a new stub and replays it thereafter (so it
   * keeps serving when the upstream is down). `predicateGenerators` keys the
   * recorded stub on the request method + path, so a later identical request
   * matches the recording instead of re-proxying.
   */
  def proxyStub(m: RequestMatch, upstream: String, id: RuleId): Json =
    capStub(
      m,
      id,
      Json.Obj(
        "proxy" -> Json.Obj(
          "to"   -> Json.Str(upstream),
          "mode" -> Json.Str("proxyOnce"),
          "predicateGenerators" -> Json.Arr(
            Json.Obj("matches" -> Json.Obj("method" -> Json.Bool(true), "path" -> Json.Bool(true)))
          )
        )
      )
    )

  private def templateSourceName(source: TemplateSource): String = source match
    case TemplateSource.Path => "path"
    case TemplateSource.Body => "body"

  /**
   * A templated `is` response: the body carries each capture's `token` literal,
   * and a Mountebank `copy` behavior per capture substitutes it with the value
   * extracted (by regex) from the request.
   */
  def templateStub(m: RequestMatch, template: ResponseTemplate, id: RuleId): Json =
    val copies = template.captures.map(c =>
      Json.Obj(
        "from"  -> Json.Str(templateSourceName(c.source)),
        "into"  -> Json.Str(c.token),
        "using" -> Json.Obj("method" -> Json.Str("regex"), "selector" -> Json.Str(c.regex))
      )
    )
    capStub(
      m,
      id,
      Json.Obj(
        "is"         -> Json.Obj("statusCode" -> Json.Num(template.status), "body" -> Json.Str(template.body)),
        "_behaviors" -> Json.Obj("copy" -> Json.Arr(copies*))
      )
    )

  def predicates(m: RequestMatch): List[Json] =
    val methodPred = m.method.map(meth => equalsObj("method" -> Json.Str(methodName(meth))))
    val pathPred = m.path match
      case PathMatch.Any         => None
      case PathMatch.Exact(p)    => Some(equalsObj("path" -> Json.Str(p)))
      case PathMatch.Regex(r)    => Some(matchesObj("path" -> Json.Str(r)))
      case PathMatch.Template(t) => Some(matchesObj("path" -> Json.Str(templateToRegex(t))))
    val queryPreds  = m.query.toList.map((k, vm) => valuePred("query", k, vm))
    val headerPreds = m.headers.toList.map((k, vm) => valuePred("headers", k, vm))
    val bodyPred    = m.body.map(bodyPredicate)
    methodPred.toList ++ pathPred.toList ++ queryPreds ++ headerPreds ++ bodyPred.toList

  def response(r: ResponseDef): Json =
    val headers =
      if r.headers.isEmpty then Nil
      else
        // Rift's wire accepts a string for a single value and an array for many (rift#241).
        List("headers" -> Json.Obj(r.headers.entries.map { (k, vs) =>
          k -> (vs match
            case v :: Nil => Json.Str(v)
            case many     => Json.Arr(many.map(Json.Str(_))*)
          )
        }*))
    val body = r.body match
      case Body.Empty     => Nil
      case Body.Text(v)   => List("body" -> Json.Str(v))
      case Body.Json(v)   => List("body" -> Json.Str(v))
      case Body.Base64(v) => List("body" -> Json.Str(v), "_mode" -> Json.Str("binary"))
    val isObj = Json.Obj((("statusCode" -> Json.Num(r.status)) :: headers ++ body)*)
    val behaviors =
      r.delay.map(d => "_behaviors" -> Json.Obj("wait" -> Json.Num(d.toMillis.toInt))).toList
    Json.Obj((("is" -> isObj) :: behaviors)*)

  /** Rebuild a raw imposter doc, forcing our pool port and recording on. */
  def imposterFromRaw(port: Int, name: String, raw: String): Either[String, (Json, Int)] =
    raw.fromJson[Json].flatMap {
      case obj: Json.Obj =>
        val fields    = obj.fields.toMap
        val stubCount = fields.get("stubs").collect { case Json.Arr(es) => es.length }.getOrElse(0)
        val forced = upsert(
          obj,
          "port"           -> Json.Num(port),
          "protocol"       -> fields.getOrElse("protocol", Json.Str("http")),
          "name"           -> fields.getOrElse("name", Json.Str(name)),
          "recordRequests" -> Json.Bool(true)
        )
        Right((forced, stubCount))
      case _ => Left("raw Rift source must be a JSON object (an imposter document)")
    }

  // ---------------------------------------------------------------------------
  // Mountebank wire JSON -> canonical model (recorded requests)
  // ---------------------------------------------------------------------------

  // headers decode as the raw AST per key so we accept both Rift's single-value
  // string form (`"k": "v"`) and the multi-value array form (`"k": ["a", "b"]`, rift#241).
  private case class WireReq(
    method: String,
    path: String,
    headers: Option[Map[String, Json]] = None,
    body: Option[String] = None
  ) derives JsonDecoder
  private case class WireView(requests: Option[List[WireReq]] = None) derives JsonDecoder

  private case class WireScenario(name: String, state: String) derives JsonDecoder
  private case class WireScenarios(scenarios: List[WireScenario] = Nil) derives JsonDecoder

  /**
   * The current state of scenario `name` from a `GET
   * /imposters/:port/scenarios` body (rift#190); `None` if the scenario isn't
   * declared on the imposter.
   */
  def parseScenarioState(viewJson: String, name: String): Either[String, Option[String]] =
    viewJson.fromJson[WireScenarios].map(_.scenarios.find(_.name == name).map(_.state))

  /** Parse the `requests[]` of a `GET /imposters/:port` view. */
  def parseRecorded(viewJson: String): Either[String, List[RecordedRequest]] =
    viewJson.fromJson[WireView].map(_.requests.getOrElse(Nil).map(toRecorded))

  /**
   * Parse the bare JSON array returned by `GET
   * /imposters/:port/requests?match=…` — the header/flow-id-filtered recorded
   * requests (rift#201), used by Correlated `received`.
   */
  def parseRequestsArray(arrayJson: String): Either[String, List[RecordedRequest]] =
    arrayJson.fromJson[List[WireReq]].map(_.map(toRecorded))

  private def toRecorded(r: WireReq): RecordedRequest =
    val headers = r.headers.getOrElse(Map.empty).foldLeft(Headers.empty) { case (h, (k, json)) =>
      json match
        case Json.Str(v) => h.add(k, v)
        case Json.Arr(elems) =>
          elems.foldLeft(h)((acc, j) => j match { case Json.Str(v) => acc.add(k, v); case _ => acc })
        case _ => h
    }
    RecordedRequest(methodFromWire(r.method), r.path, headers, r.body)

  // ---------------------------------------------------------------------------
  // helpers
  // ---------------------------------------------------------------------------

  def methodName(m: Method): String = m.toString.toUpperCase

  // Rift records standard, upper-case HTTP method names; an unrecognized token
  // (never produced by Rift) falls back to GET rather than failing a read.
  def methodFromWire(s: String): Method =
    Method.values.find(_.toString.equalsIgnoreCase(s)).getOrElse(Method.Get)

  // "/users/{id}" -> anchored regex "^/users/[^/]+$" (Mountebank has no template).
  def templateToRegex(t: String): String =
    "^" + t.replaceAll("\\{[^/}]+\\}", "[^/]+") + "$"

  private def equalsObj(fields: (String, Json)*): Json  = Json.Obj("equals" -> Json.Obj(fields*))
  private def matchesObj(fields: (String, Json)*): Json = Json.Obj("matches" -> Json.Obj(fields*))

  private def valuePred(selector: String, key: String, vm: ValueMatch): Json =
    val (op, value) = vm match
      case ValueMatch.Equals(v)   => ("equals", v)
      case ValueMatch.Contains(v) => ("contains", v)
      case ValueMatch.Matches(r)  => ("matches", r)
    Json.Obj(op -> Json.Obj(selector -> Json.Obj(key -> Json.Str(value))))

  private def bodyPredicate(bm: BodyMatch): Json =
    bm match
      case BodyMatch.Equals(v)   => Json.Obj("equals" -> Json.Obj("body" -> Json.Str(v)))
      case BodyMatch.Contains(v) => Json.Obj("contains" -> Json.Obj("body" -> Json.Str(v)))
      case BodyMatch.Matches(r)  => Json.Obj("matches" -> Json.Obj("body" -> Json.Str(r)))
      case BodyMatch.JsonPath(path, expected) =>
        selectorPred("jsonpath", path, expected)
      case BodyMatch.XPath(path, expected) =>
        selectorPred("xpath", path, expected)

  private def selectorPred(kind: String, path: String, expected: Option[String]): Json =
    val sel = kind -> Json.Obj("selector" -> Json.Str(path))
    expected match
      case Some(e) => Json.Obj(sel, "equals" -> Json.Obj("body" -> Json.Str(e)))
      case None    => Json.Obj(sel, "exists" -> Json.Obj("body" -> Json.Bool(true)))

  private def upsert(obj: Json.Obj, overrides: (String, Json)*): Json.Obj =
    val overrideKeys = overrides.map(_._1).toSet
    val kept         = obj.fields.filterNot((k, _) => overrideKeys(k))
    Json.Obj(kept ++ overrides)
