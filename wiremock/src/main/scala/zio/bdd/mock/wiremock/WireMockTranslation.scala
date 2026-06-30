package zio.bdd.mock.wiremock

import zio.bdd.mock.*

import com.github.tomakehurst.wiremock.client.{MappingBuilder, ResponseDefinitionBuilder, WireMock as WM}
import com.github.tomakehurst.wiremock.http.Fault
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import com.github.tomakehurst.wiremock.stubbing.StubMapping

import java.util.UUID

/**
 * Translates the backend-neutral [[MockRule]] model to WireMock stub mappings.
 */
private[wiremock] object WireMockTranslation:

  /**
   * Build the stub for `rule` on space `id`. In Correlated mode `correlation`
   * stamps the matcher (header == this space) onto the request pattern so only
   * requests carrying the space's tag can match it; in PerInstance mode it is
   * `None` (the space owns its server outright).
   */
  def stub(id: SpaceId, rule: MockRule, priority: Priority, correlation: Option[Correlation]): StubMapping =
    matcher(id, rule.`match`, priority, correlation).willReturn(response(rule.respond)).build()

  /**
   * Build a fault stub for space `id` matching `m` (#128): same request matcher
   * as [[stub]] (so correlation/priority behave identically), but the response
   * injects `fault` via WireMock's native `Fault` enum (connection kinds) or a
   * fixed delay ([[FaultKind.LatencySpike]]).
   */
  def faultStub(
    id: SpaceId,
    m: RequestMatch,
    fault: FaultKind,
    priority: Priority,
    correlation: Option[Correlation]
  ): StubMapping =
    matcher(id, m, priority, correlation).willReturn(faultResponse(fault)).build()

  // Shared matcher builder for normal and fault stubs: the request matcher
  // (requestBuilder) plus the Priority-enum precedence and a fresh id.
  private def matcher(
    id: SpaceId,
    m: RequestMatch,
    priority: Priority,
    correlation: Option[Correlation]
  ): MappingBuilder =
    val base = requestBuilder(id, m, correlation)
    base.atPriority(priority match
      case Priority.Overlay => 1
      case Priority.Base    => 10
    )
    base.withId(UUID.randomUUID())
    base

  private def faultResponse(fault: FaultKind): ResponseDefinitionBuilder = fault match
    case FaultKind.ConnectionReset => WM.aResponse().withFault(Fault.CONNECTION_RESET_BY_PEER)
    case FaultKind.EmptyResponse   => WM.aResponse().withFault(Fault.EMPTY_RESPONSE)
    case FaultKind.MalformedChunk  => WM.aResponse().withFault(Fault.MALFORMED_RESPONSE_CHUNK)
    case FaultKind.RandomThenClose => WM.aResponse().withFault(Fault.RANDOM_DATA_THEN_CLOSE)
    case FaultKind.LatencySpike(d) => WM.aResponse().withStatus(200).withFixedDelay(d.toMillis.toInt)

  /**
   * Build one edge of a scenario FSM (#130): the request matcher (plus the
   * space's correlation header) gated by
   * `inScenario(name).whenScenarioStateIs`, optionally transitioning via
   * `willSetStateTo` (`None` = stay). `priority` comes from the rule's
   * declaration index so that among rules eligible in the same state, the
   * first-declared wins — WireMock otherwise breaks an equal-priority tie in
   * favour of the most-recently-added stub.
   */
  def statefulStub(
    id: SpaceId,
    scenarioName: String,
    whenState: ScenarioState,
    thenState: Option[ScenarioState],
    rule: MockRule,
    priority: Int,
    correlation: Option[Correlation]
  ): StubMapping =
    val sc = requestBuilder(id, rule.`match`, correlation)
      .inScenario(scenarioName)
      .whenScenarioStateIs(whenState.value)
    thenState.foreach(s => sc.willSetStateTo(s.value))
    sc.atPriority(priority)
    sc.withId(UUID.randomUUID())
    sc.willReturn(response(rule.respond)).build()

  private def requestBuilder(id: SpaceId, m: RequestMatch, correlation: Option[Correlation]): MappingBuilder =
    val base = methodAndUrl(m)
    m.headers.foreach((k, v) => base.withHeader(k, valuePattern(v)))
    m.query.foreach((k, v) => base.withQueryParam(k, valuePattern(v)))
    m.body.foreach(b => base.withRequestBody(bodyPattern(b)))
    correlation.foreach(c => base.withHeader(c.header, c.matcher(id)))
    base

  private def methodAndUrl(m: RequestMatch): MappingBuilder =
    val url = m.path match
      case PathMatch.Any         => WM.anyUrl()
      case PathMatch.Exact(p)    => WM.urlPathEqualTo(p)
      case PathMatch.Regex(p)    => WM.urlPathMatching(p)
      case PathMatch.Template(t) => WM.urlPathTemplate(t)
    m.method match
      case Some(method) => WM.request(method.toString.toUpperCase, url)
      case None         => WM.any(url)

  private def valuePattern(v: ValueMatch): StringValuePattern = v match
    case ValueMatch.Equals(value)   => WM.equalTo(value)
    case ValueMatch.Contains(value) => WM.containing(value)
    case ValueMatch.Matches(regex)  => WM.matching(regex)

  private def bodyPattern(b: BodyMatch): StringValuePattern = b match
    case BodyMatch.Equals(value)   => WM.equalTo(value)
    case BodyMatch.Contains(value) => WM.containing(value)
    case BodyMatch.Matches(regex)  => WM.matching(regex)
    case BodyMatch.JsonPath(path, expected) =>
      expected.fold(WM.matchingJsonPath(path))(e => WM.matchingJsonPath(path, WM.equalTo(e)))
    case BodyMatch.XPath(path, expected) =>
      expected.fold(WM.matchingXPath(path))(e => WM.matchingXPath(path, WM.equalTo(e)))

  private def response(r: ResponseDef): ResponseDefinitionBuilder =
    val rb = WM.aResponse().withStatus(r.status)
    r.headers.entries.foreach((k, vs) => rb.withHeader(k, vs*)) // one header line per value (#162)
    r.body match
      case Body.Empty     => ()
      case Body.Text(v)   => rb.withBody(v)
      case Body.Json(v)   => rb.withBody(v)
      case Body.Base64(v) => rb.withBase64Body(v)
    r.delay.foreach(d => rb.withFixedDelay(d.toMillis.toInt))
    rb

  /** A [[RecordedRequest]] from a WireMock logged request. */
  def recorded(method: String, url: String, headers: Headers, body: Option[String]): RecordedRequest =
    RecordedRequest(parseMethod(method), url, headers, body)

  private def parseMethod(name: String): Method = name.toUpperCase match
    case "GET"     => Method.Get
    case "POST"    => Method.Post
    case "PUT"     => Method.Put
    case "DELETE"  => Method.Delete
    case "PATCH"   => Method.Patch
    case "HEAD"    => Method.Head
    case "OPTIONS" => Method.Options
    case "TRACE"   => Method.Trace
    case "CONNECT" => Method.Connect
    case other     => Method.Get
