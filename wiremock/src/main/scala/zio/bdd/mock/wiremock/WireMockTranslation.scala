package zio.bdd.mock.wiremock

import zio.bdd.mock.*

import com.github.tomakehurst.wiremock.client.{MappingBuilder, ResponseDefinitionBuilder, WireMock as WM}
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
    val base = methodAndUrl(rule.`match`)
    rule.`match`.headers.foreach((k, v) => base.withHeader(k, valuePattern(v)))
    rule.`match`.query.foreach((k, v) => base.withQueryParam(k, valuePattern(v)))
    rule.`match`.body.foreach(b => base.withRequestBody(bodyPattern(b)))
    correlation.foreach(c => base.withHeader(c.header, c.matcher(id)))
    base.atPriority(priority match
      case Priority.Overlay => 1
      case Priority.Base    => 10
    )
    base.withId(UUID.randomUUID())
    base.willReturn(response(rule.respond)).build()

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
