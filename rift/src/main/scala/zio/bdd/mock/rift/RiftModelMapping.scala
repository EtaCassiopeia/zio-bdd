package zio.bdd.mock.rift

import scala.concurrent.duration.DurationLong

import zio.bdd.mock as spi

import rift.RiftError
import rift.dsl.*
import rift.json.Json
import rift.model.{
  Method as RiftMethod,
  Port,
  RecordedRequest,
  ScenarioRef,
  ScriptEngine as RiftScriptEngine,
  ScriptSource,
  Stub,
  StubId,
  TcpFaultKind
}
import rift.bridge.ImposterDefinition

/**
 * Pure translation between zio-bdd's canonical mock model and rift-scala's
 * typed dsl/model — the adapter's whole wire knowledge, unit-testable without
 * an engine (#285).
 *
 * The emitted shapes mirror the pre-#285 hand-rolled adapter (`RiftProtocol`,
 * deleted) so a suite moving across the rebase observes identical engine
 * behavior: separate ANDed predicates per constraint, a 404 unmatched default,
 * `_rift.flowState` always attached (stubs added after creation would otherwise
 * hit a NoOp flow store and scenario state would never advance),
 * faults/capabilities as first-match stubs, and the `proxyOnce` + method/path
 * predicate-generator recording contract.
 */
private[rift] object RiftModelMapping:

  // ── portable match -> rift.dsl predicates ────────────────────────────────────────────────────

  def toMatch(m: spi.RequestMatch): StubBuilder[StubPhase.Matching] =
    val withMethod =
      m.method.fold(onRequest)(meth => onRequest.where(method.is(methodName(meth))))
    val withPath = m.path match
      case spi.PathMatch.Any         => withMethod
      case spi.PathMatch.Exact(p)    => withMethod.where(path.is(p))
      case spi.PathMatch.Regex(r)    => withMethod.where(path.matches(r))
      case spi.PathMatch.Template(t) => withMethod.where(path.matches(templateToRegex(t)))
    // Map iteration order is unspecified; sort so the emitted predicate order is deterministic.
    val withQuery = m.query.toVector.sortBy(_._1).foldLeft(withPath) { case (b, (k, vm)) =>
      b.where(valueClause(query(k), vm))
    }
    val withHeaders = m.headers.toVector.sortBy(_._1).foldLeft(withQuery) { case (b, (k, vm)) =>
      b.where(valueClause(header(k), vm))
    }
    m.body.fold(withHeaders)(bm => withHeaders.where(bodyClause(bm)))

  private def valueClause(sel: FieldSelector, vm: spi.ValueMatch): PredicateBuilder = vm match
    case spi.ValueMatch.Equals(v)   => sel.is(v)
    case spi.ValueMatch.Contains(v) => sel.contains(v)
    case spi.ValueMatch.Matches(r)  => sel.matches(r)

  private def bodyClause(bm: spi.BodyMatch): PredicateBuilder = bm match
    case spi.BodyMatch.Equals(v)            => body.is(v)
    case spi.BodyMatch.Contains(v)          => body.contains(v)
    case spi.BodyMatch.Matches(r)           => body.matches(r)
    case spi.BodyMatch.JsonPath(p, Some(e)) => body.jsonPath(p).is(e)
    case spi.BodyMatch.JsonPath(p, None)    => body.jsonPath(p).exists
    case spi.BodyMatch.XPath(p, Some(e))    => body.xpath(p).is(e)
    case spi.BodyMatch.XPath(p, None)       => body.xpath(p).exists

  /**
   * "/users/{id}" -> anchored "^/users/[^/]+$" — the engine has no template
   * matcher.
   */
  def templateToRegex(t: String): String =
    "^" + t.replaceAll("\\{[^/}]+\\}", "[^/]+") + "$"

  private def methodName(m: spi.Method): String = m.toString.toUpperCase

  // ── portable response -> rift.dsl is-response ────────────────────────────────────────────────

  def toResponse(r: spi.ResponseDef): Either[spi.MockError, IsResponseBuilder] =
    bodyOf(r.body, status(r.status)).map { withBody =>
      val withHeaders = r.headers.entries.sortBy(_._1).foldLeft(withBody) { case (b, (k, vs)) =>
        vs.foldLeft(b)((acc, v) => acc.header(k, v))
      }
      r.delay.fold(withHeaders)(d => withHeaders.after(d.toMillis.millis))
    }

  private def bodyOf(b: spi.Body, base: IsResponseBuilder): Either[spi.MockError, IsResponseBuilder] =
    b match
      case spi.Body.Empty   => Right(base)
      case spi.Body.Text(v) => Right(base.text(v))
      // A string body, NOT `.json(v)`: parsing and re-serializing could reorder keys, and the
      // portable contract serves the authored bytes verbatim.
      case spi.Body.Json(v) => Right(base.text(v))
      case spi.Body.Base64(v) =>
        try Right(base.binary(java.util.Base64.getDecoder.decode(v)))
        catch
          case e: IllegalArgumentException =>
            Left(spi.MockError.InvalidDefinition(s"invalid base64 body: ${e.getMessage}"))

  // ── rules and capability stubs ────────────────────────────────────────────────────────────────

  private def stubIdOf(id: spi.RuleId): StubId = StubId(id.value)

  def stubFor(rule: spi.MockRule, id: spi.RuleId): Either[spi.MockError, StubBuilder[StubPhase.Complete]] =
    toResponse(rule.respond).map(resp => toMatch(rule.`match`).withId(stubIdOf(id)).reply(resp))

  def faultStub(
    m: spi.RequestMatch,
    fault: spi.FaultKind,
    id: spi.RuleId
  ): Either[spi.MockError, StubBuilder[StubPhase.Complete]] =
    Right(toMatch(m).withId(stubIdOf(id)).reply(faultResponse(fault)))

  private def faultResponse(fault: spi.FaultKind): IsResponseBuilder = fault match
    case spi.FaultKind.LatencySpike(d) => ok.withLatencyFault(1.0, d.toMillis.millis)
    case spi.FaultKind.ConnectionReset => ok.withTcpFault(TcpFaultKind.ConnectionResetByPeer)
    case spi.FaultKind.EmptyResponse   => ok.withTcpFault(TcpFaultKind.EmptyResponse)
    case spi.FaultKind.MalformedChunk  => ok.withTcpFault(TcpFaultKind.MalformedResponseChunk)
    case spi.FaultKind.RandomThenClose => ok.withTcpFault(TcpFaultKind.RandomDataThenClose)

  def scriptStub(
    m: spi.RequestMatch,
    s: spi.Script,
    id: spi.RuleId
  ): Either[spi.MockError, StubBuilder[StubPhase.Complete]] =
    Right(toMatch(m).withId(stubIdOf(id)).reply(script(ScriptSource.Inline(engineOf(s.engine), s.code))))

  private def engineOf(e: spi.ScriptEngine): RiftScriptEngine = e match
    case spi.ScriptEngine.Rhai       => RiftScriptEngine.Rhai
    case spi.ScriptEngine.JavaScript => RiftScriptEngine.JavaScript

  def proxyStub(
    m: spi.RequestMatch,
    upstream: String,
    id: spi.RuleId
  ): Either[spi.MockError, StubBuilder[StubPhase.Complete]] =
    Right(
      toMatch(m)
        .withId(stubIdOf(id))
        .reply(
          proxyTo(upstream).proxyOnce.generateBy(rift.model.RequestField.Method, rift.model.RequestField.Path)
        )
    )

  def templateStub(
    m: spi.RequestMatch,
    template: spi.ResponseTemplate,
    id: spi.RuleId
  ): Either[spi.MockError, StubBuilder[StubPhase.Complete]] =
    val base = status(template.status).text(template.body)
    val withCopies = template.captures.foldLeft(base) { (b, c) =>
      b.copy(from = sourceSelector(c.source), into = c.token, extractWith = CopyUsing.Regex(c.regex))
    }
    Right(toMatch(m).withId(stubIdOf(id)).reply(withCopies))

  private def sourceSelector(s: spi.TemplateSource): FieldSelector = s match
    case spi.TemplateSource.Path => path
    case spi.TemplateSource.Body => body

  // ── scenarios ─────────────────────────────────────────────────────────────────────────────────

  /**
   * One stub per FSM edge, carrying the engine's scenario wire triplet. Built
   * as raw model stubs (not through `rift.dsl.scenario`) because the ids that
   * make each edge individually removable and the SPI-declared initial state
   * both live outside that builder's surface.
   */
  def scenarioStubs(sc: spi.ScenarioDef, ids: Vector[spi.RuleId]): Either[spi.MockError, Vector[Stub]] =
    sc.rules.toVector.zip(ids).foldLeft(Right(Vector.empty): Either[spi.MockError, Vector[Stub]]) {
      case (acc, (rule, id)) =>
        for
          built <- acc
          resp  <- toResponse(rule.respond)
        yield built :+ Stub(
          predicates = toMatch(rule.request).predicates,
          responses = Vector(resp.build),
          id = Some(stubIdOf(id)),
          scenario = Some(ScenarioRef(sc.name, Some(rule.whenState.value), rule.thenState.map(_.value)))
        )
    }

  // ── imposter shells + raw sources ────────────────────────────────────────────────────────────

  /**
   * The imposter every provisioned space lives on: named, recording, 404 for
   * unmatched requests (the cross-adapter contract; the engine's own default is
   * 200-empty), flow state always attached, and the port either
   * authored/pool-drawn verbatim (`Some`) or engine-assigned (`None`) — see
   * `RiftPortPool` for which transports need a pool-drawn port instead of an
   * engine-assigned one.
   */
  def imposterShell(name: String, port: Option[Int], correlationHeader: Option[String]): ImposterBuilder =
    val base0 = imposter(name).record.defaultResponse(notFound)
    val based = port.fold(base0)(p => base0.port(p))
    val flow = correlationHeader.fold(inMemoryFlowState.ttl(300.seconds))(h =>
      inMemoryFlowState.ttl(300.seconds).flowIdFromHeader(h)
    )
    based.flowState(flow)

  /**
   * Parse a raw imposter document, defaulting recording ON unless the document
   * explicitly opts out, and either honouring its own `port`
   * (`provisionNative`) or stripping it so the engine/pool assigns one
   * (portable `provision`, where every space must get a fresh port).
   */
  def fromRaw(raw: String, honourDocPort: Boolean): Either[spi.MockError, ImposterDefinition] =
    (for
      json       <- Json.parse(raw).left.map(_.toString)
      definition <- ImposterDefinition.fromJson(json).left.map(_.toString)
    yield
      val optedOut = json.get("recordRequests").contains(Json.Bool(false))
      definition.copy(
        port = if honourDocPort then definition.port else None,
        recordRequests = definition.recordRequests || !optedOut
      )
    ).left.map(spi.MockError.InvalidDefinition(_))

  // ── readback + errors ────────────────────────────────────────────────────────────────────────

  /**
   * `Throwable#getMessage` is `null` for
   * `NullPointerException`/`MatchError`/many JDK exceptions — assigning it raw
   * into a `MockError` string `reason` produces a literal-null reason that NPEs
   * any caller logging or matching on it, far from the actual cause. Falls back
   * to the exception's simple class name, which is never null and at least
   * names what broke (the deleted pre-#285 adapter's own `message(t)` helper,
   * reintroduced for #285).
   */
  def message(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)

  def toRecorded(r: RecordedRequest): spi.RecordedRequest =
    spi.RecordedRequest(
      method = methodOf(r.method),
      uri = r.path,
      headers = r.headers.entries.foldLeft(spi.Headers.empty) { case (h, (k, v)) => h.add(k, v) },
      body = r.bodyText.orElse(r.body.map(_.render))
    )

  /**
   * Structural, not `toString`-based: rift wraps any non-standard verb in
   * `Method.Custom`, whose rendering (`Custom(TRACE)`) would never match —
   * TRACE/CONNECT must resolve to their real SPI cases. Only a verb the SPI
   * genuinely cannot represent (e.g. WebDAV PROPFIND) falls back to `Get`.
   */
  private def methodOf(m: RiftMethod): spi.Method = m match
    case RiftMethod.GET     => spi.Method.Get
    case RiftMethod.POST    => spi.Method.Post
    case RiftMethod.PUT     => spi.Method.Put
    case RiftMethod.DELETE  => spi.Method.Delete
    case RiftMethod.PATCH   => spi.Method.Patch
    case RiftMethod.HEAD    => spi.Method.Head
    case RiftMethod.OPTIONS => spi.Method.Options
    case RiftMethod.Custom(name) =>
      spi.Method.values.find(_.toString.equalsIgnoreCase(name)).getOrElse(spi.Method.Get)

  /**
   * `Some(rawName)` iff `methodOf` had to fall back to `Get` for a verb the SPI
   * has no portable case for (e.g. WebDAV `PROPFIND`/`MKCOL`) — `None` when the
   * mapping was exact. The SPI `Method` enum must stay a closed, portable
   * contract, so `methodOf` itself stays total and silent; callers that care
   * whether the coercion fired (e.g. `received`, before logging a diagnostic)
   * use this instead of silently trusting the coerced value (#285/B13).
   */
  def methodFallback(m: RiftMethod): Option[String] = m match
    case RiftMethod.Custom(name) if !spi.Method.values.exists(_.toString.equalsIgnoreCase(name)) => Some(name)
    case _                                                                                       => None

  /**
   * Total `RiftError -> MockError`. `ImposterNotFound` becomes `SpaceNotFound`
   * only when the failing call was addressed to a known space; without that
   * context it stays a communication failure rather than inventing a space id.
   */
  def toMockError(space: Option[spi.SpaceId])(e: RiftError): spi.MockError = e match
    case RiftError.InvalidDefinition(msg, _)  => spi.MockError.InvalidDefinition(msg)
    case RiftError.EngineUnavailable(msg, _)  => spi.MockError.ProvisionFailed(msg)
    case RiftError.CommunicationError(msg, _) => spi.MockError.CommunicationError(msg)
    case RiftError.ImposterNotFound(port) =>
      space match
        case Some(id) => spi.MockError.SpaceNotFound(id)
        case None     => spi.MockError.CommunicationError(s"imposter on port ${Port.value(port)} not found")
    case RiftError.EngineError(code, msg)     => spi.MockError.CommunicationError(s"engine error $code: $msg")
    case RiftError.VerificationFailed(report) => spi.MockError.CommunicationError(report.render)
    case RiftError.DecodeFailed(msg, _)       => spi.MockError.InvalidDefinition(msg)
