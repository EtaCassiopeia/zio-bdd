package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.{Body, Client, Header, MediaType, Request, URL, Method as HttpMethod}
import zio.json.*
import zio.json.ast.Json

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The Correlated-space wire-format ops (rift#223) over Rift's admin API,
 * factored out so both adapters that reach it — the container adapter
 * ([[RiftMockControl]] over the mapped admin port) and the embedded adapter
 * (over the v2 in-process admin plane) — issue the byte-identical
 * `/imposters/:port/spaces/:flowId/...` calls with a single implementation.
 *
 * These are pure functions of `(client, adminBase, port, flowId, ...)` — no
 * adapter state — mirroring [[RiftScenarioAdmin]]'s shared-HTTP-primitives
 * pattern. Stub tracking (which rules/faults/extras are registered under a
 * space) stays in each adapter; only the wire calls are shared.
 */
private[rift] object RiftCorrelatedSpace:

  /** Add one portable rule's stub under `(port, flowId)`. */
  def postStub(client: Client, adminBase: String, port: Int, flowId: String, rule: MockRule): IO[MockError, Unit] =
    for
      u   <- url(adminBase, s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(client, jsonRequest(HttpMethod.POST, u, RiftProtocol.stub(rule)))
      _   <- expectSuccess(s"add space stub to $flowId", res)
    yield ()

  /**
   * Add one pre-built stub (a capability stub: script/proxy/template) under
   * `(port, flowId)`.
   */
  def postRawStub(client: Client, adminBase: String, port: Int, flowId: String, stub: Json): IO[MockError, Unit] =
    for
      u   <- url(adminBase, s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(client, jsonRequest(HttpMethod.POST, u, stub))
      _   <- expectSuccess(s"add capability stub to $flowId", res)
    yield ()

  /** Add one fault stub under `(port, flowId)`. */
  def postFaultStub(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    m: RequestMatch,
    fault: FaultKind,
    id: RuleId
  ): IO[MockError, Unit] =
    for
      u   <- url(adminBase, s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(client, jsonRequest(HttpMethod.POST, u, RiftProtocol.faultStub(m, fault, id)))
      _   <- expectSuccess(s"add fault stub to $flowId", res)
    yield ()

  /** Register the tracked portable rules under `(port, flowId)`, in order. */
  def registerStubs(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(rules)((rid, rule) => postStub(client, adminBase, port, flowId, rule.copy(id = Some(rid))))

  /** Register the tracked capability stubs under `(port, flowId)`, in order. */
  def registerRawStubs(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    extras: Vector[(RuleId, Json)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(extras)((_, stub) => postRawStub(client, adminBase, port, flowId, stub))

  /** Register the tracked fault stubs under `(port, flowId)`, in order. */
  def registerFaultStubs(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    faults: Vector[(RuleId, RequestMatch, FaultKind)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(faults)((rid, m, fault) => postFaultStub(client, adminBase, port, flowId, m, fault, rid))

  /**
   * Tear down the whole space (`DELETE /imposters/:port/spaces/:flowId`) — also
   * clears its recorded requests + flow state. A 404 means the space is already
   * gone, so teardown is idempotent.
   */
  def deleteSpace(client: Client, adminBase: String, port: Int, flowId: String): IO[MockError, Unit] =
    for
      u   <- url(adminBase, s"/imposters/$port/spaces/${enc(flowId)}")
      res <- send(client, Request(method = HttpMethod.DELETE, url = u))
      _   <- ZIO.unless(res._1 == 404)(expectSuccess(s"teardown space $flowId", res))
    yield ()

  /**
   * rift#223 has no per-stub-in-space delete: tear the whole space down and
   * re-register `extras` (capability stubs) → `faults` → `rules`, in that
   * first-match order (capability stubs and faults must win first-match over a
   * normal rule).
   */
  def rebuild(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    extras: Vector[(RuleId, Json)],
    faults: Vector[(RuleId, RequestMatch, FaultKind)],
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    deleteSpace(client, adminBase, port, flowId) *>
      registerRawStubs(client, adminBase, port, flowId, extras) *>
      registerFaultStubs(client, adminBase, port, flowId, faults) *>
      registerStubs(client, adminBase, port, flowId, rules)

  /**
   * The requests recorded under `flowId` on the shared imposter, filtered
   * server-side by the correlation `header` (`GET
   * /imposters/:port/requests?match=header:<header>=<flowId>`, rift#201).
   */
  def received(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    header: String
  ): IO[MockError, List[RecordedRequest]] =
    for
      u    <- url(adminBase, s"/imposters/$port/requests?match=${enc(s"header:$header=$flowId")}")
      res  <- send(client, Request(method = HttpMethod.GET, url = u))
      body <- expectSuccess(s"read filtered requests for space $flowId", res)
      recs <- ZIO.fromEither(RiftProtocol.parseRequestsArray(body)).mapError(MockError.CommunicationError(_))
    yield recs

  private def enc(s: String): String = URLEncoder.encode(s, UTF_8)

  private def url(adminBase: String, path: String): IO[MockError, URL] =
    ZIO
      .fromEither(URL.decode(adminBase + path))
      .mapError(e => MockError.CommunicationError(s"invalid admin URL $adminBase$path: ${e.getMessage}"))

  private def jsonRequest(method: HttpMethod, u: URL, body: Json): Request =
    Request(method = method, url = u, body = Body.fromString(body.toJson))
      .addHeader(Header.ContentType(MediaType.application.json))

  private def send(client: Client, req: Request): IO[MockError, (Int, String)] =
    Client
      .batched(req)
      .flatMap(resp => resp.body.asString.map(body => (resp.status.code, body)))
      .provideEnvironment(ZEnvironment(client))
      .mapError { t =>
        val msg = Option(t.getMessage).filter(_.nonEmpty).fold("")(m => s": $m")
        MockError.CommunicationError(s"${t.getClass.getSimpleName}$msg")
      }

  private def expectSuccess(action: String, res: (Int, String)): IO[MockError, String] =
    val (code, body) = res
    if code >= 200 && code < 300 then ZIO.succeed(body)
    else ZIO.fail(MockError.CommunicationError(s"$action: Rift returned HTTP $code: ${body.take(1000)}"))
