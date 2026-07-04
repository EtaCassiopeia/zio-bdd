package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.{MockError, ScenarioState}
import zio.http.{Body, Client, Header, MediaType, Request, URL, Method as HttpMethod}
import zio.json.*
import zio.json.ast.Json

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The scenario / flow-state control-plane calls over Rift's admin API, factored
 * out so both adapters that reach it — the container adapter
 * ([[RiftMockControl]] over the mapped admin port) and the embedded adapter
 * (over the v2 in-process admin plane, rift#343) — issue the byte-identical
 * calls with a single implementation.
 *
 * These are the endpoints the C-ABI could not reach before v2: pin the initial
 * state (`PUT /imposters/:port/scenarios/:name/state`) and read the current
 * state (`GET /imposters/:port/scenarios?flowId=…`). `flowId` addresses the
 * state slice a request resolves to — the imposter port (PerInstance) or the
 * correlation value (Correlated). The stub registration + `Ref` bookkeeping
 * stay in each adapter (they differ by transport and isolation); only this HTTP
 * glue and the [[RiftProtocol]] codec are shared.
 */
private[rift] object RiftScenarioAdmin:

  /**
   * Pin scenario `name` on `(port, flowId)` to `state` (`PUT
   * …/scenarios/:name/state`).
   */
  def putState(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    name: String,
    state: ScenarioState
  ): IO[MockError, Unit] =
    for
      u   <- url(adminBase, s"/imposters/$port/scenarios/${enc(name)}/state")
      body = Json.Obj("state" -> Json.Str(state.value), "flowId" -> Json.Str(flowId))
      res <- send(client, jsonRequest(HttpMethod.PUT, u, body))
      _   <- expectSuccess(s"set scenario '$name' state on imposter $port", res)
    yield ()

  /**
   * The current state of scenario `name` on `(port, flowId)`, or `None` if the
   * scenario isn't declared (`GET …/scenarios?flowId=…`). Callers map `None` to
   * their own space-scoped error.
   */
  def readState(
    client: Client,
    adminBase: String,
    port: Int,
    flowId: String,
    name: String
  ): IO[MockError, Option[String]] =
    for
      u    <- url(adminBase, s"/imposters/$port/scenarios?flowId=${enc(flowId)}")
      res  <- send(client, Request(method = HttpMethod.GET, url = u))
      body <- expectSuccess(s"read scenarios for imposter $port", res)
      st   <- ZIO.fromEither(RiftProtocol.parseScenarioState(body, name)).mapError(MockError.CommunicationError(_))
    yield st

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
