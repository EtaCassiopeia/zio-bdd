package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json
import zio.http.{Body, Client, Header, MediaType, Request, URL, Method as HttpMethod}

/**
 * The Rift adapter: implements the portable [[MockControl]] core port by
 * driving Rift's Mountebank-compatible admin API over zio-http.
 *
 * PerInstance isolation: each [[MockSpace]] is one imposter on its own port and
 * `inject == identity`. `destroy` deletes exactly that imposter (`DELETE
 * /imposters/:port`) — never the global `DELETE /imposters` reset, so tearing
 * down one space never touches another (#113 AC2).
 *
 * Rule identity: Mountebank addresses stubs by positional index, which shifts
 * as stubs are inserted/removed. The adapter therefore hands out stable opaque
 * [[RuleId]]s and tracks their current order per imposter, translating an id to
 * its live index on removal — so a [[RuleId]] never silently points at the
 * wrong stub after an overlay insert.
 *
 * Capabilities are intentionally empty here: the optional capability
 * *interfaces* (faults, scenarios, …) are fleshed out in M3, so this adapter
 * advertises none and every accessor fails fast with [[Unsupported]].
 */
private[rift] final case class RiftMockControl(
  endpoint: RiftEndpoint,
  client: Client,
  provisioning: Provisioning,
  spaces: Ref[Map[SpaceId, RiftMockControl.Imposter]],
  ids: Ref[Int]
) extends MockControl:

  def backendName: String           = "rift"
  def capabilities: Set[Capability] = Set.empty

  def provision(source: MockSource): IO[MockError, List[MockSpace]] =
    for
      sources <- provisioning.normalize(source)
      created <- Ref.make(List.empty[MockSpace])
      // Provision atomically: tear down any spaces already stood up if a later
      // space in the same source fails, so a partial batch never orphans
      // imposters/ports the caller can't reach.
      spaces <- ZIO
                  .foreach(sources)(src => serveSpace(src).tap(s => created.update(s :: _)))
                  .onError(_ => created.get.flatMap(ZIO.foreachDiscard(_)(s => destroy(s).ignore)))
    yield spaces

  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
    spec match
      // A native Rift imposter goes through the same serveSpace machinery as a
      // portable Raw source (pool port, `_rift`/stub passthrough, tracking), so
      // the resulting space participates in destroy/received/overlays/isolation
      // exactly like a portable one.
      case NativeSpec.Rift(imposterJson) =>
        serveSpace(NormalizedSource("native", SourcePayload.Raw(imposterJson), None)).map(List(_))
      case NativeSpec.WireMock(_) =>
        ZIO.fail(MockError.InvalidDefinition("the Rift adapter cannot provision a WireMock native spec"))

  def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    withImposter(space) { imp =>
      for
        ruleId <- freshId
        // First match wins in Mountebank, so an overlay rule goes on top
        // (index 0) and a base rule is appended after the existing stubs.
        index <- priority match
                   case Priority.Overlay => ZIO.succeed(0)
                   case Priority.Base    => imp.stubs.get.map(_.size)
        u   <- url(s"/imposters/${imp.port}/stubs")
        res <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.addStubBody(index, rule)))
        _   <- expectSuccess(s"add rule to imposter ${imp.port}", res)
        _   <- imp.stubs.update(v => if priority == Priority.Overlay then ruleId +: v else v :+ ruleId)
      yield ruleId
    }

  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
    withImposter(space) { imp =>
      imp.stubs.get.flatMap { ordered =>
        ordered.indexOf(id) match
          case -1 => ZIO.fail(MockError.RuleNotFound(space.id, id))
          case idx =>
            for
              u   <- url(s"/imposters/${imp.port}/stubs/$idx")
              res <- send(Request(method = HttpMethod.DELETE, url = u))
              _   <- expectSuccess(s"remove rule from imposter ${imp.port}", res)
              _   <- imp.stubs.update(_.patch(idx, Nil, 1))
            yield ()
      }
    }

  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
    withImposter(space) { imp =>
      for
        ruleIds <- freshIds(rules.size)
        u       <- url(s"/imposters/${imp.port}/stubs")
        res     <- send(jsonRequest(HttpMethod.PUT, u, RiftProtocol.replaceStubsBody(rules)))
        _       <- expectSuccess(s"replace rules on imposter ${imp.port}", res)
        _       <- imp.stubs.set(ruleIds)
      yield ()
    }

  def destroy(space: MockSpace): IO[MockError, Unit] =
    withImposter(space) { imp =>
      for
        u   <- url(s"/imposters/${imp.port}")
        res <- send(Request(method = HttpMethod.DELETE, url = u))
        // A 404 means the imposter is already gone — destroy is idempotent.
        _ <- ZIO.unless(res._1 == 404)(expectSuccess(s"destroy imposter ${imp.port}", res))
        _ <- endpoint.releasePort(imp.port)
        _ <- spaces.update(_ - space.id)
      yield ()
    }

  def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
    withImposter(space) { imp =>
      for
        u    <- url(s"/imposters/${imp.port}")
        res  <- send(Request(method = HttpMethod.GET, url = u))
        body <- expectSuccess(s"read imposter ${imp.port}", res)
        // A malformed view is a backend/protocol fault, not a bad user definition.
        recs <- ZIO.fromEither(RiftProtocol.parseRecorded(body)).mapError(MockError.CommunicationError(_))
      yield recs
    }

  def faults: IO[Unsupported, Faults]                   = unsupported(Capability.Faults)
  def scenarios: IO[Unsupported, StatefulScenarios]     = unsupported(Capability.StatefulScenarios)
  def stateInspection: IO[Unsupported, StateInspection] = unsupported(Capability.StateInspection)
  def scripting: IO[Unsupported, Scripting]             = unsupported(Capability.Scripting)
  def proxyRecord: IO[Unsupported, ProxyRecord]         = unsupported(Capability.ProxyRecord)
  def templating: IO[Unsupported, Templating]           = unsupported(Capability.Templating)

  // ---------------------------------------------------------------------------

  private def serveSpace(src: NormalizedSource): IO[MockError, MockSpace] =
    endpoint.acquirePort.flatMap { port =>
      val stand =
        for
          bodyAndCount <- imposterBody(port, src)
          (body, count) = bodyAndCount
          _            <- postImposter(port, body)
          ruleIds      <- freshIds(count)
          stubs        <- Ref.make(ruleIds)
          id            = SpaceId(s"${src.name}-$port")
          space         = MockSpace(endpoint.baseUriFor(port), identity, id)
          _            <- spaces.update(_.updated(id, RiftMockControl.Imposter(port, stubs)))
        yield space
      // Release the port back to the pool on *any* failure after acquiring it
      // (malformed source, POST rejected, …) so a bad provision can't leak slots.
      stand.onError(_ => endpoint.releasePort(port))
    }

  private def imposterBody(port: Int, src: NormalizedSource): IO[MockError, (Json, Int)] =
    src.payload match
      case SourcePayload.Rules(rules) =>
        ZIO.succeed((RiftProtocol.imposter(port, src.name, rules), rules.length))
      case SourcePayload.Raw(text) =>
        ZIO.fromEither(RiftProtocol.imposterFromRaw(port, src.name, text)).mapError(MockError.InvalidDefinition(_))

  private def postImposter(port: Int, body: Json): IO[MockError, Unit] =
    for
      u   <- url("/imposters")
      res <- send(jsonRequest(HttpMethod.POST, u, body))
      _   <- expectSuccess(s"create imposter on port $port", res)
    yield ()

  private def freshId: UIO[RuleId] = ids.updateAndGet(_ + 1).map(n => RuleId(s"r$n"))

  private def freshIds(n: Int): UIO[Vector[RuleId]] = ZIO.foreach(0 until n)(_ => freshId).map(_.toVector)

  private def withImposter[A](space: MockSpace)(f: RiftMockControl.Imposter => IO[MockError, A]): IO[MockError, A] =
    spaces.get.flatMap(_.get(space.id) match
      case Some(imp) => f(imp)
      case None      => ZIO.fail(MockError.SpaceNotFound(space.id))
    )

  private def unsupported[A](c: Capability): IO[Unsupported, A] = ZIO.fail(Unsupported(c, backendName))

  private def url(path: String): IO[MockError, URL] =
    ZIO
      .fromEither(URL.decode(endpoint.adminBase + path))
      .mapError(e => MockError.CommunicationError(s"invalid admin URL ${endpoint.adminBase}$path: ${e.getMessage}"))

  private def jsonRequest(method: HttpMethod, u: URL, body: Json): Request =
    Request(method = method, url = u, body = Body.fromString(body.toJson))
      .addHeader(Header.ContentType(MediaType.application.json))

  private def send(req: Request): IO[MockError, (Int, String)] =
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

private[rift] object RiftMockControl:
  final case class Imposter(port: Int, stubs: Ref[Vector[RuleId]])

  /**
   * Build an adapter against an endpoint, taking the zio-http [[Client]] and
   * [[Provisioning]] from the environment.
   */
  def make(endpoint: RiftEndpoint): URIO[Client & Provisioning, MockControl] =
    for
      client <- ZIO.service[Client]
      prov   <- ZIO.service[Provisioning]
      spaces <- Ref.make(Map.empty[SpaceId, Imposter])
      ids    <- Ref.make(0)
    yield RiftMockControl(endpoint, client, prov, spaces, ids)
