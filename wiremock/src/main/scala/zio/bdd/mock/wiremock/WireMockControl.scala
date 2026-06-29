package zio.bdd.mock.wiremock

import zio.*
import zio.bdd.mock.*

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.options
import com.github.tomakehurst.wiremock.stubbing.StubMapping

import scala.jdk.CollectionConverters.*

/**
 * The WireMock adapter: implements the portable [[MockControl]] core port over
 * an embedded WireMockServer — pure-JVM, zero-Docker.
 *
 * Correlated isolation (the default): every space shares one server; each of a
 * space's stubs carries an `X-Mock-Space: <spaceId>` header matcher and
 * `inject` stamps that header on the SUT's requests, so a request can only
 * match (and be `received` by) its own space. `destroy` removes exactly that
 * space's stubs — never `server.resetAll`. A request without the header matches
 * nothing and is recorded by no space.
 *
 * PerInstance isolation (option): each space owns a fresh server on its own
 * port, `inject == identity`, and `destroy` stops that server.
 */
private[wiremock] final case class WireMockControl(
  mode: WireMock.Mode,
  shared: Option[WireMockServer],
  provisioning: Provisioning,
  spaces: Ref[Map[SpaceId, WireMockControl.SpaceState]],
  ids: Ref[Int]
) extends MockControl:

  import WireMockControl.SpaceState

  def backendName: String           = "wiremock"
  def capabilities: Set[Capability] = Set.empty

  def provision(source: MockSource): IO[MockError, List[MockSpace]] =
    for
      sources <- provisioning.normalize(source)
      created <- Ref.make(List.empty[MockSpace])
      // Tear down any spaces already stood up if a later one fails, so a partial
      // batch never orphans a started server or leaves half a source registered.
      spaces <-
        ZIO
          .foreach(sources)(src => serveSpace(src).tap(s => created.update(s :: _)))
          .onError(_ =>
            created.get.flatMap(
              ZIO.foreachDiscard(_)(s =>
                destroy(s).catchAllCause(c => ZIO.logWarningCause("WireMock provision rollback: destroy failed", c))
              )
            )
          )
    yield spaces

  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
    spec match
      // A native WireMock stub defines its own matching, so it always gets its
      // own server (PerInstance) — correlation can't be grafted onto it.
      case NativeSpec.WireMock(stubMappingJson) =>
        for
          id       <- freshSpaceId("native")
          stub     <- ZIO.attempt(StubMapping.buildFrom(stubMappingJson)).mapError(e => MockError.InvalidDefinition(msg(e)))
          server   <- WireMockControl.startServer.mapError(e => MockError.ProvisionFailed(msg(e)))
          rulesRef <- Ref.make(Vector.empty[(RuleId, StubMapping)])
          ruleId   <- freshRuleId
          // Stop the server we just started if registration/tracking fails or is
          // interrupted before it lands in `spaces` — otherwise it leaks.
          space <- (ZIO
                     .attempt(server.addStubMapping(stub))
                     .zipRight(rulesRef.set(Vector(ruleId -> stub)))
                     .mapError(e => MockError.CommunicationError(msg(e)))
                     .zipRight(spaces.update(_.updated(id, SpaceState(server, ownsServer = true, None, rulesRef))))
                     .as(MockSpace(server.baseUrl(), identity, id)))
                     .onError(_ => stop(server))
        yield List(space)
      case NativeSpec.Rift(_) =>
        ZIO.fail(MockError.InvalidDefinition("the WireMock adapter cannot provision a Rift native spec"))

  def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    withSpace(space)(st => registerRule(st, space.id, rule, priority))

  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
    withSpace(space) { st =>
      st.rules.get.flatMap(_.find(_._1 == id) match
        case None => ZIO.fail(MockError.RuleNotFound(space.id, id))
        case Some((_, stub)) =>
          ZIO
            .attempt(st.server.removeStubMapping(stub))
            .mapError(e => MockError.CommunicationError(msg(e)))
            .zipRight(st.rules.update(_.filterNot(_._1 == id)))
      )
    }

  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
    withSpace(space) { st =>
      for
        current <- st.rules.getAndSet(Vector.empty)
        _       <- removeStubs(st.server, current.map(_._2))
        _       <- ZIO.foreachDiscard(rules)(r => registerRule(st, space.id, r, Priority.Base))
      yield ()
    }

  def destroy(space: MockSpace): IO[MockError, Unit] =
    withSpace(space) { st =>
      for
        current <- st.rules.get
        _       <- removeStubs(st.server, current.map(_._2))
        _       <- ZIO.when(st.ownsServer)(stop(st.server))
        _       <- spaces.update(_ - space.id)
      yield ()
    }

  def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
    withSpace(space) { st =>
      // Only the backend call is wrapped — a defect in the pure translation below
      // must stay a defect, not get relabelled as a communication error.
      ZIO
        .attempt(st.server.getAllServeEvents.asScala.toList)
        .mapError(e => MockError.CommunicationError(msg(e)))
        .map(_.reverse.flatMap { ev =>
          val req = ev.getRequest
          val keep = st.correlation match
            case Some(c) => Option(req.getHeader(c.header)).exists(h => c.matcher(space.id).`match`(h).isExactMatch)
            case None    => true
          Option.when(keep)(
            WireMockTranslation.recorded(
              req.getMethod.getName,
              req.getUrl,
              req.getHeaders.all.asScala.map(h => h.key.toLowerCase -> h.firstValue).toMap,
              Option(req.getBodyAsString).filter(_.nonEmpty)
            )
          )
        })
    }

  def faults: IO[Unsupported, Faults]                   = unsupported(Capability.Faults)
  def scenarios: IO[Unsupported, StatefulScenarios]     = unsupported(Capability.StatefulScenarios)
  def stateInspection: IO[Unsupported, StateInspection] = unsupported(Capability.StateInspection)
  def scripting: IO[Unsupported, Scripting]             = unsupported(Capability.Scripting)
  def proxyRecord: IO[Unsupported, ProxyRecord]         = unsupported(Capability.ProxyRecord)
  def templating: IO[Unsupported, Templating]           = unsupported(Capability.Templating)

  // Stop every server this adapter still owns — the layer's scope finalizer.
  private[wiremock] def stopAll: UIO[Unit] =
    for
      snap <- spaces.get
      _    <- ZIO.foreachDiscard(snap.values.filter(_.ownsServer))(st => stop(st.server))
      _    <- ZIO.foreachDiscard(shared.toList)(stop)
    yield ()

  // ---------------------------------------------------------------------------

  private def serveSpace(src: NormalizedSource): IO[MockError, MockSpace] =
    for
      rules    <- rulesOf(src)
      id       <- freshSpaceId(src.name)
      rulesRef <- Ref.make(Vector.empty[(RuleId, StubMapping)])
      corr      = correlationOf
      server   <- serverFor(id)
      st        = SpaceState(server, ownsServer = corr.isEmpty, corr, rulesRef)
      // If a rule fails mid-batch the space is never tracked, so destroy can
      // never reach it: undo its stubs (and stop its own server) here, or they
      // orphan on the shared server.
      _ <- ZIO
             .foreachDiscard(rules)(r => registerRule(st, id, r, Priority.Base))
             .onError(_ => cleanupSpace(st))
      _ <- spaces.update(_.updated(id, st))
    yield MockSpace(server.baseUrl(), injectFor(id, corr), id)

  // Best-effort teardown of a half-built (untracked) space.
  private def cleanupSpace(st: SpaceState): UIO[Unit] =
    for
      rs <- st.rules.get
      _  <- removeStubs(st.server, rs.map(_._2)).ignore
      _  <- ZIO.when(st.ownsServer)(stop(st.server))
    yield ()

  private def registerRule(st: SpaceState, id: SpaceId, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    for
      ruleId <- freshRuleId
      stub    = WireMockTranslation.stub(id, rule.copy(id = Some(ruleId)), priority, st.correlation)
      _ <- ZIO
             .attempt(st.server.addStubMapping(stub))
             .mapError(e => MockError.CommunicationError(msg(e)))
      _ <- st.rules.update(v => if priority == Priority.Overlay then (ruleId, stub) +: v else v :+ (ruleId, stub))
    yield ruleId

  private def removeStubs(server: WireMockServer, stubs: Vector[StubMapping]): IO[MockError, Unit] =
    ZIO
      .attempt(stubs.foreach(server.removeStubMapping))
      .mapError(e => MockError.CommunicationError(msg(e)))

  private def rulesOf(src: NormalizedSource): IO[MockError, List[MockRule]] =
    src.payload match
      case SourcePayload.Rules(rules) => ZIO.succeed(rules)
      case SourcePayload.Raw(_) =>
        ZIO.fail(
          MockError.InvalidDefinition(
            "a raw/native WireMock source must go through provisionNative(NativeSpec.WireMock)"
          )
        )

  private def correlationOf: Option[Correlation] = mode match
    case WireMock.Mode.Correlated(c) => Some(c)
    case WireMock.Mode.PerInstance   => None

  private def serverFor(id: SpaceId): IO[MockError, WireMockServer] = mode match
    case WireMock.Mode.Correlated(_) => ZIO.succeed(shared.get)
    case WireMock.Mode.PerInstance   => WireMockControl.startServer.mapError(e => MockError.ProvisionFailed(msg(e)))

  private def injectFor(id: SpaceId, corr: Option[Correlation]): HttpRequest => HttpRequest =
    corr match
      case Some(c) => req => req.copy(headers = req.headers + (c.header -> c.value(id)))
      case None    => identity

  private def stop(server: WireMockServer): UIO[Unit] =
    ZIO
      .attempt(server.stop())
      .catchAllCause(c => ZIO.logWarningCause("WireMock server stop failed", c))

  private def freshRuleId: UIO[RuleId]                 = ids.updateAndGet(_ + 1).map(n => RuleId(s"r$n"))
  private def freshSpaceId(name: String): UIO[SpaceId] = ids.updateAndGet(_ + 1).map(n => SpaceId(s"$name-$n"))

  private def withSpace[A](space: MockSpace)(f: SpaceState => IO[MockError, A]): IO[MockError, A] =
    spaces.get.flatMap(_.get(space.id) match
      case Some(st) => f(st)
      case None     => ZIO.fail(MockError.SpaceNotFound(space.id))
    )

  private def unsupported[A](c: Capability): IO[Unsupported, A] = ZIO.fail(Unsupported(c, backendName))

  private def msg(t: Throwable): String =
    def render(x: Throwable): String =
      Option(x.getMessage).filter(_.nonEmpty).fold(x.getClass.getSimpleName)(m => s"${x.getClass.getSimpleName}: $m")
    // Keep the root cause — WireMock/JVM faults wrap the actionable detail
    // (e.g. a BindException behind a generic startup error).
    val cause = Option(t.getCause).filter(_ ne t).fold("")(c => s" (cause: ${render(c)})")
    s"${render(t)}$cause"

private[wiremock] object WireMockControl:
  final case class SpaceState(
    server: WireMockServer,
    ownsServer: Boolean,
    correlation: Option[Correlation],
    rules: Ref[Vector[(RuleId, StubMapping)]]
  )

  /**
   * Build the adapter for `mode`, starting the shared server in Correlated mode
   * and registering a scope finalizer that stops every server it owns.
   */
  def make(mode: WireMock.Mode): ZIO[Provisioning & Scope, Throwable, MockControl] =
    for
      prov   <- ZIO.service[Provisioning]
      spaces <- Ref.make(Map.empty[SpaceId, SpaceState])
      ids    <- Ref.make(0)
      shared <- mode match
                  case WireMock.Mode.Correlated(_) => startServer.map(Some(_))
                  case WireMock.Mode.PerInstance   => ZIO.none
      control = WireMockControl(mode, shared, prov, spaces, ids)
      _      <- ZIO.addFinalizer(control.stopAll)
    yield control

  // Create and start an embedded server on an OS-assigned port.
  private def startServer: Task[WireMockServer] =
    ZIO.attempt {
      val server = new WireMockServer(options().dynamicPort())
      server.start()
      server
    }
