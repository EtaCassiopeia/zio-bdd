package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json
import zio.http.{Body, Client, Header, MediaType, Request, URL, Method as HttpMethod}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The Rift adapter: implements the portable [[MockControl]] core port by
 * driving Rift's Mountebank-compatible admin API over zio-http.
 *
 * Two isolation modes ([[RiftMode]]):
 *
 *   - PerInstance (default): each [[MockSpace]] is one imposter on its own
 *     port, `inject == identity`. `destroy` deletes exactly that imposter —
 *     never the global reset, so tearing down one space never touches another
 *     (#113 AC2).
 *
 *   - Correlated (#156): all spaces share ONE imposter whose `flowIdSource` is
 *     `header:<correlation>`; each space's stubs are registered under its
 *     flowId (`POST /imposters/:port/spaces/:flowId/stubs`, rift#223), `inject`
 *     stamps the correlation header, `received` filters by it (rift#201), and
 *     `destroy` tears down exactly that space (`DELETE
 *     /imposters/:port/spaces/:flowId`). Trades a port-per-space for one shared
 *     imposter — for heavy parallelism.
 *
 * A native imposter ([[provisionNative]]) always gets its own port regardless
 * of mode (full-fidelity), so the two space kinds coexist; per-space operations
 * dispatch by which space a [[MockSpace]] belongs to, not by the active mode.
 *
 * rift#223 exposes only whole-space teardown (no per-stub-in-space delete), so
 * a Correlated `removeRule`/`replaceRules` (and an `Overlay` `addRule`, which
 * must be first-match) rebuilds the space: it re-registers the tracked stubs
 * after a space teardown — which also clears that space's recorded requests +
 * flow state. Benign because mutations happen at scenario boundaries (overlay
 * release after `received` is read; `replaceRules` at setup before any
 * requests).
 */
private[rift] final case class RiftMockControl(
  endpoint: RiftEndpoint,
  client: Client,
  provisioning: Provisioning,
  mode: RiftMode,
  spaces: Ref[Map[SpaceId, RiftMockControl.Imposter]],
  correlated: Ref[Map[SpaceId, RiftMockControl.CorrSpace]],
  sharedPort: Ref.Synchronized[Option[Int]],
  ids: Ref[Int]
) extends MockControl:

  import RiftMockControl.{CorrSpace, Imposter}

  def backendName: String = "rift"
  def capabilities: Set[Capability] =
    Set(
      Capability.Faults,
      Capability.StatefulScenarios,
      Capability.StateInspection,
      Capability.Scripting,
      Capability.ProxyRecord,
      Capability.Templating
    )

  override def isolation: Isolation = mode match
    case RiftMode.Correlated(_) => Isolation.Correlated
    case RiftMode.PerInstance   => Isolation.PerInstance

  def provision(source: MockSource): IO[MockError, List[MockSpace]] =
    for
      sources <- provisioning.normalize(source)
      created <- Ref.make(List.empty[MockSpace])
      // Provision atomically: if a later source fails, best-effort tear down the
      // spaces already stood up. The original failure propagates; a cleanup
      // failure is logged (not surfaced) rather than masking it.
      spaces <- ZIO
                  .foreach(sources)(src => serveSpace(src).tap(s => created.update(s :: _)))
                  .onError(_ =>
                    created.get.flatMap(
                      ZIO.foreachDiscard(_)(s =>
                        destroy(s).catchAllCause(c => ZIO.logWarningCause(s"rollback: destroy ${s.id} failed", c))
                      )
                    )
                  )
    yield spaces

  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
    spec match
      // A native imposter always gets its own port (PerInstance), full-fidelity,
      // regardless of the active isolation mode.
      case NativeSpec.Rift(imposterJson) =>
        serveImposter(NormalizedSource("native", SourcePayload.Raw(imposterJson), None)).map(List(_))
      case NativeSpec.WireMock(_) =>
        ZIO.fail(MockError.InvalidDefinition("the Rift adapter cannot provision a WireMock native spec"))

  def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    dispatch(space)(cs => corrAddRule(cs, rule, priority))(imp => piAddRule(imp, rule, priority))

  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
    dispatch(space)(cs => corrRemoveRule(cs, space.id, id))(imp => piRemoveRule(imp, space.id, id))

  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
    dispatch(space)(cs => corrReplaceRules(cs, rules))(imp => piReplaceRules(imp, rules))

  def destroy(space: MockSpace): IO[MockError, Unit] =
    dispatch(space)(cs => corrDestroy(space.id, cs))(imp => piDestroy(space, imp))

  def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
    dispatch(space)(corrReceived)(piReceived)

  def faults: IO[Unsupported, Faults]                   = ZIO.succeed(riftFaults)
  def scenarios: IO[Unsupported, StatefulScenarios]     = ZIO.succeed(statefulScenarios)
  def stateInspection: IO[Unsupported, StateInspection] = ZIO.succeed(stateInspector)
  def scripting: IO[Unsupported, Scripting]             = ZIO.succeed(riftScripting)
  def proxyRecord: IO[Unsupported, ProxyRecord]         = ZIO.succeed(riftProxyRecord)
  def templating: IO[Unsupported, Templating]           = ZIO.succeed(riftTemplating)

  // ===== Faults (#128) — `_rift.fault` (rift#239) =================================

  private val riftFaults: Faults = new Faults:
    def inject(space: MockSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
      dispatch(space)(cs => corrInjectFault(cs, m, fault))(imp => piInjectFault(imp, m, fault))

  // PerInstance: first-match (index 0) so the fault wins over any normal rule on
  // the same match; tracked in `imp.stubs` so it stays index-aligned with the
  // server and is removable via `removeRule`.
  private def piInjectFault(imp: Imposter, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      u      <- url(s"/imposters/${imp.port}/stubs")
      res    <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.addFaultStubBody(0, m, fault, ruleId)))
      _      <- expectSuccess(s"inject fault into imposter ${imp.port}", res)
      _      <- imp.stubs.update(ruleId +: _)
    yield ruleId

  // Correlated: track the fault and rebuild the space with faults registered ahead
  // of the rules, so it wins first-match (rift#223 has no per-stub index). Tracked
  // in `cs.faults` so it survives a later rule rebuild and is removable via
  // `removeRule`; `destroy` clears it with the whole flow. Server-first, commit on
  // success (mirrors corrAddRule's Overlay rebuild).
  private def corrInjectFault(cs: CorrSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      rules  <- cs.rules.get
      faults <- cs.faults.get
      extras <- cs.extras.get
      next    = (ruleId, m, fault) +: faults
      _      <- rebuild(cs, extras, next, rules)
      _      <- cs.faults.set(next)
    yield ruleId

  // ===== Optional capabilities (#132): scripting / proxy-record / templating ======
  // Each installs a special-response stub (`_rift.script`, a Mountebank `proxy`, or
  // an `is` + `_behaviors.copy`) for matching requests. Like faults, the stub is
  // first-match (PerInstance: index 0, tracked in `imp.stubs`; Correlated: tracked in
  // `cs.extras`, re-registered ahead of rules on rebuild) so it wins over a normal
  // rule, is removable via `removeRule`, and survives a later space rebuild.

  private val riftScripting: Scripting = new Scripting:
    def inject(space: MockSpace, m: RequestMatch, script: Script): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.scriptStub(m, script, rid))

  private val riftProxyRecord: ProxyRecord = new ProxyRecord:
    def proxy(space: MockSpace, m: RequestMatch, upstream: String): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.proxyStub(m, upstream, rid))

  private val riftTemplating: Templating = new Templating:
    def inject(space: MockSpace, m: RequestMatch, template: ResponseTemplate): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.templateStub(m, template, rid))

  private def injectStub(space: MockSpace, buildStub: RuleId => Json): IO[MockError, RuleId] =
    freshId.flatMap(rid =>
      dispatch(space)(cs => corrInjectStub(cs, rid, buildStub(rid)))(imp => piInjectStub(imp, rid, buildStub(rid)))
    )

  private def piInjectStub(imp: Imposter, ruleId: RuleId, stub: Json): IO[MockError, RuleId] =
    for
      u   <- url(s"/imposters/${imp.port}/stubs")
      res <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.addStubBodyOf(0, stub)))
      _   <- expectSuccess(s"inject capability stub into imposter ${imp.port}", res)
      _   <- imp.stubs.update(ruleId +: _)
    yield ruleId

  private def corrInjectStub(cs: CorrSpace, ruleId: RuleId, stub: Json): IO[MockError, RuleId] =
    for
      rules  <- cs.rules.get
      faults <- cs.faults.get
      extras <- cs.extras.get
      next    = (ruleId, stub) +: extras
      _      <- rebuild(cs, next, faults, rules)
      _      <- cs.extras.set(next)
    yield ruleId

  // ===== StatefulScenarios + StateInspection (#131) ===============================
  // Rift partitions scenario state by (flow_id, scenarioName), so locality is native
  // (no name-namespacing). flow_id = the imposter port (PerInstance) / the X-Mock-Space
  // value (Correlated), so admin calls pass that flowId to address the same slice the
  // requests resolve to. `define` installs the stubs in declaration order (Rift is
  // first-match-by-order) and pins the initial state.
  //
  // Limitations (untested edges — the conformance gate is PerInstance, one define per
  // space): re-`define` of an existing name APPENDS stubs rather than replacing the
  // prior ones (provision a fresh space per scenario, as the conformance does); and on
  // a Correlated space a plain-rule mutation (addRule/removeRule/replaceRules) — or a
  // capability injection (faults/script/proxy/template), which rebuilds the same way —
  // drops the space's scenario stubs (and discards any proxy recording). Don't mix
  // scenarios with rule mutation or capability injection on one Correlated space.

  private val statefulScenarios: StatefulScenarios = new StatefulScenarios:
    def define(space: MockSpace, scenarioDef: ScenarioDef): IO[MockError, Unit] =
      dispatch(space)(cs => corrDefine(cs, scenarioDef))(imp => piDefine(imp, scenarioDef))
    def reset(space: MockSpace, name: String): IO[MockError, Unit] =
      dispatch(space)(cs => corrReset(cs, space.id, name))(imp => piReset(imp, space.id, name))

  private val stateInspector: StateInspection = new StateInspection:
    def currentState(space: MockSpace, name: String): IO[MockError, ScenarioState] =
      dispatch(space)(cs => readState(cs.port, cs.flowId, space.id, name))(imp =>
        readState(imp.port, imp.port.toString, space.id, name)
      )
    def setState(space: MockSpace, name: String, to: ScenarioState): IO[MockError, Unit] =
      dispatch(space)(cs => guardDefined(cs.scenarios, space.id, name)(putState(cs.port, cs.flowId, name, to)))(imp =>
        guardDefined(imp.scenarios, space.id, name)(putState(imp.port, imp.port.toString, name, to))
      )

  private def piDefine(imp: Imposter, sc: ScenarioDef): IO[MockError, Unit] =
    for
      _ <- ZIO.foreachDiscard(sc.rules) { r =>
             for
               ruleId <- freshId
               index  <- imp.stubs.get.map(_.size)
               u      <- url(s"/imposters/${imp.port}/stubs")
               stub    = RiftProtocol.statefulStub(sc.name, r.whenState, r.thenState, MockRule(r.request, r.respond))
               res    <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.addStubBodyOf(index, stub)))
               _      <- expectSuccess(s"add scenario stub to imposter ${imp.port}", res)
               _      <- imp.stubs.update(_ :+ ruleId)
             yield ()
           }
      // Always pin the initial state: it puts the scenario in its declared start
      // state (so a re-define resets it) and registers an explicit FlowStore entry.
      _ <- putState(imp.port, imp.port.toString, sc.name, sc.initial)
      _ <- imp.scenarios.update(_ + (sc.name -> sc.initial))
    yield ()

  private def corrDefine(cs: CorrSpace, sc: ScenarioDef): IO[MockError, Unit] =
    for
      _ <- ZIO.foreachDiscard(sc.rules) { r =>
             val stub = RiftProtocol.statefulStub(sc.name, r.whenState, r.thenState, MockRule(r.request, r.respond))
             for
               u   <- url(s"/imposters/${cs.port}/spaces/${enc(cs.flowId)}/stubs")
               res <- send(jsonRequest(HttpMethod.POST, u, stub))
               _   <- expectSuccess(s"add scenario stub to space ${cs.flowId}", res)
             yield ()
           }
      _ <- putState(cs.port, cs.flowId, sc.name, sc.initial)
      _ <- cs.scenarios.update(_ + (sc.name -> sc.initial))
    yield ()

  private def piReset(imp: Imposter, spaceId: SpaceId, name: String): IO[MockError, Unit] =
    imp.scenarios.get.flatMap(_.get(name) match
      case Some(initial) => putState(imp.port, imp.port.toString, name, initial)
      case None          => ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  private def corrReset(cs: CorrSpace, spaceId: SpaceId, name: String): IO[MockError, Unit] =
    cs.scenarios.get.flatMap(_.get(name) match
      case Some(initial) => putState(cs.port, cs.flowId, name, initial)
      case None          => ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  private def guardDefined(
    scenarios: Ref[Map[String, ScenarioState]],
    spaceId: SpaceId,
    name: String
  )(action: => IO[MockError, Unit]): IO[MockError, Unit] =
    scenarios.get.flatMap(s =>
      if s.contains(name) then action
      else ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  // The scenario pin/read calls are shared with the embedded adapter (RiftScenarioAdmin) so both
  // reach the byte-identical admin endpoints from one implementation; readState maps the shared
  // `Option` to this adapter's space-scoped InvalidDefinition on an undeclared scenario.
  private def putState(port: Int, flowId: String, name: String, state: ScenarioState): IO[MockError, Unit] =
    RiftScenarioAdmin.putState(client, endpoint.adminBase, port, flowId, name, state)

  private def readState(port: Int, flowId: String, spaceId: SpaceId, name: String): IO[MockError, ScenarioState] =
    RiftScenarioAdmin.readState(client, endpoint.adminBase, port, flowId, name).flatMap {
      case Some(s) => ZIO.succeed(ScenarioState(s))
      case None    => ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    }

  // ---------------------------------------------------------------------------

  private def serveSpace(src: NormalizedSource): IO[MockError, MockSpace] =
    mode match
      case RiftMode.Correlated(corr) => serveCorrelatedSpace(src, corr)
      case RiftMode.PerInstance      => serveImposter(src)

  // A space's operations follow the space, not the mode: a Correlated portable
  // space, or a PerInstance/native imposter space.
  private def dispatch[A](space: MockSpace)(onCorr: CorrSpace => IO[MockError, A])(
    onImp: Imposter => IO[MockError, A]
  ): IO[MockError, A] =
    correlated.get.flatMap(_.get(space.id) match
      case Some(cs) => onCorr(cs)
      case None =>
        spaces.get.flatMap(_.get(space.id) match
          case Some(imp) => onImp(imp)
          case None      => ZIO.fail(MockError.SpaceNotFound(space.id))
        )
    )

  // ===== PerInstance — one imposter per space =====================================

  private def serveImposter(src: NormalizedSource): IO[MockError, MockSpace] =
    endpoint.acquirePort.flatMap { port =>
      val stand =
        for
          bodyAndCount <- imposterBody(port, src)
          (body, count) = bodyAndCount
          _            <- postImposter(port, body)
          ruleIds      <- freshIds(count)
          stubs        <- Ref.make(ruleIds)
          scen         <- Ref.make(Map.empty[String, ScenarioState])
          id            = SpaceId(s"${src.name}-$port")
          space         = MockSpace(endpoint.baseUriFor(port), identity, id)
          _            <- spaces.update(_.updated(id, Imposter(port, stubs, scen)))
        yield space
      stand.onError(_ => endpoint.releasePort(port))
    }

  private def piAddRule(imp: Imposter, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      // First match wins in Mountebank: overlay on top (index 0), base appended.
      index <- priority match
                 case Priority.Overlay => ZIO.succeed(0)
                 case Priority.Base    => imp.stubs.get.map(_.size)
      u   <- url(s"/imposters/${imp.port}/stubs")
      res <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.addStubBody(index, rule)))
      _   <- expectSuccess(s"add rule to imposter ${imp.port}", res)
      _   <- imp.stubs.update(v => if priority == Priority.Overlay then ruleId +: v else v :+ ruleId)
    yield ruleId

  private def piRemoveRule(imp: Imposter, spaceId: SpaceId, id: RuleId): IO[MockError, Unit] =
    imp.stubs.get.flatMap { ordered =>
      ordered.indexOf(id) match
        case -1 => ZIO.fail(MockError.RuleNotFound(spaceId, id))
        case idx =>
          for
            u   <- url(s"/imposters/${imp.port}/stubs/$idx")
            res <- send(Request(method = HttpMethod.DELETE, url = u))
            _   <- expectSuccess(s"remove rule from imposter ${imp.port}", res)
            _   <- imp.stubs.update(_.patch(idx, Nil, 1))
          yield ()
    }

  private def piReplaceRules(imp: Imposter, rules: List[MockRule]): IO[MockError, Unit] =
    for
      ruleIds <- freshIds(rules.size)
      u       <- url(s"/imposters/${imp.port}/stubs")
      res     <- send(jsonRequest(HttpMethod.PUT, u, RiftProtocol.replaceStubsBody(rules)))
      _       <- expectSuccess(s"replace rules on imposter ${imp.port}", res)
      _       <- imp.stubs.set(ruleIds)
    yield ()

  private def piDestroy(space: MockSpace, imp: Imposter): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/${imp.port}")
      res <- send(Request(method = HttpMethod.DELETE, url = u))
      _   <- ZIO.unless(res._1 == 404)(expectSuccess(s"destroy imposter ${imp.port}", res))
      _   <- endpoint.releasePort(imp.port)
      _   <- spaces.update(_ - space.id)
    yield ()

  private def piReceived(imp: Imposter): IO[MockError, List[RecordedRequest]] =
    for
      u    <- url(s"/imposters/${imp.port}")
      res  <- send(Request(method = HttpMethod.GET, url = u))
      body <- expectSuccess(s"read imposter ${imp.port}", res)
      recs <- ZIO.fromEither(RiftProtocol.parseRecorded(body)).mapError(MockError.CommunicationError(_))
    yield recs

  private def imposterBody(port: Int, src: NormalizedSource): IO[MockError, (Json, Int)] =
    src.payload match
      case SourcePayload.Rules(rules) =>
        ZIO.succeed((RiftProtocol.imposter(port, src.name, rules), rules.length))
      case SourcePayload.Raw(text) =>
        ZIO.fromEither(RiftProtocol.imposterFromRaw(port, src.name, text)).mapError(MockError.InvalidDefinition(_))

  // ===== Correlated — one shared imposter, spaces by flowId =======================

  private def serveCorrelatedSpace(src: NormalizedSource, corr: Correlation): IO[MockError, MockSpace] =
    for
      rules  <- rulesOf(src)
      port   <- ensureSharedImposter(corr)
      id     <- freshSpaceId(src.name)
      flowId  = corr.value(id)
      tagged <- tagRules(rules)
      // Mirror serveImposter's release-on-error: if a stub POST fails partway, tear
      // the half-built flow down so it can't orphan stubs on the shared imposter.
      _         <- registerStubs(port, flowId, tagged).onError(_ => deleteSpace(port, flowId).ignore)
      rulesRef  <- Ref.make(tagged)
      faultsRef <- Ref.make(Vector.empty[(RuleId, RequestMatch, FaultKind)])
      extrasRef <- Ref.make(Vector.empty[(RuleId, Json)])
      scen      <- Ref.make(Map.empty[String, ScenarioState])
      _ <- correlated.update(
             _.updated(id, CorrSpace(port, flowId, corr.header, rulesRef, faultsRef, extrasRef, scen))
           )
    yield MockSpace(endpoint.baseUriFor(port), req => req.copy(headers = req.headers.add(corr.header, flowId)), id)

  // Create the one shared imposter on first Correlated provision (race-safe).
  private def ensureSharedImposter(corr: Correlation): IO[MockError, Int] =
    sharedPort.modifyZIO {
      case existing @ Some(p) => ZIO.succeed((p, existing))
      case None =>
        endpoint.acquirePort.flatMap { p =>
          postImposter(p, RiftProtocol.correlatedImposter(p, "correlated", corr.header))
            .as((p, Some(p)))
            .onError(_ => endpoint.releasePort(p))
        }
    }

  private def corrAddRule(cs: CorrSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    freshId.flatMap { ruleId =>
      val tagged = (ruleId, rule)
      // Server-first, then commit tracking — so a failed admin call never leaves
      // `cs.rules` claiming a stub the server didn't accept (mirrors PerInstance).
      val act = priority match
        // Base appends (Rift's space POST appends).
        case Priority.Base =>
          postSpaceStub(cs.port, cs.flowId, rule.copy(id = Some(ruleId))) *> cs.rules.update(_ :+ tagged)
        // Overlay must be first-match — rebuild with the prepended set.
        case Priority.Overlay =>
          cs.rules.get.flatMap(cur => rebuildSpaceWith(cs, tagged +: cur) *> cs.rules.set(tagged +: cur))
      act.as(ruleId)
    }

  private def corrRemoveRule(cs: CorrSpace, spaceId: SpaceId, id: RuleId): IO[MockError, Unit] =
    for
      rules  <- cs.rules.get
      faults <- cs.faults.get
      extras <- cs.extras.get
      _ <- // commit tracking only on success; a removed id is a rule, a fault, or a capability stub
        if rules.exists(_._1 == id) then
          val next = rules.filterNot(_._1 == id)
          rebuild(cs, extras, faults, next) *> cs.rules.set(next)
        else if faults.exists(_._1 == id) then
          val nextF = faults.filterNot(_._1 == id)
          rebuild(cs, extras, nextF, rules) *> cs.faults.set(nextF)
        else if extras.exists(_._1 == id) then
          val nextE = extras.filterNot(_._1 == id)
          rebuild(cs, nextE, faults, rules) *> cs.extras.set(nextE)
        else ZIO.fail(MockError.RuleNotFound(spaceId, id))
    yield ()

  private def corrReplaceRules(cs: CorrSpace, rules: List[MockRule]): IO[MockError, Unit] =
    tagRules(rules).flatMap(next => rebuildSpaceWith(cs, next) *> cs.rules.set(next))

  private def corrDestroy(spaceId: SpaceId, cs: CorrSpace): IO[MockError, Unit] =
    deleteSpace(cs.port, cs.flowId) *> correlated.update(_ - spaceId)

  private def corrReceived(cs: CorrSpace): IO[MockError, List[RecordedRequest]] =
    for
      u    <- url(s"/imposters/${cs.port}/requests?match=${enc(s"header:${cs.header}=${cs.flowId}")}")
      res  <- send(Request(method = HttpMethod.GET, url = u))
      body <- expectSuccess(s"read filtered requests for space ${cs.flowId}", res)
      recs <- ZIO.fromEither(RiftProtocol.parseRequestsArray(body)).mapError(MockError.CommunicationError(_))
    yield recs

  // rift#223 has no per-stub-in-space delete: re-register the space's stubs after a
  // whole-space teardown. Server-first — the caller commits tracking only on success.
  // A mid-rebuild failure leaves the space's *server* stubs partial (surfaced as a
  // CommunicationError); tracking is left unchanged. (A successful rebuild also clears
  // the space's recorded requests + flow state — benign at scenario boundaries; see
  // the class doc.) Re-register with the currently-tracked faults (a rule mutation
  // must not drop an injected fault).
  private def rebuildSpaceWith(cs: CorrSpace, rules: Vector[(RuleId, MockRule)]): IO[MockError, Unit] =
    for
      extras <- cs.extras.get
      faults <- cs.faults.get
      _      <- rebuild(cs, extras, faults, rules)
    yield ()

  // Capability stubs (extras) and faults are registered ahead of the rules so they
  // win first-match (Mountebank is first-match-wins, and rift#223 space stubs append
  // in registration order).
  private def rebuild(
    cs: CorrSpace,
    extras: Vector[(RuleId, Json)],
    faults: Vector[(RuleId, RequestMatch, FaultKind)],
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    deleteSpace(cs.port, cs.flowId) *>
      registerRawStubs(cs.port, cs.flowId, extras) *>
      registerFaultStubs(cs.port, cs.flowId, faults) *>
      registerStubs(cs.port, cs.flowId, rules)

  private def registerRawStubs(port: Int, flowId: String, extras: Vector[(RuleId, Json)]): IO[MockError, Unit] =
    ZIO.foreachDiscard(extras)((_, stub) => postSpaceRawStub(port, flowId, stub))

  private def postSpaceRawStub(port: Int, flowId: String, stub: Json): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(jsonRequest(HttpMethod.POST, u, stub))
      _   <- expectSuccess(s"add capability stub to $flowId", res)
    yield ()

  private def registerFaultStubs(
    port: Int,
    flowId: String,
    faults: Vector[(RuleId, RequestMatch, FaultKind)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(faults)((rid, m, fault) => postSpaceFaultStub(port, flowId, m, fault, rid))

  private def postSpaceFaultStub(
    port: Int,
    flowId: String,
    m: RequestMatch,
    fault: FaultKind,
    id: RuleId
  ): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.faultStub(m, fault, id)))
      _   <- expectSuccess(s"add fault stub to $flowId", res)
    yield ()

  // Assign each rule a fresh id, keyed for tracking + re-registration under a space.
  private def tagRules(rules: List[MockRule]): UIO[Vector[(RuleId, MockRule)]] =
    freshIds(rules.size).map(ids => rules.zip(ids).map((r, rid) => (rid, r)).toVector)

  private def registerStubs(port: Int, flowId: String, rules: Vector[(RuleId, MockRule)]): IO[MockError, Unit] =
    ZIO.foreachDiscard(rules)((rid, rule) => postSpaceStub(port, flowId, rule.copy(id = Some(rid))))

  private def postSpaceStub(port: Int, flowId: String, rule: MockRule): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/$port/spaces/${enc(flowId)}/stubs")
      res <- send(jsonRequest(HttpMethod.POST, u, RiftProtocol.stub(rule)))
      _   <- expectSuccess(s"add space stub to $flowId", res)
    yield ()

  private def deleteSpace(port: Int, flowId: String): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/$port/spaces/${enc(flowId)}")
      res <- send(Request(method = HttpMethod.DELETE, url = u))
      // A 404 means the space is already gone — teardown is idempotent.
      _ <- ZIO.unless(res._1 == 404)(expectSuccess(s"teardown space $flowId", res))
    yield ()

  private def rulesOf(src: NormalizedSource): IO[MockError, List[MockRule]] =
    src.payload match
      case SourcePayload.Rules(rules) => ZIO.succeed(rules)
      case SourcePayload.Raw(_) =>
        ZIO.fail(
          MockError.InvalidDefinition(
            "Correlated mode needs portable rule sources; provision a raw Rift imposter via provisionNative"
          )
        )

  // ===== shared admin plumbing =====================================================

  private def postImposter(port: Int, body: Json): IO[MockError, Unit] =
    for
      u   <- url("/imposters")
      res <- send(jsonRequest(HttpMethod.POST, u, body))
      _   <- expectSuccess(s"create imposter on port $port", res)
    yield ()

  // Best-effort teardown of the shared Correlated imposter — the one imposter no
  // single space owns (PerInstance leaves `sharedPort` empty, so this is a no-op).
  // Registered as a scope finalizer in `make` so it is reclaimed in `connect`
  // mode too, not only when `managed` stops the container.
  private[rift] def teardownShared: UIO[Unit] =
    sharedPort.get.flatMap {
      case Some(p) =>
        (deleteImposter(p) *> endpoint.releasePort(p))
          .catchAllCause(c => ZIO.logWarningCause("shared Correlated imposter teardown failed", c))
      case None => ZIO.unit
    }

  private def deleteImposter(port: Int): IO[MockError, Unit] =
    for
      u   <- url(s"/imposters/$port")
      res <- send(Request(method = HttpMethod.DELETE, url = u))
      _   <- ZIO.unless(res._1 == 404)(expectSuccess(s"delete shared imposter $port", res))
    yield ()

  private def freshId: UIO[RuleId] = ids.updateAndGet(_ + 1).map(n => RuleId(s"r$n"))

  private def freshIds(n: Int): UIO[Vector[RuleId]] = ZIO.foreach(0 until n)(_ => freshId).map(_.toVector)

  private def freshSpaceId(name: String): UIO[SpaceId] = ids.updateAndGet(_ + 1).map(n => SpaceId(s"$name-s$n"))

  private def unsupported[A](c: Capability): IO[Unsupported, A] = ZIO.fail(Unsupported(c, backendName))

  private def enc(s: String): String = URLEncoder.encode(s, UTF_8)

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
  final case class Imposter(
    port: Int,
    stubs: Ref[Vector[RuleId]],
    scenarios: Ref[Map[String, ScenarioState]] // defined scenario name -> initial state (for reset)
  )
  final case class CorrSpace(
    port: Int,
    flowId: String,
    header: String,
    rules: Ref[Vector[(RuleId, MockRule)]],
    faults: Ref[Vector[(RuleId, RequestMatch, FaultKind)]],
    extras: Ref[Vector[(RuleId, Json)]], // pre-built capability stubs (#132): script / proxy / template
    scenarios: Ref[Map[String, ScenarioState]]
  )

  /**
   * Build an adapter against an endpoint in the given isolation `mode`, taking
   * the zio-http [[Client]] and [[Provisioning]] from the environment. Scoped:
   * the shared Correlated imposter is torn down when the scope closes.
   */
  def make(endpoint: RiftEndpoint, mode: RiftMode): URIO[Client & Provisioning & Scope, MockControl] =
    for
      client     <- ZIO.service[Client]
      prov       <- ZIO.service[Provisioning]
      spaces     <- Ref.make(Map.empty[SpaceId, Imposter])
      correlated <- Ref.make(Map.empty[SpaceId, CorrSpace])
      sharedPort <- Ref.Synchronized.make(Option.empty[Int])
      ids        <- Ref.make(0)
      control     = RiftMockControl(endpoint, client, prov, mode, spaces, correlated, sharedPort, ids)
      _          <- ZIO.addFinalizer(control.teardownShared)
    yield control
