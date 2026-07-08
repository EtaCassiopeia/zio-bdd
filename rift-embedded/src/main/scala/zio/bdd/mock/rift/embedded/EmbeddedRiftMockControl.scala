package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.{Correlation, RiftMode, RiftProtocol}
import zio.json.*
import zio.json.ast.Json

/**
 * The embedded Rift adapter (#133): implements the portable [[MockControl]]
 * core port by driving the Rift engine in-process over the `librift_ffi` C-ABI
 * (Panama FFM), with no Docker.
 *
 * It shares the Mountebank-compatible JSON codec ([[RiftProtocol]]) with the
 * container adapter — the FFI consumes the same imposter/stub model the admin
 * API does — so the wire fidelity is identical; only the transport differs.
 *
 * Requires the C-ABI v0.11.0 (rift#411): the adapter drives Rift '''entirely
 * over FFI''' — no loopback HTTP admin plane (#244). The hot-path data plane is
 * `create_imposter`/`replace_stubs`/`recorded`; the admin long tail —
 * scenario/flow state (`rift_flow_state_get`/`put`) and, in Correlated mode,
 * the whole per-space stub plane (`rift_space_add_stub`/`delete`/`recorded`) —
 * are direct downcalls too, over the same manager, so there is no TCP + HTTP
 * round trip for an in-process engine. `destroy` frees the port via
 * `rift_delete_imposter`. All six capabilities are advertised. (Older Rift is
 * unsupported — a pre-v0.11.0 library fails fast at load when the bridge binds
 * the mandatory symbols, so there is no legacy tier.)
 *
 * Two isolation modes ([[RiftMode]]), mirroring the container adapter:
 *
 *   - PerInstance (default): each [[MockSpace]] is one imposter on its own
 *     OS-assigned port, `inject == identity`. The FFI surface for stubs is
 *     intentionally small: create an imposter, replace all of its stubs, read
 *     its recorded requests. So this path keeps each space's ordered rules in a
 *     [[Ref]] and realises every rule/scenario/capability mutation as a
 *     whole-imposter `rift_replace_stubs` (first-match order: capability stubs,
 *     then scenario stubs, then rules, then native base stubs).
 *
 *   - Correlated: all spaces share ONE imposter (created via
 *     `rift_create_imposter` on an allocator-issued port, guarded by
 *     `sharedPort` so it is created at most once), whose `flowIdSource` is
 *     `header:<correlation>`. The shared imposter's spaces are driven over the
 *     space FFI downcalls through [[EmbeddedCorrelatedSpace]] — the
 *     FFI-transport mirror of the container adapter's `RiftCorrelatedSpace`,
 *     building the byte-identical stub JSON via [[RiftProtocol]] and
 *     registering/tearing it down via `rift_space_add_stub`/`delete`/`recorded`
 *     — so isolation semantics are byte-identical to the container, with no
 *     HTTP hop.
 *
 * A native imposter ([[provisionNative]]) always gets its own port regardless
 * of mode, so the two space kinds coexist; per-space operations dispatch by
 * which space a [[MockSpace]] belongs to, not by the active mode.
 */
private[embedded] final case class EmbeddedRiftMockControl(
  engine: EmbeddedEngine,
  provisioning: Provisioning,
  mode: RiftMode,
  spaces: Ref[Map[SpaceId, EmbeddedRiftMockControl.Imposter]],
  correlated: Ref[Map[SpaceId, EmbeddedRiftMockControl.CorrSpace]],
  sharedPort: Ref.Synchronized[Option[Int]],
  ids: Ref[Int],
  interceptStarted: Ref.Synchronized[Option[Int]]
) extends MockControl:

  import EmbeddedRiftMockControl.{CorrSpace, Imposter}

  def backendName: String = "embedded"

  // The FFI admin plane + intercept make the embedded adapter capability-complete: all seven.
  def capabilities: Set[Capability] =
    Set(
      Capability.Faults,
      Capability.Scripting,
      Capability.ProxyRecord,
      Capability.Templating,
      Capability.StatefulScenarios,
      Capability.StateInspection,
      Capability.Intercept
    )

  override def isolation: Isolation = mode match
    case RiftMode.Correlated(_) => Isolation.Correlated
    case RiftMode.PerInstance   => Isolation.PerInstance

  def provision(source: MockSource): IO[MockError, List[MockSpace]] =
    for
      sources <- provisioning.normalize(source)
      created <- Ref.make(List.empty[MockSpace])
      // Provision atomically: if a later source fails, tear down the spaces already stood up so a
      // partial provision doesn't leave them serving. The original failure propagates; a cleanup
      // failure is logged, never masks it.
      spaces <- ZIO
                  .foreach(sources)(src => serveSpace(src).tap(s => created.update(s :: _)))
                  .onError(_ =>
                    created.get.flatMap(
                      ZIO.foreachDiscard(_)(s =>
                        destroy(s).catchAllCause(c => ZIO.logWarningCause(s"rollback: destroy ${s.id.value} failed", c))
                      )
                    )
                  )
    yield spaces

  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
    spec match
      // A native imposter always gets its own port (PerInstance), full-fidelity, regardless of mode.
      case NativeSpec.Rift(imposterJson) =>
        serveImposter(NormalizedSource("native", SourcePayload.Raw(imposterJson), None)).map(List(_))
      case NativeSpec.WireMock(_) =>
        ZIO.fail(MockError.InvalidDefinition("the embedded Rift adapter cannot provision a WireMock native spec"))

  def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    dispatch(space)(cs => corrAddRule(cs, rule, priority))(imp => piAddRule(imp, rule, priority))

  private def piAddRule(imp: Imposter, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      cur    <- imp.rules.get
      extras <- imp.extras.get
      // First match wins: an overlay sits on top (index 0), a base rule is appended.
      next = priority match
               case Priority.Overlay => (ruleId, rule) +: cur
               case Priority.Base    => cur :+ (ruleId, rule)
      // Server-first, commit tracking on success — so a failed downcall never leaves the Ref
      // claiming a rule the engine didn't accept.
      _ <- rebuildCurrent(imp, extras, next)
      _ <- imp.rules.set(next)
    yield ruleId

  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
    dispatch(space)(cs => corrRemoveRule(cs, space.id, id))(imp => piRemoveRule(imp, space.id, id))

  private def piRemoveRule(imp: Imposter, spaceId: SpaceId, id: RuleId): IO[MockError, Unit] =
    for
      rules  <- imp.rules.get
      extras <- imp.extras.get
      // commit tracking only on success; a removed id is a rule or a capability stub
      _ <-
        if rules.exists(_._1 == id) then
          val next = rules.filterNot(_._1 == id)
          rebuildCurrent(imp, extras, next) *> imp.rules.set(next)
        else if extras.exists(_._1 == id) then
          val nextE = extras.filterNot(_._1 == id)
          rebuildCurrent(imp, nextE, rules) *> imp.extras.set(nextE)
        else ZIO.fail(MockError.RuleNotFound(spaceId, id))
    yield ()

  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
    dispatch(space)(cs => corrReplaceRules(cs, rules))(imp => piReplaceRules(imp, rules))

  private def piReplaceRules(imp: Imposter, rules: List[MockRule]): IO[MockError, Unit] =
    for
      next   <- tagRules(rules)
      extras <- imp.extras.get
      _      <- rebuildCurrent(imp, extras, next)
      _      <- imp.rules.set(next)
    yield ()

  def destroy(space: MockSpace): IO[MockError, Unit] =
    dispatch(space)(cs => corrDestroy(space.id, cs))(imp => piDestroy(space, imp))

  private def piDestroy(space: MockSpace, imp: Imposter): IO[MockError, Unit] =
    // rift_delete_imposter frees the bound port immediately (an equal-port re-provision then
    // succeeds); tracking is dropped so every later op on the space fails SpaceNotFound.
    engine.deleteImposter(imp.port) *> spaces.update(_ - space.id)

  def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
    dispatch(space)(corrReceived)(piReceived)

  private def piReceived(imp: Imposter): IO[MockError, List[RecordedRequest]] =
    engine
      .recorded(imp.port)
      .flatMap(json => ZIO.fromEither(RiftProtocol.parseRequestsArray(json)).mapError(MockError.CommunicationError(_)))

  def faults: IO[Unsupported, Faults]           = ZIO.succeed(embeddedFaults)
  def scripting: IO[Unsupported, Scripting]     = ZIO.succeed(embeddedScripting)
  def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.succeed(embeddedProxyRecord)

  // Built-in HTTPS intercept (#219) over the intercept FFI (rift#410); one instance per adapter so the
  // TLS-MITM listener starts at most once (memoized in interceptStarted).
  private val embeddedIntercept: Intercept           = EmbeddedIntercept(engine, interceptStarted)
  override def intercept: IO[Unsupported, Intercept] = ZIO.succeed(embeddedIntercept)
  def templating: IO[Unsupported, Templating]        = ZIO.succeed(embeddedTemplating)

  // Scenario/state route over the in-process admin plane (rift#343).
  def scenarios: IO[Unsupported, StatefulScenarios]     = ZIO.succeed(embeddedScenarios)
  def stateInspection: IO[Unsupported, StateInspection] = ZIO.succeed(embeddedStateInspection)

  // ===== Stub-based capabilities (#132/#185) ======================================
  // Each installs a special-response stub (`_rift.fault`, `_rift.script`, a Mountebank
  // `proxy`, or an `is` + `_behaviors.copy`) — PerInstance: tracked in `imp.extras` and
  // re-registered ahead of the space's rules on every `rift_replace_stubs`; Correlated: a
  // fault is tracked in `cs.faults`, the rest in `cs.extras`, both re-registered ahead of
  // `cs.rules` on every space rebuild (mirrors the container adapter) — so each wins
  // first-match over a normal rule and is removable via `removeRule`.

  private val embeddedFaults: Faults = new Faults:
    def inject(space: MockSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
      dispatch(space)(cs => corrInjectFault(cs, m, fault))(imp =>
        piInjectExtra(imp, rid => RiftProtocol.faultStub(m, fault, rid))
      )

  private val embeddedScripting: Scripting = new Scripting:
    def inject(space: MockSpace, m: RequestMatch, script: Script): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.scriptStub(m, script, rid))

  private val embeddedProxyRecord: ProxyRecord = new ProxyRecord:
    def proxy(space: MockSpace, m: RequestMatch, upstream: String): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.proxyStub(m, upstream, rid))

  private val embeddedTemplating: Templating = new Templating:
    def inject(space: MockSpace, m: RequestMatch, template: ResponseTemplate): IO[MockError, RuleId] =
      injectStub(space, rid => RiftProtocol.templateStub(m, template, rid))

  private def injectStub(space: MockSpace, buildStub: RuleId => Json): IO[MockError, RuleId] =
    dispatch(space)(cs => corrInjectStub(cs, buildStub))(imp => piInjectExtra(imp, buildStub))

  // Track a fresh capability stub and rebuild the imposter with it ahead of the rules.
  // Server-first, commit tracking on success — so a failed downcall never leaves `imp.extras`
  // claiming a stub the engine didn't accept (mirrors addRule).
  private def piInjectExtra(imp: Imposter, buildStub: RuleId => Json): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      rules  <- imp.rules.get
      extras <- imp.extras.get
      next    = (ruleId, buildStub(ruleId)) +: extras
      _      <- rebuildCurrent(imp, next, rules)
      _      <- imp.extras.set(next)
    yield ruleId

  // Correlated: track the fault and rebuild the space with faults ahead of the rules, so it
  // wins first-match. Tracked in `cs.faults` so it survives a later rule rebuild and is
  // removable via `removeRule`; `destroy` clears it with the whole space. Server-first, commit
  // on success (mirrors the container adapter's corrInjectFault).
  private def corrInjectFault(cs: CorrSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      rules  <- cs.rules.get
      faults <- cs.faults.get
      extras <- cs.extras.get
      next    = (ruleId, m, fault) +: faults
      _      <- rebuildCorr(cs, extras, next, rules)
      _      <- cs.faults.set(next)
    yield ruleId

  // Correlated: track the pre-built capability stub in `cs.extras` and rebuild the space with
  // it ahead of the rules (mirrors the container adapter's corrInjectStub).
  private def corrInjectStub(cs: CorrSpace, buildStub: RuleId => Json): IO[MockError, RuleId] =
    for
      ruleId <- freshId
      rules  <- cs.rules.get
      faults <- cs.faults.get
      extras <- cs.extras.get
      next    = (ruleId, buildStub(ruleId)) +: extras
      _      <- rebuildCorr(cs, next, faults, rules)
      _      <- cs.extras.set(next)
    yield ruleId

  // ===== StatefulScenarios + StateInspection (v2, #131/#193) ======================
  // The v2 in-process admin plane runs over the same ImposterManager the FFI downcalls drive
  // (rift#343), so a stateful stub registered via rift_replace_stubs is visible to the admin
  // scenario endpoints, and pinning state over loopback HTTP affects the FFI-served imposter. The
  // stateful stubs are tracked (imp.scenarioStubs) so a later rule/capability rebuild preserves
  // them; the state pin/read reuses RiftScenarioAdmin (shared with the container adapter). flowId =
  // the imposter port (PerInstance), matching how requests resolve their state slice.

  private val embeddedScenarios: StatefulScenarios = new StatefulScenarios:
    def define(space: MockSpace, sc: ScenarioDef): IO[MockError, Unit] =
      dispatch(space)(cs => corrDefine(cs, sc))(imp => piDefine(imp, sc))
    def reset(space: MockSpace, name: String): IO[MockError, Unit] =
      dispatch(space)(cs => corrReset(cs, space.id, name))(imp => piReset(imp, space.id, name))

  private def piDefine(imp: Imposter, sc: ScenarioDef): IO[MockError, Unit] =
    val newStubs =
      sc.rules.map(r => RiftProtocol.statefulStub(sc.name, r.whenState, r.thenState, MockRule(r.request, r.respond)))
    for
      cur    <- imp.scenarioStubs.get
      extras <- imp.extras.get
      rules  <- imp.rules.get
      nextS   = cur ++ newStubs
      // Ordering mirrors the container adapter's piDefine (server-first): register the stubs,
      // commit the stub-tracking Ref (so a later rebuild preserves them), pin the initial state,
      // then commit the scenarios map. Like the container, a re-define of an existing name appends
      // rather than replaces — provision a fresh space per scenario (as the conformance does).
      _ <- rebuildWith(imp, extras, nextS, rules)
      _ <- imp.scenarioStubs.set(nextS)
      _ <- putScenarioState(imp.port, imp.port.toString, sc.name, sc.initial)
      _ <- imp.scenarios.update(_ + (sc.name -> sc.initial))
    yield ()

  private def piReset(imp: Imposter, spaceId: SpaceId, name: String): IO[MockError, Unit] =
    imp.scenarios.get.flatMap(_.get(name) match
      case Some(initial) => putScenarioState(imp.port, imp.port.toString, name, initial)
      case None          => ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  // Correlated: unlike PerInstance, the stateful stubs aren't tracked for rebuild-survival — a
  // later plain-rule mutation or capability injection on the same space drops them (mirrors the
  // container adapter's corrDefine and its documented limitation: don't mix scenarios with rule
  // mutation on one Correlated space).
  private def corrDefine(cs: CorrSpace, sc: ScenarioDef): IO[MockError, Unit] =
    for
      _ <- ZIO.foreachDiscard(sc.rules) { r =>
             val stub = RiftProtocol.statefulStub(sc.name, r.whenState, r.thenState, MockRule(r.request, r.respond))
             EmbeddedCorrelatedSpace.postRawStub(engine, cs.port, cs.flowId, stub)
           }
      _ <- putScenarioState(cs.port, cs.flowId, sc.name, sc.initial)
      _ <- cs.scenarios.update(_ + (sc.name -> sc.initial))
    yield ()

  private def corrReset(cs: CorrSpace, spaceId: SpaceId, name: String): IO[MockError, Unit] =
    cs.scenarios.get.flatMap(_.get(name) match
      case Some(initial) => putScenarioState(cs.port, cs.flowId, name, initial)
      case None          => ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  private val embeddedStateInspection: StateInspection = new StateInspection:
    // Guard `currentState` on the locally-tracked scenarios (like `setState`/`reset`) so an undeclared
    // name is an accurate InvalidDefinition from the Ref — not inferred from a `flow_state_get` null,
    // which the crate also returns on a genuine engine error (rift#415: null conflates absent-key and
    // error).
    def currentState(space: MockSpace, name: String): IO[MockError, ScenarioState] =
      dispatch(space)(cs => guardDefined(cs.scenarios, space.id, name)(readState(cs.port, cs.flowId, space.id, name)))(
        imp => guardDefined(imp.scenarios, space.id, name)(readState(imp.port, imp.port.toString, space.id, name))
      )
    def setState(space: MockSpace, name: String, to: ScenarioState): IO[MockError, Unit] =
      dispatch(space)(cs => guardDefined(cs.scenarios, space.id, name)(putScenarioState(cs.port, cs.flowId, name, to)))(
        imp => guardDefined(imp.scenarios, space.id, name)(putScenarioState(imp.port, imp.port.toString, name, to))
      )

  private def guardDefined[A](
    scenarios: Ref[Map[String, ScenarioState]],
    spaceId: SpaceId,
    name: String
  )(action: => IO[MockError, A]): IO[MockError, A] =
    scenarios.get.flatMap(s =>
      if s.contains(name) then action
      else ZIO.fail(MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))
    )

  // Reached only for a scenario the guard confirmed is declared (so `define` has pinned its state):
  // a `Some` is its current state. A `None` here is anomalous — a declared scenario whose flow-state
  // key vanished, or a genuine engine error the null-return can't distinguish — surfaced as a
  // CommunicationError rather than masked as "no scenario" (which the guard already rules out).
  private def readState(port: Int, flowId: String, spaceId: SpaceId, name: String): IO[MockError, ScenarioState] =
    engine.flowStateGet(port, flowId, name).flatMap {
      case Some(s) => ZIO.succeed(ScenarioState(s))
      case None =>
        ZIO.fail(MockError.CommunicationError(s"scenario '$name' on space ${spaceId.value} has no readable state"))
    }

  // flowId = the imposter port (PerInstance) / the correlation value (Correlated): the slice a
  // request resolves its scenario state to. Scenario state is flow-state keyed by the scenario name,
  // stored as a JSON string — the same slice `set_scenario_state` writes (rift-core), so pinning via
  // `rift_flow_state_put(name, "\"state\"")` is byte-identical to the admin `/scenarios/:name/state`.
  private def putScenarioState(port: Int, flowId: String, name: String, state: ScenarioState): IO[MockError, Unit] =
    engine.flowStatePut(port, flowId, name, Json.Str(state.value).toJson)

  // ---------------------------------------------------------------------------

  // A space's operations follow the space, not the mode: a Correlated portable space, or a
  // PerInstance/native imposter space.
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

  private def serveSpace(src: NormalizedSource): IO[MockError, MockSpace] =
    mode match
      case RiftMode.Correlated(corr) => serveCorrelatedSpace(src, corr)
      case RiftMode.PerInstance      => serveImposter(src)

  // ===== PerInstance — one imposter per space =====================================

  // Stand up one imposter for `src`. Honour a caller-authored port verbatim (#211), else auto-assign
  // a free localhost port (the share-nothing default) — `Provisioning.choosePort` picks. The engine
  // binds the chosen port directly (`rift_create_imposter` echoes the requested port); a mismatch
  // means the port was taken in the bind window — surfaced as a MockError, never hidden or retried.
  private def serveImposter(src: NormalizedSource): IO[MockError, MockSpace] =
    for
      port  <- provisioning.choosePort(src.authoredPort)
      built <- buildImposter(port, src)
      bound <- engine.createImposter(built.configJson)
      _ <- ZIO.unless(bound == port)(
             ZIO.fail(MockError.ProvisionFailed(s"embedded Rift bound port $bound but $port was requested"))
           )
      rules     <- Ref.make(built.tagged)
      extras    <- Ref.make(Vector.empty[(RuleId, Json)])
      scenStubs <- Ref.make(Vector.empty[Json])
      scenarios <- Ref.make(Map.empty[String, ScenarioState])
      id         = SpaceId(s"${src.name}-$port")
      _         <- spaces.update(_.updated(id, Imposter(port, rules, extras, scenStubs, scenarios, built.nativeStubs)))
    yield MockSpace(s"http://localhost:$port", identity, id)

  // ===== Correlated — one shared imposter, spaces by flowId =======================

  private def serveCorrelatedSpace(src: NormalizedSource, corr: Correlation): IO[MockError, MockSpace] =
    for
      rules  <- rulesOf(src)
      port   <- ensureSharedImposter(corr)
      id     <- freshSpaceId(src.name)
      flowId  = corr.value(id)
      tagged <- tagRules(rules)
      // Mirror serveImposter's release-on-error: if a stub POST fails partway, tear the half-built
      // space down so it can't orphan stubs on the shared imposter.
      _ <- EmbeddedCorrelatedSpace
             .registerStubs(engine, port, flowId, tagged)
             .onError(_ =>
               EmbeddedCorrelatedSpace
                 .deleteSpace(engine, port, flowId)
                 .catchAllCause(c => ZIO.logWarningCause(s"rollback: teardown of half-built space $flowId failed", c))
             )
      rulesRef  <- Ref.make(tagged)
      faultsRef <- Ref.make(Vector.empty[(RuleId, RequestMatch, FaultKind)])
      extrasRef <- Ref.make(Vector.empty[(RuleId, Json)])
      scen      <- Ref.make(Map.empty[String, ScenarioState])
      _ <- correlated.update(
             _.updated(id, CorrSpace(port, flowId, rulesRef, faultsRef, extrasRef, scen))
           )
    yield MockSpace(s"http://localhost:$port", req => req.copy(headers = req.headers.add(corr.header, flowId)), id)

  // Create the one shared imposter on first Correlated provision (race-safe): the FFI, not HTTP,
  // since the embedded engine's imposters are always FFI-created; a bind mismatch surfaces the same
  // way serveImposter's does.
  private def ensureSharedImposter(corr: Correlation): IO[MockError, Int] =
    sharedPort.modifyZIO {
      case existing @ Some(p) => ZIO.succeed((p, existing))
      case None =>
        for
          port  <- provisioning.allocator.freePort
          bound <- engine.createImposter(RiftProtocol.correlatedImposter(port, "correlated", corr.header).toJson)
          _ <- ZIO.unless(bound == port)(
                 ZIO.fail(MockError.ProvisionFailed(s"embedded Rift bound port $bound but $port was requested"))
               )
        yield (port, Some(port))
    }

  private def corrAddRule(cs: CorrSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    freshId.flatMap { ruleId =>
      val tagged = (ruleId, rule)
      // Server-first, then commit tracking — so a failed admin call never leaves `cs.rules`
      // claiming a stub the server didn't accept (mirrors PerInstance).
      val act = priority match
        // Base appends (Rift's space POST appends).
        case Priority.Base =>
          EmbeddedCorrelatedSpace.postStub(engine, cs.port, cs.flowId, rule.copy(id = Some(ruleId))) *>
            cs.rules.update(_ :+ tagged)
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
      // commit tracking only on success; a removed id is a rule, a fault, or a capability stub
      _ <-
        if rules.exists(_._1 == id) then
          val next = rules.filterNot(_._1 == id)
          rebuildCorr(cs, extras, faults, next) *> cs.rules.set(next)
        else if faults.exists(_._1 == id) then
          val nextF = faults.filterNot(_._1 == id)
          rebuildCorr(cs, extras, nextF, rules) *> cs.faults.set(nextF)
        else if extras.exists(_._1 == id) then
          val nextE = extras.filterNot(_._1 == id)
          rebuildCorr(cs, nextE, faults, rules) *> cs.extras.set(nextE)
        else ZIO.fail(MockError.RuleNotFound(spaceId, id))
    yield ()

  private def corrReplaceRules(cs: CorrSpace, rules: List[MockRule]): IO[MockError, Unit] =
    tagRules(rules).flatMap(next => rebuildSpaceWith(cs, next) *> cs.rules.set(next))

  private def corrDestroy(spaceId: SpaceId, cs: CorrSpace): IO[MockError, Unit] =
    EmbeddedCorrelatedSpace.deleteSpace(engine, cs.port, cs.flowId) *> correlated.update(_ - spaceId)

  // Space-scoped recorded requests: the header-filtered admin query (rift#201) returns everything
  // recorded under this space's flowId — identical to the container adapter, so a cross-space
  // request (a different flowId) is excluded, while this space's own traffic (matched or not) is
  // returned. Parity with `RiftMockControl`; no client-side re-filtering.
  private def corrReceived(cs: CorrSpace): IO[MockError, List[RecordedRequest]] =
    EmbeddedCorrelatedSpace.received(engine, cs.port, cs.flowId)

  // rift#223 has no per-stub-in-space delete: re-register the space's stubs after a whole-space
  // teardown. Server-first — the caller commits tracking only on success. Re-register with the
  // currently-tracked faults (a rule mutation must not drop an injected fault).
  private def rebuildSpaceWith(cs: CorrSpace, rules: Vector[(RuleId, MockRule)]): IO[MockError, Unit] =
    for
      extras <- cs.extras.get
      faults <- cs.faults.get
      _      <- rebuildCorr(cs, extras, faults, rules)
    yield ()

  // Capability stubs (extras) and faults are registered ahead of the rules so they win
  // first-match (Mountebank is first-match-wins, and rift#223 space stubs append in registration
  // order).
  private def rebuildCorr(
    cs: CorrSpace,
    extras: Vector[(RuleId, Json)],
    faults: Vector[(RuleId, RequestMatch, FaultKind)],
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    EmbeddedCorrelatedSpace.rebuild(engine, cs.port, cs.flowId, extras, faults, rules)

  private def rulesOf(src: NormalizedSource): IO[MockError, List[MockRule]] =
    src.payload match
      case SourcePayload.Rules(rules) => ZIO.succeed(rules)
      case SourcePayload.Raw(_) =>
        ZIO.fail(
          MockError.InvalidDefinition(
            "Correlated mode needs portable rule sources; provision a raw Rift imposter via provisionNative"
          )
        )

  // ---------------------------------------------------------------------------

  // Build the create-imposter body for `src`, the portable rules to track, and the native base
  // stubs to preserve. A DSL source's rules are each tagged with a fresh id and tracked; a raw
  // imposter document's own stubs aren't portable rules, so they are kept verbatim as a base layer
  // that every later `rift_replace_stubs` rebuild re-registers beneath the tracked rules.
  private def buildImposter(port: Int, src: NormalizedSource): IO[MockError, EmbeddedRiftMockControl.Built] =
    src.payload match
      case SourcePayload.Rules(rules) =>
        tagRules(rules).map(tagged =>
          EmbeddedRiftMockControl.Built(
            RiftProtocol.imposter(port, src.name, withIds(tagged)).toJson,
            tagged,
            Vector.empty
          )
        )
      case SourcePayload.Raw(text) =>
        ZIO
          .fromEither(RiftProtocol.imposterFromRaw(port, src.name, text))
          .mapError(MockError.InvalidDefinition(_))
          .map((doc, _) => EmbeddedRiftMockControl.Built(doc.toJson, Vector.empty, stubsOf(doc)))

  // The `stubs` array of an imposter document, or empty if absent/malformed-shaped.
  private def stubsOf(imposterDoc: Json): Vector[Json] =
    imposterDoc match
      case o: Json.Obj => o.fields.collectFirst { case ("stubs", Json.Arr(es)) => es.toVector }.getOrElse(Vector.empty)
      case _           => Vector.empty

  private def tagRules(rules: List[MockRule]): UIO[Vector[(RuleId, MockRule)]] =
    freshIds(rules.size).map(ids => rules.zip(ids).map((r, rid) => (rid, r)).toVector)

  private def withIds(tagged: Vector[(RuleId, MockRule)]): List[MockRule] =
    tagged.map((rid, r) => r.copy(id = Some(rid))).toList

  // Re-register a space's whole stub list via `rift_replace_stubs`, first-match order: the tracked
  // capability stubs (faults/script/proxy/template), then the scenario stubs, then the portable
  // rules, then the native base stubs. `rebuildCurrent` reads the current scenario stubs; `rebuildWith`
  // takes them explicitly so `define` can rebuild-then-commit server-first.
  private def rebuildCurrent(
    imp: Imposter,
    extras: Vector[(RuleId, Json)],
    tracked: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    imp.scenarioStubs.get.flatMap(sc => rebuildWith(imp, extras, sc, tracked))

  private def rebuildWith(
    imp: Imposter,
    extras: Vector[(RuleId, Json)],
    scenarioStubs: Vector[Json],
    tracked: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    val stubs = extras.map(_._2) ++ scenarioStubs ++ withIds(tracked).map(RiftProtocol.stub) ++ imp.nativeStubs
    engine.replaceStubs(imp.port, Json.Arr(stubs*).toJson)

  private def freshId: UIO[RuleId]                     = ids.updateAndGet(_ + 1).map(n => RuleId(s"r$n"))
  private def freshIds(n: Int): UIO[Vector[RuleId]]    = ZIO.foreach(0 until n)(_ => freshId).map(_.toVector)
  private def freshSpaceId(name: String): UIO[SpaceId] = ids.updateAndGet(_ + 1).map(n => SpaceId(s"$name-s$n"))

  // Best-effort teardown of the shared Correlated imposter — the one imposter no single space
  // owns (PerInstance leaves `sharedPort` empty, so this is a no-op). Registered as a scope
  // finalizer in `make` so it is reclaimed with the rest of the engine's scope.
  private[embedded] def teardownShared: UIO[Unit] =
    sharedPort.get.flatMap {
      case Some(p) =>
        engine
          .deleteImposter(p)
          .catchAllCause(c => ZIO.logWarningCause("shared Correlated imposter teardown failed", c))
      case None => ZIO.unit
    }

private[embedded] object EmbeddedRiftMockControl:

  /**
   * A live imposter: its bound port, the ordered portable rules tracked for
   * `rift_replace_stubs`, the pre-built capability stubs (#132/#185) and
   * scenario stubs (#193) re-registered ahead of the rules on every rebuild,
   * the defined scenarios' initial states (for reset), and the native base
   * stubs (from a raw imposter document) re-registered beneath the tracked
   * rules.
   */
  final case class Imposter(
    port: Int,
    rules: Ref[Vector[(RuleId, MockRule)]],
    extras: Ref[Vector[(RuleId, Json)]],
    scenarioStubs: Ref[Vector[Json]],
    scenarios: Ref[Map[String, ScenarioState]],
    nativeStubs: Vector[Json]
  )

  /**
   * A Correlated space (rift#223): its shared imposter's port, its `flowId` on
   * that imposter, the space's tracked portable rules, faults, and pre-built
   * capability stubs (each re-registered ahead of the rules on a rebuild —
   * rift#223 has no per-stub-in-space delete), and its defined scenarios'
   * initial states (for reset). Mirrors the container adapter's `CorrSpace`
   * (minus the correlation header: the space FFI filters `recorded` by the
   * resolved flow id itself, so the adapter never re-filters). `flowId`
   * addresses the space.
   */
  final case class CorrSpace(
    port: Int,
    flowId: String,
    rules: Ref[Vector[(RuleId, MockRule)]],
    faults: Ref[Vector[(RuleId, RequestMatch, FaultKind)]],
    extras: Ref[Vector[(RuleId, Json)]],
    scenarios: Ref[Map[String, ScenarioState]]
  )

  /**
   * The create-imposter body, the rules to track, and the native base stubs for
   * one space.
   */
  final case class Built(configJson: String, tagged: Vector[(RuleId, MockRule)], nativeStubs: Vector[Json])

  /**
   * Build the adapter in the given isolation `mode`. Scoped: the shared
   * Correlated imposter is torn down when the scope closes (mirrors the
   * container adapter's `make`).
   */
  def make(
    engine: EmbeddedEngine,
    provisioning: Provisioning,
    mode: RiftMode = RiftMode.PerInstance
  ): URIO[Scope, MockControl] =
    for
      spaces     <- Ref.make(Map.empty[SpaceId, Imposter])
      correlated <- Ref.make(Map.empty[SpaceId, CorrSpace])
      sharedPort <- Ref.Synchronized.make(Option.empty[Int])
      ids        <- Ref.make(0)
      intercept  <- Ref.Synchronized.make(Option.empty[Int])
      control     = EmbeddedRiftMockControl(engine, provisioning, mode, spaces, correlated, sharedPort, ids, intercept)
      _          <- ZIO.addFinalizer(control.teardownShared)
    yield control
