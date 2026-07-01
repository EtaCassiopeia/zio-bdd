package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.RiftProtocol
import zio.json.*
import zio.json.ast.Json

/**
 * The embedded Rift adapter (#133): implements the portable [[MockControl]]
 * core port by driving the Rift engine in-process over the `librift_ffi` C-ABI
 * (Panama FFM), with no Docker.
 *
 * It shares the Mountebank-compatible JSON codec ([[RiftProtocol]]) with the
 * container adapter — the FFI consumes the same imposter/stub model the admin
 * API does — so the wire fidelity is identical; only the transport differs (FFM
 * downcalls instead of HTTP).
 *
 * The FFI surface is intentionally small: create an imposter, replace all of
 * its stubs, read its recorded requests. So this adapter keeps each space's
 * ordered rules in a [[Ref]] and realises every rule mutation as a
 * whole-imposter `rift_replace_stubs` (first-match order: overlay prepends,
 * base appends). It is PerInstance only — one imposter per space on its own
 * OS-assigned localhost port (the host allocates a free port, the engine binds
 * it directly; no container port-mapping).
 *
 * The stub-based optional capabilities — [[Faults]], [[Scripting]],
 * [[ProxyRecord]], [[Templating]] — need no transport beyond
 * `rift_replace_stubs` (each is a special-response stub the engine serves
 * natively), so they are wired the same way the container adapter wires them
 * over the admin API: the capability stub is tracked in `imp.extras` and
 * re-registered ahead of the space's rules on every rebuild, so it wins
 * first-match and is removable via [[removeRule]]. [[StatefulScenarios]] and
 * [[StateInspection]] stay unsupported: they require the scenario-state
 * endpoints (pin/read/reset the initial state) the C-ABI does not expose. So
 * the embedded capability set is deliberately those four; the two stateful
 * capabilities negotiate a justified SKIP.
 */
private[embedded] final case class EmbeddedRiftMockControl(
  engine: EmbeddedEngine,
  provisioning: Provisioning,
  spaces: Ref[Map[SpaceId, EmbeddedRiftMockControl.Imposter]],
  ids: Ref[Int]
) extends MockControl:

  import EmbeddedRiftMockControl.Imposter

  def backendName: String = "embedded"
  def capabilities: Set[Capability] =
    Set(Capability.Faults, Capability.Scripting, Capability.ProxyRecord, Capability.Templating)

  def provision(source: MockSource): IO[MockError, List[MockSpace]] =
    for
      sources <- provisioning.normalize(source)
      created <- Ref.make(List.empty[MockSpace])
      // Provision atomically: if a later source fails, tear down the spaces already stood up so a
      // partial provision doesn't leave them serving (their bound ports are reclaimed only when the
      // engine stops). The original failure propagates; a cleanup failure is logged, never masks it.
      spaces <- ZIO
                  .foreach(sources)(src => serve(src).tap(s => created.update(s :: _)))
                  .onError(_ => created.get.flatMap(ZIO.foreachDiscard(_)(s => destroy(s).ignoreLogged)))
    yield spaces

  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
    spec match
      case NativeSpec.Rift(imposterJson) =>
        serve(NormalizedSource("native", SourcePayload.Raw(imposterJson), None)).map(List(_))
      case NativeSpec.WireMock(_) =>
        ZIO.fail(MockError.InvalidDefinition("the embedded Rift adapter cannot provision a WireMock native spec"))

  def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
    withImposter(space) { imp =>
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
        _ <- rebuild(imp, extras, next)
        _ <- imp.rules.set(next)
      yield ruleId
    }

  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
    withImposter(space) { imp =>
      for
        rules  <- imp.rules.get
        extras <- imp.extras.get
        // commit tracking only on success; a removed id is a rule or a capability stub
        _ <-
          if rules.exists(_._1 == id) then
            val next = rules.filterNot(_._1 == id)
            rebuild(imp, extras, next) *> imp.rules.set(next)
          else if extras.exists(_._1 == id) then
            val nextE = extras.filterNot(_._1 == id)
            rebuild(imp, nextE, rules) *> imp.extras.set(nextE)
          else ZIO.fail(MockError.RuleNotFound(space.id, id))
      yield ()
    }

  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
    withImposter(space) { imp =>
      for
        next   <- tagRules(rules)
        extras <- imp.extras.get
        _      <- rebuild(imp, extras, next)
        _      <- imp.rules.set(next)
      yield ()
    }

  def destroy(space: MockSpace): IO[MockError, Unit] =
    withImposter(space) { imp =>
      // The C-ABI has no per-imposter delete (only `rift_delete_all`, which would tear down sibling
      // spaces), so destroy empties the imposter's stubs (it then serves only the 404 default) and
      // drops local tracking — after which every op on this space fails SpaceNotFound. The bound
      // port lingers until the engine stops; that is reclaimed when the scoped layer closes.
      engine.replaceStubs(imp.port, "[]") *> spaces.update(_ - space.id)
    }

  def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
    withImposter(space) { imp =>
      engine
        .recorded(imp.port)
        .flatMap(json =>
          ZIO.fromEither(RiftProtocol.parseRequestsArray(json)).mapError(MockError.CommunicationError(_))
        )
    }

  def faults: IO[Unsupported, Faults]           = ZIO.succeed(embeddedFaults)
  def scripting: IO[Unsupported, Scripting]     = ZIO.succeed(embeddedScripting)
  def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.succeed(embeddedProxyRecord)
  def templating: IO[Unsupported, Templating]   = ZIO.succeed(embeddedTemplating)

  // No scenario-state endpoints over the C-ABI (see the class doc), so these stay unsupported.
  def scenarios: IO[Unsupported, StatefulScenarios]     = unsupported(Capability.StatefulScenarios)
  def stateInspection: IO[Unsupported, StateInspection] = unsupported(Capability.StateInspection)

  // ===== Stub-based capabilities (#132/#185) ======================================
  // Each installs a special-response stub (`_rift.fault`, `_rift.script`, a Mountebank
  // `proxy`, or an `is` + `_behaviors.copy`) tracked in `imp.extras` and re-registered
  // ahead of the space's rules on every rebuild — so it wins first-match over a normal
  // rule and is removable via `removeRule`.

  private val embeddedFaults: Faults = new Faults:
    def inject(space: MockSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId] =
      injectExtra(space, rid => RiftProtocol.faultStub(m, fault, rid))

  private val embeddedScripting: Scripting = new Scripting:
    def inject(space: MockSpace, m: RequestMatch, script: Script): IO[MockError, RuleId] =
      injectExtra(space, rid => RiftProtocol.scriptStub(m, script, rid))

  private val embeddedProxyRecord: ProxyRecord = new ProxyRecord:
    def proxy(space: MockSpace, m: RequestMatch, upstream: String): IO[MockError, RuleId] =
      injectExtra(space, rid => RiftProtocol.proxyStub(m, upstream, rid))

  private val embeddedTemplating: Templating = new Templating:
    def inject(space: MockSpace, m: RequestMatch, template: ResponseTemplate): IO[MockError, RuleId] =
      injectExtra(space, rid => RiftProtocol.templateStub(m, template, rid))

  // Track a fresh capability stub and rebuild the imposter with it ahead of the rules.
  // Server-first, commit tracking on success — so a failed downcall never leaves `imp.extras`
  // claiming a stub the engine didn't accept (mirrors addRule).
  private def injectExtra(space: MockSpace, buildStub: RuleId => Json): IO[MockError, RuleId] =
    withImposter(space) { imp =>
      for
        ruleId <- freshId
        rules  <- imp.rules.get
        extras <- imp.extras.get
        next    = (ruleId, buildStub(ruleId)) +: extras
        _      <- rebuild(imp, next, rules)
        _      <- imp.extras.set(next)
      yield ruleId
    }

  // ---------------------------------------------------------------------------

  // Stand up one imposter for `src` on its own OS-assigned localhost port. The host allocates a
  // free port (PortAllocator) and the engine binds it directly (`rift_create_imposter` echoes the
  // requested port); a mismatch means the port was taken in the bind window — surfaced, not hidden.
  private def serve(src: NormalizedSource): IO[MockError, MockSpace] =
    for
      port  <- provisioning.allocator.freePort
      built <- buildImposter(port, src)
      bound <- engine.createImposter(built.configJson)
      _ <- ZIO.unless(bound == port)(
             ZIO.fail(MockError.ProvisionFailed(s"embedded Rift bound port $bound but $port was requested"))
           )
      rules  <- Ref.make(built.tagged)
      extras <- Ref.make(Vector.empty[(RuleId, Json)])
      id      = SpaceId(s"${src.name}-$port")
      _      <- spaces.update(_.updated(id, Imposter(port, rules, extras, built.nativeStubs)))
    yield MockSpace(s"http://localhost:$port", identity, id)

  private def withImposter[A](space: MockSpace)(f: Imposter => IO[MockError, A]): IO[MockError, A] =
    spaces.get.flatMap(_.get(space.id) match
      case Some(imp) => f(imp)
      case None      => ZIO.fail(MockError.SpaceNotFound(space.id))
    )

  // Build the create-imposter body for `src`, the portable rules to track, and the native base
  // stubs to preserve. A DSL source's rules are each tagged with a fresh id and tracked; a raw
  // imposter document's own stubs aren't portable rules, so they are kept verbatim as a base layer
  // that every later `rift_replace_stubs` rebuild re-registers beneath the tracked rules — so a
  // mutation (e.g. an overlay) on a natively-provisioned space doesn't drop its native stubs.
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

  // Re-register a space's whole stub list via `rift_replace_stubs`: the tracked capability stubs
  // (faults/script/proxy/template) first so they win first-match, then the tracked portable rules
  // in first-match order, then the native base stubs (so an overlay rule shadows a native stub).
  private def rebuild(
    imp: Imposter,
    extras: Vector[(RuleId, Json)],
    tracked: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    val stubs = extras.map(_._2) ++ withIds(tracked).map(RiftProtocol.stub) ++ imp.nativeStubs
    engine.replaceStubs(imp.port, Json.Arr(stubs*).toJson)

  private def freshId: UIO[RuleId]                  = ids.updateAndGet(_ + 1).map(n => RuleId(s"r$n"))
  private def freshIds(n: Int): UIO[Vector[RuleId]] = ZIO.foreach(0 until n)(_ => freshId).map(_.toVector)

  private def unsupported[A](c: Capability): IO[Unsupported, A] = ZIO.fail(Unsupported(c, backendName))

private[embedded] object EmbeddedRiftMockControl:

  /**
   * A live imposter: its bound port, the ordered portable rules tracked for
   * `rift_replace_stubs`, the pre-built capability stubs (#132/#185: fault /
   * script / proxy / template) re-registered ahead of the rules on every
   * rebuild, and the native base stubs (from a raw imposter document)
   * re-registered beneath the tracked rules — both empty for a plain
   * DSL-provisioned space with no capability injected.
   */
  final case class Imposter(
    port: Int,
    rules: Ref[Vector[(RuleId, MockRule)]],
    extras: Ref[Vector[(RuleId, Json)]],
    nativeStubs: Vector[Json]
  )

  /**
   * The create-imposter body, the rules to track, and the native base stubs for
   * one space.
   */
  final case class Built(configJson: String, tagged: Vector[(RuleId, MockRule)], nativeStubs: Vector[Json])

  def make(engine: EmbeddedEngine, provisioning: Provisioning): UIO[MockControl] =
    for
      spaces <- Ref.make(Map.empty[SpaceId, Imposter])
      ids    <- Ref.make(0)
    yield EmbeddedRiftMockControl(engine, provisioning, spaces, ids)
