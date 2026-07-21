package zio.bdd.mock.rift

import java.nio.file.{Files, Path}

import zio.*
import zio.bdd.mock as spi

import rift.RiftError
import rift.dsl.{StubBuilder, StubPhase}
import rift.model.{FlowId, Port, RecordedRequest, Stub, StubId}
import rift.bridge.CaMaterial
import rift.zio.{ImposterHandle, InterceptHandle, Rift as SdkRift, SpaceHandle}

import RiftModelMapping as M

/**
 * The bind settings for the [[spi.Capability.Intercept]] listener (#285 unifies
 * what used to be a container-specific `(host, port)` pair and the embedded
 * adapter's own `InterceptConfig`): the host the SDK's `rift.intercept` RPC
 * binds, the port (`0` = engine-assigned), and an optional persistent CA (both
 * files or neither). Resolved lazily — on first intercept use, not at layer
 * construction — so a suite that never intercepts never touches the filesystem
 * or fails on a misconfigured CA path.
 */
private[rift] final case class InterceptSettings(
  bindHost: String = "127.0.0.1",
  port: Int = 0,
  caCert: Option[Path] = None,
  caKey: Option[Path] = None
):
  def resolve: IO[spi.MockError, rift.bridge.InterceptConfig] =
    for
      _ <- ZIO.fromEither(InterceptSettings.validateCaPair(caCert, caKey))
      _ <- ZIO.fromEither(InterceptSettings.validateBindHost(bindHost))
      ca <- ZIO.foreach(caCert.zip(caKey)) { case (cert, key) =>
              ZIO
                .attemptBlocking((Files.readString(cert), Files.readString(key)))
                .mapError(t => spi.MockError.InvalidDefinition(s"reading intercept CA: ${M.message(t)}"))
            }
    yield rift.bridge.InterceptConfig(bindHost, port, ca.map(CaMaterial.apply))

private[rift] object InterceptSettings:
  // Both-or-neither: a lone caCert/caKey is a misconfiguration. Reject it early with a clear message
  // rather than silently dropping to an ephemeral CA / an opaque engine error.
  def validateCaPair(caCert: Option[Path], caKey: Option[Path]): Either[spi.MockError, Unit] =
    (caCert, caKey) match
      case (Some(_), None) | (None, Some(_)) =>
        Left(spi.MockError.InvalidDefinition("intercept caCert and caKey must be set together (both or neither)"))
      case _ => Right(())

  // The SDK's InterceptOptions.Builder.host(...) validates the bind host is an IP literal and THROWS
  // IllegalArgumentException otherwise — not a RiftError, so it would die as a defect through
  // blockingIO's refineToOrDie. Reject it here first with a typed MockError (#262's original intent).
  def validateBindHost(host: String): Either[spi.MockError, Unit] =
    if isIpLiteral(host) then Right(())
    else
      Left(
        spi.MockError.InvalidDefinition(
          s"intercept bindHost must be an IP literal (e.g. 0.0.0.0 or a NIC address), not a hostname: $host"
        )
      )

  private def isIpLiteral(host: String): Boolean =
    def asciiDigit(c: Char): Boolean = c >= '0' && c <= '9'
    def isIpv4: Boolean =
      val parts = host.split("\\.", -1)
      parts.length == 4 && parts.forall { p =>
        p.nonEmpty && p.length <= 3 && p.forall(asciiDigit) && p.toInt <= 255 && (p == "0" || !p.startsWith("0"))
      }
    def isIpv6: Boolean =
      host.contains(":") && host.forall { c =>
        asciiDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c == ':' || c == '.'
      }
    isIpv4 || isIpv6

/**
 * The single `MockControl` adapter over `rift.zio.Rift` (#285 re-bases the Rift
 * adapters onto the official rift-scala SDK): one implementation for every
 * transport (embedded FFM, container, or a bare `connect`), since the SDK
 * already abstracts the wire protocol — only `portPool` (present for a
 * transport whose imposters must bind a pre-declared port; absent for
 * embedded/bare engines, see [[RiftPortPool]]) and `interceptSettings` vary by
 * transport, both threaded in by `Rift`'s thin layer constructors.
 *
 * Space bookkeeping mirrors the pre-#285 hand-rolled adapter's semantics
 * (first-match capability stubs, correlated-space rebuild-on-mutation,
 * provision rollback, 404 unmatched default), but the mechanics now ride the
 * SDK's typed surface instead of hand-rolled admin HTTP/FFI:
 *
 *   - PerInstance: one imposter per space, rules addressed by **native stub
 *     ids** (`handle.stub(StubId)`) instead of client-tracked positional
 *     indexes.
 *   - Correlated: one shared imposter partitioned by a correlation header, each
 *     space a typed [[SpaceHandle]]. The space endpoint exposes only
 *     whole-space teardown, so any mutation beyond a Base append rebuilds the
 *     space (extras -> faults -> rules, first-registered wins) — which also
 *     clears its recorded requests and flow state; mutate at scenario
 *     boundaries.
 *
 * Correlated scenarios: `define` (raw scenario-stub install per flow) and
 * `currentState` (per-flow read) work; only the state *writes*
 * (`reset`/`setState`) and a custom initial state remain gapped (typed
 * `InvalidDefinition`, never silent) — the SDK's `Scenarios` surface exposes a
 * per-flow read but no per-flow write yet (rift-java#151).
 */
private[rift] final case class RiftMockControl(
  rift: SdkRift,
  portPool: Option[RiftPortPool],
  provisioning: spi.Provisioning,
  mode: RiftMode,
  scope: Scope,
  spaces: Ref[Map[spi.SpaceId, SpaceRec]],
  shared: Ref.Synchronized[Option[ImposterHandle]],
  interceptCell: Ref.Synchronized[Option[InterceptHandle]],
  interceptSettings: InterceptSettings,
  // Whether the intercept LISTENER is actually reachable for this transport (#285's unification
  // regressed this to "always" — see the type's own doc for why that's unsound for container/connect).
  interceptCapable: Boolean,
  counter: Ref[Int]
) extends spi.MockControl:

  def backendName: String = "rift"

  /**
   * Every capability except [[spi.Capability.Intercept]] is transport-agnostic
   * — the SDK's admin plane covers it identically over
   * embedded/container/connect. Intercept is different: the listener itself
   * must be host-reachable for `require(Capability.Intercept)` to mean
   * anything. Embedded always can (it starts in-process, on the host).
   * Container/connect can only when the caller actually configured a reachable
   * bind (`interceptPort`/`interceptProxy`) — otherwise the listener binds
   * inside the container's netns (or nowhere), and advertising it anyway would
   * let `require` succeed while `proxyPort` returns an endpoint the SUT can
   * never reach. A clean `Unsupported` beats an unreachable endpoint (the
   * pre-#285 contract, restored here).
   */
  def capabilities: Set[spi.Capability] =
    if interceptCapable then spi.Capability.values.toSet
    else spi.Capability.values.toSet - spi.Capability.Intercept

  override def isolation: spi.Isolation = mode match
    case RiftMode.PerInstance   => spi.Isolation.PerInstance
    case RiftMode.Correlated(_) => spi.Isolation.Correlated

  // ── core port ────────────────────────────────────────────────────────────────────────────────

  def provision(source: spi.MockSource): IO[spi.MockError, List[spi.MockSpace]] =
    provisioning.normalize(source).flatMap { sources =>
      Ref.make(List.empty[spi.MockSpace]).flatMap { created =>
        ZIO
          .foreach(sources)(src => serveSpace(src).tap(s => created.update(s :: _)))
          .onError(_ =>
            created.get.flatMap(
              ZIO.foreachDiscard(_)(s =>
                destroy(s).catchAllCause(c => ZIO.logWarningCause(s"rollback: destroy ${s.id.value} failed", c))
              )
            )
          )
      }
    }

  def provisionNative[B <: spi.Backend](spec: spi.NativeSpec[B]): IO[spi.MockError, List[spi.MockSpace]] =
    spec match
      case spi.NativeSpec.Rift(imposterJson) =>
        // A raw provisionNative document DOES honour its own port (unlike portable provision()), but
        // under a container/connect pool that port still must be validated + claimed against the
        // pool (or, absent one, a fresh pool port assigned) — exactly like the Rules branch — or the
        // imposter either binds a port Docker never published (#303/#214) or collides with a later
        // auto-provision that never saw it claimed (#213).
        for
          parsed           <- ZIO.fromEither(M.fromRaw(imposterJson, honourDocPort = true))
          portResolution   <- resolvePort(parsed.port.map(Port.value))
          (portOpt, pooled) = portResolution
          port             <- ZIO.foreach(portOpt)(p => ZIO.fromEither(Port.from(p)).mapError(spi.MockError.InvalidDefinition(_)))
          definition        = parsed.copy(port = port)
          handle <- rift
                      .create(definition)
                      .mapError(M.toMockError(None))
                      .onError(_ => ZIO.whenDiscard(pooled)(portOpt.fold(ZIO.unit)(releasePortValue)))
          space <- registerPerInstance("native", handle, pooled)
        yield List(space)
      case spi.NativeSpec.WireMock(_) =>
        ZIO.fail(spi.MockError.InvalidDefinition("the Rift adapter cannot provision a WireMock native spec"))

  def addRule(space: spi.MockSpace, rule: spi.MockRule, priority: spi.Priority): IO[spi.MockError, spi.RuleId] =
    freshRuleId.flatMap { id =>
      ZIO.fromEither(M.stubFor(rule, id)).flatMap { builder =>
        withSpace(space) {
          case rec: SpaceRec.PerInstance =>
            val add = priority match
              case spi.Priority.Overlay => rec.handle.addStubFirst(builder)
              case spi.Priority.Base    => rec.handle.addStub(builder)
            add.mapError(M.toMockError(Some(space.id))) *> rec.ruleIds.update(_ + id).as(id)
          case rec: SpaceRec.Correlated =>
            rec.state.modifyZIO { st =>
              priority match
                case spi.Priority.Base =>
                  rec.space.addStub(builder).mapError(M.toMockError(Some(space.id))) *>
                    ZIO.succeed((id, st.copy(rules = st.rules :+ (id -> builder))))
                case spi.Priority.Overlay =>
                  val next = st.copy(rules = (id -> builder) +: st.rules)
                  rebuildCorrelated(space.id, rec.space, next).as((id, next))
            }
        }
      }
    }

  def removeRule(space: spi.MockSpace, id: spi.RuleId): IO[spi.MockError, Unit] =
    withSpace(space) {
      case rec: SpaceRec.PerInstance =>
        rec.ruleIds.get.flatMap { known =>
          if !known.contains(id) then ZIO.fail(spi.MockError.RuleNotFound(space.id, id))
          else
            rec.handle.stub(StubId(id.value)).flatMap(_.delete).mapError(M.toMockError(Some(space.id))) *>
              rec.ruleIds.update(_ - id)
        }
      case rec: SpaceRec.Correlated =>
        rec.state.updateZIO { st =>
          val next =
            if st.rules.exists(_._1 == id) then Some(st.copy(rules = st.rules.filterNot(_._1 == id)))
            else if st.faults.exists(_._1 == id) then Some(st.copy(faults = st.faults.filterNot(_._1 == id)))
            else if st.extras.exists(_._1 == id) then Some(st.copy(extras = st.extras.filterNot(_._1 == id)))
            // `scenariosImpl.define` mints RuleIds into `CorrState.scenarios` too (one per FSM edge) —
            // without this tier, removeRule on an id the adapter itself issued fails RuleNotFound (#285/B8).
            else if st.scenarios.exists(_._1 == id) then Some(st.copy(scenarios = st.scenarios.filterNot(_._1 == id)))
            else None
          next match
            case Some(target) => rebuildCorrelated(space.id, rec.space, target).as(target)
            case None         => ZIO.fail(spi.MockError.RuleNotFound(space.id, id))
        }
    }

  def replaceRules(space: spi.MockSpace, rules: List[spi.MockRule]): IO[spi.MockError, Unit] =
    withSpace(space) {
      case rec: SpaceRec.PerInstance =>
        for
          tagged <- tagAll(rules)
          stubs   = Chunk.fromIterable(tagged.map(_._2.build))
          _      <- rec.handle.replaceStubs(stubs).mapError(M.toMockError(Some(space.id)))
          _      <- rec.ruleIds.set(tagged.map(_._1).toSet)
          // The PUT-equivalent replaced every stub — including scenario stubs — so tracked scenario
          // names would otherwise pass guardDefined for FSMs that no longer exist on the server.
          _ <- rec.scenarios.set(Map.empty)
        yield ()
      case rec: SpaceRec.Correlated =>
        tagAll(rules).flatMap { tagged =>
          rec.state.updateZIO { st =>
            val next = st.copy(rules = tagged)
            rebuildCorrelated(space.id, rec.space, next).as(next)
          }
        }
    }

  def destroy(space: spi.MockSpace): IO[spi.MockError, Unit] =
    withSpace(space) {
      case rec: SpaceRec.PerInstance =>
        rec.handle.delete.catchSome { case RiftError.ImposterNotFound(_) => ZIO.unit }
          .mapError(M.toMockError(Some(space.id))) *>
          spaces.update(_ - space.id) *>
          ZIO.whenDiscard(rec.pooled)(releasePort(rec.handle))
      case rec: SpaceRec.Correlated =>
        rec.space.delete.mapError(M.toMockError(Some(space.id))) *> spaces.update(_ - space.id)
    }

  def received(space: spi.MockSpace): IO[spi.MockError, List[spi.RecordedRequest]] =
    withSpace(space) {
      case rec: SpaceRec.PerInstance =>
        rec.handle.recorded.mapError(M.toMockError(Some(space.id))).flatMap(toRecordedList(space.id, _))
      case rec: SpaceRec.Correlated =>
        rec.space.recorded.mapError(M.toMockError(Some(space.id))).flatMap(toRecordedList(space.id, _))
    }

  /**
   * `M.toRecorded`'s method readback is total but coerces an unmappable verb
   * (WebDAV `PROPFIND`, ...) to `Get` — since `RecordedRequest` is assertion
   * data, a suite asserting "no GET was issued" would otherwise get a silently
   * wrong answer. Diagnostic, not silent: log a warning naming the raw verb
   * whenever the coercion fires (#285/B13).
   */
  private def toRecordedList(spaceId: spi.SpaceId, recs: Chunk[RecordedRequest]): UIO[List[spi.RecordedRequest]] =
    ZIO.foreach(recs.toList) { r =>
      M.methodFallback(r.method) match
        case Some(raw) =>
          ZIO.logWarning(
            s"space ${spaceId.value}: recorded request method '$raw' has no portable spi.Method " +
              "equivalent; reading back as Method.Get (a readback coercion, not a real GET — see " +
              "RiftModelMapping.methodOf)"
          ) *> ZIO.succeed(M.toRecorded(r))
        case None => ZIO.succeed(M.toRecorded(r))
    }

  // ── capabilities ─────────────────────────────────────────────────────────────────────────────

  def faults: IO[spi.Unsupported, spi.Faults]                   = ZIO.succeed(faultsImpl)
  def scenarios: IO[spi.Unsupported, spi.StatefulScenarios]     = ZIO.succeed(scenariosImpl)
  def stateInspection: IO[spi.Unsupported, spi.StateInspection] = ZIO.succeed(stateImpl)
  def scripting: IO[spi.Unsupported, spi.Scripting]             = ZIO.succeed(scriptingImpl)
  def proxyRecord: IO[spi.Unsupported, spi.ProxyRecord]         = ZIO.succeed(proxyImpl)
  def templating: IO[spi.Unsupported, spi.Templating]           = ZIO.succeed(templatingImpl)
  override def intercept: IO[spi.Unsupported, spi.Intercept] =
    if interceptCapable then ZIO.succeed(interceptImpl)
    else ZIO.fail(spi.Unsupported(spi.Capability.Intercept, backendName))

  private val faultsImpl: spi.Faults = new spi.Faults:
    def inject(space: spi.MockSpace, m: spi.RequestMatch, fault: spi.FaultKind): IO[spi.MockError, spi.RuleId] =
      injectFirstMatch(space, CorrTier.Faults)(id => M.faultStub(m, fault, id))

  private val scriptingImpl: spi.Scripting = new spi.Scripting:
    def inject(space: spi.MockSpace, m: spi.RequestMatch, script: spi.Script): IO[spi.MockError, spi.RuleId] =
      injectFirstMatch(space, CorrTier.Extras)(id => M.scriptStub(m, script, id))

  private val proxyImpl: spi.ProxyRecord = new spi.ProxyRecord:
    def proxy(space: spi.MockSpace, m: spi.RequestMatch, upstream: String): IO[spi.MockError, spi.RuleId] =
      injectFirstMatch(space, CorrTier.Extras)(id => M.proxyStub(m, upstream, id))

  private val templatingImpl: spi.Templating = new spi.Templating:
    def inject(
      space: spi.MockSpace,
      m: spi.RequestMatch,
      template: spi.ResponseTemplate
    ): IO[spi.MockError, spi.RuleId] =
      injectFirstMatch(space, CorrTier.Extras)(id => M.templateStub(m, template, id))

  /**
   * A capability stub must win over any normal rule on the same match: first
   * position (`addStubFirst` / rebuilt ahead of the rules) and tracked so
   * `removeRule` can lift it. On a Correlated space the stub lands in its tier
   * (extras for script/proxy/template, faults for fault injection) so a rule
   * mutation preserves it in first-match position.
   */
  private def injectFirstMatch(space: spi.MockSpace, tier: CorrTier)(
    build: spi.RuleId => Either[spi.MockError, StubBuilder[StubPhase.Complete]]
  ): IO[spi.MockError, spi.RuleId] =
    freshRuleId.flatMap { id =>
      ZIO.fromEither(build(id)).flatMap { builder =>
        withSpace(space) {
          case rec: SpaceRec.PerInstance =>
            rec.handle.addStubFirst(builder).mapError(M.toMockError(Some(space.id))) *>
              rec.ruleIds.update(_ + id).as(id)
          case rec: SpaceRec.Correlated =>
            rec.state.modifyZIO { st =>
              val next = tier match
                case CorrTier.Faults => st.copy(faults = (id -> builder) +: st.faults)
                case CorrTier.Extras => st.copy(extras = (id -> builder) +: st.extras)
              rebuildCorrelated(space.id, rec.space, next).as((id, next))
            }
        }
      }
    }

  private val scenariosImpl: spi.StatefulScenarios = new spi.StatefulScenarios:
    def define(space: spi.MockSpace, scenario: spi.ScenarioDef): IO[spi.MockError, Unit] =
      withSpace(space) {
        case rec: SpaceRec.PerInstance =>
          for
            ids     <- ZIO.foreach(Vector.range(0, scenario.rules.size))(_ => freshRuleId)
            stubs   <- ZIO.fromEither(M.scenarioStubs(scenario, ids))
            current <- rec.handle.stubs.mapError(M.toMockError(Some(space.id)))
            _ <- rec.handle
                   .replaceStubs(current ++ Chunk.fromIterable(stubs))
                   .mapError(M.toMockError(Some(space.id)))
            // Pin the declared initial state — starts a re-defined scenario over and registers an
            // explicit flow-store entry.
            _ <- rec.handle.scenarios
                   .setState(scenario.name, scenario.initial.value)
                   .mapError(M.toMockError(Some(space.id)))
            _ <- rec.ruleIds.update(_ ++ ids)
            _ <- rec.scenarios.update(_ + (scenario.name -> scenario.initial))
          yield ()
        case rec: SpaceRec.Correlated =>
          // The engine seeds every scenario at exactly "Started" and exposes no per-flow write to move
          // it, so any other initial state can't be pinned on a Correlated space yet (rift-java#151).
          if scenario.initial != spi.ScenarioState.Started then
            ZIO.fail(
              spi.MockError.InvalidDefinition(
                s"a Correlated scenario with a non-default initial state ('${scenario.initial.value}') " +
                  "needs per-flow setState, which the rift.zio surface does not expose yet " +
                  "(rift-java#151) — use the default 'Started' initial state, or PerInstance isolation"
              )
            )
          else
            for
              ids   <- ZIO.foreach(Vector.range(0, scenario.rules.size))(_ => freshRuleId)
              stubs <- ZIO.fromEither(M.scenarioStubs(scenario, ids))
              tagged = ids.zip(stubs)
              _ <- rec.state.modifyZIO { st =>
                     ZIO
                       .foreachDiscard(tagged)((_, s) => rec.space.addStub(s))
                       .mapError(M.toMockError(Some(space.id)))
                       .as(((), st.copy(scenarios = st.scenarios ++ tagged)))
                   }
            yield ()
      }

    def reset(space: spi.MockSpace, name: String): IO[spi.MockError, Unit] =
      withSpace(space) {
        case rec: SpaceRec.PerInstance =>
          rec.scenarios.get.flatMap(_.get(name) match
            case Some(initial) =>
              rec.handle.scenarios.setState(name, initial.value).mapError(M.toMockError(Some(space.id)))
            case None => noScenario(space.id, name)
          )
        case _: SpaceRec.Correlated => correlatedScenarioWriteGap
      }

  private val stateImpl: spi.StateInspection = new spi.StateInspection:
    def currentState(space: spi.MockSpace, name: String): IO[spi.MockError, spi.ScenarioState] =
      withSpace(space) {
        case rec: SpaceRec.PerInstance =>
          guardDefined(rec, space.id, name)(
            rec.handle.scenarios.state(name).mapBoth(M.toMockError(Some(space.id)), spi.ScenarioState(_))
          )
        case rec: SpaceRec.Correlated =>
          rec.imposter.scenarios
            .state(name, rec.space.flowId)
            .mapBoth(M.toMockError(Some(space.id)), spi.ScenarioState(_))
      }

    def setState(space: spi.MockSpace, name: String, to: spi.ScenarioState): IO[spi.MockError, Unit] =
      withSpace(space) {
        case rec: SpaceRec.PerInstance =>
          guardDefined(rec, space.id, name)(
            rec.handle.scenarios.setState(name, to.value).mapError(M.toMockError(Some(space.id)))
          )
        case _: SpaceRec.Correlated => correlatedScenarioWriteGap
      }

  private def guardDefined[A](rec: SpaceRec.PerInstance, spaceId: spi.SpaceId, name: String)(
    action: IO[spi.MockError, A]
  ): IO[spi.MockError, A] =
    rec.scenarios.get.flatMap(s => if s.contains(name) then action else noScenario(spaceId, name))

  private def noScenario[A](spaceId: spi.SpaceId, name: String): IO[spi.MockError, A] =
    ZIO.fail(spi.MockError.InvalidDefinition(s"no scenario '$name' on space ${spaceId.value}"))

  private def correlatedScenarioWriteGap[A]: IO[spi.MockError, A] =
    ZIO.fail(
      spi.MockError.InvalidDefinition(
        "per-flow scenario state writes (reset/setState) on a Correlated space need a per-flow " +
          "setState the rift.zio surface does not expose yet (rift-java#151) — use PerInstance " +
          "isolation, or read-only currentState"
      )
    )

  private val interceptImpl: spi.Intercept = RiftIntercept(interceptHandle, imposterOf)

  /**
   * The intercept listener is opt-in and lazy (the SPI contract): resolved (CA
   * files read, host validated) and acquired on first use into the adapter
   * layer's scope — memoized, torn down when the layer releases.
   */
  private def interceptHandle: IO[spi.MockError, InterceptHandle] =
    interceptCell.modifyZIO {
      case current @ Some(handle) => ZIO.succeed((handle, current))
      case None =>
        for
          config <- interceptSettings.resolve
          handle <- scope.extend(rift.intercept(config)).mapError(M.toMockError(None))
        yield (handle, Some(handle))
    }

  /**
   * The imposter behind a space (a Correlated space's is the shared one) — the
   * redirect target for an intercept rule. A space this adapter never
   * provisioned is rejected rather than silently forwarded to a bogus target.
   */
  private def imposterOf(space: spi.MockSpace): IO[spi.MockError, ImposterHandle] =
    spaces.get.flatMap(_.get(space.id) match
      case Some(rec: SpaceRec.PerInstance) => ZIO.succeed(rec.handle)
      case Some(rec: SpaceRec.Correlated)  => ZIO.succeed(rec.imposter)
      case None =>
        ZIO.fail(
          spi.MockError.InvalidDefinition(s"intercept redirect target ${space.id.value} is not a known Rift space")
        )
    )

  // ── provisioning internals ───────────────────────────────────────────────────────────────────

  private def serveSpace(src: spi.NormalizedSource): IO[spi.MockError, spi.MockSpace] =
    mode match
      case RiftMode.PerInstance      => servePerInstance(src)
      case RiftMode.Correlated(corr) => serveCorrelated(src, corr)

  private def servePerInstance(src: spi.NormalizedSource): IO[spi.MockError, spi.MockSpace] =
    src.payload match
      case spi.SourcePayload.Rules(rules) =>
        for
          portResolution   <- resolvePort(src.authoredPort)
          (portOpt, pooled) = portResolution
          tagged           <- tagAll(rules)
          // A throw here is more likely an adapter/SDK defect than bad user input — the rules were
          // already validated on the way to `StubBuilder`s — so the class name is prefixed to make
          // that distinguishable from genuine InvalidDefinition callers, on top of the null-safe
          // message (#285/B10).
          builder <- ZIO
                       .attempt(tagged.foldLeft(M.imposterShell(src.name, portOpt, None))((b, t) => b.stub(t._2)))
                       .mapError(t => spi.MockError.InvalidDefinition(s"${t.getClass.getSimpleName}: ${M.message(t)}"))
          handle <- rift
                      .create(builder)
                      .mapError(M.toMockError(None))
                      .onError(_ => ZIO.whenDiscard(pooled)(portOpt.fold(ZIO.unit)(releasePortValue)))
          space <- registerPerInstance(src.name, handle, pooled, tagged.map(_._1).toSet)
        yield space
      case spi.SourcePayload.Raw(text) =>
        // A raw source is the portable `provision()` path (unlike `provisionNative`), so the
        // document's own port is stripped (`honourDocPort = false`) and, like the Rules branch,
        // this space must still get a fresh port under a container/connect pool — otherwise the
        // imposter binds a port Docker never published and is host-unreachable (#285/B2).
        for
          parsed           <- ZIO.fromEither(M.fromRaw(text, honourDocPort = false))
          portResolution   <- resolvePort(src.authoredPort)
          (portOpt, pooled) = portResolution
          port             <- ZIO.foreach(portOpt)(p => ZIO.fromEither(Port.from(p)).mapError(spi.MockError.InvalidDefinition(_)))
          definition        = parsed.copy(port = port)
          handle <- rift
                      .create(definition)
                      .mapError(M.toMockError(None))
                      .onError(_ => ZIO.whenDiscard(pooled)(portOpt.fold(ZIO.unit)(releasePortValue)))
          space <- registerPerInstance(src.name, handle, pooled)
        yield space

  /**
   * Resolve the port an imposter binds on and whether it was drawn from
   * `portPool` (so `destroy` must return it): an authored port is validated +
   * claimed against the pool when one exists (so a concurrent auto-provision
   * can't collide, and an out-of-pool container port fails fast rather than
   * dying deep inside the SDK's hostResolver); no authored port draws a fresh
   * pool port when a pool exists, or leaves the imposter's port unset
   * (engine-assigned) when it doesn't.
   */
  private def resolvePort(authoredPort: Option[Int]): IO[spi.MockError, (Option[Int], Boolean)] =
    (authoredPort, portPool) match
      case (Some(p), Some(pool)) => pool.ensureInRange(p) *> pool.claimPort(p).map(claimed => (Some(p), claimed))
      case (Some(p), None)       => ZIO.succeed((Some(p), false))
      case (None, Some(pool))    => pool.acquirePort.map(p => (Some(p), true))
      case (None, None)          => ZIO.succeed((None, false))

  private def releasePort(handle: ImposterHandle): UIO[Unit] =
    portPool.fold(ZIO.unit)(_.releasePort(Port.value(handle.port)))

  // Total release-by-value counterpart to the above, for a port that never made it into an
  // ImposterHandle (a rollback before `rift.create` succeeded). `portPool.get` would be partial —
  // this stays total even if `pooled` is ever miscomputed, rather than dying as a defect (#285/B11).
  private def releasePortValue(port: Int): UIO[Unit] =
    portPool.fold(ZIO.unit)(_.releasePort(port))

  private def registerPerInstance(
    name: String,
    handle: ImposterHandle,
    pooled: Boolean,
    ruleIds: Set[spi.RuleId] = Set.empty
  ): IO[spi.MockError, spi.MockSpace] =
    for
      ids  <- Ref.make(ruleIds)
      scen <- Ref.make(Map.empty[String, spi.ScenarioState])
      id    = spi.SpaceId(s"$name-${Port.value(handle.port)}")
      space = spi.MockSpace(handle.uri.toString, identity, id)
      _    <- spaces.update(_.updated(id, SpaceRec.PerInstance(handle, ids, scen, pooled)))
    yield space

  private def serveCorrelated(src: spi.NormalizedSource, corr: Correlation): IO[spi.MockError, spi.MockSpace] =
    src.payload match
      case spi.SourcePayload.Raw(_) =>
        ZIO.fail(
          spi.MockError.InvalidDefinition(
            "Correlated isolation needs portable rule sources; provision a raw Rift imposter via provisionNative"
          )
        )
      case spi.SourcePayload.Rules(rules) =>
        for
          imposter   <- ensureShared(corr.header)
          n          <- counter.updateAndGet(_ + 1)
          flowRaw     = s"${src.name}-s$n"
          flowId     <- ZIO.fromEither(FlowId.from(flowRaw).left.map(spi.MockError.InvalidDefinition(_)))
          spaceHandle = imposter.space(flowId)
          tagged     <- tagAll(rules)
          _ <- ZIO
                 .foreachDiscard(tagged)((_, b) => spaceHandle.addStub(b))
                 .mapError(M.toMockError(None))
                 .onError(_ =>
                   spaceHandle.delete.catchAllCause(c =>
                     ZIO.logWarningCause(s"rollback: delete correlated space $flowRaw failed", c)
                   )
                 )
          state <- Ref.Synchronized.make(CorrState(tagged, Vector.empty, Vector.empty, Vector.empty))
          id     = spi.SpaceId(flowRaw)
          inject = (req: spi.HttpRequest) => req.copy(headers = req.headers.add(corr.header, corr.value(id)))
          space  = spi.MockSpace(imposter.uri.toString, inject, id)
          _     <- spaces.update(_.updated(id, SpaceRec.Correlated(imposter, spaceHandle, state)))
        yield space

  /**
   * The one shared Correlated imposter, created on first provision into the
   * layer scope (so it is torn down on release) — race-safe via the
   * synchronized cell.
   */
  private def ensureShared(header: String): IO[spi.MockError, ImposterHandle] =
    shared.modifyZIO {
      case current @ Some(handle) => ZIO.succeed((handle, current))
      case None =>
        for
          portResolution   <- resolvePort(None)
          (portOpt, pooled) = portResolution
          handle <-
            scope
              .extend(
                ZIO.acquireRelease(
                  rift
                    .create(M.imposterShell("correlated", portOpt, Some(header)))
                    .mapError(M.toMockError(None))
                )(handle =>
                  handle.delete.catchAllCause(c => ZIO.logWarningCause("shared correlated imposter teardown failed", c))
                )
              )
              // Give the claimed port back if the imposter never came up. Without this a transient
              // create failure shrinks the pool by one for the layer's lifetime — the pre-#285
              // `ensureSharedImposter` compensated the same way (#285/B16).
              .onError(_ => ZIO.whenDiscard(pooled)(portOpt.fold(ZIO.unit)(releasePortValue)))
        yield (handle, Some(handle))
    }

  /**
   * rift's space endpoint has no per-stub delete: mutations beyond a Base
   * append tear the space down and re-register the target set, extras and
   * faults ahead of the rules so they keep winning first-match. Server-first —
   * callers run this inside the space's synchronized state cell and commit only
   * on success. A mid-rebuild failure leaves the space's *server* stubs partial
   * (surfaced as the failing call's error) while the tracked state stays
   * unchanged; recovery is another mutation or `destroy`.
   */
  private def rebuildCorrelated(
    spaceId: spi.SpaceId,
    space: SpaceHandle,
    target: CorrState
  ): IO[spi.MockError, Unit] =
    for
      _ <- space.delete.mapError(M.toMockError(Some(spaceId)))
      _ <- ZIO
             .foreachDiscard(target.extras ++ target.faults ++ target.rules)((_, b) => space.addStub(b))
             .mapError(M.toMockError(Some(spaceId)))
      // Scenario stubs are raw `Stub`s (they carry a `ScenarioRef` the builder can't), so they
      // re-register through the other `addStub` overload; appended last, after the plain rules.
      _ <- ZIO
             .foreachDiscard(target.scenarios)((_, s) => space.addStub(s))
             .mapError(M.toMockError(Some(spaceId)))
    yield ()

  // ── shared plumbing ──────────────────────────────────────────────────────────────────────────

  private def withSpace[A](space: spi.MockSpace)(f: SpaceRec => IO[spi.MockError, A]): IO[spi.MockError, A] =
    spaces.get.flatMap(_.get(space.id) match
      case Some(rec) => f(rec)
      case None      => ZIO.fail(spi.MockError.SpaceNotFound(space.id))
    )

  private def freshRuleId: UIO[spi.RuleId] = counter.updateAndGet(_ + 1).map(n => spi.RuleId(s"r$n"))

  private def tagAll(
    rules: List[spi.MockRule]
  ): IO[spi.MockError, Vector[(spi.RuleId, StubBuilder[StubPhase.Complete])]] =
    ZIO.foreach(rules.toVector) { rule =>
      freshRuleId.flatMap(id => ZIO.fromEither(M.stubFor(rule, id)).map(b => (id, b)))
    }

private[rift] enum SpaceRec:
  case PerInstance(
    handle: ImposterHandle,
    ruleIds: Ref[Set[spi.RuleId]],
    scenarios: Ref[Map[String, spi.ScenarioState]],
    pooled: Boolean // the imposter's port came from a RiftPortPool -> return it on destroy
  )
  case Correlated(imposter: ImposterHandle, space: SpaceHandle, state: Ref.Synchronized[CorrState])

/**
 * Which capability tier a Correlated stub belongs to — both re-register ahead
 * of the base rules.
 */
private[rift] enum CorrTier:
  case Faults, Extras

/**
 * A Correlated space's tracked stub sets. Held in ONE `Ref.Synchronized` cell
 * so every mutation (which reads the sets, rebuilds the space over the network,
 * then commits) is serialized — two racing mutations on plain `Ref`s could each
 * rebuild from the same stale snapshot and the loser's commit would silently
 * drop the winner's rule.
 */
private[rift] final case class CorrState(
  rules: Vector[(spi.RuleId, StubBuilder[StubPhase.Complete])],
  faults: Vector[(spi.RuleId, StubBuilder[StubPhase.Complete])],
  extras: Vector[(spi.RuleId, StubBuilder[StubPhase.Complete])],
  // Scenario-FSM stubs (raw `Stub`s carrying a `ScenarioRef`) installed by `define`. Tracked so a
  // later mutation's `rebuildCorrelated` re-registers them instead of silently dropping the scenario.
  scenarios: Vector[(spi.RuleId, Stub)]
)

private[rift] object RiftMockControl:

  /**
   * Build the adapter against an already-acquired SDK `rift.zio.Rift`, in the
   * given isolation `mode`. Scoped: the shared Correlated imposter (and, once
   * started, the intercept listener) are torn down when the scope closes.
   * `portPool` is `Some` for a transport whose imposters must bind a
   * pre-declared port (container, or a `connect` to a pooled remote engine);
   * `None` lets the engine assign ports (embedded, or a bare-metal `connect`).
   * `interceptCapable` says whether the intercept listener is actually
   * host-reachable for this transport — see [[RiftMockControl.capabilities]].
   */
  def make(
    rift: SdkRift,
    portPool: Option[RiftPortPool],
    mode: RiftMode,
    interceptSettings: InterceptSettings,
    interceptCapable: Boolean
  ): URIO[spi.Provisioning & Scope, spi.MockControl] =
    for
      provisioning  <- ZIO.service[spi.Provisioning]
      scope         <- ZIO.scope
      spaces        <- Ref.make(Map.empty[spi.SpaceId, SpaceRec])
      shared        <- Ref.Synchronized.make(Option.empty[ImposterHandle])
      interceptCell <- Ref.Synchronized.make(Option.empty[InterceptHandle])
      counter       <- Ref.make(0)
      control = RiftMockControl(
                  rift,
                  portPool,
                  provisioning,
                  mode,
                  scope,
                  spaces,
                  shared,
                  interceptCell,
                  interceptSettings,
                  interceptCapable,
                  counter
                )
      // Release-time sweep: spaces the suite never destroy()ed are reclaimed when the adapter layer
      // closes. An embedded engine dies with its layer anyway; this is for a long-lived `connect`/
      // container engine, which would otherwise accumulate orphaned imposters run over run.
      _ <- ZIO.addFinalizer(
             spaces.get.flatMap(recs =>
               ZIO.foreachDiscard(recs.toVector) { (id, rec) =>
                 val cleanup = rec match
                   case r: SpaceRec.PerInstance => r.handle.delete
                   case r: SpaceRec.Correlated  => r.space.delete
                 cleanup.catchAllCause(c => ZIO.logWarningCause(s"release sweep: destroy space ${id.value} failed", c))
               }
             )
           )
    yield control
