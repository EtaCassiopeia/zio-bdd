package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.RiftProtocol
import zio.json.*
import zio.json.ast.Json

/**
 * The Correlated-space wire ops (rift#223) over the embedded engine's direct
 * C-ABI (rift#411) — the FFI-transport mirror of
 * [[zio.bdd.mock.rift.RiftCorrelatedSpace]] (which the container adapter drives
 * over HTTP). It builds the byte-identical Mountebank stub JSON via
 * [[RiftProtocol]] and registers / tears it down through the space FFI
 * downcalls, so wire fidelity is the same as the container adapter; only the
 * transport differs (no loopback HTTP admin plane, #244).
 *
 * Pure functions of `(engine, port, flowId, …)` — no adapter state — mirroring
 * `RiftCorrelatedSpace`'s shared-primitives pattern. Stub tracking (which
 * rules/faults/extras are registered under a space) stays in
 * [[EmbeddedRiftMockControl]]; only the wire calls live here.
 */
private[embedded] object EmbeddedCorrelatedSpace:

  /** Add one portable rule's stub under `(port, flowId)`. */
  def postStub(engine: EmbeddedEngine, port: Int, flowId: String, rule: MockRule): IO[MockError, Unit] =
    engine.spaceAddStub(port, flowId, RiftProtocol.stub(rule).toJson)

  /**
   * Add one pre-built stub (a capability stub: script/proxy/template) under
   * `(port, flowId)`.
   */
  def postRawStub(engine: EmbeddedEngine, port: Int, flowId: String, stub: Json): IO[MockError, Unit] =
    engine.spaceAddStub(port, flowId, stub.toJson)

  /** Add one fault stub under `(port, flowId)`. */
  def postFaultStub(
    engine: EmbeddedEngine,
    port: Int,
    flowId: String,
    m: RequestMatch,
    fault: FaultKind,
    id: RuleId
  ): IO[MockError, Unit] =
    engine.spaceAddStub(port, flowId, RiftProtocol.faultStub(m, fault, id).toJson)

  /** Register the tracked portable rules under `(port, flowId)`, in order. */
  def registerStubs(
    engine: EmbeddedEngine,
    port: Int,
    flowId: String,
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(rules)((rid, rule) => postStub(engine, port, flowId, rule.copy(id = Some(rid))))

  /** Register the tracked capability stubs under `(port, flowId)`, in order. */
  def registerRawStubs(
    engine: EmbeddedEngine,
    port: Int,
    flowId: String,
    extras: Vector[(RuleId, Json)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(extras)((_, stub) => postRawStub(engine, port, flowId, stub))

  /** Register the tracked fault stubs under `(port, flowId)`, in order. */
  def registerFaultStubs(
    engine: EmbeddedEngine,
    port: Int,
    flowId: String,
    faults: Vector[(RuleId, RequestMatch, FaultKind)]
  ): IO[MockError, Unit] =
    ZIO.foreachDiscard(faults)((rid, m, fault) => postFaultStub(engine, port, flowId, m, fault, rid))

  /**
   * Tear down the whole space — also clears its recorded requests + flow state.
   * Idempotent.
   */
  def deleteSpace(engine: EmbeddedEngine, port: Int, flowId: String): IO[MockError, Unit] =
    engine.spaceDelete(port, flowId)

  /**
   * rift#223 has no per-stub-in-space delete: tear the whole space down and
   * re-register `extras` (capability stubs) → `faults` → `rules`, in that
   * first-match order (capability stubs and faults must win first-match over a
   * normal rule).
   */
  def rebuild(
    engine: EmbeddedEngine,
    port: Int,
    flowId: String,
    extras: Vector[(RuleId, Json)],
    faults: Vector[(RuleId, RequestMatch, FaultKind)],
    rules: Vector[(RuleId, MockRule)]
  ): IO[MockError, Unit] =
    deleteSpace(engine, port, flowId) *>
      registerRawStubs(engine, port, flowId, extras) *>
      registerFaultStubs(engine, port, flowId, faults) *>
      registerStubs(engine, port, flowId, rules)

  /**
   * The requests recorded under `flowId` on the shared imposter, filtered by
   * the space's resolved flow id (the header-filtered `received`, rift#201 —
   * parity with the container adapter).
   */
  def received(engine: EmbeddedEngine, port: Int, flowId: String): IO[MockError, List[RecordedRequest]] =
    engine
      .spaceRecorded(port, flowId)
      .flatMap(json => ZIO.fromEither(RiftProtocol.parseRequestsArray(json)).mapError(MockError.CommunicationError(_)))
