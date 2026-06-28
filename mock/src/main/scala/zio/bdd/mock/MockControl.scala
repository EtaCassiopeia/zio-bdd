package zio.bdd.mock

import zio.IO

/**
 * Type-level tag for a concrete backend, used to pin a [[NativeSpec]] to one
 * provider. Concrete backends (Rift, WireMock) define their own marker; the
 * native escape hatch is fleshed out in #119.
 */
trait Backend

/**
 * A backend-specific provisioning payload — the escape hatch that trades
 * portability for full access to one backend. Fleshed out in #119.
 */
trait NativeSpec[B <: Backend]

/**
 * The total core port every adapter MUST implement. This is the hinge of the
 * portable mocking feature (#109): steps, fixtures, overlays and the
 * conformance suite all program against this interface, never against a
 * concrete backend.
 *
 * Errors are values: the core operations fail with the typed [[MockError]] and
 * the capability accessors with the typed [[Unsupported]] — no `Throwable`
 * leaks into these signatures.
 */
trait MockControl:

  /** The optional capabilities this adapter advertises. */
  def capabilities: Set[Capability]

  /** Stand up one or more mock spaces from a portable source. */
  def provision(source: MockSource): IO[MockError, List[MockSpace]]

  /**
   * Stand up mock spaces from a backend-specific native spec (escape hatch).
   */
  def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]]

  /**
   * Add a rule to a space, returning its id. Overlay rules sit above base
   * rules.
   */
  def addRule(space: MockSpace, rule: MockRule, priority: Priority = Priority.Overlay): IO[MockError, RuleId]

  /** Remove a single rule from a space. */
  def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]

  /** Replace all rules of a space with the given set. */
  def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit]

  /** Tear down exactly this space — never a global reset. */
  def destroy(space: MockSpace): IO[MockError, Unit]

  /** The requests this space recorded, for assertions. */
  def received(space: MockSpace): IO[MockError, List[RecordedRequest]]

  // Typed capability accessors. Each returns an instance when the capability is
  // advertised, or fails fast with `Unsupported` (naming the gap and backend).

  def faults: IO[Unsupported, Faults]
  def scenarios: IO[Unsupported, StatefulScenarios]
  def stateInspection: IO[Unsupported, StateInspection]
  def scripting: IO[Unsupported, Scripting]
  def proxyRecord: IO[Unsupported, ProxyRecord]
  def templating: IO[Unsupported, Templating]
