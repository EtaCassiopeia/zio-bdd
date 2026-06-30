package zio.bdd.mock

import zio.IO

/**
 * Optional capabilities an adapter MAY implement beyond the total core port. An
 * adapter advertises a capability via [[MockControl.capabilities]]; for each
 * advertised capability its typed accessor returns an instance, and for each
 * un-advertised one the accessor fails fast with [[Unsupported]].
 */
enum Capability:
  case Faults, StatefulScenarios, StateInspection, Scripting, ProxyRecord, Templating

/**
 * How a backend keeps mock spaces isolated under parallel features/scenarios.
 *   - PerInstance: a unique base URI per space (e.g. Rift's own port).
 *   - Correlated: a shared base URI partitioned by a correlation header (e.g.
 *     WireMock).
 *
 * The behaviour itself lives in [[MockSpace.inject]] (which hides the mode from
 * the Gherkin); an adapter reports its mode via [[MockControl.isolation]]
 * (#123).
 */
enum Isolation:
  case PerInstance, Correlated

// Capability interfaces returned by the typed accessors on [[MockControl]].
// Faults/Scripting/ProxyRecord/Templating remain markers until their M3 issues.

/**
 * Fault injection (latency, errors, connection resets). Capability:
 * [[Capability.Faults]].
 */
trait Faults:
  /**
   * Inject `fault` for requests matching `m` in `space`, returning the id of
   * the fault rule (removable via [[MockControl.removeRule]]). The four
   * connection kinds make the SUT's client observe a transport-level failure;
   * [[FaultKind.LatencySpike]] delays an otherwise normal 200 response. The
   * fault takes precedence over a normal rule on the same match.
   */
  def inject(space: MockSpace, m: RequestMatch, fault: FaultKind): IO[MockError, RuleId]

/**
 * Single-token FSM per scenario (#129): a request eligible in the scenario's
 * current state serves its response and (optionally) transitions the state.
 * Capability: [[Capability.StatefulScenarios]].
 */
trait StatefulScenarios:
  /**
   * Install (replacing any existing) the named scenario on `space`, in its
   * initial state.
   */
  def define(space: MockSpace, scenario: ScenarioDef): IO[MockError, Unit]

  /** Return the named scenario on `space` to its initial state. */
  def reset(space: MockSpace, name: String): IO[MockError, Unit]

/**
 * Arrange/assert scenario state without driving requests — split from
 * [[StatefulScenarios]] because not every backend exposes direct state poking.
 * Capability: [[Capability.StateInspection]].
 */
trait StateInspection:
  /** The scenario's current state on `space`. */
  def currentState(space: MockSpace, name: String): IO[MockError, ScenarioState]

  /** Force the scenario on `space` to state `to`. */
  def setState(space: MockSpace, name: String, to: ScenarioState): IO[MockError, Unit]

/** Backend scripting hooks. Capability: [[Capability.Scripting]]. */
trait Scripting:
  /**
   * Install `script` to compute the response for requests matching `m`,
   * returning the rule id. The script runs on the backend's engine and may read
   * the matched request; a backend without a scripting engine does not
   * advertise this capability.
   */
  def inject(space: MockSpace, m: RequestMatch, script: Script): IO[MockError, RuleId]

/**
 * Proxy/record passthrough to a real upstream. Capability:
 * [[Capability.ProxyRecord]].
 */
trait ProxyRecord:
  /**
   * Proxy requests matching `m` to `upstream`, recording the first response and
   * replaying it on subsequent calls — so the SUT keeps getting the recorded
   * response even after the upstream goes down. Returns the rule id.
   *
   * The recording is owned by the backend, not this port: once a request has
   * recorded a response, removing the proxy rule via [[MockControl.removeRule]]
   * is unreliable (the backend may have inserted a recorded stub this port does
   * not track), and on a `Correlated` space any rule mutation rebuilds the
   * space and discards the recording. Inject a proxy on its own space and don't
   * mutate rules after the first recorded call.
   */
  def proxy(space: MockSpace, m: RequestMatch, upstream: String): IO[MockError, RuleId]

/** Response templating. Capability: [[Capability.Templating]]. */
trait Templating:
  /**
   * Install a templated response for requests matching `m`: each capture in
   * `template` pulls a value from the request and substitutes its token into
   * the response body. Returns the rule id.
   */
  def inject(space: MockSpace, m: RequestMatch, template: ResponseTemplate): IO[MockError, RuleId]
