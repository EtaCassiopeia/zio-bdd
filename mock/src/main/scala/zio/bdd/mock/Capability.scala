package zio.bdd.mock

import zio.IO

/**
 * Optional capabilities an adapter MAY implement beyond the total core port. An
 * adapter advertises a capability via [[MockControl.capabilities]]; for each
 * advertised capability its typed accessor returns an instance, and for each
 * un-advertised one the accessor fails fast with [[Unsupported]].
 */
enum Capability:
  case Faults, StatefulScenarios, StateInspection, Scripting, ProxyRecord, Templating, Intercept

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

/**
 * Built-in HTTPS intercept (#219): a TLS-MITM forward-proxy that intercepts
 * matched hosts and either forwards them to a mock space or answers them inline
 * — so a SUT that hard-codes an external HTTPS host (whose address you cannot
 * change) can be redirected to a mock without an external mitmproxy container.
 * Capability: [[Capability.Intercept]].
 *
 * The SUT is wired in two steps (both automated by this port): it trusts the
 * intercept CA (via the [[trustStore]] file + password) and points its HTTPS
 * proxy at [[proxyPort]] on loopback. The MITM constraint is intrinsic — the
 * SUT must trust the CA — so the goal is to *provision* that trust, not
 * eliminate it.
 *
 * Starting the intercept listener is opt-in and lazy: it happens on the first
 * call to any method here, so a suite that never intercepts pays nothing.
 */
trait Intercept:
  /**
   * The loopback port the SUT points its HTTPS proxy at. Starts the intercept
   * listener on first use (memoized), so repeat calls return the same port.
   */
  def proxyPort: IO[MockError, Int]

  /**
   * Install an intercept `rule` — build it with
   * `dsl.intercept(host).redirectTo(space)` / `.respondWith(stub)`. Rules are
   * first-match in installation order.
   */
  def add(rule: InterceptRule): IO[MockError, Unit]

  /**
   * Export a truststore containing the intercept CA to a fresh temp file,
   * returning its path + password — hand both to the SUT's JVM so it trusts the
   * intercept's per-host leaf certificates. Defaults to JKS: it loads as a JVM
   * truststore out of the box (a `trustedCertEntry` the default
   * `TrustManagerFactory` reads); PKCS#12 is selectable but see rift#417 —
   * rift's PKCS#12 export is not yet exposed as a trust anchor by the JVM's
   * `KeyStore`.
   */
  def trustStore(format: TrustStoreFormat = TrustStoreFormat.Jks): IO[MockError, TrustStore]

  /**
   * Convenience: intercept `host` and forward its requests to `space`'s
   * imposter.
   */
  final def redirectTo(host: String, space: MockSpace): IO[MockError, Unit] =
    add(InterceptRule.Redirect(host, space))

  /**
   * Convenience: intercept `host` and answer its requests inline with `stub`.
   */
  final def respondWith(host: String, stub: InterceptStub): IO[MockError, Unit] =
    add(InterceptRule.Serve(host, stub))

/** Response templating. Capability: [[Capability.Templating]]. */
trait Templating:
  /**
   * Install a templated response for requests matching `m`: each capture in
   * `template` pulls a value from the request and substitutes its token into
   * the response body. Returns the rule id.
   */
  def inject(space: MockSpace, m: RequestMatch, template: ResponseTemplate): IO[MockError, RuleId]
