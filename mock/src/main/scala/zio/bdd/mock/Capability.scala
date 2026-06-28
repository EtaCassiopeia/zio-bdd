package zio.bdd.mock

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
 * Marker type only at this stage; the per-adapter behaviour lands with the
 * isolation model in M2 (#123).
 */
enum Isolation:
  case PerInstance, Correlated

// Capability interfaces returned by the typed accessors on [[MockControl]].
// Empty markers here — their operations are fleshed out in M3 (#128/#129/#132).

/**
 * Fault injection (latency, errors, connection resets). Capability:
 * [[Capability.Faults]].
 */
trait Faults

/**
 * Single-state-token FSM per scenario. Capability:
 * [[Capability.StatefulScenarios]].
 */
trait StatefulScenarios

/**
 * Arrange/assert scenario state without driving requests. Capability:
 * [[Capability.StateInspection]].
 */
trait StateInspection

/** Backend scripting hooks. Capability: [[Capability.Scripting]]. */
trait Scripting

/**
 * Proxy/record passthrough to a real upstream. Capability:
 * [[Capability.ProxyRecord]].
 */
trait ProxyRecord

/** Response templating. Capability: [[Capability.Templating]]. */
trait Templating
