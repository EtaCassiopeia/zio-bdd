package zio.bdd.mock

import zio.Duration

/**
 * Backend-neutral canonical model for the portable MockControl SPI (#110).
 *
 * Every adapter (Rift, WireMock, …) normalises its own wire format to these
 * types, so Gherkin steps, hooks, overlays and assertions stay backend-neutral.
 * All types here are immutable value types — case classes and enums.
 */

/** HTTP method a rule matches or a recorded request used. */
enum Method:
  case Get, Post, Put, Delete, Patch, Head, Options, Trace, Connect

/** How a request path is matched. The default is [[PathMatch.Any]]. */
enum PathMatch:
  case Any
  case Exact(path: String)
  case Regex(pattern: String)
  case Template(template: String) // e.g. "/users/{id}"

/** How a single scalar value (query param or header) is matched. */
enum ValueMatch:
  case Equals(value: String)
  case Contains(value: String)
  case Matches(regex: String)

/** How a request body is matched. */
enum BodyMatch:
  case Equals(value: String)
  case Contains(value: String)
  case Matches(regex: String)
  case JsonPath(path: String, expected: Option[String] = None)
  case XPath(path: String, expected: Option[String] = None)

/**
 * A response body payload. Covers Text | Json | Base64 (plus the empty body).
 */
enum Body:
  case Empty
  case Text(value: String)
  case Json(value: String)
  case Base64(value: String)

/** Ordering bucket for a rule: overlay rules sit above base rules. */
enum Priority:
  case Base, Overlay

/** Opaque identifier for a rule within a [[MockSpace]]. */
opaque type RuleId = String
object RuleId:
  def apply(value: String): RuleId         = value
  extension (id: RuleId) def value: String = id

/** Opaque identifier for a [[MockSpace]] (the unit of isolation). */
opaque type SpaceId = String
object SpaceId:
  def apply(value: String): SpaceId         = value
  extension (id: SpaceId) def value: String = id

/**
 * Portable HTTP request the SUT issues. [[MockSpace.inject]] decorates it (e.g.
 * rewriting the base URI or adding a correlation header) so a mock space can be
 * reached under share-nothing isolation. SUT-side wiring lands in #117.
 */
final case class HttpRequest(
  method: Method,
  uri: String,
  headers: Map[String, String] = Map.empty,
  body: Option[String] = None
)

/**
 * A request the backend recorded, used to assert what the SUT actually sent.
 */
final case class RecordedRequest(
  method: Method,
  uri: String,
  headers: Map[String, String] = Map.empty,
  body: Option[String] = None
)

/**
 * What to match on an incoming request. Every field is optional/permissive by
 * default.
 */
final case class RequestMatch(
  method: Option[Method] = None,
  path: PathMatch = PathMatch.Any,
  query: Map[String, ValueMatch] = Map.empty,
  headers: Map[String, ValueMatch] = Map.empty,
  body: Option[BodyMatch] = None
)

/** The response a matched rule serves. */
final case class ResponseDef(
  status: Int = 200,
  headers: Map[String, String] = Map.empty,
  body: Body = Body.Empty,
  delay: Option[Duration] = None
)

/** A single stub: when a request matches, respond accordingly. */
final case class MockRule(
  `match`: RequestMatch,
  respond: ResponseDef,
  id: Option[RuleId] = None
)

/**
 * A scenario FSM state token. The default initial state is
 * [[ScenarioState.Started]].
 */
opaque type ScenarioState = String
object ScenarioState:
  val Started: ScenarioState                     = "Started"
  def apply(value: String): ScenarioState        = value
  extension (s: ScenarioState) def value: String = s

/**
 * One edge of a single-token scenario FSM (#129): while the scenario is in
 * `whenState` and a request matches `request`, serve `respond` and transition
 * to `thenState` — `None` stays in `whenState`. The portable subset both
 * adapters (#130 WireMock, #131 Rift) honour.
 */
final case class StatefulRule(
  whenState: ScenarioState,
  request: RequestMatch,
  respond: ResponseDef,
  thenState: Option[ScenarioState] = None
)

/** A named single-token FSM: starts in `initial`, advances via its `rules`. */
final case class ScenarioDef(
  name: String,
  rules: List[StatefulRule],
  initial: ScenarioState = ScenarioState.Started
)

/**
 * The unit of isolation. Hides *how* isolation is achieved:
 *   - PerInstance: a unique `baseUri` per space; `inject = identity`.
 *   - Correlated: a shared `baseUri`; `inject` stamps a correlation header.
 *
 * The SUT must target `baseUri` AND apply `inject` so the correlation rides
 * along. Isolation invariants and SUT wiring land in #117.
 */
final case class MockSpace(
  baseUri: String,
  inject: HttpRequest => HttpRequest,
  id: SpaceId
)
