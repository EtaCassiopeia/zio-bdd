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
 * HTTP headers with multi-value support (#162). Keys are canonicalised to
 * lower-case so cross-adapter assertions are well-defined — WireMock
 * lower-cases recorded keys, Rift preserves them, and this normalises both.
 * Values keep their insertion order per key.
 */
opaque type Headers = Map[String, List[String]]
object Headers:
  val empty: Headers = Map.empty

  /** One value per key — the common case (`Headers("Accept" -> "json")`). */
  def apply(pairs: (String, String)*): Headers =
    pairs.foldLeft(empty)((h, kv) => h.add(kv._1, kv._2))

  /**
   * Several values per key (`Headers.multi("Set-Cookie" -> List("a", "b"))`).
   */
  def multi(entries: (String, List[String])*): Headers =
    entries.foldLeft(empty)((h, kv) => kv._2.foldLeft(h)((acc, v) => acc.add(kv._1, v)))

  /** Lift a single-valued map (keys canonicalised). */
  def fromSingle(m: Map[String, String]): Headers =
    m.foldLeft(empty)((h, kv) => h.add(kv._1, kv._2))

  extension (h: Headers)
    /**
     * Append `value` under `key` (key lower-cased), preserving existing values.
     */
    def add(key: String, value: String): Headers =
      val k = key.toLowerCase
      h.updated(k, h.getOrElse(k, Nil) :+ value)
    def values(key: String): List[String]  = h.getOrElse(key.toLowerCase, Nil)
    def first(key: String): Option[String] = values(key).headOption
    def contains(key: String): Boolean     = h.contains(key.toLowerCase)
    def keys: Set[String]                  = h.keySet
    def isEmpty: Boolean                   = h.isEmpty
    def nonEmpty: Boolean                  = h.nonEmpty

    /** The underlying multimap (lower-cased keys, ordered values). */
    def toMultiMap: Map[String, List[String]] = h
    def entries: List[(String, List[String])] = h.toList

/**
 * Portable HTTP request the SUT issues. [[MockSpace.inject]] decorates it (e.g.
 * rewriting the base URI or adding a correlation header) so a mock space can be
 * reached under share-nothing isolation. SUT-side wiring lands in #117.
 */
final case class HttpRequest(
  method: Method,
  uri: String,
  headers: Headers = Headers.empty,
  body: Option[String] = None
)

/**
 * A request the backend recorded, used to assert what the SUT actually sent.
 */
final case class RecordedRequest(
  method: Method,
  uri: String,
  headers: Headers = Headers.empty,
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
  headers: Headers = Headers.empty,
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
