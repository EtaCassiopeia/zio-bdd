package zio.bdd.mock

import zio.Duration

/**
 * Portable fluent builders over the canonical mock model (#112).
 *
 * The DSL is pure convenience: every builder returns one of the canonical value
 * types from `model.scala` (or composes them), so DSL output is *structurally
 * identical* to the same model written by hand. Nothing here is required — a
 * suite can be authored entirely from raw `.json` sources via [[MockSource]]
 * and still drive every mutation/assertion on [[MockControl]]. Whatever the DSL
 * produces normalises through the one [[Provisioning]] path to the same wire
 * contract an adapter consumes.
 *
 * Import the whole object to use it: `import zio.bdd.mock.dsl.*`.
 */
object dsl:

  /**
   * A refinement applied to a [[RequestMatch]] — the value `header(..)`,
   * `query(..)`, `jsonPath(..)` and the body matchers produce. Kept opaque so
   * it reads as a first-class clause in signatures while remaining a plain
   * model transform under the hood.
   */
  opaque type MatchClause = RequestMatch => RequestMatch

  // --- request entry points -------------------------------------------------

  /** Match `method` against the exact `path`. */
  def request(method: Method, path: String): RequestMatch =
    RequestMatch(method = Some(method), path = PathMatch.Exact(path))

  def get(path: String): RequestMatch     = request(Method.Get, path)
  def post(path: String): RequestMatch    = request(Method.Post, path)
  def put(path: String): RequestMatch     = request(Method.Put, path)
  def delete(path: String): RequestMatch  = request(Method.Delete, path)
  def patch(path: String): RequestMatch   = request(Method.Patch, path)
  def head(path: String): RequestMatch    = request(Method.Head, path)
  def options(path: String): RequestMatch = request(Method.Options, path)

  /** Match any request — no method, no path constraint. */
  val anyRequest: RequestMatch = RequestMatch()

  // --- value-match clause builders ------------------------------------------

  /** A header/query clause: `header("Authorization").matches("Bearer .*")`. */
  final class ValueClause private[dsl] (attach: ValueMatch => MatchClause):
    def equalTo(value: String): MatchClause  = attach(ValueMatch.Equals(value))
    def contains(value: String): MatchClause = attach(ValueMatch.Contains(value))
    def matches(regex: String): MatchClause  = attach(ValueMatch.Matches(regex))

  def header(name: String): ValueClause =
    ValueClause(m => rm => rm.copy(headers = rm.headers.updated(name, m)))

  def query(name: String): ValueClause =
    ValueClause(m => rm => rm.copy(query = rm.query.updated(name, m)))

  /** A JSON-path body clause: `jsonPath("$.a").exists`. */
  final class JsonPathClause private[dsl] (path: String):
    def exists: MatchClause                 = body(BodyMatch.JsonPath(path, None))
    def equalTo(value: String): MatchClause = body(BodyMatch.JsonPath(path, Some(value)))

  def jsonPath(path: String): JsonPathClause = JsonPathClause(path)

  def bodyEquals(value: String): MatchClause   = body(BodyMatch.Equals(value))
  def bodyContains(value: String): MatchClause = body(BodyMatch.Contains(value))
  def bodyMatches(regex: String): MatchClause  = body(BodyMatch.Matches(regex))
  def xPath(path: String): MatchClause         = body(BodyMatch.XPath(path, None))

  private def body(m: BodyMatch): MatchClause = rm => rm.copy(body = Some(m))

  // --- request refinement ---------------------------------------------------

  extension (rm: RequestMatch)
    /** Refine the match by folding the clauses on, left to right. */
    def where(clauses: MatchClause*): RequestMatch =
      clauses.foldLeft(rm)((acc, c) => c(acc))

    /** Replace the path matcher (use for regex/template matching). */
    def withPath(m: PathMatch): RequestMatch = rm.copy(path = m)

    /** Pair this match with a response to form a rule. */
    def respondWith(response: ResponseDef): MockRule = MockRule(rm, response)

  // --- response entry points + refinement -----------------------------------

  /** A 200 OK response with an empty body. */
  val ok: ResponseDef = ResponseDef(status = 200)

  /** A response with the given status and an empty body. */
  def status(code: Int): ResponseDef = ResponseDef(status = code)

  extension (r: ResponseDef)
    def json(value: String): ResponseDef   = r.copy(body = Body.Json(value))
    def text(value: String): ResponseDef   = r.copy(body = Body.Text(value))
    def base64(value: String): ResponseDef = r.copy(body = Body.Base64(value))
    def withBody(b: Body): ResponseDef     = r.copy(body = b)
    def withStatus(code: Int): ResponseDef = r.copy(status = code)
    // Appends, so calling it twice for one key emits a multi-value response header (#162).
    def withHeader(name: String, value: String): ResponseDef =
      r.copy(headers = r.headers.add(name, value))
    def withLatency(d: Duration): ResponseDef = r.copy(delay = Some(d))

  // --- rule + spec assembly -------------------------------------------------

  extension (rule: MockRule) def withId(id: String): MockRule = rule.copy(id = Some(RuleId(id)))

  /** Assemble rules into a spec. */
  def mock(rules: MockRule*): MockSpec = MockSpec(rules.toList)

  extension (spec: MockSpec)
    /**
     * Set the advisory port (always stripped on provision — see [[MockSpec]]).
     */
    def onPort(port: Int): MockSpec = spec.copy(port = Some(port))

    /** Wrap this spec as a DSL [[MockSource]]. */
    def source: MockSource = MockSource.Dsl(spec)

  /** Build a DSL [[MockSource]] straight from rules. */
  def dslSource(rules: MockRule*): MockSource = mock(rules*).source

  // --- stateful scenario builder (#129) -------------------------------------
  // Fluent FSM builder: scenario("invoice").startingAt("Created")
  //   .when("Created", get("/x")).respond(ok.text("a")).goTo("Paid")
  //   .when("Paid", get("/x")).respond(ok.text("b")).stay
  //   .build  ->  a ScenarioDef structurally identical to the hand-written one.

  /**
   * Begin a [[ScenarioDef]] named `name`, starting in
   * [[ScenarioState.Started]].
   */
  def scenario(name: String): ScenarioBuilder = ScenarioBuilder(name, ScenarioState.Started, Vector.empty)

  final case class ScenarioBuilder private[dsl] (
    name: String,
    initial: ScenarioState,
    rules: Vector[StatefulRule]
  ):
    /** Override the initial state (default [[ScenarioState.Started]]). */
    def startingAt(state: String): ScenarioBuilder = copy(initial = ScenarioState(state))

    /**
     * Begin an edge that fires while the FSM is in `state` and a request
     * matches `request` (continue with `.respond`).
     */
    def when(state: String, request: RequestMatch): WhenStep = WhenStep(this, ScenarioState(state), request)

    def build: ScenarioDef = ScenarioDef(name, rules.toList, initial)

  final case class WhenStep private[dsl] (sb: ScenarioBuilder, whenState: ScenarioState, request: RequestMatch):
    /** ...serves `response` (continue with `.goTo`/`.stay`). */
    def respond(response: ResponseDef): RespondedStep = RespondedStep(sb, whenState, request, response)

  final case class RespondedStep private[dsl] (
    sb: ScenarioBuilder,
    whenState: ScenarioState,
    request: RequestMatch,
    response: ResponseDef
  ):
    /** ...then transition to `state`. */
    def goTo(state: String): ScenarioBuilder = add(Some(ScenarioState(state)))

    /** ...and stay in the current state. */
    def stay: ScenarioBuilder = add(None)

    private def add(thenState: Option[ScenarioState]): ScenarioBuilder =
      sb.copy(rules = sb.rules :+ StatefulRule(whenState, request, response, thenState))
