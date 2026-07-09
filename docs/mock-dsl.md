# Mock DSL Reference

`zio.bdd.mock.dsl` is a set of fluent builders over the canonical mock model.
It never adds capability — every builder returns (or composes) one of the
plain value types from the model, so DSL output is structurally identical to
the same rule written by hand. Use it when you want type-checked, refactorable
stubs in Scala; use raw JSON sources when you're porting an existing
Mountebank/WireMock fixture (see §6).

Covers request matching, response, rule-id, and scenario builders. The
`intercept(host)` builder (`Capability.Intercept` rules) is documented
alongside that capability — see [Advanced](mock-advanced.md) §9.

---

## 1. Import + shape

```scala
import zio.bdd.mock.dsl.*
```

A rule pairs a request matcher with a response:

```scala
<request>.respondWith(<response>)   // => MockRule
```

Assemble rules into a spec, then wrap the spec as a source your backend can
provision:

```scala
mock(rule1, rule2, rule3)    // MockSpec  — rules.toList
mock(rule1, rule2).source    // MockSource.Dsl(spec)
dslSource(rule1, rule2)      // shorthand for mock(rules*).source
```

`MockSpec` also carries an opt-in fixed `port` (`.onPort(8080)`, #211): when set,
the backend binds the imposter on exactly that port instead of an auto-assigned
one; when omitted, a fresh free port is chosen. If a fixed port happens to fall
inside a backend's managed port pool (e.g. the container Rift adapter's pool), it
is claimed from that pool so an auto-assigned imposter can't collide with it (#213).

---

## 2. Matching requests

Start from a method + path entry point, or `anyRequest` for no constraint at
all:

```scala
get(path)      // Method.Get
post(path)     // Method.Post
put(path)      // Method.Put
delete(path)   // Method.Delete
patch(path)    // Method.Patch
head(path)     // Method.Head
options(path)  // Method.Options
request(method, path)  // any Method
anyRequest              // no method, no path constraint
```

Each entry point produces a `RequestMatch` with an **exact-path** matcher by
default. Refine it with `.where(...)`, passing one or more clauses:

```scala
get(path).where(header(h).equalTo(v)).respondWith(ok.text(body))

get(path).where(header(h).contains(v)).respondWith(ok.text(body))

get(path).where(query(q).matches(re)).respondWith(ok.text(body))

post(path).where(bodyEquals(b)).respondWith(ok.text(body))

post(path).where(bodyContains(b)).respondWith(ok.text(body))

post(path).where(jsonPath(jp).equalTo(v)).respondWith(ok.text(body))
```

(Verbatim from `Value03Suite` / `Body04Suite` in the samples.)

`header`/`query` return a `ValueClause` and `jsonPath` returns a
`JsonPathClause` — both `private[dsl]`, so you always go through these
builders rather than constructing a clause directly. Every clause builder
(including the body matchers) ultimately produces a `MatchClause`, an
`opaque type` over `RequestMatch => RequestMatch`.

To match on something other than an exact path, replace the path matcher with
`.withPath(...)`:

```scala
get("/ignored").withPath(PathMatch.Regex(pattern)).respondWith(ok.text(body))
get("/ignored").withPath(PathMatch.Template(tmpl)).respondWith(ok.text(body))
anyRequest.withPath(PathMatch.Any).respondWith(ok.text(body))
```

The `get("/ignored")` base is only there to pick the HTTP method — `withPath`
overwrites whatever path matcher the entry point set.

| Clause | Produces | Notes |
|---|---|---|
| `header(name).equalTo(v)` | `ValueMatch.Equals` | exact header value |
| `header(name).contains(v)` | `ValueMatch.Contains` | substring match |
| `header(name).matches(re)` | `ValueMatch.Matches` | regex match |
| `query(name).equalTo(v)` / `.contains(v)` / `.matches(re)` | same as above, on a query param | |
| `jsonPath(path).exists` | `BodyMatch.JsonPath(path, None)` | path must exist in body |
| `jsonPath(path).equalTo(v)` | `BodyMatch.JsonPath(path, Some(v))` | path's value must equal `v` |
| `bodyEquals(v)` | `BodyMatch.Equals(v)` | whole body equals `v` |
| `bodyContains(v)` | `BodyMatch.Contains(v)` | body contains `v` |
| `bodyMatches(re)` | `BodyMatch.Matches(re)` | body matches regex `re` |
| `xPath(path)` | `BodyMatch.XPath(path, None)` | XML path must exist |
| `.withPath(PathMatch.Regex(p))` | overrides the path matcher | anchors are backend-defined |
| `.withPath(PathMatch.Template(t))` | overrides the path matcher | e.g. `"/users/{id}"` |

`header`/`query` clauses stack per-name in `.where(...)` — only body clauses
are mutually exclusive (each rule has at most one `BodyMatch`; the last one in
`.where(...)` wins).

> **Headers reference.** `Headers` (`zio.bdd.mock.Headers`) is an opaque
> `Map[String, List[String]]` — keys are lower-cased **on construction**, so
> `header("X-Trace").equalTo(v)` and a recorded `X-Trace`/`x-trace` header
> match regardless of case.
>
> Construct one with `Headers.empty`, `Headers(pairs*)` (one value per key),
> `Headers.multi(entries*)` (several values per key), or
> `Headers.fromSingle(map)` (lift a single-valued map). There is no
> `Headers.of`.
>
> Query it with `values(key)`, `first(key)` (the head in insertion order),
> `contains(key)`, `keys`, `isEmpty`, `nonEmpty`, `toMultiMap`, and `entries`.
> There is no `get`/`getFirst`/`getAll`.
>
> ```scala
> val h = Headers.multi("Set-Cookie" -> List("a=1", "b=2"))
> h.values("set-cookie") // List("a=1", "b=2")
> h.first("Set-Cookie")  // Some("a=1")
> ```

---

## 3. Building responses

Two entry points:

```scala
ok             // ResponseDef(status = 200), empty body
status(code)   // ResponseDef(status = code), empty body
```

Refine with extension methods, chaining freely:

```scala
status(code).text(body)     // Body.Text
status(code).json(body)     // Body.Json
ok.base64(b64)              // Body.Base64
ok.withStatus(code)         // override the status after the fact
ok.withBody(Body.Empty)     // set an arbitrary Body value directly
ok.withHeader(name, value)  // append a response header
ok.withLatency(ms.millis)   // delay the response by a zio.Duration
```

Calling `.withHeader(name, v)` twice for the same name **appends** rather than
overwrites, producing a multi-value response header:

```scala
stageRule(get(path).respondWith(ok.withHeader(name, v1).withHeader(name, v2)))
```

Chained example from the samples (base64 body + delayed text):

```scala
stageRule(get(path).respondWith(ok.base64(b64)))

stageRule(get(path).respondWith(ok.text(body).withLatency(ms.millis)))
```

---

## 4. Rule ids + precedence

Give a rule an explicit id with `.withId(...)` so you can target it later
(e.g. with `MockControl#removeRule`):

```scala
extension (rule: MockRule) def withId(id: String): MockRule
```

```scala
val rule = get(path).respondWith(ok.text(body)).withId("greeting-stub")
```

Rules added via `MockControl#addRule` take a `Priority`, which decides
ordering when more than one rule matches the same request:

```scala
ctl(_.addRule(space, get(path).respondWith(ok.text(body)), Priority.Overlay))
```

`Priority.Overlay` rules sit above `Priority.Base` rules — an overlay added on
top of a base stub shadows it for matching requests until it's removed
(`removeRule`) or the base rules are swapped out wholesale (`replaceRules`).
See [Mocking overview](mocking.md) for how spaces and isolation relate to rule
precedence, and [Adapters](mock-adapters.md) for how each backend implements
overlay/base ordering.

---

## 5. Stateful scenario builder

`scenario(name)` builds a `ScenarioDef` — a single-token FSM — fluently. Each
`.when(state, request).respond(response)` edge fires while the scenario is in
`state` and an incoming request matches; it then either transitions
(`.goTo(nextState)`) or stays (`.stay`):

```scala
private def defineRetry(path: String) =
  scenario("retry")
    .startingAt("Started")
    .when("Started", get(path)).respond(status(503)).goTo("Attempt1")
    .when("Attempt1", get(path)).respond(status(503)).goTo("Attempt2")
    .when("Attempt2", get(path)).respond(ok.text("ok")).goTo("Done")
    .build
```

`.startingAt(state)` overrides the default initial state
(`ScenarioState.Started`). `.build` produces the `ScenarioDef` you hand to the
`StatefulScenarios` capability:

```scala
sc <- ZIO.serviceWithZIO[MockControl](_.scenarios)
_  <- sc.define(space, defineRetry(path))
```

The scenario runs entirely inside the backend once defined — the DSL only
builds the definition. See [Advanced](mock-advanced.md) for the
`StatefulScenarios` capability itself (`define`/`reset`/inspecting state) and
which adapters support it.

---

## 6. Raw JSON sources vs the DSL

`MockSource` is an enum with five cases; only `Dsl` goes through the builders
above:

```scala
enum MockSource:
  case Dsl(spec: MockSpec)
  case Json(raw: String)
  case Resource(path: String)
  case File(path: String)
  case Dir(path: String)
```

`Json`/`Resource`/`File`/`Dir` carry raw backend wire text (e.g. a Mountebank
imposter or WireMock mapping) instead of a `MockSpec`:

```scala
provisionSource(mock(get(path).respondWith(ok.text(body))).source)  // Dsl

provisionSource(MockSource.Resource("mocksources/rift/orders.json"))
provisionSource(MockSource.File(path))
provisionSource(MockSource.Dir(path))   // one space per file in the directory
```

A resource file looks like a plain imposter body:

```json
{
  "port": 0,
  "protocol": "http",
  "stubs": [
    {
      "predicates": [{ "equals": { "method": "GET", "path": "/from-resource" } }],
      "responses": [{ "is": { "statusCode": 200, "body": "resource-body" } }]
    }
  ]
}
```

All four raw variants still go through the same `Provisioning` normalization
path as `Dsl` — `port` is stripped/reassigned the same way. The practical
difference is portability: raw JSON is backend wire format (a Rift imposter
won't parse as a WireMock mapping), so a `File`/`Dir`/`Resource`/`Json` source
generally needs a `@rift`- or `@wiremock`-tagged scenario, while `Dsl` sources
run unmodified against any adapter that implements the base capabilities.

Reach for raw sources when a team already maintains Mountebank/WireMock
fixtures and porting them to the DSL isn't worth the churn, or when a fixture
needs adapter-native features the portable model doesn't expose. Reach for the
DSL for everything else — it's refactorable, type-checked, and portable across
backends.

Note: `NativeSpec` (`NativeSpec.Rift(imposterJson)` / `NativeSpec.WireMock(json)`)
looks similar but is **not** a `MockSource` — it's a separate, backend-pinned
path (`MockControl#provisionNative`) that bypasses normalization entirely.

---

Next: [Adapters](mock-adapters.md) · [Gherkin integration](mock-gherkin.md)
