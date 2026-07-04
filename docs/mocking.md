# Mocking with MockControl

`zio-bdd-mock` gives you one portable API for stubbing HTTP dependencies —
write a scenario's mocks once, then run the same scenario against a container,
an embedded process, or an in-JVM server without touching a line of it.

---

## 1. What MockControl is

`MockControl` is a backend-neutral SPI: a small, total port that every mocking
adapter implements identically, plus a set of optional capabilities an adapter
may advertise beyond the core. Code written against `MockControl` — Gherkin
steps, hooks, overlays, assertions — never imports a concrete backend.

Three adapters implement it today:

| Adapter | Transport | Isolation default | Capabilities |
|---|---|---|---|
| Rift container (`Rift.managed`/`Rift.connect`) | Docker container | PerInstance | all six |
| Rift embedded (`EmbeddedRift.layer`) | in-process (FFM) | PerInstance | all six (parity with container) |
| WireMock (`WireMock.correlated`/`.perInstance`) | in-process JVM | Correlated (via `.correlated`) | Faults, StatefulScenarios, StateInspection only |

The **cross-adapter portability guarantee**: any scenario built from the
portable model (`MockSource`, `RequestMatch`, `ResponseDef`, …) provisions,
matches, and tears down identically on every adapter that advertises the
capabilities it uses. Swapping backends is a one-line layer change — see
[Adapters](mock-adapters.md). A backend-pinned escape hatch (`provisionNative`)
exists for when you need one backend's native format; it trades away that
guarantee, so it's covered separately in [Advanced](mock-advanced.md).

A minimal example, lifted from the sample corpus — a step definition that
stages a stub rule and a suite that wires a `MockControl` layer to run it
against:

```scala
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

Given("a stub GET /hello returns 200 text " / string) { (body: String) =>
  stageRule(get("/hello").respondWith(ok.text(body)))
}
```

`get("/hello")` builds a `RequestMatch`, `.respondWith(ok.text(body))` pairs it
with a `ResponseDef` to form a `MockRule` — both are canonical model types
(§4), assembled with the fluent DSL (§5, full reference in
[the DSL](mock-dsl.md)). The suite itself only ever depends on `MockControl`:

```scala
object Basic01Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:
  // ...steps...
  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
```

`Backends.mockControl` picks Rift, embedded Rift, or WireMock at runtime from
an env var — the suite above never changes. See [Adapters](mock-adapters.md)
for how each backend's layer is constructed.

---

## 2. The `MockControl` trait

`MockControl` groups a handful of lifecycle/rule/verification operations that
every adapter implements, plus typed accessors for the optional capabilities.

### Lifecycle

| Method | Signature | Purpose |
|---|---|---|
| `provision` | `(source: MockSource): IO[MockError, List[MockSpace]]` | Stand up one or more mock spaces from a portable source (§5). |
| `provisionNative` | `[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]]` | Backend-pinned escape hatch — see [Advanced](mock-advanced.md). |
| `destroy` | `(space: MockSpace): IO[MockError, Unit]` | Tear down exactly this space, never a global reset. |

### Rules

| Method | Signature | Purpose |
|---|---|---|
| `addRule` | `(space: MockSpace, rule: MockRule, priority: Priority = Priority.Overlay): IO[MockError, RuleId]` | Add a rule; overlay rules sit above base rules. |
| `removeRule` | `(space: MockSpace, id: RuleId): IO[MockError, Unit]` | Remove a single rule by id. |
| `replaceRules` | `(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit]` | Replace all of a space's rules with the given set. |

### Verification

| Method | Signature | Purpose |
|---|---|---|
| `received` | `(space: MockSpace): IO[MockError, List[RecordedRequest]]` | The requests this space recorded, for assertions. |

### Capability accessors

Each accessor returns the capability instance when the adapter advertises it,
or fails fast with `Unsupported(capability, backendName)` otherwise. Gate a
suite's requirements up front with `require(Capability*): IO[Unsupported, Unit]`
(e.g. at layer-construction time) so a backend gap fails before any scenario
runs, not mid-scenario. Full behaviour of each capability is in
[Advanced](mock-advanced.md).

| Accessor | Returns | Capability |
|---|---|---|
| `faults` | `IO[Unsupported, Faults]` | `Capability.Faults` — inject transport failures or latency. |
| `scenarios` | `IO[Unsupported, StatefulScenarios]` | `Capability.StatefulScenarios` — per-scenario request/response FSM. |
| `stateInspection` | `IO[Unsupported, StateInspection]` | `Capability.StateInspection` — read/force FSM state directly. |
| `scripting` | `IO[Unsupported, Scripting]` | `Capability.Scripting` — compute responses with a backend script. |
| `proxyRecord` | `IO[Unsupported, ProxyRecord]` | `Capability.ProxyRecord` — proxy to and record from a real upstream. |
| `templating` | `IO[Unsupported, Templating]` | `Capability.Templating` — substitute captured request values into a response body. |

Two more members round out the port: `backendName: String` (named in every
`Unsupported` this adapter raises) and `capabilities: Set[Capability]` (what
it advertises).

---

## 3. `MockSpace` and `Isolation`

A `MockSpace` is the unit of isolation — one mock backend "instance" a
scenario provisions, targets, and destroys independently of every other space:

```scala
final case class MockSpace(
  baseUri: String,
  inject: HttpRequest => HttpRequest,
  id: SpaceId
)
```

- `baseUri` — where the SUT should send requests for this space.
- `inject` — decorates an outgoing `HttpRequest` (e.g. stamping a correlation
  header) so it reaches the right space under share-nothing isolation.
- `id` — the space's `SpaceId`, used to address it in adapter-internal state.

Steps never need to know *how* isolation works — they read `baseUri` and apply
`inject`, and the behaviour is correct under either model. The model itself is
one of two `Isolation` values, reported via `MockControl.isolation`:

- **`Isolation.PerInstance`** — each space gets a unique `baseUri`; `inject`
  is the identity. This is the default, and how both Rift adapters isolate
  (each space is its own imposter/port).
- **`Isolation.Correlated`** — every space in a run shares one `baseUri`;
  `inject` stamps a correlation header the backend uses to route the request
  to the right space's rules. This is how `WireMock.correlated` isolates
  without spinning up a server per space.

---

## 4. The canonical request/response model

Every adapter normalises its own wire format to this backend-neutral model
(`zio.bdd.mock.*`) before it reaches an adapter — Gherkin steps, hooks,
overlays and assertions program against these types only.

| Type | Meaning |
|---|---|
| `Method` | HTTP method enum: `Get`, `Post`, `Put`, `Delete`, `Patch`, `Head`, `Options`, `Trace`, `Connect`. |
| `PathMatch` | How a path is matched: `Any` (default), `Exact(path)`, `Regex(pattern)`, `Template(template)` (e.g. `"/users/{id}"`). |
| `ValueMatch` | How a single scalar (query param or header) is matched: `Equals`, `Contains`, `Matches(regex)`. |
| `BodyMatch` | How a request body is matched: `Equals`, `Contains`, `Matches(regex)`, `JsonPath(path, expected)`, `XPath(path, expected)`. |
| `Body` | A response body payload: `Empty`, `Text(value)`, `Json(value)`, `Base64(value)`. |
| `Priority` | Rule ordering bucket: `Base` or `Overlay` — overlay rules sit above base rules. |
| `RequestMatch` | What to match on an incoming request: `method`, `path` (default `Any`), `query`, `headers`, `body` — every field optional/permissive by default. |
| `ResponseDef` | The response a matched rule serves: `status` (default `200`), `headers`, `body` (default `Empty`), `delay`. |
| `MockRule` | One stub: `` `match`: RequestMatch ``, `respond: ResponseDef`, `id: Option[RuleId]`. |
| `RecordedRequest` | A request the backend recorded: `method`, `uri`, `headers`, `body` — what `received` returns. |
| `Headers` | Opaque multi-map type (lower-cased keys, ordered values per key) shared by requests, matches and responses. |

`ValueMatch`/`BodyMatch` (this model's matchers) are distinct from the DSL's
fluent `ValueClause` (§5/[the DSL](mock-dsl.md)) that builds them, and a
response's `Body` is distinct from a request matcher's `BodyMatch` — same
shape family, different purpose (serve vs. match).

---

## 5. `MockSource` — feeding mocks in

`MockSource` is where mock definitions come from. Every case normalises
through one internal provisioning path to a per-space normalized form before
reaching an adapter, so adapters never re-implement loading or auto-port
assignment:

```scala
enum MockSource:
  case Dsl(spec: MockSpec)
  case Json(raw: String)
  case Resource(path: String)
  case File(path: String)
  case Dir(path: String)
```

- **`Dsl(spec)`** — already-canonical rules built in-code via
  `zio.bdd.mock.dsl.*` (e.g. `mock(get("/hello").respondWith(ok)).source`).
- **`Json(raw)`** — a raw backend wire document, inline as a string.
- **`Resource(path)`** — a classpath resource holding a raw wire document.
- **`File(path)`** — a filesystem file holding a raw wire document.
- **`Dir(path)`** — a filesystem directory; each regular file becomes one
  space.

The `Json`/`Resource`/`File`/`Dir` cases stay unparsed at this layer — each
adapter parses its own wire format. `provision(source: MockSource)` is the
single portable way in; the backend-pinned `provisionNative(NativeSpec)` is a
separate, non-portable path (`NativeSpec` is *not* a `MockSource`) covered in
[Advanced](mock-advanced.md). Full DSL builder reference — `get`/`post`/…,
`.where(...)`, response builders, scenario builder — lives in
[the DSL](mock-dsl.md).

---

## 6. `MockError` — the typed failures

Core `MockControl` operations fail with the typed `MockError`, never a bare
`Throwable`:

```scala
enum MockError:
  case ProvisionFailed(reason: String)
  case SpaceNotFound(space: SpaceId)
  case RuleNotFound(space: SpaceId, rule: RuleId)
  case InvalidDefinition(reason: String)
  case CommunicationError(reason: String)
```

- **`ProvisionFailed`** — `provision`/`provisionNative` couldn't stand up a
  space (bad source, backend rejected the definition).
- **`SpaceNotFound`** — an operation addressed a `MockSpace` the backend
  doesn't know about (already destroyed, wrong adapter instance).
- **`RuleNotFound`** — `removeRule` addressed a `RuleId` that doesn't exist on
  that space.
- **`InvalidDefinition`** — a rule or source failed backend-side validation.
- **`CommunicationError`** — the adapter couldn't reach its backend (network,
  container, process).

Capability accessors use a separate, narrower error: `Unsupported(capability,
backend)` with a `.message`, raised when an adapter doesn't advertise a
requested capability — this is how WireMock rejects `require(Capability.Scripting)`
or a direct `.scripting` call, since it lacks Scripting/ProxyRecord/Templating.

---

Next: [the DSL](mock-dsl.md) · [Adapters](mock-adapters.md)
