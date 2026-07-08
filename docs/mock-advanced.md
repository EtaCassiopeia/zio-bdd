# Advanced Mocking: Capabilities, the Native Escape Hatch, Negotiation

Beyond the core `MockControl` port (provision/addRule/received/…) an adapter
may advertise optional capabilities: fault injection, stateful FSM scenarios,
direct state inspection, backend scripting, proxy/record, and response
templating. This page covers all six, the `provisionNative` escape hatch for
when the portable model isn't enough, and how a suite negotiates capabilities
so a missing one fails fast and by name instead of mid-scenario.

---

## 1. Capabilities overview

Every capability accessor on `MockControl` returns `IO[Unsupported, X]`: fetch
the capability instance first, then call its methods. If the backend doesn't
advertise the capability, the accessor fails fast with
`Unsupported(capability, backendName)` instead of the capability's method ever
running:

```scala
sc <- ZIO.serviceWithZIO[MockControl](_.scenarios).mapError(u => new RuntimeException(u.message))
_  <- sc.define(space, defineRetry(path))
```

| Capability | Accessor | Rift container | Rift embedded | WireMock |
|---|---|---|---|---|
| `Capability.Faults` | `faults: IO[Unsupported, Faults]` | yes | yes | yes |
| `Capability.StatefulScenarios` | `scenarios: IO[Unsupported, StatefulScenarios]` | yes | yes | yes |
| `Capability.StateInspection` | `stateInspection: IO[Unsupported, StateInspection]` | yes | yes | yes |
| `Capability.Scripting` | `scripting: IO[Unsupported, Scripting]` | yes | yes | no |
| `Capability.ProxyRecord` | `proxyRecord: IO[Unsupported, ProxyRecord]` | yes | yes | no |
| `Capability.Templating` | `templating: IO[Unsupported, Templating]` | yes | yes | no |
| `Capability.Intercept` | `intercept: IO[Unsupported, Intercept]` | opt-in¹ | yes | no |

Faults, StatefulScenarios, and StateInspection are portable across all three
adapters. Scripting, ProxyRecord, and Templating are Rift-only (container or
embedded — embedded is capability-complete, a pure backend swap for the
container, not a reduced subset); WireMock does not advertise them. `Intercept`
(built-in HTTPS intercept, §9) runs the TLS-MITM listener over the **embedded**
FFI always-on, and over the **container** adapter opt-in (#253) — only when
`Rift.managed`/`Rift.connect` is given an `interceptPort`/`interceptProxy`;
WireMock inherits the `Unsupported` fallback. See
[Adapters](mock-adapters.md) for the full per-adapter breakdown and
[Mocking overview](mocking.md) for the `MockControl` port these accessors sit
on.

¹ The container adapter advertises `Capability.Intercept` only when started with
an `interceptPort` (`Rift.managed`) or `interceptProxy` (`Rift.connect`); without
one, `intercept` returns `Unsupported`.

---

## 2. Faults

`Faults.inject` installs a fault for requests matching `m` in `space`,
returning the fault rule's id (removable via `MockControl#removeRule`). The
four connection kinds make the SUT's HTTP client observe a transport-level
failure; `FaultKind.LatencySpike` instead delays an otherwise normal 200
response. A fault takes precedence over a normal rule on the same match:

```scala
enum FaultKind:
  case ConnectionReset
  case EmptyResponse
  case MalformedChunk
  case RandomThenClose
  case LatencySpike(delay: Duration)
```

From `Faults11Suite` in the sample corpus:

```scala
Given("the backend supports faults") {
  requiring(Capability.Faults)
}

Given("a " / string / " fault is injected for GET " / string) { (kind: String, path: String) =>
  for
    space <- activeSpace
    fk    <- faultKind(kind)
    fs    <- ZIO.serviceWithZIO[MockControl](_.faults).mapError(u => new RuntimeException(u.message))
    _     <- fs.inject(space, get(path), fk).mapError(e => new RuntimeException(s"$e"))
  yield ()
}

Given("a latency fault of " / int / " millis is injected for GET " / string) { (ms: Int, path: String) =>
  for
    space <- activeSpace
    fs    <- ZIO.serviceWithZIO[MockControl](_.faults).mapError(u => new RuntimeException(u.message))
    _     <- fs.inject(space, get(path), FaultKind.LatencySpike(ms.millis)).mapError(e => new RuntimeException(s"$e"))
  yield ()
}
```

---

## 3. Stateful scenarios + state inspection

`StatefulScenarios` runs a single-token FSM per scenario: a request eligible
in the scenario's current state serves its response and (optionally)
transitions the state. Build the FSM with the `scenario(...)` DSL builder (see
[the DSL](mock-dsl.md) §5), then install it with `define` and rewind it with
`reset`:

```scala
private def defineRetry(path: String) =
  scenario("retry")
    .startingAt("Started")
    .when("Started", get(path)).respond(status(503)).goTo("Attempt1")
    .when("Attempt1", get(path)).respond(status(503)).goTo("Attempt2")
    .when("Attempt2", get(path)).respond(ok.text("ok")).goTo("Done")
    .build

Given("the backend supports stateful scenarios") {
  requiring(Capability.StatefulScenarios)
}

Given("a fail-twice-then-succeed scenario for GET " / string) { (path: String) =>
  for
    space <- activeSpace
    sc    <- ZIO.serviceWithZIO[MockControl](_.scenarios).mapError(u => new RuntimeException(u.message))
    _     <- sc.define(space, defineRetry(path)).mapError(e => new RuntimeException(s"$e"))
  yield ()
}

When("the scenario " / string / " is reset") { (name: String) =>
  for
    space <- activeSpace
    sc    <- ZIO.serviceWithZIO[MockControl](_.scenarios).mapError(u => new RuntimeException(u.message))
    _     <- sc.reset(space, name).mapError(e => new RuntimeException(s"$e"))
  yield ()
}
```

(Lifted from `Stateful12Suite`.)

`StateInspection` is split out from `StatefulScenarios` because not every
backend exposes direct state poking: `currentState` reads a scenario's
current state and `setState` forces a transition, both without driving any
request. From `StateInspect13Suite`:

```scala
Given("the backend supports state inspection") {
  requiring(Capability.StateInspection)
}

When("the state of " / string / " is forced to " / string) { (name: String, to: String) =>
  for
    space <- activeSpace
    s     <- ZIO.serviceWithZIO[MockControl](_.stateInspection).mapError(u => new RuntimeException(u.message))
    _     <- s.setState(space, name, ScenarioState(to)).mapError(e => new RuntimeException(s"$e"))
  yield ()
}

Then("the current state of " / string / " is " / string) { (name: String, expected: String) =>
  for
    space <- activeSpace
    s     <- ZIO.serviceWithZIO[MockControl](_.stateInspection).mapError(u => new RuntimeException(u.message))
    cur   <- s.currentState(space, name).mapError(e => new RuntimeException(s"$e"))
    _     <- Assertions.assertEquals(cur.value, expected, s"current state of '$name'")
  yield ()
}
```

---

## 4. Scripting

`Scripting.inject` installs a script that computes the response for requests
matching `m`, running on the backend's own scripting engine and able to read
the matched request. Rift only:

```scala
final case class Script(engine: ScriptEngine, code: String)

enum ScriptEngine:
  case Rhai, Lua, JavaScript
```

From `Scripting14Suite` — a Rhai script that echoes the request method into
the body (a static stub couldn't do this, so a passing round-trip proves the
engine actually ran the script):

```scala
private val rhaiEchoMethod =
  "fn should_inject(request, flow_store) { #{inject: true, fault: \"error\", status: 200, " +
    "body: `scripted-${request.method}`, headers: #{\"Content-Type\": \"text/plain\"}} }"

Given("the backend supports scripting") {
  requiring(Capability.Scripting)
}

Given("a " / string / " script echoing the method for GET " / string) { (engine: String, path: String) =>
  val eng = engine match
    case "Rhai"       => ScriptEngine.Rhai
    case "Lua"        => ScriptEngine.Lua
    case "JavaScript" => ScriptEngine.JavaScript
    case other        => throw new IllegalArgumentException(s"unknown engine '$other'")
  for
    space <- activeSpace
    sc    <- ZIO.serviceWithZIO[MockControl](_.scripting).mapError(u => new RuntimeException(u.message))
    _     <- sc.inject(space, get(path), Script(eng, rhaiEchoMethod)).mapError(e => new RuntimeException(s"$e"))
  yield ()
}
```

---

## 5. Proxy / record

`ProxyRecord.proxy` proxies requests matching `m` to a real `upstream`,
recording the first response and replaying it on subsequent calls — so the
SUT keeps getting the recorded response even after the upstream goes down.
Rift only. The recording is owned by the backend, not the port: once a
request has recorded a response, removing the proxy rule is unreliable, and
on a `Correlated` space any rule mutation rebuilds the space and discards the
recording — inject a proxy on its own space and don't mutate rules after the
first recorded call.

From `ProxyRecord15Suite`:

```scala
Given("the backend supports proxy/record") {
  requiring(Capability.ProxyRecord)
}

When("a proxy to " / string / " is installed for GET " / string) { (upstream: String, path: String) =>
  for
    space <- activeSpace
    pr    <- ZIO.serviceWithZIO[MockControl](_.proxyRecord).mapError(u => new RuntimeException(u.message))
    id    <- pr.proxy(space, get(path), upstream).mapError(e => new RuntimeException(s"$e"))
    _     <- Assertions.assertTrue(id.value.nonEmpty, "proxy rule id should be non-empty")
  yield ()
}
```

---

## 6. Templating

`Templating.inject` installs a templated response for requests matching `m`:
each `TemplateCapture` in the template pulls a value from the request and
substitutes its token into the response body. Rift only:

```scala
final case class ResponseTemplate(body: String, captures: List[TemplateCapture], status: Int = 200)
final case class TemplateCapture(token: String, source: TemplateSource, regex: String)

enum TemplateSource:
  case Path, Body
```

From `Templating16Suite`:

```scala
Given("the backend supports templating") {
  requiring(Capability.Templating)
}

Given("a template greeting the last path segment at " / string) { (pathRegex: String) =>
  for
    space <- activeSpace
    t     <- ZIO.serviceWithZIO[MockControl](_.templating).mapError(u => new RuntimeException(u.message))
    template = ResponseTemplate("Hello ${NAME}", List(TemplateCapture("${NAME}", TemplateSource.Path, "[^/]+$")))
    _ <- t.inject(space, RequestMatch(path = PathMatch.Regex(pathRegex)), template).mapError(e => new RuntimeException(s"$e"))
  yield ()
}

Given("a body-echo template for POST " / string) { (path: String) =>
  for
    space <- activeSpace
    t     <- ZIO.serviceWithZIO[MockControl](_.templating).mapError(u => new RuntimeException(u.message))
    template = ResponseTemplate("echo:${WHO}", List(TemplateCapture("${WHO}", TemplateSource.Body, "\\w+")))
    _ <- t.inject(space, post(path), template).mapError(e => new RuntimeException(s"$e"))
  yield ()
}
```

---

## 7. The `provisionNative` escape hatch

`provisionNative[B <: Backend](spec: NativeSpec[B])` stands up a space from a
raw, backend-specific document instead of the portable model — the escape
hatch for when you need one backend's native format:

```scala
enum NativeSpec[B <: Backend]:
  case Rift(imposterJson: String) extends NativeSpec[Backend.Rift]
  case WireMock(stubMappingJson: String) extends NativeSpec[Backend.WireMock]
```

`NativeSpec` is **not** a `MockSource` — it lives in `MockControl.scala` as a
separate path that bypasses normalization entirely. A `NativeSpec[Backend.Rift]`
only provisions against a Rift adapter and a `NativeSpec[Backend.WireMock]`
only against WireMock; the phantom `Backend` type parameter pins the spec to
one concrete backend at compile time, so the escape hatch is honest about the
portability it trades away. A natively-provisioned `MockSpace` still
participates in `destroy`/`received`/overlays/isolation exactly like a
portable one — only its *definition* is backend-pinned, so use it when you
need full-fidelity access to a backend's raw feature set (e.g. a Mountebank
imposter field or a WireMock stub-mapping option the canonical model doesn't
expose), not for anything you expect to run against a second adapter.

A malformed `NativeSpec` fails fast at provision-time — invalid JSON syntax is
rejected locally with `MockError.InvalidDefinition`, and a syntactically valid
document the backend itself refuses surfaces as `MockError.CommunicationError`
— but a document that's valid and accepted yet semantically wrong isn't caught
there at all: matching against it is delegated entirely to the backend's own
matcher, so the defect only shows up later as an unexpected match result, not
a typed error. Contrast the portable model's `MatchClause`, which is opaque
and never user-constructible outside the DSL builders (`header(..)`,
`bodyEquals(..)`, …), so a malformed match can't be built in the first place.

From `Native17Suite` — each scenario is tagged for its backend and the
harness runs the matching one:

```scala
Given("a native WireMock stub for GET " / string / " returning " / string) { (path: String, body: String) =>
  val json =
    s"""{"request":{"method":"GET","urlPath":"$path"},"response":{"status":200,"body":"$body"}}"""
  provisionNativeSpec(NativeSpec.WireMock(json))
}

Given("a native Rift imposter for GET " / string / " returning " / string) { (path: String, body: String) =>
  val json =
    s"""{"port":0,"protocol":"http","stubs":[{"predicates":[{"equals":{"method":"GET","path":"$path"}}],""" +
      s""""responses":[{"is":{"statusCode":200,"body":"$body"}}]}]}"""
  provisionNativeSpec(NativeSpec.Rift(json))
}

private def provisionNativeSpec[B <: Backend](spec: NativeSpec[B]) =
  for
    spaces <- ctl(_.provisionNative(spec))
    space  <- ZIO.fromOption(spaces.headOption).orElseFail(new IllegalStateException("no space"))
    _      <- Stage.put(space)
  yield ()
```

---

## 8. Capability negotiation

`require(Capability*): IO[Unsupported, Unit]` fails fast — before any rule is
staged or request sent — when a needed capability isn't in `capabilities:
Set[Capability]`, naming both the missing capability and the backend via
`Unsupported.message`. Run it at wiring/setup time so a backend gap surfaces
immediately, not mid-scenario; the typed accessors (`faults`, `scripting`, …)
remain the same per-call gate `require` just moves earlier.

From `Negotiation19Suite` — verifying negotiation itself, not assuming it:

```scala
Then("the backend advertises capability " / string) { (name: String) =>
  ZIO
    .serviceWith[MockControl](_.capabilities.contains(cap(name)))
    .flatMap(has => Assertions.assertTrue(has, s"backend does not advertise $name"))
}

Then("requiring " / string / " succeeds") { (name: String) =>
  ZIO.serviceWithZIO[MockControl](_.require(cap(name))).mapError(u => new RuntimeException(u.message)).unit
}

When("requiring " / string / " is attempted") { (name: String) =>
  ZIO
    .serviceWithZIO[MockControl](_.require(cap(name)).either)
    .flatMap {
      case Left(u)  => Stage.put(RequireOutcome(failed = true, u.message))
      case Right(_) => Stage.put(RequireOutcome(failed = false, ""))
    }
}

Then("it fails naming the missing capability " / string) { (name: String) =>
  Stage
    .get[RequireOutcome]
    .mapError(e => new IllegalStateException(e.message))
    .flatMap(o => Assertions.assertTrue(o.failed && o.message.contains(name), s"outcome=$o"))
}
```

A portable suite that needs, say, Scripting `require`s it up front: against
Rift (container or embedded) that succeeds and the suite runs; against
WireMock it fails with `Unsupported`, and the harness's per-backend tag
exclusion is what turns that into a clean SKIP instead of a failed run — the
capability gap is real, not a bug to route around in step code. The
conformance suite in the sample corpus is the executable spec of "correctly
implements `MockControl`": every adapter runs the same portable scenarios,
and the negotiation/exclusion machinery above is what lets a capability
adapters don't share (Scripting/ProxyRecord/Templating on WireMock) skip
cleanly rather than fail the suite.

---

Next: [Cookbook](mock-cookbook.md)

See also: [Mocking overview](mocking.md) · [the DSL](mock-dsl.md) ·
[Adapters](mock-adapters.md)

---

## 9. HTTPS intercept — drop the mitmproxy container (`Capability.Intercept`)

When a SUT **hard-codes** an external HTTPS host you cannot change
(`https://cdn.example.com/…`), the usual trick is to stand up a separate
**mitmproxy** container that TLS-MITMs that host and redirects it to a mock. The
embedded Rift provider does this **in-process, with no extra container**: it runs
a TLS-MITM forward-proxy, mints a per-host leaf cert signed by its own CA, and
either forwards the intercepted host to a mock space or answers it inline.

The MITM constraint is intrinsic — the SUT must trust the intercept CA — so the
goal is to *provision* that trust, not eliminate it: `trustStore` hands you a
ready-to-use JVM truststore (JKS by default) + password, and `proxyPort` is the
loopback port the SUT proxies through. Starting the listener is lazy and opt-in:
a suite that never calls `mc.intercept` pays nothing.

> **Fail loudly in a hermetic CI suite.** The embedded specs guard on
> `EmbeddedRift.available` and **SKIP** (pass) when no `librift_ffi` resolves —
> right for a cross-platform matrix, but for a suite whose whole point is to
> exercise intercept, a missing/misconfigured natives jar would then read as a
> green run that never ran. Guard the setup so it fails loudly instead:
> `EmbeddedRift.requireAvailable` (a `no-op` when a native resolves; otherwise a
> `MockError.ProvisionFailed` naming the host `os-arch` and how to fix it — add
> the `zio-bdd-rift-embedded-natives` dependency or set `-Drift.ffi.lib`). Use
> the boolean `EmbeddedRift.available` only where a SKIP is genuinely desired.

Build rules with the DSL — `intercept(host).redirectTo(space)` or
`.respondWith(stub)` — and apply them through the capability:

```scala
import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import zio.bdd.mock.rift.embedded.EmbeddedRift

// A layer for the in-process engine — no Docker, no mitmproxy.
val mock = Provisioning.live >>> EmbeddedRift.layer

val setUp: ZIO[MockControl, Throwable, TrustStore] =
  for
    mc     <- ZIO.service[MockControl]
    // the imposter that will answer the intercepted host
    spaces <- mc.provision(MockSource.Resource("cdn-config.json")).mapError(e => new RuntimeException(e.toString))
    ic     <- mc.intercept.mapError(u => new RuntimeException(u.message))
    // redirect the hard-coded external host to that mock space
    _      <- ic.add(intercept("cdn.example.com").redirectTo(spaces.head))
              .mapError(e => new RuntimeException(e.toString))
    // (or answer inline, no imposter needed:)
    //   _ <- ic.add(intercept("cdn.example.com").respondWith(InterceptStub(200, body = Some("""{"cdn":"mocked"}"""))))
    ts     <- ic.trustStore().mapError(e => new RuntimeException(e.toString)) // JKS path + password
    port   <- ic.proxyPort.mapError(e => new RuntimeException(e.toString))    // the loopback proxy port
  yield ts
```

Wire the JVM SUT in one line each — trust the CA and route HTTPS through the
intercept port:

```
-Djavax.net.ssl.trustStore=<ts.path> -Djavax.net.ssl.trustStorePassword=<ts.password>
-Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=<port>
```

The SUT's call to `https://cdn.example.com/config.json` is intercepted, its TLS
terminated with a leaf cert the truststore trusts, and the request forwarded to
your mock imposter (or answered by the inline stub) — the mitmproxy container is
gone. See `EmbeddedInterceptSpec` for the runnable end-to-end version (a real
`java.net.http.HttpClient` driving the intercept).

**Isolation semantics.** `redirectTo` forwards to the target space's imposter
**port**; under PerInstance (the embedded default) each space has its own port,
so the redirect lands on exactly that space. `respondWith` is
isolation-independent. Because a Correlated space shares one imposter port and is
separated by a correlation header the intercepted request does not carry, use
PerInstance (the default) for host redirects.

> **Truststore format.** `trustStore` defaults to **PKCS#12** (the JVM's default
> keystore type since JDK 9); `TrustStoreFormat.Jks` is selectable. Both load as a
> `trustedCertEntry` the default `TrustManagerFactory` reads out of the box, so
> either works for a JVM SUT.

**CA-only vs. merged truststore.** `trustStore` exports a store containing **only**
the intercept CA. Pointing a SUT at it via `-Djavax.net.ssl.trustStore` *replaces*
the JVM's default trust store (`cacerts`) wholesale — the SUT trusts the mocked
hosts but **loses trust for every real HTTPS endpoint** (AWS, other downstreams).
That's exactly right for a **fully hermetic** SUT (every external call mocked). For
a **mostly hermetic** SUT that mocks one vendor yet still calls real infrastructure,
use `trustStoreWithSystemCAs`, which exports the intercept CA **plus** the JVM's
default trust anchors so both keep validating:

```scala
ts <- ic.trustStoreWithSystemCAs()   // intercept CA + the JVM's cacerts, one store
```

**Containerized SUT (bind a reachable interface).** The examples above bind the
intercept proxy to **loopback** — right when the SUT is a host process. When the
SUT runs in a **Docker container** while the test + engine run on the host, a
loopback socket refuses the container's gateway-originated connection. Bind a
wider interface with `InterceptConfig` (opt-in — the default stays `127.0.0.1`,
so nothing is exposed off loopback unless you ask):

```scala
import zio.bdd.mock.rift.embedded.EmbeddedRift.InterceptConfig

// Reachable from another container/host via the Docker host-gateway; pin the port
// so a compose-orchestrated SUT can be told its proxy target up front.
val mock = Provisioning.live >>> EmbeddedRift.layer(InterceptConfig(bindHost = "0.0.0.0", port = Some(8888)))
```

`bindHost` must be an IP literal — `0.0.0.0`, a specific NIC address, and the
like; a hostname such as `localhost` is rejected with
`MockError.InvalidDefinition` the first time the intercept listener starts
(#262).

`proxyEndpoint` reports the **actually-bound** host and port (not just the
loopback port), so you can log or inject them. Export the truststore **straight
to a bind-mounted path** the container reads via `to` (default is a temp file),
so there's no copy step — it also creates parent dirs and works with
`trustStoreWithSystemCAs`:

```scala
// /host/mnt is bind-mounted to /mnt in the SUT container:
ts <- ic.trustStore(to = Some(java.nio.file.Path.of("/host/mnt/intercept-truststore.p12")))
```

The SUT container points its HTTPS proxy at the Docker host-gateway and trusts
that mounted truststore:

```
# in the SUT container (host-gateway = host.docker.internal on Docker Desktop):
-Djavax.net.ssl.trustStore=/mnt/intercept-truststore.p12 -Djavax.net.ssl.trustStorePassword=<pw>
-Dhttps.proxyHost=host.docker.internal -Dhttps.proxyPort=8888
```

Only the **one** intercept proxy port needs to be container-reachable: the engine
forwards intercepted traffic to the target space's imposter internally on
loopback, so imposter ports stay private. See the `bindHost 0.0.0.0` case in
`EmbeddedInterceptSpec` for a runnable check over a non-loopback interface.

**Committed CA — no run-model change (requires rift ≥ 0.11.3).** By default the
embedded proxy mints a **fresh CA on every start**, so a long-lived SUT container
can't trust it at JVM startup (the truststore doesn't exist until the test starts
the proxy). Point `InterceptConfig` at a **persistent, committed** CA instead — the
proxy loads it at runtime, so the truststore you exported and committed once already
matches:

```scala
val ca  = java.nio.file.Path.of("test/ca/intercept-ca.pem")   // committed once
val key = java.nio.file.Path.of("test/ca/intercept-ca.key")
val mock = Provisioning.live >>>
  EmbeddedRift.layer(InterceptConfig(bindHost = "0.0.0.0", port = Some(8888), caCert = Some(ca), caKey = Some(key)))
```

`caCert`/`caKey` are **both-or-neither** (supplying one fails with
`MockError.InvalidDefinition`); absent, the ephemeral default is unchanged. Now the
SUT container mounts the pre-exported truststore and starts normally via `docker
compose` — no test-owned container lifecycle, no two-phase ordering. See the
persistent-CA reuse case in `EmbeddedInterceptSpec` (two instances, one committed CA,
mutually-trusted leaves).

**CONTAINER variant (`Rift.managed`).** The containerized Rift adapter
(#253) advertises `Capability.Intercept` too, opt-in: pass a
container-internal `interceptPort` to `Rift.managed`, and testcontainers
exposes + maps it alongside the admin and imposter ports:

```scala
import zio.bdd.mock.rift.Rift

// Opt-in: without interceptPort, the container adapter doesn't advertise Intercept, exactly as before.
val mock = Provisioning.live >>> Rift.managed(interceptPort = Some(8474))
```

The `Intercept` capability accessor and the `add`/`redirectTo`/`respondWith`/
`trustStore`/`proxyPort` ergonomics are identical to the embedded provider — the
DSL example above works unchanged against this layer. The one difference is the
control plane: the container's intercept listener is started by the container
itself at boot (`--intercept-port`), not lazily on first use, and every call
here goes over the mapped admin port instead of an in-process FFI downcall. The
SUT proxies through the **host-mapped** port (`ic.proxyPort` reports it, already
translated), so it works the same whether the SUT is a host process or another
container reaching the published port. See `RiftInterceptSpec` (RIFT_IT-gated)
for the runnable end-to-end version against a real container.

---

## 10. Scoped overlays — `MockOverlay`

`MockOverlay` (#116) layers revertible rule mutation on top of the core
`MockControl` port. `scoped` installs each rule at `Priority.Overlay` (highest
priority, shadowing the base rules) on acquire and removes it on release, so
rules applied inside a `ZIO.scoped` block — in a hook, or a `When` step —
auto-revert exactly when the scope closes. Each rule registers its own
finalizer, so a failure partway through still tears down only the rules that
were actually added; a teardown failure is logged and skipped so one bad
removal can't strand its siblings. `remove` and `replaceAll` are the
non-scoped counterparts — targeted and wholesale mutation for steps that
permanently change a live space (e.g. "now everything times out").

```scala
import zio.bdd.mock.MockOverlay

ZIO.scoped {
  for
    _ <- MockOverlay.scoped(space)(MockRule(get("/flaky"), ResponseDef(status = 503)))
    _ <- runScenarioAgainstFlakyEndpoint(space) // sees the 503 overlay
  yield ()
} // scope closes here — the overlay rule is removed, base rules resume
```

`MockOverlay.remove(space, id)` deletes a single rule by its `RuleId`;
`MockOverlay.replaceAll(space, rules*)` swaps every rule in `space` at once.

---

## 11. Mock test aspects — `MockAspects`

`MockAspects` wraps `MockOverlay.scoped` as `TestAspect`s that override a
`MockSpace`'s behavior for one test and auto-revert afterward — mirroring
zio-openfeature's `TestFeatureProvider` delay/error aspects. `withLatency(d)`
adds delay `d` to every response in the space for the duration of the test;
`withImposter(rules*)` overlays an arbitrary set of rules as a scoped
imposter. Both require `MockControl & MockSpace` in the test environment.

```scala
import zio.bdd.mock.MockAspects.*

test("times out under latency") {
  ...
} @@ withLatency(2.seconds)
```
