# Mock Adapters

Three backends implement `MockControl`: a Docker-based Rift container, an
in-process WireMock server, and a no-Docker embedded Rift bound via FFM. This
page covers their coordinates, capabilities, and ZLayer wiring, and how to
pick one.

---

## 1. The three backends at a glance

| Adapter | Coordinates (1.4.3) | Docker? | JDK | Isolation default | Capabilities |
|---|---|---|---|---|---|
| Rift container | `zio-bdd-rift` | yes (testcontainers) | 17+ | PerInstance | all six always on; Intercept only with `interceptPort` |
| WireMock | `zio-bdd-wiremock` | no | 11+ | Correlated (via `.correlated`) | Faults, StatefulScenarios, StateInspection only |
| Rift embedded | `zio-bdd-rift` | no (FFM) | 17+ compile / 22+ runtime | PerInstance (default) / Correlated | all seven, always on |

Rift container, `connect`, and Rift embedded are three entry points of the
**same** published artifact and the same `RiftMockControl` adapter (#285
re-based the Rift adapters onto the official `rift-scala-zio` SDK, collapsing
what used to be four separate published modules into this one) — see §2 and
§4 for what each entry point needs.

### Capability × adapter matrix

| Capability | Rift container | Rift embedded | WireMock |
|---|---|---|---|
| Faults | yes | yes | yes |
| StatefulScenarios | yes | yes | yes |
| StateInspection | yes | yes | yes |
| Scripting | yes | yes | no |
| ProxyRecord | yes | yes | no |
| Templating | yes | yes | no |
| Intercept | only with `interceptPort`/`interceptProxy` | yes | no |

Rift container and Rift embedded implement the same seven capabilities, but
Intercept is transport-aware, not uniform: embedded always advertises it (the
listener starts in-process, on the host, so it's always reachable), while
container/`connect` advertise it only when the caller actually configured a
host-reachable listener (`Rift.managed`'s `interceptPort`, or `Rift.connect`'s
`interceptProxy`). Without one, the container's intercept listener would bind
inside the container's own network namespace — unreachable from a SUT outside
it — so `RiftMockControl` does not advertise the capability in that case: a
clean `Unsupported` beats an unreachable endpoint. Passing `interceptPort`/
`interceptProxy` is what makes `intercept` capability negotiation succeed in
the first place, not just what makes the resulting listener host-reachable
(§9 of [Advanced mocking](mock-advanced.md)). WireMock implements exactly the
three capabilities in the first column of the top table; requesting
`Capability.Scripting`, `Capability.ProxyRecord`, `Capability.Templating`, or
`Capability.Intercept` against it fails with `Unsupported` (§3 below, and see
[Mocking overview](mocking.md) for the `MockError`/`Unsupported` model).

---

## 2. Rift container (`zio-bdd-rift`)

```scala
"io.github.etacassiopeia" %% "zio-bdd-rift" % "1.4.3"
```

`zio-bdd-rift` is a single published artifact that covers the container
entry point, the bare `connect` entry point, and the no-Docker embedded
provider (§4) — all riding the official `rift-scala-zio` SDK (#285). Its JDK
floor is 17 (it depends on the SDK's `bridge` module, which links
`rift-java-core`, JDK-17 bytecode); every other zio-bdd module (core,
gherkin, mock, wiremock, conformance) stays on JDK 11.

Two entry points, both requiring only a `Provisioning` in the environment —
the SDK owns the transport, so no `zio-http` `Client` is needed (an intended
source break from the pre-#285 adapter):

- **`Rift.managed(...)`** starts the pinned Rift image via the SDK's
  testcontainers transport and stops it when the layer's scope closes:

  ```scala
  def managed(
    image: String = DefaultImage,
    poolSize: Int = DefaultPoolSize,
    adminPort: Int = DefaultAdminPort,
    imposterBasePort: Int = DefaultImposterBase,
    mode: RiftMode = RiftMode.PerInstance,
    interceptPort: Option[Int] = None
  ): ZLayer[Provisioning, MockError, MockControl]
  ```

  `DefaultImage` is the pinned Rift image, currently `zainalpour/rift-proxy:v0.14.0`
  — derived from the single `riftVersion` in `build.sbt`, so treat it as "the
  pinned Rift image" rather than a hardcoded tag. `adminPort` is effectively
  fixed at `DefaultAdminPort` (2525): the SDK's container transport
  (`RiftContainer`) hardcodes the admin port with no builder override, so a
  non-default `adminPort` now fails fast with a typed
  `MockError.InvalidDefinition` instead of silently taking whatever port you
  configured. Pass `interceptPort` to also start the container's
  HTTPS-intercept listener bound to `0.0.0.0` and have testcontainers publish
  it as a host-reachable port (§9 of [Advanced mocking](mock-advanced.md)) —
  without it, the listener would only ever be reachable inside the
  container's own network namespace, so `Capability.Intercept` is not
  advertised at all in that case (§1): pass `interceptPort` to get both a
  host-reachable listener and successful `intercept` capability negotiation.

- **`Rift.connect(...)`** targets an already-running Rift admin endpoint
  instead of starting a container (used by tests, or when Rift runs
  out-of-band):

  ```scala
  def connect(
    adminBase: String,
    imposterPorts: List[Int],
    mode: RiftMode = RiftMode.PerInstance,
    interceptProxy: Option[(String, Int)] = None
  )(hostFor: Int => String): ZLayer[Provisioning, MockError, MockControl]
  ```

  Widened from a `URLayer` pre-#285: the SDK's `connect` performs a real
  handshake (an admin-version check) at layer construction, so a bad
  `adminBase` or an unreachable engine now surfaces as a typed `MockError`
  there instead of only on first use. Like `Rift.managed`'s `interceptPort`,
  `interceptProxy` gates `Capability.Intercept`: omit it and the adapter does
  not advertise Intercept at all, since there'd be no host-reachable address
  to report.

Both take a `RiftMode`: `RiftMode.PerInstance` (default — one imposter port
per `MockSpace`) or `RiftMode.Correlated(correlation)` / the `RiftMode.correlated`
shorthand (one shared imposter, spaces tagged by a correlation header).

Wiring, lifted from the sample corpus's `support/Backends.scala`:

```scala
import zio.bdd.mock.*
import zio.bdd.mock.rift.Rift

val provisioning: ULayer[Provisioning] = ZLayer(Provisioning.make)

val mockControl: ZLayer[Any, Throwable, MockControl] =
  provisioning >>> Rift.managed().mapError(e => new RuntimeException(s"Rift: $e"))
```

See [layers](layers.md) for how this composes into a suite's `environment`.

---

## 3. WireMock (`zio-bdd-wiremock`)

```scala
"io.github.etacassiopeia" %% "zio-bdd-wiremock" % "1.4.3"
```

In-process, no Docker, runs on JDK 11+. Only requires `Provisioning`:

```scala
def correlated(correlation: Correlation = Correlation.spaceHeader): ZLayer[Provisioning, Throwable, MockControl]
val perInstance: ZLayer[Provisioning, Throwable, MockControl]
```

- **`WireMock.correlated()`** (default) — one shared server, spaces
  distinguished by a correlation header (`X-Mock-Space` by default via
  `Correlation.spaceHeader`); isolation is `Isolation.Correlated`.
- **`WireMock.perInstance`** — a fresh server per `MockSpace`; isolation is
  `Isolation.PerInstance`.

```scala
import zio.bdd.mock.wiremock.WireMock

val mockControl: ZLayer[Any, Throwable, MockControl] =
  provisioning >>> WireMock.correlated()
```

WireMock is the cheapest adapter to stand up but is capped at three
capabilities — Faults, StatefulScenarios, StateInspection. It has no
Scripting, ProxyRecord, or Templating support; code that calls
`.require(Capability.Scripting)` (or the equivalent direct accessor) against a
WireMock-backed `MockControl` fails with `Unsupported`, by design — that's
capability negotiation working as intended, not a bug to work around.

---

## 4. Embedded Rift (no-Docker, FFM)

The embedded provider drives the Rift engine in-process through the official
`rift-scala-zio` SDK's embedded transport over Project Panama FFM — no
container, no external process — while remaining **capability-complete**
(parity with the container adapter — all seven capabilities, always on). This
is the adapter to reach for when you want full fidelity without Docker.

`EmbeddedRift` (package `zio.bdd.mock.rift.embedded`) lives in the same
`zio-bdd-rift` artifact as the container/`connect` entry points (#285 folded
the old standalone `rift-embedded` module into `rift`) — there's no separate
coordinate to add.

### Runtime dependencies

Unlike the pre-#285 adapter, there's no zio-bdd-published natives jar to add.
The embedded provider resolves the SDK's own `rift-java-embedded` engine plus
a matching `rift-java-natives` classifier jar via Java's `ServiceLoader` at
runtime — ordinary Maven/sbt dependencies your build adds itself (these are
runtime `ServiceLoader` dependencies, not compile deps of `zio-bdd-rift`,
which only adds them at `Test` scope for its own specs):

```scala
libraryDependencies ++= Seq(
  "io.github.achird-labs" % "rift-java-embedded" % "0.1.3",
  ("io.github.achird-labs" % "rift-java-natives" % "0.1.3").classifier("darwin-aarch64")
)
```

Classifiers: `linux-x86_64`, `linux-aarch64`, `darwin-x86_64`,
`darwin-aarch64`. `project/RiftNatives.scala` in this repo exposes
`RiftNatives.currentClassifier`, which computes the host's classifier
programmatically for a `build.sbt` — the same helper `rift`'s own build uses.
An application wiring `EmbeddedRift.layer` into production or other test code
(beyond this repo's own specs) needs to add both dependencies itself.

If you'd rather point at a native library you built or installed yourself,
skip the natives classifier and set `-Drift.ffi.lib=<path>` instead.

### Test-JVM flags

The embedded provider needs `--enable-native-access` to silence the
restricted-method warning. It does **not** need `--enable-preview` — the
SDK's embedded engine targets JDK 22+ stable FFM only, with no
JDK-21-preview variant:

```scala
// build.sbt
Test / fork := true,
Test / javaOptions += "--enable-native-access=ALL-UNNAMED"
```

### Which JDK

`zio-bdd-rift` itself compiles targeting JDK 17, but the embedded *provider*
is a runtime dependency gated at the `ServiceLoader`/classload level: the
minimum JDK to actually use `EmbeddedRift` is 22 (stable FFM), what
`rift-java-embedded` needs.

| Your test JDK | Embedded available? |
|---|---|
| 22 or newer | yes — `EmbeddedRift.available` resolves the SDK's provider |
| 17–21 | no — use WireMock or Rift container instead |

### API and wiring

```scala
def available: Boolean                                          // true iff an embedded engine provider resolves for this host
def requireAvailable: IO[MockError, Unit]                        // fail loudly instead of SKIP when unavailable
def layer: ZLayer[Provisioning, MockError, MockControl]
```

`EmbeddedRift.available` checks whether the SDK's embedded engine provider
(`rift-java-embedded` + a matching `rift-java-natives` classifier) resolves
via `ServiceLoader` — not whether a bundled cdylib resource is present on the
classpath, the pre-#285 mechanism. It lets a harness park the backend (skip)
rather than fail outright when no provider resolves for the current host/JDK
combination — check it before wiring `EmbeddedRift.layer` in environments
where the host isn't guaranteed to have a matching provider.

```scala
import zio.bdd.mock.rift.embedded.EmbeddedRift

val mockControl: ZLayer[Any, Throwable, MockControl] =
  provisioning >>> EmbeddedRift.layer.mapError(e => new RuntimeException(s"Embedded Rift: $e"))
```

Unlike the container adapter, `EmbeddedRift.layer` needs only `Provisioning`
in its environment — it builds its own loopback admin client internally, so
no `Client` layer is required.

Like the container adapter, the embedded provider supports **both isolation
modes** — pass a `RiftMode`:

```scala
import zio.bdd.mock.rift.RiftMode

// PerInstance (default): one imposter per space on its own OS-assigned port
val perInstance = Provisioning.live >>> EmbeddedRift.layer                       // == EmbeddedRift.layer(RiftMode.PerInstance)
// Correlated: all spaces share ONE imposter, isolated by a per-space correlation
// header (like WireMock.correlated) — cheaper under heavy scenario-parallelism
val correlated  = Provisioning.live >>> EmbeddedRift.layer(RiftMode.correlated)
```

#### Sharing one engine across `@Suite` classes — `EmbeddedRift.shared`

Standing up the native engine per suite is expensive, so it's tempting to share
one across suite classes. Do **not** hand-roll that with a `static val` that runs
`Runtime.default.unsafe.run(EmbeddedRift.layer.build)` at class-load: under sbt's
**default** `Test / parallelExecution := true`, each `@Suite` runs on the shared
runtime concurrently, and a suite's `<clinit>` executing on a runtime worker
thread while a sibling suite blocks on the JVM class-init lock **deadlocks** —
surfacing as a cause-less `Interrupted by thread "zio-fiber-N"` with `0 results`.

Use the supported, memoized layer instead:

```scala
import zio.bdd.mock.rift.embedded.EmbeddedRift

// One engine, built at most once, shared by every suite that uses it, released
// on JVM shutdown. Concurrent suites are safe — no `parallelExecution := false`.
val shared = Provisioning.live >>> EmbeddedRift.shared
```

`EmbeddedRift.shared` memoizes the build behind a semaphore (see
`SharedLayer.memoize`), so concurrent construction from independent suites builds
the engine exactly once and hands every caller the same `MockControl`. (You can
still keep `Test / parallelExecution := false` if you prefer serial suites; it is
no longer *required* just to share an embedded engine.)

In `Correlated` mode `isolation` reports `Isolation.Correlated`, each space's
stubs and `received` are scoped by its `flowId`, and `destroy` tears down just
that space — behaviour identical to the container adapter's Correlated mode. As
there, Correlated needs portable rule sources (`MockSource.Dsl`); a raw imposter
document is `PerInstance`-only via `provisionNative`.

---

## 5. Choosing an adapter

- **No Docker, need all seven capabilities, JDK 22+ available** → Rift
  embedded (`zio-bdd-rift`, add `rift-java-embedded` + a matching
  `rift-java-natives` classifier at runtime, §4). Full fidelity, zero
  container overhead.
- **No Docker, stuck below JDK 22** → embedded isn't available (§4); fall
  back to WireMock if the three-capability limit is acceptable, or to the
  Rift container adapter (which only needs JDK 17+ to compile against) if
  Docker is an option.
- **Already on WireMock, or need to run on JDK 11** → WireMock
  (`zio-bdd-wiremock`). Cheapest to stand up; covers Faults,
  StatefulScenarios, StateInspection.
- **Full fidelity and Docker is available (e.g. CI with testcontainers)** →
  Rift container (`zio-bdd-rift`). Same six always-on capabilities as
  embedded, closer to a production Rift deployment; pass `interceptPort` to
  also get `Capability.Intercept` advertised with a host-reachable listener,
  matching embedded's already-usable intercept.

The sample corpus's `support/Backends.scala` shows the pattern for switching
adapters at runtime via an environment variable, so a suite's `environment`
stays fixed at compile time while CI picks the backend per job:

```scala
object Backends:
  val selected: String = sys.env.getOrElse("MOCK_BACKEND", "wiremock")

  private val provisioning: ULayer[Provisioning] = ZLayer(Provisioning.make)

  val mockControl: ZLayer[Any, Throwable, MockControl] =
    selected match
      case "wiremock"    => provisioning >>> WireMock.correlated()
      case "wiremock-pi" => provisioning >>> WireMock.perInstance
      case "rift"        => provisioning >>> Rift.managed().mapError(e => new RuntimeException(s"Rift: $e"))
      case "embedded"    => provisioning >>> EmbeddedRift.layer.mapError(e => new RuntimeException(s"Embedded Rift: $e"))
      case other =>
        ZLayer.fail(new IllegalArgumentException(s"unknown MOCK_BACKEND='$other'"))
```

Capability differences between backends are handled by excluding tags per
backend at the harness level (e.g. skipping `@scripting` on WireMock runs),
never by branching adapter-specific logic into step definitions — code
written against `MockControl` stays adapter-agnostic (see
[Mocking overview](mocking.md)).

## 6. Verifying a third-party adapter (`zio-bdd-mock-conformance`)

The conformance suite that gates the bundled adapters is published, so an
adapter written outside this repo can run the *official* definition of
"implements `MockControl`" in its own CI instead of hand-rolled contract
tests:

```scala
"io.github.etacassiopeia" %% "zio-bdd-mock-conformance" % "1.4.3" % Test
```

Its compile-scope dependency is the SPI (`zio-bdd-mock`) alone — pulling it
into a test classpath brings **no** bundled backend along.

The artifact ships the portable scenario sets — `CoreConformanceScenarios`,
`NegotiationErrorScenarios`, `FaultScenarios`, `ScriptingScenarios`,
`TemplatingScenarios`, `CapStatefulScenarios` (each a
`lazy val all: List[ConformanceScenario]`) — and the `ConformanceHarness`
that runs them. Register the adapter as a `MockBackendUnderTest` with the
capabilities it advertises, run the scenarios through the harness, and assert
the column is conformant:

```scala
val backend = MockBackendUnderTest(
  name = "my-adapter", layer = myAdapterLayer,
  capabilities = Set(Capability.Faults), isolation = Isolation.PerInstance
)
for matrix <- ConformanceHarness.run(List(backend), CoreConformanceScenarios.all)
yield assertTrue(matrix.conformant(backend))
```

A scenario that requires a capability the backend does not advertise is
**SKIP**, never **FAIL** — a negotiated gap is not a conformance breach, so
`conformant` holds as long as no cell fails and every skip is justified by
the advertised capability set (the same acceptance rule the bundled Rift and
WireMock adapters are held to).

The complete, compile-checked example (a full `ZIOSpecDefault` running all six
scenario sets) lives in
[Verified Examples](verified-examples.md#verifying-a-third-party-adapter-conformance-kit) —
it is compiled against the real API on every CI run, so it can't silently
drift the way a hand-written snippet can.

---

Next: [Gherkin integration](mock-gherkin.md) · [Cookbook](mock-cookbook.md)
