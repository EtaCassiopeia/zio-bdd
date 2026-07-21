# Mock Adapters

Three backends implement `MockControl`: a Docker-based Rift container, an
in-process WireMock server, and a no-Docker embedded Rift bound via FFM. This
page covers their coordinates, capabilities, and ZLayer wiring, and how to
pick one.

---

## 1. The three backends at a glance

| Adapter | Coordinates (1.4.2) | Docker? | JDK | Isolation default | Capabilities |
|---|---|---|---|---|---|
| Rift container | `zio-bdd-rift` | yes (testcontainers) | 11+ | PerInstance | all six core + Intercept opt-in |
| WireMock | `zio-bdd-wiremock` | no | 11+ | Correlated (via `.correlated`) | Faults, StatefulScenarios, StateInspection only |
| Rift embedded | `zio-bdd-rift-embedded` / `zio-bdd-rift-embedded-jdk21` | no (FFM) | 22+ / 21 | PerInstance (default) / Correlated | all seven, always on |

### Capability × adapter matrix

| Capability | Rift container | Rift embedded | WireMock |
|---|---|---|---|
| Faults | yes | yes | yes |
| StatefulScenarios | yes | yes | yes |
| StateInspection | yes | yes | yes |
| Scripting | yes | yes | no |
| ProxyRecord | yes | yes | no |
| Templating | yes | yes | no |
| Intercept | opt-in (`interceptPort`) | yes | no |

Rift container and Rift embedded are **capability-complete** on the six core
capabilities — embedded is a pure backend swap for the container adapter, not
a reduced subset. `Intercept` (built-in HTTPS intercept) is always-on for
embedded but opt-in for the container adapter: pass `interceptPort` to
`Rift.managed` (or `interceptProxy` to `Rift.connect`) to advertise it; without
that, the container adapter's `intercept` accessor fails with `Unsupported`
just like WireMock's. WireMock implements exactly the three capabilities in
the first column of the top table; requesting `Capability.Scripting`,
`Capability.ProxyRecord`, `Capability.Templating`, or `Capability.Intercept`
against it fails with `Unsupported` (§3 below, and see
[Mocking overview](mocking.md) for the `MockError`/`Unsupported` model).

---

## 2. Rift container (`zio-bdd-rift`)

```scala
"io.github.etacassiopeia" %% "zio-bdd-rift" % "1.4.4"
```

Two entry points, both requiring a zio-http `Client` and a `Provisioning` in
the environment:

- **`Rift.managed(...)`** starts the pinned Rift image via testcontainers and
  stops it when the layer's scope closes:

  ```scala
  def managed(
    image: String = DefaultImage,
    poolSize: Int = DefaultPoolSize,
    adminPort: Int = DefaultAdminPort,
    imposterBasePort: Int = DefaultImposterBase,
    mode: RiftMode = RiftMode.PerInstance,
    interceptPort: Option[Int] = None
  ): ZLayer[Client & Provisioning, MockError, MockControl]
  ```

  `DefaultImage` is the pinned Rift image, currently `zainalpour/rift-proxy:v0.14.0`
  — derived from the single `riftVersion` in `build.sbt`, so treat it as "the
  pinned Rift image" rather than a hardcoded tag. Pass `interceptPort` to also
  start the container's HTTPS-intercept listener and advertise
  `Capability.Intercept` (§9 of [Advanced mocking](mock-advanced.md)); omitted,
  the container behaves exactly as before.

- **`Rift.connect(...)`** targets an already-running Rift admin endpoint
  instead of starting a container (used by tests, or when Rift runs
  out-of-band):

  ```scala
  def connect(
    adminBase: String,
    imposterPorts: List[Int],
    mode: RiftMode = RiftMode.PerInstance,
    interceptProxy: Option[(String, Int)] = None
  )(hostFor: Int => String): URLayer[Client & Provisioning, MockControl]
  ```

Both take a `RiftMode`: `RiftMode.PerInstance` (default — one imposter port
per `MockSpace`) or `RiftMode.Correlated(correlation)` / the `RiftMode.correlated`
shorthand (one shared imposter, spaces tagged by a correlation header).

Wiring, lifted from the sample corpus's `support/Backends.scala`:

```scala
import zio.bdd.mock.*
import zio.bdd.mock.rift.Rift
import zio.http.Client

val provisioning: ULayer[Provisioning] = ZLayer(Provisioning.make)

val mockControl: ZLayer[Any, Throwable, MockControl] =
  (Client.default ++ provisioning) >>> Rift.managed().mapError(e => new RuntimeException(s"Rift: $e"))
```

See [layers](layers.md) for how this composes into a suite's `environment`.

---

## 3. WireMock (`zio-bdd-wiremock`)

```scala
"io.github.etacassiopeia" %% "zio-bdd-wiremock" % "1.4.4"
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

The embedded provider drives the Rift engine in-process over `librift_ffi` via
Project Panama FFM — no container, no external process — while remaining
**capability-complete** (parity with the container adapter's six core
capabilities, plus `Capability.Intercept` always on — all seven). This is the
adapter to reach for when you want full fidelity without Docker.

### Two version-locked artifacts

FFM is a class-file-level lock: a class compiled against JDK 21's preview FFM
API won't load on JDK 22+, and vice versa. So the embedded provider is
published as **two variants built from the same source**, and you pick
exactly one for your build's JDK:

```scala
"io.github.etacassiopeia" %% "zio-bdd-rift-embedded"       % "1.4.4" // JDK 22+ (stable FFM)
"io.github.etacassiopeia" %% "zio-bdd-rift-embedded-jdk21" % "1.4.4" // JDK 21   (preview FFM)
```

Both additionally need the native library on the classpath (or an explicit
override), and neither is scala-versioned — note the single `%`, not `%%`:

```scala
"io.github.etacassiopeia" % "zio-bdd-rift-embedded-natives" % "1.4.4" % Test
```

`zio-bdd-rift-embedded-natives` bundles the per-platform `librift_ffi`
cdylibs; the loader extracts the one matching your host to a temp file and
loads it automatically. If you'd rather point at a library you built or
installed yourself, skip the natives jar and set `-Drift.ffi.lib=<path>`
instead.

> **Platform note — system LuaJIT (natives ≤ 1.4.1 only).** The `1.4.1` and
> earlier natives repackage Rift **v0.11.3**, whose `linux-x86_64` and
> `darwin-aarch64` cdylibs dynamically link **system LuaJIT** — so those hosts
> needed `libluajit-5.1-2` (apt) / Homebrew `luajit` installed, or the library
> failed to `dlopen` at engine start (#307). **Natives from `1.4.2` onward
> (Rift v0.12.0) are self-contained** — Rift dropped the Lua engine, so no host
> LuaJIT is required on any platform. If you're pinned to an old natives version
> on a Lua-free host, either upgrade or install system LuaJIT. A bundled library
> that is present but fails to load surfaces a `MockError.ProvisionFailed` naming
> the underlying `dlopen` error and this missing-dependency hint.

### Test-JVM flags

Both variants need `--enable-native-access` to silence the restricted-method
warning; the JDK 21 variant additionally needs `--enable-preview` to load its
preview-compiled bridge:

```scala
// build.sbt — JDK 22+ (zio-bdd-rift-embedded)
Test / fork := true,
Test / javaOptions += "--enable-native-access=ALL-UNNAMED"

// build.sbt — JDK 21 (zio-bdd-rift-embedded-jdk21)
Test / fork := true,
Test / javaOptions ++= Seq("--enable-preview", "--enable-native-access=ALL-UNNAMED")
```

### Which JDK / which artifact

| Your test JDK | Artifact | Test JVM flags |
|---|---|---|
| 22 or newer | `zio-bdd-rift-embedded` | `--enable-native-access=ALL-UNNAMED` |
| 21 | `zio-bdd-rift-embedded-jdk21` | `--enable-preview --enable-native-access=ALL-UNNAMED` |
| 11–20 | neither — use WireMock or Rift container | — |

### API and wiring

```scala
def available: Boolean                                          // true iff a native lib resolves for this host/JDK
def layer: ZLayer[Provisioning, MockError, MockControl]
```

`EmbeddedRift.available` lets a harness park the backend (skip) rather than
fail outright when no native library resolves for the current host/JDK
combination — check it before wiring `EmbeddedRift.layer` in environments
where the host isn't guaranteed to have a matching native lib.

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
  embedded (`zio-bdd-rift-embedded`). Full fidelity, zero container overhead.
- **No Docker, stuck on JDK 21** → Rift embedded
  (`zio-bdd-rift-embedded-jdk21`) with `--enable-preview`, or fall back to
  WireMock if the three-capability limit is acceptable.
- **Already on WireMock, or need to run on JDK 11** → WireMock
  (`zio-bdd-wiremock`). Cheapest to stand up; covers Faults,
  StatefulScenarios, StateInspection.
- **Full fidelity and Docker is available (e.g. CI with testcontainers)** →
  Rift container (`zio-bdd-rift`). Same six core capabilities as embedded,
  closer to a production Rift deployment; pass `interceptPort` to also match
  embedded's `Capability.Intercept`.

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
      case "rift"        => (Client.default ++ provisioning) >>> Rift.managed().mapError(e => new RuntimeException(s"Rift: $e"))
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
"io.github.etacassiopeia" %% "zio-bdd-mock-conformance" % "1.4.4" % Test
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
