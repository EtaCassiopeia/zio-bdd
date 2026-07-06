# Mock Adapters

Three backends implement `MockControl`: a Docker-based Rift container, an
in-process WireMock server, and a no-Docker embedded Rift bound via FFM. This
page covers their coordinates, capabilities, and ZLayer wiring, and how to
pick one.

---

## 1. The three backends at a glance

| Adapter | Coordinates (1.3.1) | Docker? | JDK | Isolation default | Capabilities |
|---|---|---|---|---|---|
| Rift container | `zio-bdd-rift` | yes (testcontainers) | 11+ | PerInstance | all six |
| WireMock | `zio-bdd-wiremock` | no | 11+ | Correlated (via `.correlated`) | Faults, StatefulScenarios, StateInspection only |
| Rift embedded | `zio-bdd-rift-embedded` / `zio-bdd-rift-embedded-jdk21` | no (FFM) | 22+ / 21 | PerInstance (default) / Correlated | all six |

### Capability × adapter matrix

| Capability | Rift container | Rift embedded | WireMock |
|---|---|---|---|
| Faults | yes | yes | yes |
| StatefulScenarios | yes | yes | yes |
| StateInspection | yes | yes | yes |
| Scripting | yes | yes | no |
| ProxyRecord | yes | yes | no |
| Templating | yes | yes | no |

Rift container and Rift embedded are **capability-complete** — embedded is a
pure backend swap for the container adapter, not a reduced subset. WireMock
implements exactly the three capabilities in the first column; requesting
`Capability.Scripting`, `Capability.ProxyRecord`, or `Capability.Templating`
against it fails with `Unsupported` (§3 below, and see
[Mocking overview](mocking.md) for the `MockError`/`Unsupported` model).

---

## 2. Rift container (`zio-bdd-rift`)

```scala
"io.github.etacassiopeia" %% "zio-bdd-rift" % "1.3.1"
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
    mode: RiftMode = RiftMode.PerInstance
  ): ZLayer[Client & Provisioning, MockError, MockControl]
  ```

  `DefaultImage` is the pinned Rift image, currently `zainalpour/rift-proxy:v0.9.0`
  — derived from the single `riftVersion` in `build.sbt`, so treat it as "the
  pinned Rift image" rather than a hardcoded tag.

- **`Rift.connect(...)`** targets an already-running Rift admin endpoint
  instead of starting a container (used by tests, or when Rift runs
  out-of-band):

  ```scala
  def connect(
    adminBase: String,
    imposterPorts: List[Int],
    mode: RiftMode = RiftMode.PerInstance
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
"io.github.etacassiopeia" %% "zio-bdd-wiremock" % "1.3.1"
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
**capability-complete** (parity with the container adapter, all six
capabilities). This is the adapter to reach for when you want full fidelity
without Docker.

### Two version-locked artifacts

FFM is a class-file-level lock: a class compiled against JDK 21's preview FFM
API won't load on JDK 22+, and vice versa. So the embedded provider is
published as **two variants built from the same source**, and you pick
exactly one for your build's JDK:

```scala
"io.github.etacassiopeia" %% "zio-bdd-rift-embedded"       % "1.3.1" // JDK 22+ (stable FFM)
"io.github.etacassiopeia" %% "zio-bdd-rift-embedded-jdk21" % "1.3.1" // JDK 21   (preview FFM)
```

Both additionally need the native library on the classpath (or an explicit
override), and neither is scala-versioned — note the single `%`, not `%%`:

```scala
"io.github.etacassiopeia" % "zio-bdd-rift-embedded-natives" % "1.3.1" % Test
```

`zio-bdd-rift-embedded-natives` bundles the per-platform `librift_ffi`
cdylibs; the loader extracts the one matching your host to a temp file and
loads it automatically. If you'd rather point at a library you built or
installed yourself, skip the natives jar and set `-Drift.ffi.lib=<path>`
instead.

### Host prerequisite: LuaJIT

The embedded engine's Scripting capability needs LuaJIT installed on the host
running the tests:

```bash
# Debian/Ubuntu
apt install libluajit-5.1-dev

# macOS
brew install luajit
```

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

In `Correlated` mode `isolation` reports `Isolation.Correlated`, each space's
stubs and `received` are scoped by its `flowId`, and `destroy` tears down just
that space — behaviour identical to the container adapter's Correlated mode. As
there, Correlated needs portable rule sources (`MockSource.Dsl`); a raw imposter
document is `PerInstance`-only via `provisionNative`.

---

## 5. Choosing an adapter

- **No Docker, need all six capabilities, JDK 22+ available** → Rift embedded
  (`zio-bdd-rift-embedded`). Full fidelity, zero container overhead.
- **No Docker, stuck on JDK 21** → Rift embedded
  (`zio-bdd-rift-embedded-jdk21`) with `--enable-preview`, or fall back to
  WireMock if the three-capability limit is acceptable.
- **Already on WireMock, or need to run on JDK 11** → WireMock
  (`zio-bdd-wiremock`). Cheapest to stand up; covers Faults,
  StatefulScenarios, StateInspection.
- **Full fidelity and Docker is available (e.g. CI with testcontainers)** →
  Rift container (`zio-bdd-rift`). Same capability set as embedded, closer to
  a production Rift deployment.

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

---

Next: [Gherkin integration](mock-gherkin.md) · [Cookbook](mock-cookbook.md)
