# Performance

Guidance for tuning parallelism, understanding memory growth over a run, and reasoning
about the runtime cost of zio-bdd's state mechanisms. There are no published
benchmark numbers here — the framework's performance depends entirely on your step
bodies, backends, and CI hardware. What follows is the *mechanism* behind each cost
so you can reason about your own suite, plus heuristics to start from and measure.

---

## 1. Parallelism tuning

The full mechanism — `--parallelism` / `--scenario-parallelism`, the `@Suite`
annotation fields, the `ZIO_BDD_SCENARIO_PARALLELISM` / `ZIO_BDD_FEATURE_PARALLELISM`
env vars, the `scenarioParallelism` / `featureParallelism` suite overrides, and their
precedence order — is documented in
[Running Tests → Parallelism](running.md#parallelism) and
[Runtime overrides and precedence](running.md#runtime-overrides-and-precedence). This
section only adds workload-keyed guidance for *how high* to set those knobs. Treat
every number below as a starting point to measure against your own suite, not a
guarantee.

### CPU-bound, pure scenarios

Steps that only exercise in-process logic (parsing, validation, pure transformations,
in-memory fakes) are typically bottlenecked on CPU. Scenario/feature parallelism above
the number of available cores buys little — fibers end up time-slicing the same cores.
A reasonable starting point is **up to `Runtime.getRuntime.availableProcessors()`**
(which is also what `auto` / `0` resolves to for `scenarioParallelism`, per
[running.md → Parallelism](running.md#parallelism)), then back off if you see
diminishing returns or contention on a shared in-process resource (a single
in-memory database, a shared mutable fake).

### Container / mock-heavy suites (Rift)

Suites that stand up a Rift container per scenario or per feature (see
[Mock Adapters](mock-adapters.md)) are bottlenecked on Docker and network resources,
not CPU: each `Rift.managed(...)` instance binds an admin port and one or more
imposter ports (`adminPort`, `imposterBasePort` / `imposterPorts` in the adapter
config). Running many of these concurrently multiplies container start time,
Docker daemon load, and the chance of port collisions or flaky container startup.
**Keep parallelism low — 1 to 2 — for suites where each scenario or feature
provisions its own container**, and prefer a single shared long-lived Rift instance
(via `globalLayer`, see [layers.md](layers.md)) over many short-lived ones when the
scenarios don't need full isolation from each other. If scenarios only need
correlated isolation within one running instance (`RiftMode.Correlated` or a
`WireMock` per-request correlation header — see
[Mock Adapters § The three backends at a glance](mock-adapters.md#1-the-three-backends-at-a-glance)
and [Mock Advanced](mock-advanced.md)), you can raise parallelism further because
you're no longer paying a container-startup cost per scenario.

### I/O- or network-bound scenarios

Steps that mostly wait on an external HTTP call, database round-trip, or other
network I/O spend most of their wall-clock time blocked, not consuming a CPU core.
For this shape, parallelism **can exceed the core count** — ZIO's fiber scheduler
multiplexes many suspended fibers cheaply over a small thread pool, so the limiting
factor becomes the downstream service's own concurrency budget (connection pool
size, rate limits) rather than your JVM's core count. Push parallelism up
incrementally and watch the downstream service, not your own CPU graph.

### General approach

There's no single right number — measure. Start from the heuristics above, run the
suite, and look for either (a) wall-clock time not improving as you raise
parallelism (you're past the useful ceiling — CPU or a shared resource is now the
bottleneck), or (b) new flakiness/failures appearing as you raise it (you've hit a
real concurrency limit — port exhaustion, a rate limit, a shared-state bug in the
suite itself). See [running.md's precedence table](running.md#runtime-overrides-and-precedence)
for wiring different values per environment (e.g. sequential locally, parallel in CI).

---

## 2. Memory

### Captured logs grow with the whole run, not per scenario

`LogCollector` (`core/src/main/scala/zio/bdd/core/LogCollector.scala`) is
instantiated **once per suite run** — `ZIOBDDTask.execute` builds a single
`LogCollector.live(...)` layer and `++`s it into the environment shared by every
feature and scenario in that run (`core/src/main/scala/zio/bdd/ZIOBDDFramework.scala:168-169`).
Internally it's backed by a single `ConcurrentHashMap[String, CollectedLogs]` keyed
by `"scenarioId::stepId"` (`LogCollector.scala:58-91`), and entries are only ever
*added* to that map via `merge` — nothing removes or clears entries for the
duration of the run.

The practical consequence: captured-log memory is proportional to **(total
scenarios × total steps) for the entire suite run**, not to any single scenario or
feature. A suite with a handful of scenarios barely notices this. A large suite run
with `logLevel = "debug"` (or `--log-level debug`) set suite-wide captures every
debug-level log line from every step, for the run's full lifetime — with no upper
bound and no eviction, this is effectively unbounded growth while the run is in
flight. If you see rising heap usage over a long-running debug suite, this map is
the first place to look. There is currently no per-scenario eviction; the mitigation
is to scope `debug` logging narrowly — a specific `@Suite(logLevel = ...)` on a
small diagnostic suite, or `--log-level debug` on a filtered subset via
`--scenario-name` / `--include-tags` — rather than leaving it on for a full,
large-scale run. See [running.md's `@Suite` fields table](running.md#fields) and
[CLI flags reference](running.md#cli-flags-reference) for how `logLevel` /
`--log-level` are set.

### Property-test failures persist to disk, not memory

`PropertyFailureStore` (`core/src/main/scala/zio/bdd/core/property/PropertyFailureStore.scala:36`)
writes one JSON file per failing `@property` scenario to `.zio-bdd/failures/<slug>.json`
on disk — it is not an in-memory accumulation concern. These files are small (a
seed, sample index, and the shrunk values/labels) and are read back on the next run
to replay the failing case first. If your CI workspace is ephemeral, `.zio-bdd/failures/`
won't persist between runs; if it's cached/reused, stale failure records for
since-changed scenarios are self-invalidating (the store hashes the scenario's step
list and discards the record with a warning if it no longer matches). See
[Property-Based Testing](property-testing.md) for the full replay mechanism.

---

## 3. State mechanism cost

zio-bdd offers three ways to carry data across step bodies within a scenario:
`ScenarioContext` (the `S` type parameter, via `State[S]`), `Stage`, and `TypeMap`
(see [State Management](state.md) for when to use which). All three are backed by
a plain ZIO `FiberRef`, and all three have comparable get/update cost:

- **`ScenarioContext` / `State[S]`** (`core/src/main/scala/zio/bdd/core/step/State.scala:6-23`):
  `get` and `update` delegate directly to `FiberRef[S].get` / `.update`. No
  serialization happens on this path.
- **`Stage`** (`core/src/main/scala/zio/bdd/core/step/Stage.scala`): a
  `FiberRef[Map[String, Any]]` keyed by runtime class name; `put`/`get`/`modify` are
  plain immutable-`Map` operations on top of `FiberRef.update`/`.get`.
- **`TypeMap`** (`core/src/main/scala/zio/bdd/core/step/TypeMap.scala`): a
  `State[TypeMap]` (itself `FiberRef`-backed, same as `ScenarioContext`) holding an
  immutable `Map[Any, Any]` keyed by `Tag`; `TypeMapOps.get`/`update` layer a
  `Map` lookup/insert on top of the same `State.get`/`State.update`.

None of the three touches `Schema` derivation or does any serialization on the
per-step path — the cost of a `TypeMap`/`Stage` access is an immutable-map
operation plus a `FiberRef` read/write, the same order of cost as a direct
`ScenarioContext` access.

### `Schema[S]` is a startup/seed cost, not a per-step cost

`Schema[S]` is used to derive `Default[S]` (`core/src/main/scala/zio/bdd/core/Default.scala`),
which supplies the initial value for a scenario's `S` state. Per
`ScenarioExecutor.runAttempt` (`core/src/main/scala/zio/bdd/core/ScenarioExecutor.scala:81`),
`Default[S].default` is read exactly once per scenario **attempt** — when the
FiberRef backing that attempt's `ScenarioContext` is created — not once per step.
(An `@retry`/`@flaky` scenario re-seeds it once per attempt, since every attempt
gets fully independent state.) After that single seed, every `ScenarioContext.get`/
`.update` call within the scenario is a plain `FiberRef` operation as described
above — `Schema[S]` is never consulted again during step execution. There is no
per-step schema-derivation or serialization cost; the only place `Schema[S]`'s
default-value computation runs is that one-time seed at the start of each scenario
attempt.

---

## 4. Startup & discovery

A few principled considerations for suite startup time, without invented figures —
measure your own suite if startup time matters to your feedback loop:

- **`@Suite` discovery** is done by sbt's own test-framework machinery: zio-bdd
  registers an `AnnotatedFingerprint` for `zio.bdd.core.Suite`
  (`core/src/main/scala/zio/bdd/ZIOBDDFramework.scala:13-16`), and sbt's
  incremental-compiler-backed test discovery locates annotated classes from its own
  compiled-class analysis rather than zio-bdd performing a runtime reflection scan
  of the classpath. This work is proportional to your project's own compiled-class
  count via sbt/zinc, not something zio-bdd adds independently.
- **`.feature` file scanning** for a filesystem `featureDirs` entry is a flat
  `File.listFiles()` over each configured directory
  (`core/src/main/scala/zio/bdd/ZIOBDDFramework.scala:105-114`) — proportional to
  the number of files in that directory, done once at suite startup before any
  scenario runs.
- **`classpath:` feature directories** resolve via `ClassLoader.getResources(...)`
  followed by treating each resolved URL as a `java.io.File` and listing it
  (`ZIOBDDFramework.scala:98-103`). This assumes the classpath entry is an
  unpacked directory (a normal `test:resources`/`test:managedResources` layout, or
  an exploded classpath as sbt typically presents it). **If your `.feature` files
  are packaged inside a fat/uber jar** rather than left as loose classpath
  directories, a `classpath:` resource URL points *into* the jar
  (`jar:file:...!/features`), and turning that into a `File` for `listFiles()` is
  not guaranteed to enumerate the packed entries the way it does for a real
  directory. If you rely on `classpath:` feature loading from a packaged
  test-jar, verify feature discovery still finds all files in that packaging
  — or keep feature resources unpacked on the classpath (the common sbt
  multi-module case documented in [running.md's `@Suite` fields table](running.md#fields))
  rather than shaded into a single fat jar.

---

## See also

- [Running Tests → Parallelism](running.md#parallelism) — the full parallelism
  mechanism: CLI flags, `@Suite` fields, env vars, suite overrides, and precedence.
- [Reporters](reporters.md) — pretty/JUnit XML output, including how captured logs
  and log levels surface per step.
- [Troubleshooting](troubleshooting.md) — startup failures, state-not-updating
  issues, and other runtime problems with root cause and fix.
