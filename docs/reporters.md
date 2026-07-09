# Reporters Reference

zio-bdd ships two built-in reporters and a streaming event API for custom integrations.

---

## Selecting reporters

Reporters are configured on the suite object via the `@Suite` annotation:

```scala
@Suite(
  featureDirs = Array("src/test/resources/features"),
  reporters = Array("pretty", "junitxml")
)
object MySuite extends ZIOSteps[AppEnv, AppState]
```

Multiple reporter names produce a `CompositeReporter` that runs all reporters in sequence.
The recognized names are `"pretty"` and `"junitxml"`. Unknown names fall back to `"pretty"`
with a warning.

You can also select reporters from the CLI:

```
sbt "testOnly -- --reporter pretty --reporter junitxml"
```

---

## Built-in reporters

Both built-in reporters implement the batch `Reporter` trait:

```scala
trait Reporter:
  def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit]
```

`report` receives the full `List[FeatureResult]` after the run completes and requires a
`LogCollector` in its environment (used to fetch step/scenario logs while rendering — see
"LogCollector" below).

### pretty

`PrettyReporter` writes a Unicode tree to the console using ANSI colors. It is the default
reporter when `reporters` is not specified.

**Color palette:**

Colors are theme-aware (`Theme.Dark` / `Theme.Light` / `Theme.Plain`, auto-detected from
`NO_COLOR`/`TERM`/`COLORFGBG` — see `PrettyReporter.scala`). `Plain` emits no ANSI codes.
Icon and status come from the `StatusColor[A]` given instances; dark-theme codes are shown
below.

| Element | Color | Icon |
|---|---|---|
| Feature — passed | Green (`\e[38;5;35m`) | `◉` |
| Feature — ignored | Gray (`\e[90m`) | `◉` |
| Feature — failed | Red (`\e[91m`) | `◉` |
| Scenario — passed | Blue (`\e[38;5;75m`) | `✓` |
| Scenario — ignored | Gray (`\e[90m`) | `◑` |
| Scenario — pending | Orange (`\e[38;5;214m`) | `◑` |
| Scenario — failed | Red (`\e[91m`) | `✗` |
| Scenario — XFAIL (expected failure) | Gray (`\e[90m`) | `✓` |
| Scenario — unexpectedly passing (XPASS) | Red (`\e[91m`) | `✗` |
| Step — passed | Mint (`\e[38;5;121m`) | `•` |
| Step — skipped | Gray (`\e[90m`) | `○` |
| Step — pending | Orange (`\e[38;5;214m`) | `◌` |
| Step — failed | Red (`\e[91m`) | `✗` |
| Step — timed out | Red (`\e[91m`) | `⏱` |
| Summary / headings | Cyan (`\e[96m`) | — |

**Internal log level palette** (used when rendering `LogCollector` entries under a step):

| Level | Color | Icon |
|---|---|---|
| Debug | Steel gray (`\e[38;5;242m`) | — |
| Info | Dusty teal (`\e[38;2;100;180;160m`) | — |
| Warning | Muted gold (`\e[38;2;200;170;80m`) | `⚠` |
| Error | Muted rose (`\e[38;2;210;100;100m`) | `✖` |
| Fatal | Bright red (`\e[91m`) | `✖` |

**Sample output:**

```
◉ Feature: User registration (src/test/resources/features/registration.feature) - PASSED [142ms]
├─ ◑ Scenario: Valid registration:12 - PASSED [88ms]
│  ├─ • Given a valid email address [12ms]
│  ├─ • When the user submits the form [34ms]
│  ╰─ • Then the account is created [42ms]
│     Passed: 1  Failed: 0  Ignored: 0  Pending: 0
╰─ ◑ Scenario: Duplicate email:20 - FAILED [54ms]
   ├─ • Given an existing account [10ms]
   ├─ • When the same email is submitted [20ms]
   ╰─ • Then an error is returned [24ms] [FAILED]
      zio.bdd.example.DuplicateEmailError: email already registered
      ...
      Passed: 0  Failed: 1  Ignored: 0  Pending: 0
Summary: Passed: 1  Failed: 1  Ignored: 0  Pending: 0
```

**Step status annotations:**

- `[FAILED]` — step threw an exception or assertion failed
- `[SKIPPED]` — step was not executed because a prior step failed
- `[PENDING: reason]` — step body called `pending("reason")` (not yet implemented)

**Architecture — Doc algebra:**

`PrettyReporter` never builds strings incrementally. Instead it constructs a pure `Doc`
algebraic tree and hands it to a pluggable `DocRenderer`:

```
results
  └─► DocBuilder   (pure: List[FeatureResult] → Doc)
        └─► DocRenderer (effectful: Doc → Console output)
```

`Doc` variants:

```scala
sealed trait Doc
object Doc:
  case class Leaf(text: String, style: Style)                            extends Doc
  case class Branch(header: Leaf, children: List[Doc], isLast: Boolean) extends Doc
  case class Many(docs: List[Doc])                                       extends Doc
```

Available `DocRenderer` implementations:

| Class | Behavior |
|---|---|
| `AnsiRenderer` | Writes ANSI-colored text to the console (default) |
| `PlainRenderer` | Same layout but strips all color codes — suitable for CI without color |
| `BufferRenderer` | Accumulates `RenderedLine` values in memory — used in tests |

To obtain a plain-text reporter when color is not available:

```scala
// In your suite's companion or test bootstrap
val plainLayer: ULayer[Reporter] = PrettyReporter.plain
```

To inspect output in tests:

```scala
for {
  (reporter, buffer) <- PrettyReporter.buffered
  _                  <- reporter.report(results)
  captured           <- buffer.collected
} yield captured.toPlainText
```

---

### junitxml

`JUnitXMLReporter` writes one XML file per feature into `target/test-reports/`. File names
are derived from the feature name with non-alphanumeric characters replaced by `_`.

**Configuration:**

```scala
JUnitReporterConfig(
  outputDir = "target/test-reports",   // default
  format    = JUnitXMLFormatter.Format.JUnit5  // default
)
```

When selected via `@Suite(reporters = Array("junitxml"))` the defaults above are used.
To override, instantiate the reporter directly in a custom `BDDTestConfig`.

**File location:**

```
target/test-reports/
  User_registration.xml
  Shopping_cart.xml
  ...
```

**Format selection:**

| Value | Description |
|---|---|
| `Format.JUnit5` | Legacy `<testsuite>/<testcase>` structure, no `assertions` attribute (default) |
| `Format.JUnit4` | Legacy structure with `assertions` attribute on each `<testcase>` |

Both formats are compatible with Jenkins, GitLab CI, and GitHub Actions test report plugins.
See `docs/adr/junit-report-formats.md` for a full comparison of the two XML structures.

**Scenario aspects (retry, XFAIL/XPASS):**

The retry (`@retry`/`@flaky`/`@nonFlaky`) and `@expectedFailure` aspects are surfaced in the XML so
CI dashboards can distinguish tracked known-failures and flaky passes from ordinary results — build
gating is unchanged (an expected failure never fails the build; an unexpected pass always does):

| Aspect | XML representation |
|---|---|
| Ran more than once (retry) | `attempts="N"` attribute on the `<testcase>` (omitted when `N == 1`) |
| `@expectedFailure` body **failed** (XFAIL) | `<skipped message="expected failure: …"/>` — counted under `skipped`, not `failures` |
| `@expectedFailure` body **passed** (XPASS) | `<failure message="expected to fail but passed — remove @expectedFailure"/>` |

**Duration:**

The `time` attribute on each `<testcase>` is the real wall-clock duration of the scenario in
seconds (e.g., `time="0.142"`). Duration is derived from `ScenarioResult.duration` which
is measured in milliseconds by the scenario executor and converted to seconds for the XML.

---

## LogCollector

`LogCollector` captures log output (`ZIO.log*` calls made from step bodies, tagged with
`scenarioId`/`stepId` annotations) during a run so reporters can attach it to the relevant
step or scenario when rendering.

```scala
trait LogCollector:
  def log(scenarioId: String, stepId: String, message: String, level: InternalLogLevel): ZIO[Any, Nothing, Unit]
  def getLogs(scenarioId: String, stepId: String): ZIO[Any, Nothing, CollectedLogs]
  def getScenarioLogs(scenarioId: String): ZIO[Any, Nothing, CollectedLogs]
```

Supporting types:

```scala
enum InternalLogLevel:
  case Debug, Info, Warning, Error, Fatal

enum LogSource:
  case Stdout
  case Stderr

case class LogEntry(
  message:   String,
  timestamp: Instant,
  source:    LogSource,
  level:     InternalLogLevel,
  stepId:    String
)

case class CollectedLogs(entries: List[LogEntry] = Nil):
  def add(entry: LogEntry): CollectedLogs
  def toStepResultLogs: List[(String, Instant, InternalLogLevel)]

case class LogLevelConfig(minLevel: InternalLogLevel = InternalLogLevel.Info)
```

Entries below `minLevel` are dropped at capture time and never stored.

**Entry point:**

```scala
val layer: ZLayer[Any, Nothing, LogCollector] = LogCollector.live(LogLevelConfig(InternalLogLevel.Debug))
```

`LogCollector.live` installs a custom `ZLogger` (replacing, not augmenting, the runtime default)
so `ZIO.log*` calls inside step bodies are captured only by the collector, not also echoed to
stdout.

**Real consumers:**

- `PrettyReporter` calls `lc.getLogs(scenarioId, stepId)` per step (`PrettyReporter.scala:525`)
  to render log lines as children of the step's `Doc` branch.
- `JUnitXMLReporter` calls `logCollector.getScenarioLogs(scenarioId)` per scenario
  (`JUnitXMLReporter.scala:65`) to embed logs in the generated XML.

**Configuring the minimum level (`@Suite(logLevel)` / `--log-level`):**

All five levels are recognized (case-insensitive): `"debug"`, `"info"`, `"warning"`, `"error"`,
`"fatal"`. An unrecognised value **fails loud** — both `--log-level` and `@Suite(logLevel = "...")`
throw `IllegalArgumentException("Unknown log level '...'. Valid values: debug, info, warning,
error, fatal.")` rather than silently falling back to `Info`. See
`ZIOBDDTask.parseLogLevelString` / `parseLogLevelOrThrow`.

---

## TestEvent streaming

`StreamingReporter` is a lower-level interface that receives a live `ZStream` of `TestEvent`
values rather than a batch of results at the end of the run. This enables real-time output,
incremental file writing, and CI service integrations.

> **`@experimental` — not wired up today.** `TestEvent`, `StreamingReporter`, and its impls
> (`BatchReporterAdapter`, `LiveProgressReporter`) are annotated
> [`@experimental`](https://scala-lang.org/api/current/scala/annotation/experimental.html), so
> referencing them from your own code requires opting into experimental mode (mark the using
> definition `@experimental`, or compile with `-experimental`). This is deliberate: it's a
> standalone API you exercise by hand — build a `ZStream[Any, Nothing, TestEvent]` yourself and
> call `consume` on it. No `@Suite` reporter name or `--reporter` CLI flag selects a
> `StreamingReporter`; the production pipeline (`ZIOBDDFramework.scala`) only ever builds a
> `List[Reporter]` (batch). The "fan-out via `ZHub`" idea mentioned on `StreamingReporter` is
> aspirational — there is no `Hub`/`ZHub` usage anywhere in `core/src/main` today. The API may
> change or be removed without a deprecation cycle until it is wired into the run pipeline.

### TestEvent sealed trait

```scala
import scala.annotation.experimental

@experimental
sealed trait TestEvent

@experimental
object TestEvent:
  /** Emitted once before any feature starts. */
  case class SuiteStarted(featureCount: Int, dryRun: Boolean) extends TestEvent

  /** Emitted when a feature begins execution. */
  case class FeatureStarted(feature: Feature) extends TestEvent

  /** Emitted when a scenario begins execution. */
  case class ScenarioStarted(scenario: Scenario, featureName: String) extends TestEvent

  /** Emitted after each step completes. */
  case class StepFinished(
    step: Step,
    result: StepResult,
    featureName: String,
    scenarioName: String
  ) extends TestEvent

  /** Emitted after a scenario completes (pass/fail/ignore/pending). */
  case class ScenarioFinished(result: ScenarioResult, featureName: String) extends TestEvent

  /** Emitted after a feature completes (all scenarios done). */
  case class FeatureFinished(result: FeatureResult) extends TestEvent

  /** Emitted once after all features complete. */
  case class SuiteFinished(results: List[FeatureResult]) extends TestEvent
```

### StreamingReporter trait

```scala
@experimental
trait StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit]
```

`consume` is called once per test run with the full event stream. It must drain the stream
and may produce any side effects.

### Implementing a custom StreamingReporter

```scala
// A custom StreamingReporter must itself opt into experimental mode.
@experimental
object TeamCityReporter extends StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit] =
    events.runForeach {
      case TestEvent.FeatureStarted(feature) =>
        Console.printLine(s"##teamcity[testSuiteStarted name='${feature.name}']").orDie

      case TestEvent.ScenarioFinished(result, featureName) =>
        val status = if (result.isPassed) "passed" else "failed"
        Console.printLine(
          s"##teamcity[testFinished name='${result.scenario.name}' duration='${result.duration}']"
        ).orDie

      case TestEvent.FeatureFinished(result) =>
        Console.printLine(s"##teamcity[testSuiteFinished name='${result.feature.name}']").orDie

      case _ => ZIO.unit
    }
```

Key points:

- Pattern-match only the events you need; return `ZIO.unit` for the rest.
- `ScenarioResult.duration` is wall-clock milliseconds.
- `FeatureResult.duration` is the sum of all scenario durations for the feature.
- The stream is cold — `consume` is the only subscriber.

### BatchReporterAdapter

`BatchReporterAdapter` wraps a batch `Reporter` (one that receives all results at once) as a
`StreamingReporter`. It does **not** accumulate events — it drains the stream with
`events.runForeach`, ignores every event except `SuiteFinished`, and forwards that event's own
`results` payload straight to `reporter.report`:

```scala
val adapter: StreamingReporter = BatchReporterAdapter(myBatchReporter)
```

Use this when you want to plug a batch reporter into a streaming pipeline.

### LiveProgressReporter

`LiveProgressReporter` is a built-in `StreamingReporter` that prints one line per scenario
as it completes. It is suitable for CI pipelines where you want live output without the full
tree layout of `PrettyReporter`:

```
Running 3 feature(s)...
  ✓ User registration / Valid registration [88ms]
  ○ Shopping cart / Guest checkout [12ms]
  ✗ Payment / Insufficient funds [34ms]
  ⊗ Legacy API / Deprecated endpoint (expected failure) [9ms]
  ✗ Legacy API / Fixed endpoint (unexpectedly passed — remove @expectedFailure) (3 attempts) [11ms]

Done. 1/3 passed, 1 failed, 1 ignored
```

Icons: `✓` passed, `○` ignored, `✗` failed, `⊗` XFAIL (expected failure). An unexpectedly-passing
scenario (XPASS) also renders `✗`, with the note
`(unexpectedly passed — remove @expectedFailure)`. When a scenario ran more than once (retry),
an `(N attempts)` suffix is appended after the note.

`LiveProgressReporter` is an `object` — reference it directly:

```scala
val streaming: StreamingReporter = LiveProgressReporter
```

---

## Result types

Step bodies and reporters interact with the following result types.

### StepStatus

```scala
enum StepStatus:
  case Passed
  case Failed(cause: Cause[Throwable])
  case TimedOut(timeout: Duration, cause: Cause[Throwable])
  case Skipped
  case Pending(reason: String)
```

A step is `Skipped` when a prior step in the same scenario failed and it was never attempted.
A step is `Pending` when its body called `pending("reason")`.
A step is `TimedOut` when it exceeded the configured step timeout (see `docs/running.md`).

### ScenarioResult

```scala
case class ScenarioResult(
  scenario:   Scenario,
  stepResults: List[StepResult],
  setupError: Option[Cause[Throwable]] = None,
  duration:   Long = 0L          // wall-clock milliseconds
):
  def isPassed:    Boolean  // all steps passed or skipped, no setup error
  def isIgnored:   Boolean  // scenario carries the @ignore tag
  def hasPending:  Boolean  // at least one step is Pending
  def hasFailure:  Boolean  // at least one step failed, or setupError is set
  def isComplete:  Boolean  // passed, ignored, or pending — does not block the build
```

### FeatureResult

```scala
case class FeatureResult(
  feature:         Feature,
  scenarioResults: List[ScenarioResult],
  duration:        Long = 0L    // wall-clock milliseconds
):
  def isPassed:   Boolean  // every scenario is passed or ignored
  def isIgnored:  Boolean  // feature carries @ignore, or all scenarios are ignored
  def hasPending: Boolean  // at least one scenario has a pending step
  def isComplete: Boolean  // no scenario has a hard failure
```
