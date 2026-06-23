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

### pretty

`PrettyReporter` writes a Unicode tree to the console using ANSI colors. It is the default
reporter when `reporters` is not specified.

**Color palette:**

| Element | Color | Icon |
|---|---|---|
| Feature — passed | Green (`\e[92m`) | `◉` |
| Feature — ignored | Gray (`\e[90m`) | `◉` |
| Feature — failed | Red (`\e[91m`) | `◉` |
| Scenario — passed | Yellow (`\e[93m`) | `◑` |
| Scenario — ignored | Gray (`\e[90m`) | `◑` |
| Scenario — pending | Orange (`\e[38;5;214m`) | `◑` |
| Scenario — failed | Red (`\e[91m`) | `◑` |
| Step — passed | Blue (`\e[94m`) | `•` |
| Step — skipped | Gray (`\e[90m`) | `•` |
| Step — pending | Orange (`\e[38;5;214m`) | `•` |
| Step — failed | Red (`\e[91m`) | `•` |
| Summary / headings | Cyan (`\e[96m`) | — |

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

**Duration:**

The `time` attribute on each `<testcase>` is the real wall-clock duration of the scenario in
seconds (e.g., `time="0.142"`). Duration is derived from `ScenarioResult.duration` which
is measured in milliseconds by the scenario executor and converted to seconds for the XML.

---

## TestEvent streaming

`StreamingReporter` is a lower-level interface that receives a live `ZStream` of `TestEvent`
values rather than a batch of results at the end of the run. This enables real-time output,
incremental file writing, and CI service integrations.

### TestEvent sealed trait

```scala
sealed trait TestEvent

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
trait StreamingReporter:
  def consume(events: ZStream[Any, Nothing, TestEvent]): ZIO[LogCollector, Throwable, Unit]
```

`consume` is called once per test run with the full event stream. It must drain the stream
and may produce any side effects.

### Implementing a custom StreamingReporter

```scala
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
`StreamingReporter`. It accumulates events and calls `reporter.report` when `SuiteFinished`
arrives:

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

Done. 1/3 passed, 1 failed, 1 ignored
```

Icons: `✓` passed, `○` ignored, `✗` failed.

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
