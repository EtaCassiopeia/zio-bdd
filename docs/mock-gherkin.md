# Gherkin Integration

`zio.bdd.mock.steps.MockSteps` and `zio.bdd.mock.fixtures.MockFixtures` connect
the portable mocking SPI to Gherkin: ready-made step definitions for the mock
space itself, and tag-driven fixtures that deploy named catalog entries around
each scenario or feature.

---

## 1. `MockSteps[R, S]`

`MockSteps` is a mixin, not a base class — it requires the self-type
`ZIOSteps[R & MockControl, S]`, so any suite whose environment provides a
`MockControl` can mix it in and get a full set of mock-space steps for free:

```scala
trait MockSteps[R, S] { self: ZIOSteps[R & MockControl, S] =>
  ...
}
```

The active `MockSpace` and the last response live in [`Stage`](#4-stage-for-mock-data),
not scenario state — a `MockSpace` carries a function (`inject`) and isn't
Schema-serialisable, so it can't be `S`.

### Registered steps

| Step | Effect |
|---|---|
| `Given a mock space returning {int} {string} at {string}` | Provisions a space with one rule (`status`, `body`, `path`); stages the resulting `MockSpace` |
| `When the space overlays {int} {string} at {string}` | Adds an overlay rule to the staged space via `addRule(space, rule, Priority.Overlay)` |
| `When a {string} {string} is sent through the space` | Sends `method path` through the staged space (honours `inject`, the isolation decoration); stages the `MockResponse` |
| `Then the space response status is {int}` | Asserts the staged response's status |
| `Then the space response body contains {string}` | Asserts the staged response's body contains a substring |
| `Then the space response header {string} is {string}` | Asserts a header on the staged response |
| `Then the space received {int} requests` | Asserts `received(space).size` |
| `Then a {string} request to {string} was recorded` | Asserts at least one recorded request matches method + path |
| `Then exactly {int} {string} requests to {string} were recorded` | Asserts an exact recorded count for method + path |

`Given a mock space returning ...` and `When the space overlays ...` build a
single-rule `MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status, Body.Text(body)))`
— reach for a catalog entry (§3) or the DSL ([the DSL](mock-dsl.md)) once a
scenario needs more than an exact-path text response.

### Mixing it in

```scala
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.steps.MockSteps

object SpaceSuite extends ZIOSteps[MockControl, Unit] with MockSteps[MockControl, Unit]:
  override def scenarioLayer(meta: zio.bdd.gherkin.ScenarioMetadata) =
    Backends.mockControl
```

Every step MockSteps registers is now available to that suite's `.feature`
files — no lens, no extra `given`, just `MockControl` in the environment.

---

## 2. The `@mock(name, ...)` tag + fixtures

`@mock(name, other)` on a `Scenario` names catalog entries to deploy for that
scenario. `MockFixtures` resolves the names against a `Map[String, MockSource]`
catalog, provisions each source, and exposes the result as `MockFixture`:

```scala
final case class MockFixture(spaces: List[MockSpace])
```

Three constructors, one per fixture tier:

- **`MockFixtures.scenario(meta, catalog)`** — reads `@mock(...)` names off
  `meta.tags` (via `MockTag.extract`), resolves each against `catalog`, and
  provisions them fresh for that scenario. Share-nothing: a name with no
  catalog entry fails the scenario at setup. Wire it into `scenarioLayer` so
  it deploys and tears down per scenario.
- **`MockFixtures.feature(sources*)`** — provisions a fixed list of sources
  once per feature (the three-tier layer model memoizes this at the feature
  scope). Use it for heavy, read-only fixtures shared by every scenario in the
  feature, wired into a suite's feature-tier layer instead of `scenarioLayer`.
- **`MockFixtures.byFlag(reader, flag, catalog)`** — deploys the single
  catalog entry named by the *value* of `flag`, read through a `FlagReader`.
  `FlagReader.fromMetadata(meta)` reads the scenario's `@flags(k=v)` matrix tag
  (`meta.flagValues`) — the in-framework source, no SDK required (an
  OpenFeature-backed reader can plug in the same trait from outside zio-bdd).
  This composes with `@flags` expansion so one tagged scenario exercises each
  flag branch against its matching mock — see
  [Feature Flag Testing](testing-flags.md).

All three return `ZLayer[MockControl, Throwable, MockFixture]` and provision
through `ZIO.acquireRelease`: each source is its own scoped acquisition, so a
failure partway through still tears down the spaces already created, and a
teardown failure is logged and skipped rather than stranding its siblings.

---

## 3. The catalog pattern

A catalog is nothing more than `Map[String, MockSource]` — named, reusable
mock sources built with [the DSL](mock-dsl.md) and lifted with `.source`. From
the samples corpus, `support/Catalog.scala`:

```scala
package samples.mock.support

import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

/**
 * Named mock sources deployed by scenario-tier `@mock(name)` fixtures
 * ([[zio.bdd.mock.fixtures.MockFixtures]]). A scenario tagged `@mock(greeting)`
 * gets the `greeting` space provisioned fresh and torn down in its fixture scope.
 */
object Catalog:
  val entries: Map[String, MockSource] = Map(
    "greeting" -> mock(get("/hello").respondWith(ok.text("hi"))).source,
    "farewell" -> mock(get("/bye").respondWith(ok.text("bye"))).source
  )
```

Keep catalogs in a `support` object alongside a suite, one entry per named
fixture a `.feature` file references by `@mock(name)`. Nothing stops a catalog
entry from being `MockSource.Json(raw)`, `.Resource(path)`, `.File(path)`, or
`.Dir(path)` instead of `.Dsl(...)` — the catalog only fixes the *name*, not
the source kind.

### Static discovery for tooling (`allMocks`)

Override `MockSteps.mockCatalog` on the suite to declare the catalog once, then
reference it when wiring the scenario fixtures — the same map drives live
provisioning *and* becomes statically discoverable:

```scala
object MySuite extends ZIOSteps[MockControl, S] with MockSteps[MockControl, S]:
  override def mockCatalog: Map[String, MockSource] = Catalog.entries
  override def scenarioLayer(meta: ScenarioMetadata) = MockFixtures.scenario(meta, mockCatalog)
```

`MockSteps.allMocks` then returns a `List[MockSummary]` (each `name` +
`sourceKind`, name-sorted) — the catalog counterpart of `ZIOSteps.allDefinitions`
([step discovery](step-dsl.md)). Like `allDefinitions`, it is a pure reflection
target the editor tooling (LSP server, IntelliJ plugin) reads from a
no-arg-constructed suite instance with **no** live mock backend — provisioning
nothing — so it can offer `@mock(name)` completion and unknown-name diagnostics.
It is backend-agnostic: identical whether the suite runs on Rift container, Rift
embedded, or WireMock.

---

## 4. `Stage` for mock data

A `MockSpace` carries a function (`inject`) and a `MockResponse` isn't part of
most suites' scenario state `S` — so both are threaded between steps with
`Stage`, not `S`:

```scala
def put[A: ClassTag](value: A): UIO[Unit]
def get[A: ClassTag]: IO[StagingError, A]
```

`MockSteps` uses exactly this pattern internally:

```scala
Given("a mock space returning " / int / " " / string / " at " / string) { (status: Int, body: String, path: String) =>
  for
    spaces <- mock(_.provision(MockSource.Dsl(MockSpec(List(ruleFor(status, body, path))))))
    space  <- ZIO.fromOption(spaces.headOption).orElseFail(new IllegalStateException("provision returned no space"))
    _      <- Stage.put(space)
  yield ()
}

When("a " / string / " " / string / " is sent through the space") { (method: String, path: String) =>
  for
    space <- activeSpace          // Stage.get[MockSpace]
    resp  <- sendThrough(space, method, path)
    _     <- Stage.put(resp)       // MockResponse, read by the Then steps
  yield ()
}
```

`Stage.get[A]` fails with `StagingError` (`NotFound`/`TypeMismatch`) if nothing
of that type was staged yet — the same reason `MockSteps`'s `activeSpace` and
`stagedResponse` helpers wrap it with a clearer message ("no active mock
space — deploy one first"). Custom step definitions that provision or send
through a space directly should follow the same convention: `Stage.put` the
`MockSpace` after provisioning, `Stage.put` the response after sending, so
downstream `Then` steps can `Stage.get` either without touching `S`.

---

## 5. Complete example

`10-fixtures-tags.feature` — two scenarios, each tagged with `@mock(...)`:

```gherkin
@mock @fixtures @portable
Feature: Scenario-tier mock fixtures — @mock(name) deploys catalog entries

  @mock(greeting)
  Scenario: The greeting fixture is deployed for the tagged scenario
    Then the fixture provisioned 1 spaces
    When the client sends "GET" "/hello"
    Then the response body is "hi"

  @mock(greeting, farewell)
  Scenario: Two named fixtures are deployed together
    Then the fixture provisioned 2 spaces
```

`Fixtures10Suite.scala` wires `MockFixtures.scenario` on top of the backend
layer and reads the resulting `MockFixture` from the environment:

```scala
package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.gherkin.ScenarioMetadata
import zio.bdd.mock.*
import zio.bdd.mock.fixtures.{MockFixture, MockFixtures}
import samples.mock.support.{Backends, Catalog, MockState}

@Suite(
  featureDirs = Array("src/test/resources/features/mock/10-fixtures-tags.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Fixtures10Suite extends ZIOSteps[MockControl & MockFixture, MockState]:

  override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, MockControl & MockFixture] =
    Backends.mockControl >+> MockFixtures.scenario(meta, Catalog.entries)

  When("the client sends " / string / " " / string) { (m: String, path: String) =>
    for
      method <- ZIO
                  .fromOption(Method.values.find(_.toString.equalsIgnoreCase(m)))
                  .orElseFail(new IllegalArgumentException(s"bad method $m"))
      fx    <- ZIO.service[MockFixture]
      space <- ZIO.fromOption(fx.spaces.headOption).orElseFail(new IllegalStateException("fixture provisioned no space"))
      resp  <- SutClient.make(space).send(method, path)
      _     <- Stage.put(resp)
    yield ()
  }

  Then("the response body is " / string) { (expected: String) =>
    Stage
      .get[SutResponse]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(r => Assertions.assertEquals(r.body, expected, "response body"))
  }

  Then("the fixture provisioned " / int / " spaces") { (n: Int) =>
    ZIO.service[MockFixture].flatMap(fx => Assertions.assertEquals(fx.spaces.size, n, "fixture spaces"))
  }
```

`scenarioLayer(meta) = Backends.mockControl >+> MockFixtures.scenario(meta, Catalog.entries)`
is the whole wiring: `Backends.mockControl` supplies `MockControl` (picking
the adapter — see [Adapters](mock-adapters.md)), and `>+>` adds
`MockFixtures.scenario`'s `MockFixture` alongside it, so the suite's
environment is `MockControl & MockFixture` and every `@mock(...)`-tagged
scenario gets its named spaces provisioned and torn down automatically.

---

Next: [Advanced](mock-advanced.md) · [Cookbook](mock-cookbook.md)

See also: [Mocking overview](mocking.md) · [the DSL](mock-dsl.md) ·
[Adapters](mock-adapters.md) · [Feature Flag Testing](testing-flags.md)
