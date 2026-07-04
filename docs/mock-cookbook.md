# Mock Cookbook: Recipes for MockControl

Task-oriented recipes for the portable mocking SPI. Each section stands alone — pick the
one matching what you're trying to do. For concepts and the full API surface, start at
[Mocking overview](mocking.md).

---

## 1. Stand up a mock backend as a layer

**Problem:** you want a `MockControl` in your suite's environment so steps can provision
mock spaces, without installing Docker or writing backend-specific code.

Add the WireMock adapter — it runs fully in-process on JDK 11+, no containers:

```scala
// build.sbt
libraryDependencies ++= Seq(
  "io.github.etacassiopeia" %% "zio-bdd"          % "1.3.0",
  "io.github.etacassiopeia" %% "zio-bdd-wiremock" % "1.3.0" % Test
)
```

Wire the layer in the suite:

```scala
import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.wiremock.WireMock

@Suite(featureDirs = Array("src/test/resources/features/mock"))
object GreetingMockSuite extends ZIOSteps[MockControl, MockState]:

  override def environment: ZLayer[Any, Throwable, MockControl] =
    Provisioning.live >>> WireMock.correlated()

  // steps that use ZIO.service[MockControl] go here
```

`Provisioning` allocates the ports a backend needs; `Provisioning.live` is the standard
choice (equivalent to `ZLayer(Provisioning.make)`). `WireMock.correlated()` runs one
shared in-process server and routes requests to the right scenario's stubs via a
correlation header — no per-scenario process to start or stop.

**Alternatives:** the Rift container adapter (`Rift.managed()`, needs Docker, gives you
all six capabilities including Faults and Scripting) and embedded Rift
(`EmbeddedRift.layer`, capability-complete, no Docker, but needs a JDK-matched native
build). See [Adapters](mock-adapters.md) for the full comparison and setup steps for both.

---

## 2. Write a portable scenario with `MockSteps`

**Problem:** you want ready-made Gherkin steps for provisioning a mock space and sending a
request through it, and you want the same `.feature` file to run unmodified against
whichever backend you configure — WireMock today, a Rift container in CI tomorrow.

Mix `zio.bdd.mock.steps.MockSteps` into a suite whose environment provides `MockControl`.
The self-type is the only wiring required — no lens, no extra `given`:

```scala
import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.steps.MockSteps

@Suite(featureDirs = Array("src/test/resources/features/mock/basic.feature"))
object BasicMockSuite
    extends ZIOSteps[MockControl, MockState]
    with MockSteps[MockControl, MockState]:

  override def environment: ZLayer[Any, Throwable, MockControl] =
    Provisioning.live >>> WireMock.correlated()
```

```gherkin
Feature: Basic mock matching

  Scenario: A simple stub answers a GET
    Given a mock space returning 200 "hi" at "/hello"
    When a "GET" "/hello" is sent through the space
    Then the space response status is 200
    And the space response body contains "hi"
    And the space received 1 requests
```

`MockSteps` provisions the space, stages it (and the response) in `Stage`, and exposes the
usual assertions (`the space response status is …`, `the space received … requests`, …).
Because the feature text never mentions a backend, you can switch adapters purely by
changing `environment` — for example, reading `MOCK_BACKEND` from the environment and
branching between `WireMock.correlated()`, `WireMock.perInstance`, `Rift.managed()`, and
`EmbeddedRift.layer` the way `support/Backends.scala` does in the zio-bdd-samples corpus:

```scala
val mockControl: ZLayer[Any, Throwable, MockControl] =
  sys.env.getOrElse("MOCK_BACKEND", "wiremock") match
    case "wiremock" => Provisioning.live >>> WireMock.correlated()
    // Rift and embedded fail with the typed `MockError` (not a `Throwable`), so map it into the
    // layer's `Throwable` error channel — exactly as support/Backends.scala does:
    case "rift"     => (Client.default ++ Provisioning.live) >>> Rift.managed().mapError(e => new RuntimeException(s"Rift: $e"))
    case "embedded" => Provisioning.live >>> EmbeddedRift.layer.mapError(e => new RuntimeException(s"Embedded Rift: $e"))
```

Run the same suite against each backend from the command line by setting the env var
before `sbt test`. See [Gherkin integration](mock-gherkin.md) for the full step catalog and
[Adapters](mock-adapters.md) for what each backend needs at runtime.

---

## 3. Parameterize mocks with `@mock(...)` fixtures

**Problem:** several scenarios each need a different pre-built mock space, and you don't
want every scenario writing out its own `Given` rule-builder steps.

Define a catalog once — a `Map[String, MockSource]` — and reference entries by name from
the feature file with `@mock(name)`:

```scala
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

object Catalog:
  val entries: Map[String, MockSource] = Map(
    "greeting" -> mock(get("/hello").respondWith(ok.text("hi"))).source,
    "farewell" -> mock(get("/bye").respondWith(ok.text("bye"))).source
  )
```

Wire `MockFixtures.scenario` into `scenarioLayer` so the tagged entries are provisioned
fresh per scenario and torn down automatically when the scenario's fixture scope closes.
The environment then carries both `MockControl` and the resulting `MockFixture`:

```scala
import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.ScenarioMetadata
import zio.bdd.mock.*
import zio.bdd.mock.fixtures.{MockFixture, MockFixtures}

@Suite(featureDirs = Array("src/test/resources/features/mock/fixtures.feature"))
object FixturesSuite extends ZIOSteps[MockControl & MockFixture, MockState]:

  override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, MockControl & MockFixture] =
    Provisioning.live >>> WireMock.correlated() >+> MockFixtures.scenario(meta, Catalog.entries)

  Then("the fixture provisioned " / int / " spaces") { (n: Int) =>
    ZIO.service[MockFixture].flatMap(fx => Assertions.assertEquals(fx.spaces.size, n, "fixture spaces"))
  }
```

```gherkin
@mock @fixtures
Feature: Scenario-tier mock fixtures

  @mock(greeting)
  Scenario: The greeting fixture is deployed for the tagged scenario
    Then the fixture provisioned 1 spaces
    When the client sends "GET" "/hello"
    Then the response body is "hi"

  @mock(greeting, farewell)
  Scenario: Two named fixtures are deployed together
    Then the fixture provisioned 2 spaces
```

A scenario can list several names in one `@mock(a, b)` tag; each resolves to its catalog
entry and all are provisioned together. A name with no catalog entry fails the scenario
at setup, not later inside a step. See [Gherkin integration](mock-gherkin.md) for
`MockFixtures.feature` (feature-tier, shared across a feature's scenarios) and
`MockFixtures.byFlag` (drive the entry from a `@flags(...)` value).

---

## 4. Inject a fault across adapters

**Problem:** you want to verify a client's failure handling — a dropped connection, a
malformed response, a slow backend — without every scenario hardcoding which adapter it
runs against.

Faults are a capability (`Capability.Faults`), reached through `MockControl.faults`.
Guard the scenario with `require` so it fails fast and legibly on a backend that doesn't
advertise the capability, instead of failing deep inside an assertion:

```scala
import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

Given("the backend supports faults") {
  ZIO.serviceWithZIO[MockControl](_.require(Capability.Faults)).mapError(u => new RuntimeException(u.message))
}

Given("a " / string / " fault is injected for GET " / string) { (kind: String, path: String) =>
  for
    space <- activeSpace // your own helper: the scenario's provisioned MockSpace, from Stage
    fs    <- ZIO.serviceWithZIO[MockControl](_.faults).mapError(u => new RuntimeException(u.message))
    _     <- fs.inject(space, get(path), FaultKind.ConnectionReset).mapError(e => new RuntimeException(s"$e"))
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

```gherkin
@mock @faults
Feature: Fault injection

  Scenario: A connection reset aborts the client connection
    Given the backend supports faults
    And a "ConnectionReset" fault is injected for GET "/boom"
    When GET "/boom" is attempted
    Then the request fails at the transport level

  @slow
  Scenario: A latency spike delays an otherwise normal response
    Given the backend supports faults
    And a latency fault of 300 millis is injected for GET "/slow"
    When GET "/slow" is attempted
    Then the request succeeds after at least 300 millis
```

`FaultKind.ConnectionReset`, `EmptyResponse`, `MalformedChunk`, and `RandomThenClose` all
abort the transport; `LatencySpike(duration)` delays an otherwise-normal response instead
of breaking it. WireMock, Rift container, and embedded Rift all advertise
`Capability.Faults`, so this recipe runs on every adapter in the [capability
matrix](mock-adapters.md) — the `require` step is what lets the *same* feature file degrade
into an explicit skip/fail on a hypothetical backend that lacks the capability, rather than
an assertion failure that hides the real cause. See [Advanced](mock-advanced.md) for the
other five capabilities (stateful scenarios, state inspection, scripting, proxy-record,
templating) and how negotiation composes with `require`.

---

## See also

- [Mocking overview](mocking.md) — concepts: `MockControl`, `MockSpace`, isolation, capabilities.
- [the DSL](mock-dsl.md) — the fluent rule-builder (`get`, `.where`, `.respondWith`, …).
- [Adapters](mock-adapters.md) — WireMock vs Rift container vs embedded Rift, capability matrix, JDK/Docker requirements.
- [Gherkin integration](mock-gherkin.md) — `MockSteps`, the `@mock(...)` tag, `MockFixtures`.
- [Advanced](mock-advanced.md) — stateful scenarios, state inspection, scripting, proxy-record, templating.
- [Cookbook](cookbook.md) — general suite-writing recipes (state, steps, tagging) that apply outside mocking too.
