package zio.bdd.mock.steps

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*

import java.net.URI
import java.net.http.{HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}
import scala.jdk.CollectionConverters.*

/**
 * The last response from sending a request through a mock space — staged per
 * scenario so the response-assertion steps can read it.
 */
final case class MockResponse(status: Int, body: String, headers: Headers)

/**
 * Mixin providing ready-made Gherkin steps for the portable mocking SPI,
 * modelled on [[zio.bdd.http.HttpSteps]].
 *
 * Mix into any `ZIOSteps` suite whose environment provides a [[MockControl]] —
 * the self-type `ZIOSteps[R & MockControl, S]` is the only wiring required (no
 * lens, no extra given). The active [[MockSpace]] and the last response are
 * kept in [[Stage]] (not scenario state) because a `MockSpace` carries a
 * function and is not Schema-serialisable.
 *
 * ===Registered steps===
 *   - `Given a mock space returning {int} {string} at {string}` — provision a
 *     space
 *   - `When the space overlays {int} {string} at {string}` — overlay a rule
 *   - `When a {string} {string} is sent through the space` — send (honours
 *     `inject`)
 *   - `Then the space response status is {int}` / `… body contains {string}` /
 *     `… header {string} is {string}` — assert the staged response
 *   - `Then the space received {int} requests` — assert recorded count
 *     (space-local)
 *   - `Then a {string} request to {string} was recorded` — assert ≥1 match
 *   - `Then exactly {int} {string} requests to {string} were recorded` — assert
 *     exact count
 *
 * @tparam R
 *   the suite environment (must provide `MockControl`)
 * @tparam S
 *   the suite scenario state (unused by these steps)
 */
trait MockSteps[R, S] { self: ZIOSteps[R & MockControl, S] =>

  /**
   * The `@mock(name)` catalog: the named [[MockSource]] entries this suite can
   * deploy. Override it to declare the suite's catalog once, then reference it
   * when wiring the scenario fixtures (e.g. `MockFixtures.scenario(meta,
   * mockCatalog)`).
   *
   * Declaring it here — rather than only inline at layer-construction time —
   * makes the catalog statically discoverable via [[allMocks]], so editor
   * tooling can offer completion and unknown-name diagnostics for `@mock(...)`
   * tags. Pure data: reading it provisions nothing.
   */
  def mockCatalog: Map[String, MockSource] = Map.empty

  /**
   * A static summary of every [[mockCatalog]] entry, for tooling discovery of
   * `@mock(name)` tags — the catalog counterpart of `ZIOSteps.allDefinitions`.
   *
   * Reflectively callable from a no-arg-constructed suite instance without any
   * live mock backend: it only reads the pure [[mockCatalog]] map and never
   * provisions a space. Backend-agnostic — identical whether the suite runs on
   * Rift container, Rift embedded, or WireMock. Ordered by `name` so tooling
   * gets a stable list (the underlying map has no defined order).
   */
  def allMocks: List[MockSummary] =
    mockCatalog.toList.sortBy(_._1).map { case (name, source) => MockSummary(name, source.productPrefix) }

  Given("a mock space returning " / int / " " / string / " at " / string) { (status: Int, body: String, path: String) =>
    for
      spaces <- mock(_.provision(MockSource.Dsl(MockSpec(List(ruleFor(status, body, path))))))
      space  <- ZIO.fromOption(spaces.headOption).orElseFail(new IllegalStateException("provision returned no space"))
      _      <- Stage.put(space)
    yield ()
  }

  When("the space overlays " / int / " " / string / " at " / string) { (status: Int, body: String, path: String) =>
    for
      space <- activeSpace
      _     <- mock(_.addRule(space, ruleFor(status, body, path), Priority.Overlay))
    yield ()
  }

  When("a " / string / " " / string / " is sent through the space") { (method: String, path: String) =>
    for
      space <- activeSpace
      resp  <- sendThrough(space, method, path)
      _     <- Stage.put(resp)
    yield ()
  }

  Then("the space response status is " / int) { (expected: Int) =>
    stagedResponse.flatMap(r => Assertions.assertEquals(r.status, expected, "space response status mismatch"))
  }

  Then("the space response body contains " / string) { (expected: String) =>
    stagedResponse.flatMap(r =>
      Assertions.assertTrue(r.body.contains(expected), s"space response body '${r.body}' does not contain '$expected'")
    )
  }

  Then("the space response header " / string / " is " / string) { (name: String, expected: String) =>
    stagedResponse.flatMap { r =>
      r.headers.first(name) match
        case Some(actual) => Assertions.assertEquals(actual, expected, s"response header '$name' mismatch")
        case None =>
          ZIO.fail(
            new AssertionError(s"response header '$name' not present. Headers: ${r.headers.keys.mkString(", ")}")
          )
    }
  }

  Then("the space received " / int / " requests") { (expected: Int) =>
    for
      space <- activeSpace
      reqs  <- mock(_.received(space))
      _     <- Assertions.assertEquals(reqs.size, expected, s"recorded request count mismatch for space ${space.id.value}")
    yield ()
  }

  Then("a " / string / " request to " / string / " was recorded") { (method: String, path: String) =>
    for
      space <- activeSpace
      reqs  <- mock(_.received(space))
      _ <- Assertions.assertTrue(
             countMatching(reqs, method, path) >= 1,
             s"no recorded $method request to $path. Recorded: ${reqs.map(r => s"${r.method} ${r.uri}").mkString(", ")}"
           )
    yield ()
  }

  Then("exactly " / int / " " / string / " requests to " / string / " were recorded") {
    (expected: Int, method: String, path: String) =>
      for
        space <- activeSpace
        reqs  <- mock(_.received(space))
        _ <-
          Assertions.assertEquals(countMatching(reqs, method, path), expected, s"recorded $method $path count mismatch")
      yield ()
  }

  // ── helpers ─────────────────────────────────────────────────────────────

  /**
   * Run a MockControl operation, surfacing its typed error on the step's
   * Throwable channel.
   */
  private def mock[A](f: MockControl => IO[MockError, A]): ZIO[R & MockControl, Throwable, A] =
    ZIO.serviceWithZIO[MockControl](f).mapError(e => new RuntimeException(s"MockControl: ${describe(e)}"))

  private val activeSpace: IO[Throwable, MockSpace] =
    Stage
      .get[MockSpace]
      .mapError(e => new IllegalStateException(s"no active mock space — deploy one first (${e.message})"))

  private val stagedResponse: IO[Throwable, MockResponse] =
    Stage
      .get[MockResponse]
      .mapError(e => new IllegalStateException(s"no response staged — send a request first (${e.message})"))

  private def ruleFor(status: Int, body: String, path: String): MockRule =
    MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status = status, body = Body.Text(body)))

  private def countMatching(reqs: List[RecordedRequest], method: String, path: String): Int =
    reqs.count(r => r.method.toString.equalsIgnoreCase(method) && r.uri == path)

  private def methodOf(name: String): IO[Throwable, Method] =
    ZIO
      .fromOption(Method.values.find(_.toString.equalsIgnoreCase(name)))
      .orElseFail(
        new IllegalArgumentException(
          s"unknown HTTP method '$name'; expected one of ${Method.values.map(_.toString.toUpperCase).mkString(", ")}"
        )
      )

  /**
   * Send a request through the space: build the canonical request, apply the
   * space's `inject` (the isolation decoration), then issue exactly the
   * injected request — method included — with the JDK client.
   */
  private def sendThrough(space: MockSpace, method: String, path: String): IO[Throwable, MockResponse] =
    methodOf(method).flatMap { m =>
      val injected = space.inject(HttpRequest(m, space.baseUri + path))
      ZIO.attemptBlocking {
        val builder = injected.headers.entries.foldLeft(JHttpRequest.newBuilder(URI.create(injected.uri))) {
          case (b, (k, vs)) => vs.foldLeft(b)((bb, v) => bb.header(k, v))
        }
        val publisher = injected.body match
          case Some(body) => JHttpRequest.BodyPublishers.ofString(body)
          case None       => JHttpRequest.BodyPublishers.noBody()
        val request = builder.method(injected.method.toString.toUpperCase, publisher).build()
        // Pinned to HTTP/1.1 for the same reason as SutClient (#183): mock servers are cleartext
        // HTTP/1.1 and the default HTTP/2 h2c path intermittently EOFs a 201.
        val response = SutClient.http1Client.send(request, JHttpResponse.BodyHandlers.ofString())
        val headers = response.headers().map().asScala.foldLeft(Headers.empty) { case (h, (k, vs)) =>
          vs.asScala.foldLeft(h)((acc, v) => acc.add(k, v))
        }
        MockResponse(response.statusCode, response.body, headers)
      }
    }

  private def describe(e: MockError): String = e match
    case MockError.ProvisionFailed(r)    => s"provision failed: $r"
    case MockError.SpaceNotFound(s)      => s"space not found: ${s.value}"
    case MockError.RuleNotFound(s, r)    => s"rule ${r.value} not found in space ${s.value}"
    case MockError.InvalidDefinition(r)  => s"invalid definition: $r"
    case MockError.CommunicationError(r) => s"communication error: $r"
}
