package zio.bdd.mock.steps

import zio.*
import zio.bdd.core.FeatureResult
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.*
import zio.bdd.mock.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import scala.jdk.CollectionConverters.*

/**
 * Tests for MockSteps — the pre-built mock step mixin. Drives the mixin against
 * a real in-process HTTP backend (a tiny `com.sun.net.httpserver` server) that
 * implements `MockControl` directly, partitioning spaces by an id path prefix
 * so recordings are genuinely space-local.
 */
object MockStepsSpec extends ZIOSpecDefault:

  final case class TestState()
  object TestState:
    given Schema[TestState] = DeriveSchema.gen[TestState]

  // ── In-process MockControl backed by one HttpServer ─────────────────────────

  private final class TestBackend(
    server: HttpServer,
    recordings: ConcurrentHashMap[String, CopyOnWriteArrayList[RecordedRequest]],
    stubs: ConcurrentHashMap[String, (Int, String)],
    counter: AtomicInteger
  ) extends MockControl:
    private val port                  = server.getAddress.getPort
    def backendName: String           = "test"
    def capabilities: Set[Capability] = Set.empty

    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      source match
        case MockSource.Dsl(spec) =>
          ZIO.succeed {
            val id = "s" + counter.incrementAndGet()
            recordings.put(id, new CopyOnWriteArrayList[RecordedRequest]())
            spec.rules.foreach(registerStub(id, _))
            List(MockSpace(s"http://localhost:$port/$id", identity, SpaceId(id)))
          }
        case _ => ZIO.fail(MockError.InvalidDefinition("test backend only supports Dsl sources"))

    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      ZIO.succeed { registerStub(space.id.value, rule); RuleId("r") }

    def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
      ZIO.succeed(Option(recordings.get(space.id.value)).map(_.asScala.toList).getOrElse(Nil))

    def destroy(space: MockSpace): IO[MockError, Unit] =
      ZIO.succeed { recordings.remove(space.id.value); () }

    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.fail(MockError.InvalidDefinition("unsupported"))
    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]              = ZIO.unit
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] = ZIO.unit
    def faults: IO[Unsupported, Faults]                                            = ZIO.fail(Unsupported(Capability.Faults, backendName))
    def scenarios: IO[Unsupported, StatefulScenarios]                              = ZIO.fail(Unsupported(Capability.StatefulScenarios, backendName))
    def stateInspection: IO[Unsupported, StateInspection] =
      ZIO.fail(Unsupported(Capability.StateInspection, backendName))
    def scripting: IO[Unsupported, Scripting]     = ZIO.fail(Unsupported(Capability.Scripting, backendName))
    def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.fail(Unsupported(Capability.ProxyRecord, backendName))
    def templating: IO[Unsupported, Templating]   = ZIO.fail(Unsupported(Capability.Templating, backendName))

    private def registerStub(spaceId: String, rule: MockRule): Unit =
      val path = rule.`match`.path match
        case PathMatch.Exact(p) => p
        case _                  => "/"
      val body = rule.respond.body match
        case Body.Text(v) => v
        case Body.Json(v) => v
        case _            => ""
      stubs.put(stubKey(spaceId, path), (rule.respond.status, body))

  private def stubKey(spaceId: String, path: String): String = spaceId + " " + path

  private def methodOf(name: String): Method =
    Method.values.find(_.toString.equalsIgnoreCase(name)).getOrElse(Method.Get)

  private val makeBackend: ZIO[Scope, Throwable, MockControl] =
    ZIO
      .acquireRelease(
        ZIO.attempt {
          val recordings = new ConcurrentHashMap[String, CopyOnWriteArrayList[RecordedRequest]]()
          val stubs      = new ConcurrentHashMap[String, (Int, String)]()
          val server     = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
          server.createContext(
            "/",
            new HttpHandler:
              def handle(exchange: HttpExchange): Unit =
                val segments = exchange.getRequestURI.getPath.stripPrefix("/").split("/", 2)
                val spaceId  = segments(0)
                val path     = "/" + (if segments.length > 1 then segments(1) else "")
                val body     = new String(exchange.getRequestBody.readAllBytes())
                Option(recordings.get(spaceId)).foreach { rec =>
                  rec.add(
                    RecordedRequest(
                      methodOf(exchange.getRequestMethod),
                      path,
                      Headers.empty,
                      Option(body).filter(_.nonEmpty)
                    )
                  )
                }
                val (status, payload) = Option(stubs.get(stubKey(spaceId, path))).getOrElse((404, ""))
                val bytes             = payload.getBytes
                exchange.getResponseHeaders.set("X-Served-By", "test-mock")
                exchange.sendResponseHeaders(status, if bytes.isEmpty then -1 else bytes.length.toLong)
                val os = exchange.getResponseBody
                os.write(bytes)
                os.close()
          )
          server.start()
          (server, new TestBackend(server, recordings, stubs, new AtomicInteger(0)): MockControl)
        }
      )((acquired) => ZIO.attempt(acquired._1.stop(0)).orDie)
      .map(_._2)

  // ── Suite mixing in MockSteps ───────────────────────────────────────────────

  private class MockSuite(control: MockControl)
      extends ZIOSteps[MockControl, TestState]
      with MockSteps[MockControl, TestState]:
    override def environment: ZLayer[Any, Throwable, MockControl] = ZLayer.succeed(control)

  private val F = "mock-steps.feature"

  private def run(steps: List[Step]): ZIO[Any, Throwable, FeatureResult] =
    ZIO.scoped {
      makeBackend.flatMap { backend =>
        new MockSuite(backend)
          .run(List(Feature("Mock", Nil, List(Scenario("s", Nil, steps, Some(F), Some(1))))))
          .provideEnvironment(ZEnvironment(backend))
          .map(_.head)
      }
    }

  private def step(t: StepType, p: String) = Step(t, p, None, None, None)
  private val G                            = StepType.GivenStep
  private val W                            = StepType.WhenStep
  private val T                            = StepType.ThenStep

  def spec = suite("MockSteps")(
    test("AC1: a suite mixing MockSteps deploys, sends, and asserts the response with only MockControl in R") {
      run(
        List(
          step(G, """a mock space returning 200 "pong" at "/ping""""),
          step(W, """a "GET" "/ping" is sent through the space"""),
          step(T, "the space response status is 200"),
          step(T, """the space response body contains "pong""""),
          step(T, """the space response header "x-served-by" is "test-mock"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("AC2: recorded-request assertions read received(space) and are space-local") {
      run(
        List(
          step(G, """a mock space returning 200 "a" at "/x""""), // space A
          step(W, """a "GET" "/x" is sent through the space"""),
          step(W, """a "GET" "/x" is sent through the space"""),
          step(T, "the space received 2 requests"), // A recorded 2
          step(T, """a "GET" request to "/x" was recorded"""),
          step(G, """a mock space returning 200 "b" at "/y""""), // space B becomes active
          step(W, """a "GET" "/y" is sent through the space"""),
          step(T, "the space received 1 requests") // B has 1, NOT 3 -> space-local
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("the recorded-count assertion fails when the active space's count differs (negative)") {
      run(
        List(
          step(G, """a mock space returning 200 "pong" at "/ping""""),
          step(W, """a "GET" "/ping" is sent through the space"""),
          step(T, "the space received 5 requests") // actually 1
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    },
    test("an overlay rule added to the active space is served") {
      run(
        List(
          step(G, """a mock space returning 404 "" at "/over""""),
          step(W, """the space overlays 200 "yes" at "/over""""),
          step(W, """a "GET" "/over" is sent through the space"""),
          step(T, "the space response status is 200"),
          step(T, """the space response body contains "yes"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("the exact-count assertion filters recorded requests by method (and covers non-GET sends)") {
      run(
        List(
          step(G, """a mock space returning 200 "ok" at "/x""""),
          step(W, """a "GET" "/x" is sent through the space"""),
          step(W, """a "GET" "/x" is sent through the space"""),
          step(W, """a "POST" "/x" is sent through the space"""),
          step(T, """exactly 2 "GET" requests to "/x" were recorded"""), // POST excluded by the method filter
          step(T, "the space received 3 requests")                       // but all 3 are recorded
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("a step used before deploying a space fails with a clear error") {
      run(List(step(T, "the space received 0 requests")))
        .map(r => assertTrue(!r.isPassed, r.error.isDefined))
    },
    test("asserting a response header that is absent fails") {
      run(
        List(
          step(G, """a mock space returning 200 "ok" at "/h""""),
          step(W, """a "GET" "/h" is sent through the space"""),
          step(T, """the space response header "x-absent" is "nope"""")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    },
    test("a 201-with-body response round-trips through sendThrough (the case that EOFs under HTTP/2) (#183)") {
      run(
        List(
          step(G, """a mock space returning 201 "created" at "/thing""""),
          step(W, """a "POST" "/thing" is sent through the space"""),
          step(T, "the space response status is 201"),
          step(T, """the space response body contains "created"""")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("a send to an unstubbed path returns 404 through the space") {
      run(
        List(
          step(G, """a mock space returning 200 "ok" at "/known""""),
          step(W, """a "GET" "/unknown" is sent through the space"""),
          step(T, "the space response status is 404")
        )
      ).map(r => assertTrue(r.isPassed))
    }
  )
