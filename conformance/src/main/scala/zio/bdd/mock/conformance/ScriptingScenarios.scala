package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * The portable scripting conformance scenarios (#132) — the `cap-scripting`
 * feature. Gated on [[Capability.Scripting]], so a backend that does not
 * advertise it (WireMock has no scripting engine) SKIPs them. The script
 * computes the response from the matched request, proving the backend actually
 * runs it (Rift via `_rift.script`).
 */
object ScriptingScenarios:

  lazy val all: List[ConformanceScenario] = List(scriptedResponse)

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private val emptySource = MockSource.Dsl(MockSpec(Nil))

  private def space(control: MockControl): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(emptySource).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  // A Rhai script that computes the body from the request method — so a passing
  // round-trip proves the backend ran the script (a static stub couldn't echo it).
  // Rift's script API returns a decision map; `fault: "error"` is its token for "return this
  // computed { status, body, headers } response" (not an actual error). Here the body is derived
  // from request.method, so a passing round-trip proves the engine ran the script.
  private val rhaiEchoMethod =
    Script(
      ScriptEngine.Rhai,
      "fn should_inject(request, flow_store) { #{inject: true, fault: \"error\", status: 200, " +
        "body: `scripted-${request.method}`, headers: #{\"Content-Type\": \"text/plain\"}} }"
    )

  private val scriptedResponse =
    ConformanceScenario(
      "scripting: the script computes the response from the request",
      Set(Capability.Scripting),
      control =>
        for
          s         <- space(control)
          scripting <- control.scripting.mapError(u => new RuntimeException(u.toString))
          _         <- scripting.inject(s, RequestMatch(path = PathMatch.Exact("/s")), rhaiEchoMethod).mapError(asT)
          resp      <- SutClient.make(s).send(Method.Get, "/s")
          _ <- ensure(
                 resp.status == 200 && resp.body == "scripted-GET",
                 s"scripting: status=${resp.status} body=${resp.body}"
               )
        yield ()
    )
