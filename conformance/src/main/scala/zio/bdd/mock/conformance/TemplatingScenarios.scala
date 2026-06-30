package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * The portable templating conformance scenarios (#132) — the `cap-templating`
 * feature. Gated on [[Capability.Templating]], so a backend that does not
 * advertise it SKIPs them. A capture pulls a value out of the request (here the
 * last path segment) and the response body interpolates it (Rift via a
 * Mountebank `copy` behavior).
 */
object TemplatingScenarios:

  lazy val all: List[ConformanceScenario] = List(pathCapture, bodyCapture)

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private val emptySource = MockSource.Dsl(MockSpec(Nil))

  private def space(control: MockControl): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(emptySource).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  private def templatingOf(control: MockControl): IO[Throwable, Templating] =
    control.templating.mapError(u => new RuntimeException(u.toString))

  private val pathCapture =
    ConformanceScenario(
      "templating: interpolates a value captured from the request path",
      Set(Capability.Templating),
      control =>
        for
          s          <- space(control)
          templating <- templatingOf(control)
          template = ResponseTemplate(
                       body = "Hello ${NAME}",
                       captures = List(TemplateCapture("${NAME}", TemplateSource.Path, "[^/]+$"))
                     )
          _    <- templating.inject(s, RequestMatch(path = PathMatch.Regex("^/greet/[^/]+$")), template).mapError(asT)
          resp <- SutClient.make(s).send(Method.Get, "/greet/World")
          _ <-
            ensure(resp.status == 200 && resp.body == "Hello World", s"path: status=${resp.status} body=${resp.body}")
        yield ()
    )

  private val bodyCapture =
    ConformanceScenario(
      "templating: interpolates a value captured from the request body",
      Set(Capability.Templating),
      control =>
        for
          s          <- space(control)
          templating <- templatingOf(control)
          template = ResponseTemplate(
                       body = "echo:${WHO}",
                       captures = List(TemplateCapture("${WHO}", TemplateSource.Body, "\\w+"))
                     )
          _ <- templating
                 .inject(s, RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/echo")), template)
                 .mapError(asT)
          resp <- SutClient.make(s).send(Method.Post, "/echo", body = Some("Alice"))
          _    <- ensure(resp.status == 200 && resp.body == "echo:Alice", s"body: status=${resp.status} body=${resp.body}")
        yield ()
    )
