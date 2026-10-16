package samples.mock

import zio.*
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.bdd.mock.dsl.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 16 — templating (capability Templating): a response body embeds captures
 * pulled from the request. Rift only.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/16-templating.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Templating16Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  private def tmpl =
    ZIO.serviceWithZIO[MockControl](_.templating).mapError(u => new RuntimeException(u.message))

  Given("the backend supports templating") {
    requiring(Capability.Templating)
  }

  Given("a template greeting the last path segment at " / string) { (pathRegex: String) =>
    for
      space <- activeSpace
      t     <- tmpl
      template = ResponseTemplate(
                   "Hello ${NAME}",
                   List(TemplateCapture("${NAME}", TemplateSource.Path, "[^/]+$"))
                 )
      _ <- t
             .inject(space, RequestMatch(path = PathMatch.Regex(pathRegex)), template)
             .mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  Given("a body-echo template for POST " / string) { (path: String) =>
    for
      space <- activeSpace
      t     <- tmpl
      template = ResponseTemplate(
                   "echo:${WHO}",
                   List(TemplateCapture("${WHO}", TemplateSource.Body, "\\w+"))
                 )
      _ <- t.inject(space, post(path), template).mapError(e => new RuntimeException(s"$e"))
    yield ()
  }

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
