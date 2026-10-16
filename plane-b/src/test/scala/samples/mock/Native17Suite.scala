package samples.mock

import zio.*
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.core.Suite
import zio.bdd.mock.*
import samples.mock.support.{Backends, MockState, MockSupport}

/**
 * 17 — native escape hatch (provisionNative): provision a space from a raw
 * backend document. Backend-specific by construction, so each scenario is
 * tagged for its backend and the harness runs the matching one.
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/17-native-escape-hatch.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Native17Suite extends ZIOSteps[MockControl, MockState] with MockSupport[MockState]:

  Given("a native WireMock stub for GET " / string / " returning " / string) { (path: String, body: String) =>
    val json =
      s"""{"request":{"method":"GET","urlPath":"$path"},"response":{"status":200,"body":"$body"}}"""
    provisionNativeSpec(NativeSpec.WireMock(json))
  }

  Given("a native Rift imposter for GET " / string / " returning " / string) { (path: String, body: String) =>
    val json =
      s"""{"port":0,"protocol":"http","stubs":[{"predicates":[{"equals":{"method":"GET","path":"$path"}}],""" +
        s""""responses":[{"is":{"statusCode":200,"body":"$body"}}]}]}"""
    provisionNativeSpec(NativeSpec.Rift(json))
  }

  private def provisionNativeSpec[B <: Backend](spec: NativeSpec[B]) =
    for
      spaces <- ctl(_.provisionNative(spec))
      space  <- ZIO.fromOption(spaces.headOption).orElseFail(new IllegalStateException("no space"))
      _      <- Stage.put(space)
    yield ()

  override def environment: ZLayer[Any, Throwable, MockControl] = Backends.mockControl
