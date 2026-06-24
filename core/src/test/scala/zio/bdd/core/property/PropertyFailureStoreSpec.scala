package zio.bdd.core.property

import zio.*
import zio.test.*
import zio.bdd.gherkin.{Scenario, Step, StepType}

import java.nio.file.Paths

/**
 * Round-trip tests for `PropertyFailureStore`'s hand-rolled JSON encode/decode,
 * focused on value content that could break a naive (non-escape-aware) parser:
 * literal braces, quotes, backslashes, and non-ASCII text inside a sampled
 * column value.
 */
object PropertyFailureStoreSpec extends ZIOSpecDefault {

  private def scenario(name: String) =
    Scenario(name = name, steps = List(Step(StepType.GivenStep, "a step")), file = Some("failure-store-spec.feature"))

  private val failuresDir = Paths.get(".zio-bdd", "failures").toFile

  private def cleanup(sc: Scenario) = PropertyFailureStore.clear(sc)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PropertyFailureStoreSpec")(
    test("round-trips a value containing literal braces without truncating sibling keys") {
      val sc              = scenario("braces in value")
      val shrunkValues    = Map("col" -> """{"nested": "looks like json"}""")
      val generatorLabels = Map("col" -> "HasGen[Custom]")
      for {
        _        <- cleanup(sc)
        _        <- PropertyFailureStore.write(sc, seed = 1L, sampleIndex = 0, shrunkValues, generatorLabels)
        readBack <- PropertyFailureStore.read(sc)
        _        <- cleanup(sc)
      } yield assertTrue(
        readBack.isDefined,
        readBack.get.shrunkValues == shrunkValues,
        readBack.get.generatorLabels == generatorLabels
      )
    },
    test("round-trips values containing quotes and backslashes") {
      val sc           = scenario("quotes and backslashes")
      val shrunkValues = Map("path" -> """C:\Users\test\"quoted"\file.txt""", "note" -> "she said \"hi\\bye\"")
      for {
        _        <- cleanup(sc)
        _        <- PropertyFailureStore.write(sc, seed = 2L, sampleIndex = 0, shrunkValues, Map.empty)
        readBack <- PropertyFailureStore.read(sc)
        _        <- cleanup(sc)
      } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
    },
    test("round-trips non-ASCII text") {
      val sc           = scenario("unicode")
      val shrunkValues = Map("name" -> "日本語 — café — emoji 🎉")
      for {
        _        <- cleanup(sc)
        _        <- PropertyFailureStore.write(sc, seed = 3L, sampleIndex = 0, shrunkValues, Map.empty)
        readBack <- PropertyFailureStore.read(sc)
        _        <- cleanup(sc)
      } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
    },
    test("round-trips an empty shrunkValues/generatorLabels map") {
      val sc = scenario("empty maps")
      for {
        _        <- cleanup(sc)
        _        <- PropertyFailureStore.write(sc, seed = 4L, sampleIndex = 0, Map.empty, Map.empty)
        readBack <- PropertyFailureStore.read(sc)
        _        <- cleanup(sc)
      } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues.isEmpty, readBack.get.generatorLabels.isEmpty)
    },
    test("multiple columns each round-trip to their own value") {
      val sc           = scenario("multi column")
      val shrunkValues = Map("a" -> "1", "b" -> "two}", "c" -> """{"x"}""")
      for {
        _        <- cleanup(sc)
        _        <- PropertyFailureStore.write(sc, seed = 5L, sampleIndex = 0, shrunkValues, Map.empty)
        readBack <- PropertyFailureStore.read(sc)
        _        <- cleanup(sc)
      } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
    },
    test("clear deletes the file and read returns None afterwards") {
      val sc = scenario("clear test")
      for {
        _               <- PropertyFailureStore.write(sc, seed = 6L, sampleIndex = 0, Map("n" -> "1"), Map.empty)
        existsAfterWrite = Option(failuresDir.listFiles()).exists(_.nonEmpty)
        _               <- PropertyFailureStore.clear(sc)
        readBack        <- PropertyFailureStore.read(sc)
      } yield assertTrue(existsAfterWrite, readBack.isEmpty)
    }
  )
}
