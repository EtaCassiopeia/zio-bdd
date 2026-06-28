package zio.bdd.core.property

import zio.*
import zio.test.*
import zio.bdd.gherkin.{Scenario, Step, StepType}

import java.nio.file.{Files, Path, Paths}

/**
 * Round-trip tests for `PropertyFailureStore`'s hand-rolled JSON encode/decode,
 * focused on value content that could break a naive (non-escape-aware) parser:
 * literal braces, quotes, backslashes, and non-ASCII text inside a sampled
 * column value.
 *
 * Every test runs against an isolated, auto-deleted base directory via
 * `withIsolatedStore`, so the suite never touches (or races on) the
 * process-global `.zio-bdd/failures` directory — see #138.
 */
object PropertyFailureStoreSpec extends ZIOSpecDefault {

  private def scenario(name: String) =
    Scenario(name = name, steps = List(Step(StepType.GivenStep, "a step")), file = Some("failure-store-spec.feature"))

  private def cleanup(sc: Scenario) = PropertyFailureStore.clear(sc)

  private def deleteRecursively(dir: Path): UIO[Unit] =
    ZIO.attempt {
      import scala.jdk.CollectionConverters.*
      if (Files.exists(dir)) {
        val walk = Files.walk(dir)
        try walk.iterator().asScala.toList.sortBy(_.toString).reverse.foreach(Files.deleteIfExists(_))
        finally walk.close()
      }
    }.orDie

  /**
   * Run `use` with the failure store redirected to a fresh, scoped temp
   * directory.
   */
  private def withIsolatedStore[E, A](use: Path => ZIO[Any, E, A]): ZIO[Scope, E, A] =
    ZIO
      .acquireRelease(ZIO.attempt(Files.createTempDirectory("zio-bdd-failures-spec")).orDie)(deleteRecursively)
      .flatMap(dir => PropertyFailureStore.withBaseDir(dir)(use(dir)))

  private def recordFile(dir: Path, sc: Scenario): Path =
    dir.resolve(PropertyFailureStore.slug(sc) + ".json")

  // A read of `sc` warned that its record is unreadable — bound to the scenario name so a
  // warning about a different record can't satisfy it.
  private def warnedUnreadable(logs: Chunk[ZTestLogger.LogEntry], sc: Scenario): Boolean =
    logs.exists { e =>
      e.logLevel == LogLevel.Warning &&
      e.message().contains("Unreadable property-failure record") &&
      e.message().contains(sc.name)
    }

  // Install `fixture` (a present-but-unreadable record), read `sc`, and assert the read falls
  // back to None while warning about the unreadable record. Runs under ZTestLogger so the
  // warning is captured rather than printed.
  private def assertReadWarnsUnreadable(sc: Scenario)(fixture: UIO[Any]): UIO[TestResult] =
    (for {
      _      <- fixture
      result <- PropertyFailureStore.read(sc)
      logs   <- ZTestLogger.logOutput
    } yield assertTrue(result.isEmpty, warnedUnreadable(logs, sc))).provideLayer(ZTestLogger.default)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PropertyFailureStoreSpec")(
    test("a present-but-corrupt record is logged as a warning and read falls back to None (#140)") {
      withIsolatedStore { dir =>
        val sc = scenario("corrupt content")
        assertReadWarnsUnreadable(sc) {
          ZIO.attempt(Files.writeString(recordFile(dir, sc), "this is not valid json {{{")).orDie
        }
      }
    },
    test("a present-but-empty record is logged as a warning and read falls back to None (#140)") {
      withIsolatedStore { dir =>
        // A 0-byte file is the realistic interrupted-write shape: present but unparseable.
        val sc = scenario("empty file")
        assertReadWarnsUnreadable(sc) {
          ZIO.attempt(Files.writeString(recordFile(dir, sc), "")).orDie
        }
      }
    },
    test("an unreadable record (I/O error) is logged as a warning and read falls back to None (#140)") {
      withIsolatedStore { dir =>
        // A directory where a file is expected: it `exists()`, but opening it as a file throws,
        // exercising the I/O-exception path (the original `.orElse(ZIO.none)` silent swallow).
        val sc = scenario("io error")
        assertReadWarnsUnreadable(sc) {
          ZIO.attempt(Files.createDirectory(recordFile(dir, sc))).orDie
        }
      }
    },
    test("a genuinely absent record returns None with no warning (#140)") {
      withIsolatedStore { _ =>
        // The common path must stay quiet — no file written, no warning of any kind.
        val sc = scenario("absent record")
        (for {
          result <- PropertyFailureStore.read(sc)
          logs   <- ZTestLogger.logOutput
        } yield assertTrue(
          result.isEmpty,
          !logs.exists(_.logLevel == LogLevel.Warning)
        )).provideLayer(ZTestLogger.default)
      }
    },
    test("the default base directory is the production .zio-bdd/failures layout (#138)") {
      // Every other test overrides the base dir, so this pins the un-overridden default that
      // production (and external tooling reading the files) relies on — a rename would
      // otherwise pass the whole suite unnoticed.
      assertTrue(PropertyFailureStore.defaultBaseDir == Paths.get(".zio-bdd", "failures"))
    },
    test("concurrent isolated stores never observe each other's records (parallel-safe, #138)") {
      // Every fiber writes the SAME scenario slug. Under one shared global directory they
      // would all collide on a single file (last-writer-wins) and most fibers would read a
      // foreign seed. With a per-fiber `withBaseDir`, each must read back exactly its own.
      val sc = scenario("contended slug")
      ZIO
        .foreachPar(1 to 32) { i =>
          ZIO.scoped {
            withIsolatedStore { dir =>
              for {
                _      <- PropertyFailureStore.write(sc, seed = i.toLong, sampleIndex = i, Map("n" -> i.toString), Map.empty)
                rec    <- PropertyFailureStore.read(sc)
                ourFile = dir.resolve(PropertyFailureStore.slug(sc) + ".json").toFile
              } yield (i, rec, ourFile.exists())
            }
          }
        }
        .map { results =>
          assertTrue(
            results.size == 32,
            results.forall { case (i, rec, wroteUnderOurDir) => rec.exists(_.seed == i.toLong) && wroteUnderOurDir }
          )
        }
    } @@ TestAspect.nonFlaky,
    test("round-trips a value containing literal braces without truncating sibling keys") {
      withIsolatedStore { _ =>
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
      }
    },
    test("round-trips values containing quotes and backslashes") {
      withIsolatedStore { _ =>
        val sc           = scenario("quotes and backslashes")
        val shrunkValues = Map("path" -> """C:\Users\test\"quoted"\file.txt""", "note" -> "she said \"hi\\bye\"")
        for {
          _        <- cleanup(sc)
          _        <- PropertyFailureStore.write(sc, seed = 2L, sampleIndex = 0, shrunkValues, Map.empty)
          readBack <- PropertyFailureStore.read(sc)
          _        <- cleanup(sc)
        } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
      }
    },
    test("round-trips non-ASCII text") {
      withIsolatedStore { _ =>
        val sc           = scenario("unicode")
        val shrunkValues = Map("name" -> "日本語 — café — emoji 🎉")
        for {
          _        <- cleanup(sc)
          _        <- PropertyFailureStore.write(sc, seed = 3L, sampleIndex = 0, shrunkValues, Map.empty)
          readBack <- PropertyFailureStore.read(sc)
          _        <- cleanup(sc)
        } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
      }
    },
    test("round-trips an empty shrunkValues/generatorLabels map") {
      withIsolatedStore { _ =>
        val sc = scenario("empty maps")
        for {
          _        <- cleanup(sc)
          _        <- PropertyFailureStore.write(sc, seed = 4L, sampleIndex = 0, Map.empty, Map.empty)
          readBack <- PropertyFailureStore.read(sc)
          _        <- cleanup(sc)
        } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues.isEmpty, readBack.get.generatorLabels.isEmpty)
      }
    },
    test("multiple columns each round-trip to their own value") {
      withIsolatedStore { _ =>
        val sc           = scenario("multi column")
        val shrunkValues = Map("a" -> "1", "b" -> "two}", "c" -> """{"x"}""")
        for {
          _        <- cleanup(sc)
          _        <- PropertyFailureStore.write(sc, seed = 5L, sampleIndex = 0, shrunkValues, Map.empty)
          readBack <- PropertyFailureStore.read(sc)
          _        <- cleanup(sc)
        } yield assertTrue(readBack.isDefined, readBack.get.shrunkValues == shrunkValues)
      }
    },
    test("clear deletes the file and read returns None afterwards") {
      withIsolatedStore { dir =>
        val sc = scenario("clear test")
        for {
          _               <- PropertyFailureStore.write(sc, seed = 6L, sampleIndex = 0, Map("n" -> "1"), Map.empty)
          existsAfterWrite = Option(dir.toFile.listFiles()).exists(_.nonEmpty)
          _               <- PropertyFailureStore.clear(sc)
          readBack        <- PropertyFailureStore.read(sc)
        } yield assertTrue(existsAfterWrite, readBack.isEmpty)
      }
    }
  )
}
