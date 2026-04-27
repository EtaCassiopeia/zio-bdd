package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.{ZIOSteps, DefaultTypedExtractor}
import zio.bdd.gherkin.{FlagsTag, GherkinParser, ScenarioMetadata}
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for the @flags(k=v) flag-matrix expansion feature.
 *
 * Covers:
 *   1. FlagsTag.parse — individual tag parsing 2. FlagsTag.extractAll —
 *      multi-tag extraction 3. FeatureExecutor expansion — N @flags → N
 *      scenario runs 4. flagLayer override — flag values reach the ZLayer 5.
 *      ScenarioMetadata.flagValues — hooks receive flag values 6. Outline ×
 *      flags — multiplicative expansion 7. Backward compatibility — no @flags →
 *      runs once
 */
object FlagMatrixSpec extends ZIOSpecDefault {

  case class State(count: Int = 0, flags: Map[String, String] = Map.empty)
  given Schema[State] = DeriveSchema.gen[State]

  private val F = "flags.feature"

  private def parse(content: String) = GherkinParser.parseFeature(content, F)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Flag matrix")(
    suite("FlagsTag parsing")(
      test("parses a single key=value pair") {
        assertTrue(FlagsTag.parse("flags(a=true)").contains(Map("a" -> "true")))
      },
      test("parses multiple key=value pairs") {
        assertTrue(FlagsTag.parse("flags(a=true, b=false)").contains(Map("a" -> "true", "b" -> "false")))
      },
      test("parses boolean shorthand (key without =value defaults to true)") {
        assertTrue(FlagsTag.parse("flags(rateLimiting)").contains(Map("rateLimiting" -> "true")))
      },
      test("returns None for a non-flags tag") {
        assertTrue(FlagsTag.parse("smoke").isEmpty)
        assertTrue(FlagsTag.parse("ignore").isEmpty)
      },
      test("returns None for an empty string") {
        assertTrue(FlagsTag.parse("").isEmpty)
      },
      test("handles leading @ in tag name") {
        assertTrue(FlagsTag.parse("@flags(x=1)").contains(Map("x" -> "1")))
      },
      test("extractAll returns empty list when no flags tags are present") {
        assertTrue(FlagsTag.extractAll(List("smoke", "regression")).isEmpty)
      },
      test("extractAll returns one map per @flags tag") {
        val tags = List("flags(a=true)", "flags(a=false)", "smoke")
        val maps = FlagsTag.extractAll(tags)
        assertTrue(
          maps.length == 2,
          maps.contains(Map("a" -> "true")),
          maps.contains(Map("a" -> "false"))
        )
      }
    ),
    suite("Scenario expansion")(
      test("scenario without @flags runs exactly once") {
        val suite = new ZIOSteps[Any, State] {}
        for {
          f       <- parse("Feature: F\n  Scenario: s\n    Given a step\n")
          results <- suite.run(List(f))
        } yield assertTrue(results.head.scenarioResults.length == 1)
      },
      test("scenario with two @flags tags expands to two runs") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("a step")(ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(mode=fast)
                   |  @flags(mode=slow)
                   |  Scenario: parameterised
                   |    Given a step
                   |""".stripMargin
               )
          results <- suite.run(List(f))
        } yield assertTrue(results.head.scenarioResults.length == 2)
      },
      test("expanded scenario names include the flag values in brackets") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("a step")(ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(x=1)
                   |  @flags(x=2)
                   |  Scenario: Base scenario
                   |    Given a step
                   |""".stripMargin
               )
          results <- suite.run(List(f))
          names    = results.head.scenarioResults.map(_.scenario.name).sorted
        } yield assertTrue(
          names.contains("Base scenario [x=1]"),
          names.contains("Base scenario [x=2]")
        )
      },
      test("three @flags tags produce three scenario runs") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("a step")(ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(v=1)
                   |  @flags(v=2)
                   |  @flags(v=3)
                   |  Scenario: triple expansion
                   |    Given a step
                   |""".stripMargin
               )
          results <- suite.run(List(f))
        } yield assertTrue(results.head.scenarioResults.length == 3)
      }
    ),
    suite("flagLayer receives flag values")(
      test("flagLayer is called with the flag map for each expansion") {
        val capturedFlags = new java.util.concurrent.CopyOnWriteArrayList[Map[String, String]]()

        class FlagSuite extends ZIOSteps[Any, State] with DefaultTypedExtractor:
          override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, Any] = {
            capturedFlags.add(flags)
            environment
          }
          Given("a step")(ZIO.unit)

        val s = new FlagSuite {}
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(feature=on)
                   |  @flags(feature=off)
                   |  Scenario: layered
                   |    Given a step
                   |""".stripMargin
               )
          _ <- s.run(List(f))
        } yield assertTrue(
          capturedFlags.size() == 2,
          capturedFlags.contains(Map("feature" -> "on")),
          capturedFlags.contains(Map("feature" -> "off"))
        )
      },
      test("flagLayer receives empty map for scenarios without @flags") {
        var calledWithEmpty = false

        class FlagSuite extends ZIOSteps[Any, State] with DefaultTypedExtractor:
          override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, Any] = {
            if (flags.isEmpty) calledWithEmpty = true
            environment
          }
          Given("a step")(ZIO.unit)

        val s = new FlagSuite {}
        for {
          f <- parse("Feature: F\n  Scenario: plain\n    Given a step\n")
          _ <- s.run(List(f))
          // flagLayer should NOT be called for scenarios without @flags tags
          // (scenarioLayer is called instead)
        } yield assertTrue(!calledWithEmpty)
      }
    ),
    suite("ScenarioMetadata contains flag values")(
      test("beforeScenario hook receives flagValues in metadata") {
        val captured = new java.util.concurrent.CopyOnWriteArrayList[Map[String, String]]()

        class FlagSuite extends ZIOSteps[Any, State] with DefaultTypedExtractor:
          beforeScenario(meta => ZIO.attempt { captured.add(meta.flagValues); () }.orDie)
          Given("a step")(ZIO.unit)

        val s = new FlagSuite {}
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(env=qa)
                   |  @flags(env=dev)
                   |  Scenario: s
                   |    Given a step
                   |""".stripMargin
               )
          _ <- s.run(List(f))
        } yield assertTrue(
          captured.size() == 2,
          captured.contains(Map("env" -> "qa")),
          captured.contains(Map("env" -> "dev"))
        )
      }
    ),
    suite("@flags × Scenario Outline (multiplicative expansion)")(
      test("2 examples × 2 @flags tags = 4 scenario runs") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("user " / string)((_: String) => ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @flags(mode=fast)
                   |  @flags(mode=slow)
                   |  Scenario Outline: login <user>
                   |    Given user <user>
                   |  Examples:
                   |    | user  |
                   |    | Alice |
                   |    | Bob   |
                   |""".stripMargin
               )
          results <- suite.run(List(f))
        } yield assertTrue(results.head.scenarioResults.length == 4)
      }
    ),
    suite("Backward compatibility")(
      test("existing tests without @flags run exactly once") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("a step")(ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @smoke
                   |  @regression
                   |  Scenario: normal scenario
                   |    Given a step
                   |""".stripMargin
               )
          results <- suite.run(List(f))
        } yield assertTrue(results.head.scenarioResults.length == 1)
      },
      test("non-flags tags are preserved on expanded scenarios") {
        val suite = new ZIOSteps[Any, State] with DefaultTypedExtractor:
          Given("a step")(ZIO.unit)
        for {
          f <- parse(
                 """Feature: F
                   |  @smoke
                   |  @flags(x=1)
                   |  @flags(x=2)
                   |  Scenario: s
                   |    Given a step
                   |""".stripMargin
               )
          results        <- suite.run(List(f))
          allScenarioTags = results.head.scenarioResults.map(_.scenario.tags)
        } yield assertTrue(
          allScenarioTags.forall(tags => tags.contains("smoke"))
        )
      }
    )
  )
}
