package zio.bdd.gherkin

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for Mode P (property testing) syntax support in the Gherkin parser.
 *
 * Covers:
 *   - \@property tag parsing (bare, full args, various combinations)
 *   - \@property on Examples block produces Scenario.propertyConfig
 *   - header cell `:genName` annotation parsed into Scenario.columnGens
 *   - existing literal Examples behaviour is completely unchanged
 *   - mixed literal + property blocks in one Scenario Outline
 *   - \@property block with rows present falls back to literal expansion (user
 *     error guard)
 */
object PropertyTagParserSpec extends ZIOSpecDefault {

  private def parse(content: String) =
    GherkinParser.parseFeature(content, "test.feature")

  def spec: Spec[TestEnvironment & Scope, Any] = suite("PropertyTagParserSpec")(
    propertyTagSuite,
    parserIntegrationSuite,
    regressionSuite,
    stableIdSuite
  )

  // ── PropertyTag.parse ──────────────────────────────────────────────────────

  private val propertyTagSuite = suite("PropertyTag.parse")(
    test("bare @property defaults to samples=100, seed=None, shrink=true") {
      val cfg = PropertyTag.parse("property")
      assertTrue(
        cfg.isDefined,
        cfg.get.samples == 100,
        cfg.get.seed.isEmpty,
        cfg.get.shrink == true,
        cfg.get.verbose == false,
        cfg.get.replay == true
      )
    },
    test("@property with args parses all fields") {
      val cfg = PropertyTag.parse("property(samples=50, seed=12345, shrink=false, verbose=true, replay=false)")
      assertTrue(
        cfg.isDefined,
        cfg.get.samples == 50,
        cfg.get.seed.contains(12345L),
        cfg.get.shrink == false,
        cfg.get.verbose == true,
        cfg.get.replay == false
      )
    },
    test("@property with @ prefix is parsed (tag stored without @)") {
      val cfg = PropertyTag.parse("@property(samples=25)")
      assertTrue(cfg.isDefined, cfg.get.samples == 25)
    },
    test("@property ignores unknown keys gracefully") {
      val cfg = PropertyTag.parse("property(samples=10, unknownKey=foo)")
      assertTrue(cfg.isDefined, cfg.get.samples == 10)
    },
    test("@property(samples=0) and negative samples fall back to the default rather than running zero samples") {
      val zero     = PropertyTag.parse("property(samples=0)")
      val negative = PropertyTag.parse("property(samples=-5)")
      assertTrue(zero.isDefined, zero.get.samples == 100, negative.isDefined, negative.get.samples == 100)
    },
    test("non-property tags return None") {
      assertTrue(
        PropertyTag.parse("smoke").isEmpty,
        PropertyTag.parse("flags(a=b)").isEmpty,
        PropertyTag.parse("ignore").isEmpty
      )
    },
    test("PropertyTag.extract returns first match from a list of tags") {
      val tags = List("smoke", "property(samples=99)", "regression")
      val cfg  = PropertyTag.extract(tags)
      assertTrue(cfg.isDefined, cfg.get.samples == 99)
    },
    test("PropertyTag.extract returns None when no @property tag present") {
      val tags = List("smoke", "regression", "flags(a=b)")
      assertTrue(PropertyTag.extract(tags).isEmpty)
    }
  )

  // ── Parser integration ─────────────────────────────────────────────────────

  private val parserIntegrationSuite = suite("GherkinParser + @property Examples")(
    test("@property Examples block with header-only produces one scenario with propertyConfig") {
      parse(
        """Feature: Account invariants
          |  Scenario Outline: balance never goes negative
          |    Given an account with balance <amount>
          |    Then the balance is not negative
          |
          |    @property(samples=50, seed=1)
          |    Examples:
          |      | amount |
          |""".stripMargin
      ).map { feature =>
        val scenarios = feature.scenarios
        assertTrue(
          scenarios.length == 1,
          scenarios.head.propertyConfig.isDefined,
          scenarios.head.propertyConfig.get.samples == 50,
          scenarios.head.propertyConfig.get.seed.contains(1L),
          scenarios.head.columnGens.isEmpty // no overrides in this block
        )
      }
    },
    test("@property Examples block: column with :genName produces columnGens map") {
      parse(
        """Feature: Column overrides
          |  Scenario Outline: test
          |    Given account balance <amount>
          |    When limit is <limit>
          |    Then it works
          |
          |    @property(samples=10)
          |    Examples:
          |      | amount: smallAmounts | limit |
          |""".stripMargin
      ).map { feature =>
        val sc = feature.scenarios.head
        assertTrue(
          sc.propertyConfig.isDefined,
          sc.columnGens.get("amount").contains("smallAmounts"),
          !sc.columnGens.contains("limit") // plain column, no override
        )
      }
    },
    test("@property on the Scenario Outline line (not the Examples block) is also detected") {
      parse(
        """Feature: Tag placement
          |  @property(samples=30, seed=7)
          |  Scenario Outline: tagged on outline line
          |    Given account balance <amount>
          |    Then it works
          |
          |    Examples:
          |      | amount |
          |""".stripMargin
      ).map { feature =>
        val sc = feature.scenarios.head
        assertTrue(
          feature.scenarios.length == 1,
          sc.propertyConfig.isDefined,
          sc.propertyConfig.get.samples == 30,
          sc.propertyConfig.get.seed.contains(7L)
        )
      }
    },
    test("@property on the Scenario Outline line and on the Examples block both detect identically") {
      def cfgFor(content: String) = parse(content).map(_.scenarios.head.propertyConfig)
      val onOutline =
        """Feature: F
          |  @property(samples=15)
          |  Scenario Outline: x
          |    Given v <n>
          |    Then ok
          |
          |    Examples:
          |      | n |
          |""".stripMargin
      val onBlock =
        """Feature: F
          |  Scenario Outline: x
          |    Given v <n>
          |    Then ok
          |
          |    @property(samples=15)
          |    Examples:
          |      | n |
          |""".stripMargin
      for {
        c1 <- cfgFor(onOutline)
        c2 <- cfgFor(onBlock)
      } yield assertTrue(c1.isDefined, c2.isDefined, c1.get.samples == c2.get.samples)
    },
    test("literal Examples block is NOT affected by @property on a sibling block") {
      parse(
        """Feature: Mixed
          |  Scenario Outline: dual blocks
          |    Given value is <val>
          |    Then step passes
          |
          |    @regression
          |    Examples:
          |      | val |
          |      | a   |
          |      | b   |
          |
          |    @property(samples=20)
          |    Examples:
          |      | val |
          |""".stripMargin
      ).map { feature =>
        val scenarios = feature.scenarios
        // 2 literal scenarios from @regression block + 1 property scenario
        val literal  = scenarios.filter(_.propertyConfig.isEmpty)
        val property = scenarios.filter(_.propertyConfig.isDefined)
        assertTrue(
          literal.length == 2,
          property.length == 1,
          property.head.propertyConfig.get.samples == 20
        )
      }
    },
    test("@property block that also has data rows falls back to literal expansion") {
      // User error: a @property block should not have rows. We fall back to literal to avoid silent data loss.
      parse(
        """Feature: Property with rows
          |  Scenario Outline: fallback
          |    Given value is <val>
          |    Then step passes
          |
          |    @property(samples=5)
          |    Examples:
          |      | val |
          |      | 42  |
          |""".stripMargin
      ).map { feature =>
        val scenarios = feature.scenarios
        // One literal scenario (row expansion), NOT a property scenario
        assertTrue(
          scenarios.length == 1,
          scenarios.head.propertyConfig.isEmpty
        )
      }
    },
    test("scenario without Examples block is not treated as property") {
      parse(
        """Feature: Normal scenario
          |  Scenario: simple
          |    Given a step
          |    Then another step
          |""".stripMargin
      ).map { feature =>
        val sc = feature.scenarios.head
        assertTrue(sc.propertyConfig.isEmpty, sc.columnGens.isEmpty)
      }
    },
    test("multiple column overrides all parsed") {
      parse(
        """Feature: Multi overrides
          |  Scenario Outline: test
          |    Given balance <a> and limit <b> and amount <c>
          |    Then ok
          |
          |    @property(samples=5)
          |    Examples:
          |      | a: genA | b: genB | c |
          |""".stripMargin
      ).map { feature =>
        val sc = feature.scenarios.head
        assertTrue(
          sc.columnGens.get("a").contains("genA"),
          sc.columnGens.get("b").contains("genB"),
          !sc.columnGens.contains("c")
        )
      }
    }
  )

  // ── Regression — existing Examples logic unchanged ─────────────────────────

  private val regressionSuite = suite("Existing Examples/Outline behaviour unchanged")(
    test("single Examples block with rows still produces N expanded scenarios") {
      parse(
        """Feature: Outline
          |  Scenario Outline: multi row
          |    Given value is <val>
          |    Then step passes
          |  Examples:
          |    | val |
          |    | x   |
          |    | y   |
          |    | z   |
          |""".stripMargin
      ).map { feature =>
        assertTrue(feature.scenarios.length == 3, feature.scenarios.forall(_.propertyConfig.isEmpty))
      }
    },
    test("multiple Examples blocks each expand independently") {
      parse(
        """Feature: Multi blocks
          |  Scenario Outline: outline
          |    Given val <v>
          |    Then ok
          |  Examples: A
          |    | v |
          |    | 1 |
          |    | 2 |
          |  Examples: B
          |    | v |
          |    | 3 |
          |""".stripMargin
      ).map { feature =>
        assertTrue(
          feature.scenarios.length == 3,
          feature.scenarios.forall(_.propertyConfig.isEmpty)
        )
      }
    },
    test("plain column header with colon inside value is NOT misinterpreted as gen override") {
      // e.g. a header like "time:hh:mm" — split on first ':' only gives ("time", "hh:mm")
      // which is a valid gen name. Verify no crash; the gen won't be registered so
      // the value should just be taken as a column name when there's no @property tag.
      parse(
        """Feature: Colon in header
          |  Scenario Outline: test
          |    Given time is <time:hh:mm>
          |    Then ok
          |  Examples:
          |    | time:hh:mm |
          |    | 10:30      |
          |""".stripMargin
      ).map { feature =>
        // Without @property the block is literal; columnGens irrelevant; scenario should parse
        assertTrue(feature.scenarios.length == 1, feature.scenarios.head.propertyConfig.isEmpty)
      }
    },
    test("@property tag on Scenario (not Examples) is NOT treated as property scenario when block has rows") {
      // Rows present → falls through to literal expansion regardless of where @property lives.
      parse(
        """Feature: Tag on scenario
          |  @property(samples=5)
          |  Scenario Outline: outline
          |    Given value is <val>
          |    Then ok
          |  Examples:
          |    | val |
          |    | 1   |
          |""".stripMargin
      ).map { feature =>
        // @property on scenario + rows in block → literal expansion (rows win)
        val sc = feature.scenarios.head
        assertTrue(
          sc.propertyConfig.isEmpty,               // rows guard prevents property mode
          sc.tags.exists(_.startsWith("property")) // tag still retained
        )
      }
    },
    test("@property tag on Scenario Outline with header-only Examples triggers property mode") {
      // This is the pattern used in example/greeting_properties.feature —
      // @property sits on the Scenario Outline and the Examples block is header-only.
      parse(
        """Feature: Tag on scenario outline
          |  @property(samples=10, seed=1)
          |  Scenario Outline: outline
          |    Given a user named <name>
          |    Then ok
          |  Examples:
          |    | name |
          |""".stripMargin
      ).map { feature =>
        val sc = feature.scenarios.head
        assertTrue(
          sc.propertyConfig.isDefined,
          sc.propertyConfig.get.samples == 10,
          sc.propertyConfig.get.seed.contains(1L)
        )
      }
    }
  )

  // ── Scenario.id / stableId ───────────────────────────────────────────────────

  private val stableIdSuite = suite("Scenario.id / stableId")(
    test("id is derived from name/file/line when stableId is unset") {
      val sc = Scenario(name = "a", steps = Nil, file = Some("f.feature"), line = Some(3))
      assertTrue(sc.id == s"scenario:a:f.feature:3".hashCode)
    },
    test("renaming a scenario changes its id, unless stableId pins the original") {
      val original          = Scenario(name = "a", steps = Nil, file = Some("f.feature"), line = Some(3))
      val renamedWithoutPin = original.copy(name = "a [5 samples passed]")
      val renamedWithPin    = original.copy(name = "a [5 samples passed]", stableId = Some(original.id))
      assertTrue(
        renamedWithoutPin.id != original.id, // the bug PropertyExecutor's stableId works around
        renamedWithPin.id == original.id
      )
    }
  )
}
