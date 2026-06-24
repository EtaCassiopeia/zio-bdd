package zio.bdd.example

import zio.*
import zio.bdd.core.Assertions.assertTrue
import zio.bdd.core.Suite
import zio.bdd.core.property.{ColumnGenLookup, HasGen}
import zio.bdd.core.step.ZIOSteps
import zio.bdd.example.{Config => AppConfig}
import zio.schema.{DeriveSchema, Schema}
import zio.test.Gen

case class ScenarioContext(userName: String, greeting: String, title: String = "")

object ScenarioContext:
  implicit val schema: Schema[ScenarioContext] = DeriveSchema.gen[ScenarioContext]

// A small domain type — demonstrates wiring `given HasGen[T]` for a type that isn't one of
// HasGen's primitive built-ins (Int/Long/Double/Boolean/String/UUID), as opposed to the
// `HasGen.named(...)` override used for the `name` column below.
enum Title:
  case Mr, Ms, Dr

@Suite(
  featureDirs = Array("example/src/test/resources/features"),
  reporters   = Array("pretty", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive"),
  logLevel    = "info"
)
object SimpleSpec extends ZIOSteps[GreetingService, ScenarioContext]:

  // ── HasGen registration ────────────────────────────────────────────────
  // Override the built-in HasGen[String] (alphanumeric) with a generator that
  // produces capitalised human-readable names — makes failure output readable:
  //   [counterexample] name=Mxqzb → name=Alice
  HasGen.named("name")(
    for {
      first <- Gen.char('A', 'Z')
      rest  <- Gen.stringBounded(1, 9)(Gen.char('a', 'z'))
    } yield first.toString + rest
  )

  // Domain-type generator via Scala 3 `given` — the other way to supply a `HasGen[T]`,
  // for a type that's specific to this suite rather than a reusable named override.
  given HasGen[Title] with
    def gen            = Gen.elements(Title.Mr, Title.Ms, Title.Dr)
    override def label = "HasGen[Title]"

  // ── Functional BDD steps (simple.feature) ────────────────────────────────

  Given("a user named " / string) { (name: String) =>
    ScenarioContext.update(_.copy(userName = name))
  }

  When("the user is greeted") {
    for {
      ctx <- ScenarioContext.get
      _   <- ZIO.logInfo(s"Greeting user ${ctx.userName}")
      _   <- ScenarioContext.update(_.copy(greeting = s"Hello, ${ctx.userName}!"))
    } yield ()
  }

  Then("the greeting should be " / string) { (expectedGreeting: String) =>
    ScenarioContext.get.map(_.greeting).map { actualGreeting =>
      assertTrue(actualGreeting == expectedGreeting, s"Expected '$expectedGreeting', but got '$actualGreeting'")
    }
  }

  // ── Property assertion steps (greeting_properties.feature) ───────────────

  Then("the greeting starts with " / string) { (prefix: String) =>
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(
        ctx.greeting.startsWith(prefix),
        s"Expected greeting to start with '$prefix', got '${ctx.greeting}'"
      )
    }
  }

  Then("the greeting ends with " / string) { (suffix: String) =>
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(
        ctx.greeting.endsWith(suffix),
        s"Expected greeting to end with '$suffix', got '${ctx.greeting}'"
      )
    }
  }

  Then("the greeting contains the name") {
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(
        ctx.greeting.contains(ctx.userName),
        s"Expected '${ctx.greeting}' to contain name '${ctx.userName}'"
      )
    }
  }

  Then("the greeting length is greater than " / int) { (minLen: Int) =>
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(
        ctx.greeting.length > minLen,
        s"Expected greeting length > $minLen, got ${ctx.greeting.length}: '${ctx.greeting}'"
      )
    }
  }

  Then("the greeting length is less than " / int) { (maxLen: Int) =>
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(
        ctx.greeting.length < maxLen,
        s"Expected greeting length < $maxLen, got ${ctx.greeting.length}: '${ctx.greeting}'"
      )
    }
  }

  Given("a title " / string) { (title: String) =>
    ScenarioContext.update(_.copy(title = title))
  }

  Then("the title is a known honorific") {
    ScenarioContext.get.flatMap { ctx =>
      val known = Title.values.map(_.toString).toSet
      assertTrue(known.contains(ctx.title), s"Expected one of $known, got '${ctx.title}'")
    }
  }

  // ── Column generator lookup ────────────────────────────────────────────
  // columnGenLookup is the *override* path, not mandatory wiring — both columns below
  // are governed by a `string` extractor (Given("a user named " / string), Given("a
  // title " / string)), so they'd resolve automatically to the built-in HasGen[String]
  // (unconstrained alphanumeric text) with no entry here at all. Both are overridden
  // anyway because the automatic choice isn't the generator we actually want:
  //   - "name"  → a *named* override (HasGen.named("name")(...) above) for readable
  //     capitalised names instead of alphanumeric gibberish.
  //   - "title" → a *domain-type* generator (`given HasGen[Title]` above) so it only
  //     ever generates one of the three known honorifics, not arbitrary text.
  override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
    def byColumn(col: String): Option[HasGen[?]] = col match
      case "name"  => HasGen.resolve("name")
      case "title" => Some(HasGen[Title])
      case _       => None

  // ── ZLayer ────────────────────────────────────────────────────────────────

  override def environment: ZLayer[Any, Throwable, GreetingService] =
    ZLayer.succeed(AppConfig("Hello")) >>> GreetingService.live
