package zio.bdd.example

import zio.*
import zio.bdd.core.Assertions.assertTrue
import zio.bdd.core.Suite
import zio.bdd.core.property.{ColumnGenLookup, HasGen}
import zio.bdd.core.step.{TypedExtractor, ZIOSteps}
import zio.bdd.example.{Config => AppConfig}
import zio.schema.{DeriveSchema, Schema}
import zio.test.Gen

case class ScenarioContext(userName: String, greeting: String, title: String = "", age: Int = 0)

object ScenarioContext:
  implicit val schema: Schema[ScenarioContext] = DeriveSchema.gen[ScenarioContext]

// A small domain type — demonstrates wiring `given HasGen[T]` for a type that isn't one of
// HasGen's primitive built-ins (Int/Long/Double/Boolean/String/UUID).
enum Title:
  case Mr, Ms, Dr

// A dedicated extractor for Title (rather than reusing `string`) so the `<title>` column's
// step extractor actually carries `Tag[Title]` — that's what lets `@property` resolve its
// generator *automatically*, by type, with zero `columnGenLookup` entry (see
// `HasGen.registerType` below). `Title.values.map(_.toString)` ("Mr", "Ms", "Dr") must match
// this pattern, since sampled values are substituted into step text via `toString`.
val title: TypedExtractor[Title] = TypedExtractor.make[Title]("(Mr|Ms|Dr)") { (_, groups, idx) =>
  groups
    .lift(idx)
    .flatMap(s => Title.values.find(_.toString == s))
    .map(t => (t, idx + 1))
    .toRight(s"Expected one of Mr|Ms|Dr at group $idx")
}

@Suite(
  featureDirs = Array("example/src/test/resources/features"),
  reporters = Array("pretty", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive"),
  logLevel = "info"
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

  // One-time registration so `@property` can find HasGen[Title] *by type* — required
  // because Scala can't enumerate arbitrary `given HasGen[_]` instances at runtime (see
  // HasGen.registerType's doc comment). After this call, every column whose step extractor
  // produces a Title (like `title` above) resolves its generator automatically: no
  // `columnGenLookup` entry needed, unlike the `name` column below.
  HasGen.registerType(HasGen[Title])

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

  Given("a title " / title) { (t: Title) =>
    ScenarioContext.update(_.copy(title = t.toString))
  }

  Then("the title is a known honorific") {
    ScenarioContext.get.flatMap { ctx =>
      val known = Title.values.map(_.toString).toSet
      assertTrue(known.contains(ctx.title), s"Expected one of $known, got '${ctx.title}'")
    }
  }

  // Built-in-type demo: `age` is governed by the stock `int` extractor, so its generator
  // is the pre-registered `HasGen[Int]` (= `Gen.int`, the full Int range) — no `given
  // HasGen[Int]`, no `registerType` call, no `columnGenLookup` entry. Every built-in type
  // (Int/Long/Double/Boolean/String/UUID) works this way out of the box.
  And("the user's age is " / int) { (age: Int) =>
    ScenarioContext.update(_.copy(age = age))
  }

  // Deliberately tautological (true for *every* Int, including Int.MinValue/MaxValue): the
  // point of this step is to prove the sampled value reached the scenario via automatic
  // resolution, not to assert domain behaviour — `age` isn't used anywhere else.
  Then("the age is recorded") {
    ScenarioContext.get.flatMap { ctx =>
      assertTrue(ctx.age >= Int.MinValue && ctx.age <= Int.MaxValue, s"Expected an Int, got ${ctx.age}")
    }
  }

  // ── Column generator lookup ────────────────────────────────────────────
  // columnGenLookup is the *override* path, not mandatory wiring. Of the three columns
  // exercised in greeting_properties.feature's "regardless of title or age" scenario:
  //   - "title" (Title, a domain type) and "age" (Int, a built-in type) both resolve with
  //     zero entries here — automatically, by the Tag their step extractor produces (see
  //     `title`'s extractor and `HasGen.registerType(HasGen[Title])` above; built-ins are
  //     pre-registered in HasGen itself).
  //   - "name" is the one column actually using this override: it's governed by `string`,
  //     so it would *also* resolve automatically (to unconstrained alphanumeric text) —
  //     this entry exists purely to swap in the more readable named generator from
  //     `HasGen.named("name")(...)` above instead of accepting that default.
  override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
    def byColumn(col: String): Option[HasGen[?]] = col match
      case "name" => HasGen.resolve("name")
      case _      => None

  // ── ZLayer ────────────────────────────────────────────────────────────────

  override def environment: ZLayer[Any, Throwable, GreetingService] =
    ZLayer.succeed(AppConfig("Hello")) >>> GreetingService.live
