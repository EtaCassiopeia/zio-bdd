package zio.bdd.example

import zio.*
import zio.bdd.core.Assertions.assertTrue
import zio.bdd.core.Suite
import zio.bdd.core.property.{ColumnGenLookup, HasGen}
import zio.bdd.core.step.ZIOSteps
import zio.bdd.example.{Config => AppConfig}
import zio.schema.{DeriveSchema, Schema}
import zio.test.Gen

case class ScenarioContext(userName: String, greeting: String)

object ScenarioContext:
  implicit val schema: Schema[ScenarioContext] = DeriveSchema.gen[ScenarioContext]

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

  // ── Column generator lookup ────────────────────────────────────────────
  // Routes the `name` column in @property Examples blocks to the named
  // "name" generator registered above.
  override def columnGenLookup: ColumnGenLookup = new ColumnGenLookup:
    def byColumn(col: String): Option[HasGen[?]] = col match
      case "name" => HasGen.resolve("name")
      case _      => None

  // ── ZLayer ────────────────────────────────────────────────────────────────

  override def environment: ZLayer[Any, Throwable, GreetingService] =
    ZLayer.succeed(AppConfig("Hello")) >>> GreetingService.live
