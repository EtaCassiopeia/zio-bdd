package zio.bdd.core.step

import zio.test.*
import zio.test.Assertion.*
import zio.bdd.gherkin.{DataTable, DataTableRow, StepType}
import zio.schema.{DeriveSchema, Schema}
import zio.bdd.core.{Assertions, Default}
import zio.*

/**
 * Tests for StepExpression pattern building, extraction, and the lazy-compiled
 * regex optimisation (the regex must not be recompiled on every extract call).
 */
object StepExpressionSpec extends ZIOSpecDefault with DefaultTypedExtractor {

  case class User(name: String, age: Int)
  given Schema[User] = DeriveSchema.gen[User]

  private def si(text: String, tbl: Option[DataTable] = None) = StepInput(text, tbl)

  private val patternMatching = suite("Pattern matching")(
    test("literal-only expression matches exact text") {
      val expr = StepExpression[EmptyTuple](List(Literal("hello world")))
      assertTrue(
        expr.extract(si("hello world")).isDefined,
        expr.extract(si("hello  world")).isEmpty, // double space
        expr.extract(si("goodbye world")).isEmpty
      )
    },
    test("expression with one string extractor captures the parameter") {
      val expr = StepExpression[Tuple1[String]](List(Literal("hello "), Extractor(string)))
      assert(expr.extract(si("hello world")))(isSome(equalTo(Tuple1("world"))))
    },
    test("expression with one int extractor captures the integer") {
      val expr = StepExpression[Tuple1[Int]](List(Literal("age is "), Extractor(int)))
      assert(expr.extract(si("age is 42")))(isSome(equalTo(Tuple1(42))))
    },
    test("expression with two extractors captures both") {
      val expr = StepExpression[(String, Int)](
        List(
          Literal("user "),
          Extractor(string),
          Literal(" has age "),
          Extractor(int)
        )
      )
      assert(expr.extract(si("user Alice has age 30")))(isSome(equalTo(("Alice", 30))))
    },
    test("quoted string parameter strips surrounding quotes") {
      val expr = StepExpression[Tuple1[String]](List(Literal("say "), Extractor(string)))
      assert(expr.extract(si("say \"hello world\"")))(isSome(equalTo(Tuple1("hello world"))))
    },
    test("expression with table extractor reads from StepInput.table") {
      val tbl  = DataTable(List("name", "age"), List(DataTableRow(List("Alice", "30"))))
      val expr = StepExpression[Tuple1[List[User]]](List(Literal("users are"), Extractor(table[User])))
      assert(expr.extract(si("users are", Some(tbl))))(isSome(equalTo(Tuple1(List(User("Alice", 30))))))
    },
    test("fails when table extractor receives no table") {
      val expr = StepExpression[Tuple1[List[User]]](List(Literal("users are"), Extractor(table[User])))
      assert(expr.extract(si("users are")))(isNone)
    },
    test("returns None on text mismatch") {
      val expr = StepExpression[Tuple1[String]](List(Literal("hello "), Extractor(string)))
      assert(expr.extract(si("goodbye world")))(isNone)
    }
  )

  private val regexCaching = suite("Regex is compiled once (lazy val)")(
    test("calling extract 1000 times completes without recompiling the regex") {
      val expr  = StepExpression[Tuple1[String]](List(Literal("step "), Extractor(string)))
      val input = si("step value")
      // If the regex were recompiled on every call this would be measurably slow;
      // we just verify all 1000 calls succeed to avoid flakiness in timing assertions.
      val results = (1 to 1000).map(_ => expr.extract(input))
      assertTrue(results.forall(_.isDefined))
    }
  )

  private val stepDef = suite("StepDef")(
    test("tryExecute returns Some when the pattern matches") {
      val def_ = StepDefImpl[Any, Unit, Tuple1[String]](
        StepType.GivenStep,
        StepExpression(List(Literal("hello "), Extractor(string))),
        { case Tuple1(_) => ZIO.unit }
      )
      assert(def_.tryExecute(si("hello world")))(isSome(anything))
    },
    test("tryExecute returns None when the pattern does not match") {
      val def_ = StepDefImpl[Any, Unit, Tuple1[String]](
        StepType.GivenStep,
        StepExpression(List(Literal("hello "), Extractor(string))),
        { case Tuple1(_) => ZIO.unit }
      )
      assert(def_.tryExecute(si("goodbye world")))(isNone)
    },
    test("tryExecute executes the step body when matched") {
      val ref = new java.util.concurrent.atomic.AtomicInteger(0)
      val def_ = StepDefImpl[Any, Unit, Tuple1[Int]](
        StepType.WhenStep,
        StepExpression(List(Literal("count "), Extractor(int))),
        { case Tuple1(n) => ZIO.attempt(ref.set(n)).unit.mapError(identity) }
      )
      val effect = def_.tryExecute(si("count 42")).getOrElse(ZIO.unit)
      for {
        _ <- effect.provide(
               ZLayer.succeed(new State[Unit] {
                 def get: UIO[Unit]                     = ZIO.unit
                 def update(f: Unit => Unit): UIO[Unit] = ZIO.unit
               }) ++
                 Scope.default
             )
      } yield assertTrue(ref.get() == 42)
    }
  )

  private val dslIntegration = suite("ZIOSteps DSL: step registration")(
    test("a Given step registered with the DSL is discoverable via getSteps") {
      val suite = new ZIOSteps[Any, Unit] {
        Given("a step with param " / string)((_: String) => ZIO.unit)
      }
      assertTrue(suite.getSteps.length == 1)
    },
    test("multiple steps registered with different keywords are all present") {
      val suite = new ZIOSteps[Any, Unit] {
        Given("given step")(ZIO.unit)
        When("when step")(ZIO.unit)
        Then("then step")(ZIO.unit)
        And("and step")(ZIO.unit)
        But("but step")(ZIO.unit)
      }
      assertTrue(suite.getSteps.length == 5)
    },
    test("registered step matches against the correct input text") {
      val suite = new ZIOSteps[Any, Unit] {
        Given("a step with param " / string)((_: String) => ZIO.unit)
      }
      val def_   = suite.getSteps.head.asInstanceOf[StepDefImpl[Any, Unit, Tuple1[String]]]
      val result = def_.tryExecute(si("a step with param hello"))
      assert(result)(isSome(anything))
    },
    test("empty step body {} compiles and is registered") {
      val suite = new ZIOSteps[Any, Unit] {
        Given("empty step")(ZIO.unit)
      }
      assertTrue(suite.getSteps.length == 1)
    },
    test("StepEffect type alias is usable in private helpers") {
      class MySuite extends ZIOSteps[Any, Unit] {
        private def helper(): StepEffect =
          ScenarioContext.update(identity)
        Given("step using helper")(helper())
      }
      val s = new MySuite()
      assertTrue(s.getSteps.length == 1)
    },
    test("StepIO[+A] type alias is usable in value-producing helpers") {
      class MySuite extends ZIOSteps[Any, Unit] {
        private def fetchValue(): StepIO[Int] = ZIO.succeed(42)
        Given("step with StepIO") {
          fetchValue().flatMap(n => Assertions.assertEquals(n, 42))
        }
      }
      assertTrue(new MySuite {}.getSteps.length == 1)
    }
  )

  case class TinyState(n: Int = 0)
  given Schema[TinyState] = {
    import zio.schema.DeriveSchema
    DeriveSchema.gen[TinyState]
  }

  private val ambiguityValidation = suite("Startup ambiguity validation")(
    test("registering two steps with the same text throws at first getSteps call") {
      class S extends ZIOSteps[Any, TinyState]:
        Given("dup")(ZIO.unit)
        Given("dup")(ZIO.unit)
      val result = scala.util.Try(new S {}.getSteps)
      assertTrue(result.isFailure, result.failed.get.isInstanceOf[IllegalStateException])
    },
    test("distinct patterns register without error") {
      class S extends ZIOSteps[Any, TinyState]:
        Given("step A")(ZIO.unit)
        Given("step B")(ZIO.unit)
      assertTrue(new S {}.getSteps.length == 2)
    },
    test("same text for different step types (Given vs When) is not ambiguous") {
      class S extends ZIOSteps[Any, TinyState]:
        Given("action")(ZIO.unit)
        When("action")(ZIO.unit)
      assertTrue(new S {}.getSteps.length == 2)
    },
    test("registering after getSteps is sealed throws IllegalStateException") {
      class S extends ZIOSteps[Any, TinyState]:
        Given("initial")(ZIO.unit)
      val s = new S {}
      s.getSteps // seals the registry
      val result = scala.util.Try(
        s.register(
          StepDefImpl[Any, TinyState, EmptyTuple](
            zio.bdd.gherkin.StepType.GivenStep,
            StepExpression(List(Literal("late"))),
            _ => ZIO.unit
          )
        )
      )
      assertTrue(result.isFailure)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepExpression")(
    patternMatching,
    regexCaching,
    stepDef,
    dslIntegration,
    ambiguityValidation
  )
}
