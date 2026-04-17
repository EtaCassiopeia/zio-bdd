package zio.bdd.core.step

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for InlineStepMethods — the codegen-free alternative to the sbt
 * StepGeneratorPlugin.
 *
 * InlineStepMethods uses Scala 3 `transparent inline` + `TupleToFn` match types
 * to resolve step body arities at compile time, with no code generation.
 */
object InlineStepMethodsSpec extends ZIOSpecDefault {

  case class S(x: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  def spec: Spec[TestEnvironment & Scope, Any] = suite("InlineStepMethods")(
    suite("Step registration by arity")(
      test("0-arity: Given with no extractors registers one step") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("simple step")(ZIO.unit)
        assertTrue(new Suite {}.getSteps.length == 1)
      },
      test("1-arity: Given with one string extractor registers one step") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("value is " / string)((_: String) => ZIO.unit)
        assertTrue(new Suite {}.getSteps.length == 1)
      },
      test("2-arity: Given with two extractors registers one step") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("user " / string / " age " / int)((_: String, _: Int) => ZIO.unit)
        assertTrue(new Suite {}.getSteps.length == 1)
      },
      test("3-arity: Given with three extractors registers one step") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("a " / string / " b " / int / " c " / double)((_: String, _: Int, _: Double) => ZIO.unit)
        assertTrue(new Suite {}.getSteps.length == 1)
      }
    ),
    suite("All five keywords")(
      test("Given, When, Then, And, But all register steps") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("given step")(ZIO.unit)
          When("when step")(ZIO.unit)
          Then("then step")(ZIO.unit)
          And("and step")(ZIO.unit)
          But("but step")(ZIO.unit)
        assertTrue(new Suite {}.getSteps.length == 5)
      }
    ),
    suite("Step execution via InlineStepMethods")(
      test("matched inline step executes its body correctly") {
        val counter = new java.util.concurrent.atomic.AtomicInteger(0)
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("count " / int) { (n: Int) =>
            ZIO.attempt(counter.set(n)).unit.mapError(identity)
          }
        val s      = new Suite {}
        val def_   = s.getSteps.head.asInstanceOf[StepDefImpl[Any, S, Tuple1[Int]]]
        val effect = def_.tryExecute(StepInput("count 99")).getOrElse(ZIO.unit)
        for {
          _ <- effect.provide(
                 ZLayer.succeed(new State[S] {
                   def get               = ZIO.succeed(S())
                   def update(f: S => S) = ZIO.unit
                 }) ++ Scope.default
               )
        } yield assertTrue(counter.get() == 99)
      },
      test("unmatched inline step returns None from tryExecute") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          Given("exactly this")(ZIO.unit)
        val s    = new Suite {}
        val def_ = s.getSteps.head
        assertTrue(def_.tryExecute(StepInput("something else")).isEmpty)
      }
    ),
    suite("InlineStepMethods alongside GeneratedStepMethods")(
      test("mixing inline and generated steps in the same suite is supported") {
        class Suite extends ZIOSteps[Any, S] with InlineStepMethods[Any, S]:
          // Generated (via sbt plugin)
          Given("generated step")(ZIO.unit)
          // Inline variant
          step(zio.bdd.gherkin.StepType.WhenStep, "inline step" / string) { (_: String) =>
            ZIO.unit
          }
        assertTrue(new Suite {}.getSteps.length == 2)
      }
    )
  )
}
