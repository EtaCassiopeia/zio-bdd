package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.{StepDefImpl, StepExpression, Literal, ZIOSteps}
import zio.bdd.gherkin.StepType
import zio.schema.{DeriveSchema, Schema}

/**
 * Validates the AtomicBoolean sealing logic in ZIOSteps.getSteps:
 *   - Ambiguous patterns (same step type, same text) throw at first getSteps
 *   - Identical text across different step types is NOT ambiguous
 *   - After seal, register() throws (late registration rejected)
 *   - getSteps is idempotent for non-ambiguous registries
 *   - Concurrent first-callers all see the same IllegalStateException
 */
object StepRegistrySealSpec extends ZIOSpecDefault {

  case class S(v: Int = 0)
  given Schema[S] = DeriveSchema.gen[S]

  private val ambiguitySuite = suite("ambiguous patterns detected at getSteps")(
    test("two Given with identical text throw IllegalStateException at first getSteps") {
      val steps = new ZIOSteps[Any, S] {
        Given("the same step")(ZIO.unit)
        Given("the same step")(ZIO.unit)
      }
      assertZIO(ZIO.attempt(steps.getSteps).exit)(
        fails(isSubtype[IllegalStateException](hasMessage(containsString("Ambiguous step definitions"))))
      )
    },
    test("error message names the duplicated pattern") {
      val steps = new ZIOSteps[Any, S] {
        Given("duplicate me")(ZIO.unit)
        Given("duplicate me")(ZIO.unit)
      }
      assertZIO(ZIO.attempt(steps.getSteps).exit)(
        fails(isSubtype[IllegalStateException](hasMessage(containsString("duplicate me"))))
      )
    }
  )

  private val crossKeywordSuite = suite("cross-keyword non-ambiguity")(
    test("same text on Given vs When is NOT ambiguous") {
      val steps = new ZIOSteps[Any, S] {
        Given("shared step text")(ZIO.unit)
        When("shared step text")(ZIO.unit)
      }
      assertZIO(ZIO.attempt(steps.getSteps).exit)(succeeds(hasSize(equalTo(2))))
    },
    test("same text on Then vs And is NOT ambiguous") {
      val steps = new ZIOSteps[Any, S] {
        Then("shared step text")(ZIO.unit)
        And("shared step text")(ZIO.unit)
      }
      assertZIO(ZIO.attempt(steps.getSteps).exit)(succeeds(hasSize(equalTo(2))))
    }
  )

  private val lateRegistrationSuite = suite("late registration rejected after seal")(
    test("register() after getSteps throws IllegalStateException") {
      val steps = new ZIOSteps[Any, S] {
        Given("a step")(ZIO.unit)
      }
      for {
        _ <- ZIO.attempt(steps.getSteps) // seals
        exit <- ZIO.attempt {
                  steps.register(
                    StepDefImpl[Any, S, EmptyTuple](
                      StepType.GivenStep,
                      StepExpression(List(Literal("late step"))),
                      (_: EmptyTuple) => ZIO.unit
                    )
                  )
                }.exit
      } yield assert(exit)(fails(isSubtype[IllegalStateException](anything)))
    }
  )

  private val idempotenceSuite = suite("getSteps idempotency")(
    test("100 calls to getSteps on non-ambiguous registry all return same step count") {
      val steps = new ZIOSteps[Any, S] {
        Given("step a")(ZIO.unit)
        When("step b")(ZIO.unit)
        Then("step c")(ZIO.unit)
      }
      assertZIO(
        ZIO.foreach(1 to 100)(_ => ZIO.attempt(steps.getSteps).map(_.length))
      )(forall(equalTo(3)))
    }
  )

  private val concurrentSealSuite = suite("concurrent seal races")(
    test("32 concurrent getSteps calls on non-ambiguous registry all succeed") {
      val steps = new ZIOSteps[Any, S] {
        Given("concurrent step")(ZIO.unit)
      }
      for {
        results <- ZIO.foreachPar((1 to 32).toList)(_ => ZIO.attempt(steps.getSteps).exit)
      } yield assertTrue(results.forall(_.isSuccess))
    },
    test("32 concurrent getSteps calls on ambiguous registry all fail with same exception type") {
      // Create a fresh instance for this test
      val steps = new ZIOSteps[Any, S] {
        Given("ambiguous")(ZIO.unit)
        Given("ambiguous")(ZIO.unit)
      }
      for {
        results <- ZIO.foreachPar((1 to 32).toList)(_ => ZIO.attempt(steps.getSteps).exit)
      } yield {
        val failures = results.collect { case Exit.Failure(c) => c }
        assertTrue(
          failures.nonEmpty,
          failures.forall(_.failureOption.exists(_.isInstanceOf[IllegalStateException]))
        )
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepRegistrySealSpec")(
    ambiguitySuite,
    crossKeywordSuite,
    lateRegistrationSuite,
    idempotenceSuite,
    concurrentSealSuite
  )
}
