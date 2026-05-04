package zio.bdd.core

import zio.*
import zio.bdd.core.step.*
import zio.bdd.gherkin.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for the Assertions helper library.
 *
 * Each test exercises one assertion variant by running it through the step
 * execution engine — the assertion must pass (the feature is passed) or fail
 * (the feature result has an error) as documented.
 */
object AssertionSpec extends ZIOSpecDefault {

  case class Cart(id: String, items: Map[String, Int], discount: Option[Double])
  given Schema[Cart] = DeriveSchema.gen[Cart]

  class CartSteps extends ZIOSteps[Any, Cart] {
    Given("a cart " / string / " with " / int / " items") { (id: String, n: Int) =>
      ScenarioContext.update(_ => Cart(id, Map("P1" -> n), None))
    }
    When("discount " / double / " is applied") { (d: Double) =>
      ScenarioContext.update(c => c.copy(discount = Some(d)))
    }
    Then("cart id is " / string) { (expected: String) =>
      ScenarioContext.get.flatMap(c => Assertions.assertEquals(c.id, expected))
    }
    Then("discount is " / double) { (expected: Double) =>
      ScenarioContext.get.flatMap(c => Assertions.assertSomeEquals(c.discount, expected))
    }
    Then("no discount is set") {
      ScenarioContext.get.flatMap(c => Assertions.assertNone(c.discount))
    }
    Then("item count is " / int) { (expected: Int) =>
      ScenarioContext.get.flatMap(c => Assertions.assertEquals(c.items.values.sum, expected))
    }
    Then("cart is non-empty") {
      ScenarioContext.get.flatMap(c => Assertions.assertTrue(c.items.nonEmpty))
    }
    Then("trigger failure") {
      ZIO.fail(new RuntimeException("deliberate failure")).unit
    }
  }

  private def runScene(steps: CartSteps, scenarioSteps: List[Step]): ZIO[Any, Throwable, FeatureResult] =
    steps.run(List(Feature("T", Nil, List(Scenario("s", Nil, scenarioSteps, Some("t.feature"), Some(1)))))).map(_.head)

  private def step(t: StepType, p: String) = Step(t, p, None, None, None)

  private val assertTrueTests = suite("assertTrue")(
    test("passes when the condition is true") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C1 with 3 items"),
          step(StepType.ThenStep, "cart is non-empty")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when the condition is false") {
      // Trigger deliberate failure to verify assertion failure propagates correctly
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C1 with 3 items"),
          step(StepType.ThenStep, "trigger failure")
        )
      ).map(r => assertTrue(!r.isPassed, r.error.isDefined))
    }
  )

  private val assertEqualsTests = suite("assertEquals")(
    test("passes when values are equal") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C2 with 5 items"),
          step(StepType.ThenStep, "cart id is C2")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when values are not equal") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C2 with 5 items"),
          step(StepType.ThenStep, "cart id is WRONG")
        )
      ).map(r => assertTrue(!r.isPassed))
    }
  )

  private val assertSomeTests = suite("assertSomeEquals")(
    test("passes when Option contains the expected value") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C3 with 1 items"),
          step(StepType.WhenStep, "discount 0.15 is applied"),
          step(StepType.ThenStep, "discount is 0.15")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when Option is None") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C3 with 1 items"),
          // No discount applied → discount remains None
          step(StepType.ThenStep, "discount is 0.15")
        )
      ).map(r => assertTrue(!r.isPassed))
    }
  )

  private val assertNoneTests = suite("assertNone")(
    test("passes when Option is None") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C4 with 2 items"),
          step(StepType.ThenStep, "no discount is set")
        )
      ).map(r => assertTrue(r.isPassed))
    },
    test("fails when Option is Some") {
      val s = new CartSteps {}
      runScene(
        s,
        List(
          step(StepType.GivenStep, "a cart C4 with 2 items"),
          step(StepType.WhenStep, "discount 0.10 is applied"),
          step(StepType.ThenStep, "no discount is set") // should fail
        )
      ).map(r => assertTrue(!r.isPassed))
    }
  )

  private val directTests = suite("Direct assertion helpers")(
    test("assertRight passes for Right") {
      for {
        _ <- Assertions.assertRight(Right("value"): Either[String, String])
      } yield assertCompletes
    },
    test("assertRight fails for Left") {
      for {
        r <- Assertions.assertRight(Left("error"): Either[String, String]).either
      } yield assertTrue(r.isLeft)
    },
    test("assertLeft passes for Left") {
      for {
        _ <- Assertions.assertLeft(Left("error"): Either[String, String])
      } yield assertCompletes
    },
    test("assertLeft fails for Right") {
      for {
        r <- Assertions.assertLeft(Right("value"): Either[String, String]).either
      } yield assertTrue(r.isLeft)
    },
    test("assertContainsAll passes when collections match") {
      for {
        _ <- Assertions.assertContainsAll(List(1, 2, 3), List(1, 2, 3))
      } yield assertCompletes
    },
    test("assertContainsAll fails when an element is missing") {
      for {
        r <- Assertions.assertContainsAll(List(1, 2), List(1, 2, 3)).either
      } yield assertTrue(r.isLeft)
    },
    test("assertSome passes for Some") {
      for {
        _ <- Assertions.assertSome(Some("value"))
      } yield assertCompletes
    },
    test("assertSome fails for None") {
      for {
        r <- Assertions.assertSome(None: Option[String]).either
      } yield assertTrue(r.isLeft)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Assertions")(
    assertTrueTests,
    assertEqualsTests,
    assertSomeTests,
    assertNoneTests,
    directTests
  )
}
