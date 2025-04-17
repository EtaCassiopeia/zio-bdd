package zio.bdd.core

import zio.*
import zio.bdd.core.step.*
import zio.bdd.gherkin.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*
import zio.test.Assertion.*

object AssertionSpec extends ZIOSpecDefault {

  object Domain {
    case class Cart(id: String, items: Map[String, Int], discount: Option[Double])
    case class Order(total: Double, result: Either[String, Cart])

    implicit val cartSchema: Schema[Cart]   = DeriveSchema.gen[Cart]
    implicit val orderSchema: Schema[Order] = DeriveSchema.gen[Order]
  }

  import Domain._

  private class DemoSteps extends ZIOSteps[Any, Cart] {
    Given("a cart with id " / string / " and " / int / " items") { (id: String, itemCount: Int) =>
      ScenarioContext.update(_ => Cart(id, Map("P1" -> itemCount), None))
    }

    When("a discount of " / double / " is applied") { (discount: Double) =>
      for {
        cart <- ScenarioContext.get
        _    <- ScenarioContext.update(_ => cart.copy(discount = Some(discount)))
      } yield ()
    }

    When("an order is placed") {
      for {
        cart <- ScenarioContext.get
        _    <- ZIO.logInfo(s"Order placed for cart: ${cart.id}")
      } yield ()
    }

    Then("an error occurs") {
      ZIO.fail(new RuntimeException("Simulated error")).unit
    }

    Then("the cart ID is " / string) { (expectedId: String) =>
      for {
        cart <- ScenarioContext.get
        _    <- Assertions.assertEquals(cart.id, expectedId, "Cart ID mismatch")
      } yield ()
    }

    Then("the discount is " / double) { (expectedDiscount: Double) =>
      for {
        cart <- ScenarioContext.get
        _    <- Assertions.assertSomeEquals(cart.discount, expectedDiscount, "Discount value mismatch")
      } yield ()
    }

    Then("the discount is not present") {
      for {
        cart <- ScenarioContext.get
        _    <- Assertions.assertNone(cart.discount, "Discount should not be present")
      } yield ()
    }
  }

  def createFeature(scenarioName: String, steps: List[Step]): Feature =
    Feature(
      name = "Test Feature",
      scenarios = List(
        Scenario(
          name = scenarioName,
          steps = steps,
          tags = Nil,
          file = Some("test.feature"),
          line = Some(1)
        )
      ),
      file = Some("test.feature"),
      line = Some(1)
    )

  private def runFeature(steps: ZIOSteps[Any, Cart], feature: Feature) = steps.run(List(feature))

  override def spec: Spec[Any, Any] = suite("Assertions Demo")(
    test("assertTrue checks a simple condition") {
      val stepsDef = new DemoSteps {}
      val feature = createFeature(
        "Check cart ID",
        List(
          Step(StepType.GivenStep, "a cart with id C1 and 2 items", None, None, None),
          Step(StepType.ThenStep, "the cart ID is C1", None, None, None)
        )
      )
      for {
        results <- runFeature(stepsDef, feature)
      } yield assertTrue(results.head.isPassed)
    },
    test("assertEquals verifies cart ID") {
      val stepsDef = new DemoSteps {}
      val feature = createFeature(
        "Verify cart ID equality",
        List(
          Step(StepType.GivenStep, "a cart with id C2 and 3 items", None, None, None),
          Step(StepType.ThenStep, "the cart ID is C2", None, None, None)
        )
      )
      for {
        results <- runFeature(stepsDef, feature)
      } yield assertTrue(results.head.isPassed)
    },
    test("assertSomeEquals checks discount value") {
      val stepsDef = new DemoSteps {}
      val feature = createFeature(
        "Check discount application",
        List(
          Step(StepType.GivenStep, "a cart with id C3 and 1 items", None, None, None),
          Step(StepType.WhenStep, "a discount of 0.15 is applied", None, None, None),
          Step(StepType.ThenStep, "the discount is 0.15", None, None, None)
        )
      )
      for {
        results <- runFeature(stepsDef, feature)
      } yield assertTrue(results.head.isPassed)
    },
    test("assertNone verifies no discount") {
      val stepsDef = new DemoSteps {}
      val feature = createFeature(
        "Check no discount",
        List(
          Step(StepType.GivenStep, "a cart with id C4 and 5 items", None, None, None),
          Step(StepType.ThenStep, "the discount is not present", None, None, None)
        )
      )
      for {
        results <- runFeature(stepsDef, feature)
      } yield assertTrue(results.head.isPassed)
    },
    test("assert failure on error") {
      val stepsDef = new DemoSteps {}
      val feature = createFeature(
        "Check error handling",
        List(
          Step(StepType.GivenStep, "a cart with id C5 and 2 items", None, None, None),
          Step(StepType.ThenStep, "an error occurs", None, None, None)
        )
      )
      for {
        results      <- runFeature(stepsDef, feature)
        featureResult = results.head
      } yield assertTrue(!featureResult.isPassed) &&
        assert(featureResult.error)(isSome(isSubtype[RuntimeException](anything))) &&
        assert(featureResult.error.map(_.getMessage))(Assertion.isSome(Assertion.equalTo("Simulated error")))
    }
  )
}
