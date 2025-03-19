package zio.bdd.core

import zio.*
import zio.bdd.gherkin.{StepType, Step as GherkinStep}
import zio.test.*
import zio.test.Assertion.equalTo
import izumi.reflect.Tag
import zio.bdd.core.report.{ConsoleReporter, Reporter}

object AssertionTest extends ZIOSpecDefault {

  private object Domain {
    case class Product(id: String, name: String, price: Double)
    case class Cart(id: String, items: Map[String, Int], discount: Option[Double])
    case class Order(total: Double, result: Either[String, Cart])
  }
  import Domain._

  // Helper to create a minimal StepExecutor for testing
  def makeExecutor[R](
    steps: ZIOSteps[R] = ZIOSteps.empty[R]
  ): ZIO[Scope & LogCollector & Reporter, Nothing, StepExecutor[R]] =
    ZIO.scoped {
      for {
        stackRef     <- Ref.make(Chunk.empty[StepRecord])
        logCollector <- ZIO.service[LogCollector]
        reporter     <- ZIO.service[Reporter]
      } yield StepExecutor("test-scenario", steps, stackRef, reporter, logCollector)
    }

  // Define a simple step definition for testing assertions
  private val demoSteps = new ZIOSteps[Any] {
    type Env = Any
    private var steps: List[StepDef[? <: Matchable, ?]]     = Nil
    override def getSteps: List[StepDef[? <: Matchable, ?]] = steps.reverse
    override protected def register[I <: Matchable: Tag, O: Tag](
      stepType: StepType,
      pattern: String,
      fn: Step[I, O]
    ): Unit =
      steps = StepDef(stepType, pattern, fn) :: steps
    override def environment: ZLayer[Any, Nothing, Any] = ZLayer.empty

    Given("a cart with id {string} and {int} items") { case (id: String, itemCount: Int) =>
      ZIO.succeed(Cart(id, Map("P1" -> itemCount), None))
    }
    When("a discount of {double} is applied") { case (cart: Cart, discount: Double) =>
      ZIO.succeed(cart.copy(discount = Some(discount)))
    }
    When("an order is placed") { (cart: Cart) =>
      ZIO.succeed(Order(cart.items.getOrElse("P1", 0) * 10.0, Right(cart)))
    }
    Then("an error occurs") { _ =>
      ZIO.fail(new RuntimeException("Simulated error"))
    }
  }

  override def spec: Spec[Any, Any] = suite("Assertions Demo")(
    test("assertTrue checks a simple condition") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        _        <- Assertions.assertTrue(result.succeeded, "Step execution should succeed")
      } yield assertTrue(true)
    },
    test("assertEquals checks cart ID") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _        <- Assertions.assertEquals(cart.id, "C1", "Cart ID mismatch")
      } yield assertTrue(true)
    },
    test("assertThrows detects step failure as a failed StepResult") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.ThenStep, "an error occurs", None, None)
        result   <- executor.executeStep(step)
        _        <- Assertions.assertTrue(!result.succeeded, "Step should fail")
        _        <- Assertions.assertSome(result.error, "Step should have an error")
        _ <- Assertions.assertTrue(
               result.error.exists(_.message.contains("Simulated error")),
               "Step error message should contain 'Step execution failed'"
             )
      } yield assertTrue(true)
    },
    test("assertSome checks for discount presence") {
      for {
        executor   <- makeExecutor(demoSteps)
        givenStep   = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep    = GherkinStep(StepType.WhenStep, "a discount of 5.0 is applied", None, None)
        _          <- executor.executeStep(givenStep)
        result     <- executor.executeStep(whenStep)
        updatedCart = result.output.asInstanceOf[Cart]
        _          <- Assertions.assertSome(updatedCart.discount, "Discount should be present")
      } yield assertTrue(true)
    },
    test("assertSomeEquals checks discount value") {
      for {
        executor   <- makeExecutor(demoSteps)
        givenStep   = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep    = GherkinStep(StepType.WhenStep, "a discount of 5.0 is applied", None, None)
        _          <- executor.executeStep(givenStep)
        result     <- executor.executeStep(whenStep)
        updatedCart = result.output.asInstanceOf[Cart]
        _          <- Assertions.assertSomeEquals(updatedCart.discount, 5.0, "Discount value mismatch")
      } yield assertTrue(true)
    },
    test("assertNone checks for no discount") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _        <- Assertions.assertNone(cart.discount, "Discount should not be present")
      } yield assertTrue(true)
    },
    test("assertRight checks order result") {
      for {
        executor <- makeExecutor(demoSteps)
        givenStep = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep  = GherkinStep(StepType.WhenStep, "an order is placed", None, None)
        _        <- executor.executeStep(givenStep)
        result   <- executor.executeStep(whenStep)
        order     = result.output.asInstanceOf[Order]
        _        <- Assertions.assertRight(order.result, "Order result should be Right")
      } yield assertTrue(true)
    },
    test("assertRightEquals checks order result cart") {
      for {
        executor   <- makeExecutor(demoSteps)
        givenStep   = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep    = GherkinStep(StepType.WhenStep, "an order is placed", None, None)
        cartResult <- executor.executeStep(givenStep)
        cart        = cartResult.output.asInstanceOf[Cart]
        result     <- executor.executeStep(whenStep)
        order       = result.output.asInstanceOf[Order]
        _          <- Assertions.assertRightEquals(order.result, cart, "Order result cart mismatch")
      } yield assertTrue(true)
    },
    test("assertLeft checks for error case") {
      val errorSteps = new ZIOSteps[Any] {
        type Env = Any
        private var steps: List[StepDef[? <: Matchable, ?]]     = Nil
        override def getSteps: List[StepDef[? <: Matchable, ?]] = steps.reverse
        override protected def register[I <: Matchable: Tag, O: Tag](
          stepType: StepType,
          pattern: String,
          fn: Step[I, O]
        ): Unit =
          steps = StepDef(stepType, pattern, fn) :: steps
        override def environment: ZLayer[Any, Nothing, Any] = ZLayer.empty

        When("an order fails") { (_: Any) =>
          ZIO.succeed(Order(0.0, Left("Order failed")))
        }
      }
      for {
        executor <- makeExecutor(errorSteps)
        step      = GherkinStep(StepType.WhenStep, "an order fails", None, None)
        result   <- executor.executeStep(step)
        order     = result.output.asInstanceOf[Order]
        _        <- Assertions.assertLeft(order.result, "Order result should be Left")
      } yield assertTrue(true)
    },
    test("assertLeftEquals checks error message") {
      val errorSteps = new ZIOSteps[Any] {
        type Env = Any
        private var steps: List[StepDef[? <: Matchable, ?]]     = Nil
        override def getSteps: List[StepDef[? <: Matchable, ?]] = steps.reverse
        override protected def register[I <: Matchable: Tag, O: Tag](
          stepType: StepType,
          pattern: String,
          fn: Step[I, O]
        ): Unit =
          steps = StepDef(stepType, pattern, fn) :: steps
        override def environment: ZLayer[Any, Nothing, Any] = ZLayer.empty

        When("an order fails") { (_: Any) =>
          ZIO.succeed(Order(0.0, Left("Order failed")))
        }
      }
      for {
        executor <- makeExecutor(errorSteps)
        step      = GherkinStep(StepType.WhenStep, "an order fails", None, None)
        result   <- executor.executeStep(step)
        order     = result.output.asInstanceOf[Order]
        _        <- Assertions.assertLeftEquals(order.result, "Order failed", "Order error message mismatch")
      } yield assertTrue(true)
    },
    test("assertContainsAll checks exact item match") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _        <- Assertions.assertContainsAll(cart.items, Map("P1" -> 2), "Cart items exact match failed")
      } yield assertTrue(true)
    },
    test("assertContainsSubset checks item subset") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _        <- Assertions.assertContainsSubset(cart.items.keys, Set("P1"), "Cart items subset failed")
      } yield assertTrue(true)
    },
    test("assertHasField checks item count") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _ <-
          Assertions.assertHasField(cart, "items", _.items, Assertion.hasSize(equalTo(1)), "Cart item count mismatch")
      } yield assertTrue(true)
    },
    test("assertFieldEquals checks cart ID") {
      for {
        executor <- makeExecutor(demoSteps)
        step      = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        result   <- executor.executeStep(step)
        cart      = result.output.asInstanceOf[Cart]
        _        <- Assertions.assertFieldEquals(cart, "id", _.id, "C1", "Cart ID field mismatch")
      } yield assertTrue(true)
    },
    test("assertNested checks nested discount presence") {
      for {
        executor   <- makeExecutor(demoSteps)
        givenStep   = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep    = GherkinStep(StepType.WhenStep, "a discount of 5.0 is applied", None, None)
        _          <- executor.executeStep(givenStep)
        result     <- executor.executeStep(whenStep)
        updatedCart = result.output.asInstanceOf[Cart]
        _ <-
          Assertions.assertNested(updatedCart, "discount", _.discount, Assertion.isSome, "Discount presence mismatch")
      } yield assertTrue(true)
    },
    test("assertNestedEquals checks nested discount value") {
      for {
        executor   <- makeExecutor(demoSteps)
        givenStep   = GherkinStep(StepType.GivenStep, "a cart with id C1 and 2 items", None, None)
        whenStep    = GherkinStep(StepType.WhenStep, "a discount of 5.0 is applied", None, None)
        _          <- executor.executeStep(givenStep)
        result     <- executor.executeStep(whenStep)
        updatedCart = result.output.asInstanceOf[Cart]
        _          <- Assertions.assertNestedEquals(updatedCart, "discount", _.discount, Some(5.0), "Discount value mismatch")
      } yield assertTrue(true)
    }
  ).provide(LogCollector.live, ZLayer.succeed(ConsoleReporter), Scope.default)
}
