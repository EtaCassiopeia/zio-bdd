package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.gherkin.*
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

object AdvancedEcommerceSpec extends ZIOSpecDefault {

  implicit val productSchema: Schema[Product] = DeriveSchema.gen[Product]
  implicit val cartSchema: Schema[Cart]       = DeriveSchema.gen[Cart]
  implicit val orderSchema: Schema[Order]     = DeriveSchema.gen[Order]
  implicit val paymentSchema: Schema[Payment] = DeriveSchema.gen[Payment]

  type Env = ProductCatalog & ShoppingCart & OrderService & PaymentGateway

  private class EcommerceSteps extends ZIOSteps[Env, Cart] {

    Given("the product catalog contains" / table[Product]) { (products: List[Product]) =>
      for {
        catalog <- ZIO.service[ProductCatalog]
        _ <- ZIO.foreach(products) { product =>
               catalog.updateProduct(product.id, product.name, product.price, product.stock)
             }
      } yield ()
    }

    Given("a new shopping cart") {
      for {
        cartService <- ZIO.service[ShoppingCart]
        cart        <- cartService.createCart
        _           <- ScenarioContext.update(_ => cart)
      } yield ()
    }

    When("the user adds " / int / " units of product " / string / " to the cart") {
      (quantity: Int, productId: String) =>
        for {
          cartService <- ZIO.service[ShoppingCart]
          cart        <- ScenarioContext.get
          updatedCart <- cartService.addToCart(cart, productId, quantity)
          _           <- ScenarioContext.update(_ => updatedCart)
        } yield ()
    }

    When("the user places an order") {
      for {
        orderService <- ZIO.service[OrderService]
        cart         <- ScenarioContext.get
        order        <- orderService.placeOrder(cart)
        _            <- ZIO.logInfo(s"Order placed with total: ${order.total}")
      } yield ()
    }

    Then("the order total should be " / double) { (expectedTotal: Double) =>
      for {
        orderService <- ZIO.service[OrderService]
        cart         <- ScenarioContext.get
        order        <- orderService.placeOrder(cart)
        _            <- Assertions.assertEquals(order.total, expectedTotal, "Order total mismatch")
      } yield ()
    }

    When("the payment is processed") {
      for {
        paymentGateway <- ZIO.service[PaymentGateway]
        orderService   <- ZIO.service[OrderService]
        cart           <- ScenarioContext.get
        order          <- orderService.placeOrder(cart)
        payment        <- paymentGateway.processPayment(order, order.total)
        _              <- ZIO.logInfo(s"Payment status: ${payment.status}")
      } yield ()
    }

    Then("the payment status should be " / string) { (expectedStatus: String) =>
      for {
        paymentGateway <- ZIO.service[PaymentGateway]
        orderService   <- ZIO.service[OrderService]
        cart           <- ScenarioContext.get
        order          <- orderService.placeOrder(cart)
        payment        <- paymentGateway.processPayment(order, order.total)
        _              <- Assertions.assertEquals(payment.status, expectedStatus, "Payment status mismatch")
      } yield ()
    }
  }

  private val testFile = "ecommerce.feature"
  private val steps    = new EcommerceSteps {}

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Advanced Ecommerce Feature")(
    test("execute order processing feature") {
      val content = """
                      |Feature: Ecommerce Order Processing
                      |  Scenario: Successful order placement and payment
                      |    Given the product catalog contains
                      |      | id  | name     | price  | stock |
                      |      | P1  | Laptop   | 999.99 | 5     |
                      |      | P2  | Mouse    | 29.99  | 10    |
                      |    And a new shopping cart
                      |    When the user adds 2 units of product "P1" to the cart
                      |    And the user adds 3 units of product "P2" to the cart
                      |    And the user places an order
                      |    Then the order total should be 2089.95
                      |    When the payment is processed
                      |    Then the payment status should be "Success"
      """.stripMargin

      for {
        feature <- GherkinParser.parseFeature(content, testFile)
        results <- FeatureExecutor
                     .executeFeatures[Env, Cart](
                       List(feature),
                       steps.getSteps,
                       steps
                     )
                     .provide(
                       TestProductCatalog.layer,
                       TestShoppingCart.layer,
                       TestOrderService.layer,
                       TestPaymentGateway.layer
                     )
      } yield assertTrue(results.head.isPassed)
    }
  )
}
