package zio.bdd.core

import zio.*
import zio.bdd.core.Assertions.assertTrue

object ShoppingCartSteps
    extends ZIOSteps.Default[ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector] {

  Given(
    "a product {productId:String} exists with name {name:String} price {price:Float} and stock {stock:Int}"
  ) { case (productId: String, name: String, price: Float, stock: Int) =>
    for {
      catalog <- ZIO.service[ProductCatalog]
      _       <- catalog.updateProduct(productId, name, price.toDouble, stock)
    } yield ScenarioContext(products = Map(productId -> Product(productId, name, price.toDouble, stock)))
  }

  Given("an empty shopping cart exists") { _ =>
    for {
      cartService <- ZIO.service[ShoppingCart]
      cart        <- cartService.createCart
    } yield cart
  }

  When("the user adds {quantity:Int} of product {productId:String} to the cart") {
    case (cart: Cart, quantity: Int, productId: String) =>
      for {
        cartService <- ZIO.service[ShoppingCart]
        catalog     <- ZIO.service[ProductCatalog]
        product     <- catalog.getProduct(productId).someOrFail(new Exception(s"Product $productId not found"))
        _ <-
          ZIO.when(quantity < 0 || product.stock < quantity)(
            ZIO.fail(new Exception(s"Insufficient stock for $productId, quantity: $quantity, stock: ${product.stock}"))
          )
        updatedCart <- cartService.addToCart(cart, productId, quantity)
      } yield updatedCart
  }

  When("the user places the order") { (cart: Cart) =>
    for {
      orderService <- ZIO.service[OrderService]
      order        <- orderService.placeOrder(cart)
    } yield order
  }

  And("the payment is processed") { (order: Order) =>
    for {
      paymentGateway <- ZIO.service[PaymentGateway]
      payment        <- paymentGateway.processPayment(order, order.total)
    } yield payment
  }

  Then("the order total should be {total:Float}") {
    case (cart: Cart, total: Float) =>
      for {
        catalog <- ZIO.service[ProductCatalog]
        products <- ZIO
                      .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                      .map(_.flatten.toMap)
        cartTotal = cart.total(products)
        _        <- assertAlmostEqual(cartTotal, total.toDouble, s"Cart total $cartTotal != $total")
      } yield ()
    case (order: Order, total: Float) =>
      assertAlmostEqual(order.total, total.toDouble, s"Order total ${order.total} != $total")
    case (payment: Payment, total: Float) =>
      val order = payment.order
      assertAlmostEqual(order.total, total.toDouble, s"Order total ${order.total} != $total")
  }

  private def assertAlmostEqual(actual: Double, expected: Double, context: String) = {
    val precision = BigDecimal(0.01)
    val diff      = BigDecimal(actual) - BigDecimal(expected)
    assertTrue(diff.abs <= precision, s"$context mismatch: expected $expected, got $actual")
  }

  And("the current cart is set in the context") { (cart: Cart) =>
    for {
      catalog <- ZIO.service[ProductCatalog]
      products <- ZIO
                    .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                    .map(_.flatten.toMap)
    } yield ScenarioContext(products = products, cart = Some(cart))
  }

  And(
    "the cart contains {quantity:Int} of product {productId:String}"
  ) { case (context: ScenarioContext, quantity: Int, productId: String) =>
    for {
      cartService <- ZIO.service[ShoppingCart]
      cart        <- ZIO.fromOption(context.cart).orElseFail(new Exception("No cart in context"))
      product <-
        ZIO.fromOption(context.products.get(productId)).orElseFail(new Exception(s"Product $productId not found"))
      _ <- ZIO.when(quantity < 0 || product.stock < quantity)(
             ZIO.fail(new Exception(s"Insufficient stock for $productId"))
           )
      updatedCart <- cartService.addToCart(cart, productId, quantity)
    } yield context.copy(cart = Some(updatedCart))
  }

  And("the cart is retrieved from the context") { (context: ScenarioContext) =>
    ZIO.fromOption(context.cart).orElseFail(new Exception("No cart found in context"))
  }
}

case class ScenarioContext(
  products: Map[String, Product] = Map.empty,
  cart: Option[Cart] = None,
  order: Option[Order] = None,
  payments: List[Payment] = Nil
)
