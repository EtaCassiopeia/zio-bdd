package zio.bdd.core

import zio.*
import zio.bdd.core.Assertions.assertTrue

object ShoppingCartSteps
    extends ZIOSteps.Default[ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector] {
  Given[(String, String, Float, Int), Unit](
    "a product {productId:String} exists with name {name:String} price {price:Float} and stock {stock:Int}"
  ) { case (productId: String, name: String, price: Float, stock: Int) =>
    for {
      catalog <- ZIO.service[ProductCatalog]
      _       <- catalog.updateStock(productId, stock)
    } yield ()
  }

  Given[Unit, Cart]("an empty shopping cart exists") { (_: Unit) =>
    for {
      cartService <- ZIO.service[ShoppingCart]
      cart        <- cartService.createCart
    } yield cart
  }

  When[(Cart, Int, String), Cart]("the user adds {quantity:Int} of product {productId:String} to the cart") {
    case (cart: Cart, quantity: Int, productId: String) =>
      for {
        cartService <- ZIO.service[ShoppingCart]
        catalog     <- ZIO.service[ProductCatalog]
        product     <- catalog.getProduct(productId).someOrFail(new Exception(s"Product $productId not found"))
        _ <- ZIO.when(quantity < 0 || product.stock < quantity)(
               ZIO.fail(new Exception(s"Insufficient stock for $productId".replace("\"", "")))
             )
        updatedCart <- cartService.addToCart(cart, productId, quantity)
      } yield updatedCart
  }

  When[Cart, Order]("the user places the order") { (cart: Cart) =>
    for {
      orderService <- ZIO.service[OrderService]
      order        <- orderService.placeOrder(cart).provideLayer(TestProductCatalog.layer)
    } yield order
  }

  Then[((Cart | Order | Payment), Float), Unit]("the order total should be {total:Float}") {
    case (cart: Cart, total: Float) =>
      for {
        catalog <- ZIO.service[ProductCatalog]
        products <- ZIO
                      .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                      .map(_.flatten.toMap)
        cartTotal = cart.total(products)
        _        <- ZIO.succeed(assertTrue(cartTotal == total.toDouble, s"Cart total $cartTotal != $total"))
      } yield ()
    case (order: Order, total: Float) =>
      ZIO.succeed(assertTrue(order.total == total.toDouble, s"Order total ${order.total} != $total"))
    case (payment: Payment, total: Float) =>
      val order = payment.order
      ZIO.succeed(assertTrue(order.total == total.toDouble, s"Order total ${order.total} != $total"))
  }

  And[Order, Payment]("the payment is processed") { (order: Order) =>
    for {
      paymentGateway <- ZIO.service[PaymentGateway]
      payment        <- paymentGateway.processPayment(order, order.total)
    } yield payment
  }

  Then[(Payment, String), Unit]("the payment status should be {status:String}") {
    case (payment: Payment, status: String) =>
      ZIO.succeed(assertTrue(payment.status == status, s"Payment status ${payment.status} != $status"))
  }

  Given[(String, String, Float, Int), ScenarioContext](
    "a product {productId:String} exists with name {name:String} price {price:Float} and stock {stock:Int}"
  ) { case (productId: String, name: String, price: Float, stock: Int) =>
    for {
      catalog <- ZIO.service[ProductCatalog]
      _       <- catalog.updateStock(productId, stock)
    } yield ScenarioContext(products = Map(productId -> Product(productId, name, price.toDouble, stock)))
  }

  And[Cart, ScenarioContext]("the current cart is set in the context") { (cart: Cart) =>
    for {
      catalog <- ZIO.service[ProductCatalog]
      products <- ZIO
                    .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                    .map(_.flatten.toMap)
    } yield ScenarioContext(products = products, cart = Some(cart))
  }

  And[(ScenarioContext, Int, String), ScenarioContext](
    "the cart contains {quantity:Int} of product {productId:String}"
  ) { case (context: ScenarioContext, quantity: Int, productId: String) =>
    for {
      cartService <- ZIO.service[ShoppingCart]
      cart        <- cartService.getCart
      product <-
        ZIO.fromOption(context.products.get(productId)).orElseFail(new Exception(s"Product $productId not found"))
      _ <- ZIO.when(quantity < 0 || product.stock < quantity)(
             ZIO.fail(new Exception(s"Insufficient stock for $productId".replace("\"", "")))
           )
      updatedCart <- cartService.addToCart(cart, productId, quantity)
    } yield context.copy(cart = Some(updatedCart))
  }

  And[ScenarioContext, Cart]("the cart is retrieved from the context") { (context: ScenarioContext) =>
    ZIO.fromOption(context.cart).orElseFail(new Exception("No cart found in context"))
  }

  override def beforeFeature
    : ZIO[ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector, Throwable, Unit] =
    ZIO.logInfo("Preparing feature: setting up shopping system")

  override def beforeScenario(
    scenarioId: String
  ): ZIO[ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector, Throwable, Unit] =
    ZIO.logInfo(s"Starting scenario with ID: $scenarioId")

  override def beforeStep(
    scenarioId: String
  ): ZIO[ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector, Throwable, Unit] =
    ZIO.logInfo(s"Before step in scenario $scenarioId")
}

case class ScenarioContext(
  products: Map[String, Product] = Map.empty,
  cart: Option[Cart] = None,
  order: Option[Order] = None,
  payments: List[Payment] = Nil
)
