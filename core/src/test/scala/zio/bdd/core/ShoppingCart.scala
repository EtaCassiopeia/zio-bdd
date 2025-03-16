package zio.bdd.core

import zio.*

import scala.math.BigDecimal.RoundingMode

case class Product(id: String, name: String, price: Double, stock: Int)

case class Cart(items: Map[String, Int] = Map.empty) {
  def addItem(productId: String, quantity: Int): Cart =
    copy(items = items + (productId -> (items.getOrElse(productId, 0) + quantity)))

  def total(products: Map[String, Product]): Double = {
    val sum = items.map { case (productId, qty) =>
      products.get(productId).map(_.price * qty).getOrElse(0.0)
    }.sum

    BigDecimal(sum).setScale(2, RoundingMode.CEILING).toDouble
  }
}
case class Order(cart: Cart, total: Double)
case class Payment(order: Order, status: String)

trait ProductCatalog {
  def getProduct(id: String): ZIO[Any, Throwable, Option[Product]]
  def updateProduct(id: String, name: String, price: Double, stock: Int): ZIO[Any, Nothing, Unit]
}

object TestProductCatalog {
  class TestProductCatalog extends ProductCatalog {
    private val products = scala.collection.mutable.Map[String, Product]()

    def getProduct(id: String): ZIO[Any, Throwable, Option[Product]] =
      ZIO.succeed(products.get(id))

    def updateProduct(id: String, name: String, price: Double, stock: Int): ZIO[Any, Nothing, Unit] = ZIO.succeed {
      products.update(id, Product(id, name, price, stock))
    }
  }

  val layer: ZLayer[Any, Nothing, ProductCatalog] = ZLayer.fromZIO {
    ZIO.succeed(new TestProductCatalog) // Fresh instance per test
  }
}

trait ShoppingCart {
  def createCart: ZIO[Any, Nothing, Cart]

  def addToCart(cart: Cart, productId: String, quantity: Int): ZIO[Any, Nothing, Cart]
}

object TestShoppingCart {
  class TestShoppingCart extends ShoppingCart {
    def createCart: ZIO[Any, Nothing, Cart] = ZIO.succeed(Cart(Map()))

    def addToCart(cart: Cart, productId: String, quantity: Int): ZIO[Any, Nothing, Cart] = ZIO.succeed {
      val currentQty = cart.items.getOrElse(productId, 0)
      cart.copy(items = cart.items + (productId -> (currentQty + quantity)))
    }
  }

  val layer: ZLayer[Any, Nothing, ShoppingCart] = ZLayer.succeed(new TestShoppingCart)
}

trait OrderService {
  def placeOrder(cart: Cart): ZIO[ProductCatalog, Throwable, Order]
}

object TestOrderService {
  class TestOrderService(catalog: ProductCatalog) extends OrderService {
    def placeOrder(cart: Cart): ZIO[ProductCatalog, Throwable, Order] =
      for {
        orderRef <- Ref.make[Option[Order]](None) // Fresh Ref per call
        products <- ZIO
                      .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                      .map(_.flatten.toMap)
        total = cart.total(products)
        order = Order(cart, total)
        _    <- orderRef.set(Some(order))
      } yield order
  }

  val layer: ZLayer[ProductCatalog, Nothing, OrderService] = ZLayer.fromFunction(new TestOrderService(_))
}

trait PaymentGateway {
  def processPayment(order: Order, amount: Double): ZIO[Any, Nothing, Payment]
}

object TestPaymentGateway {
  class TestPaymentGateway extends PaymentGateway {
    def processPayment(order: Order, amount: Double): ZIO[Any, Nothing, Payment] =
      ZIO.succeed(Payment(order, "Success"))
  }

  val layer: ZLayer[Any, Nothing, PaymentGateway] = ZLayer.succeed(new TestPaymentGateway)
}
