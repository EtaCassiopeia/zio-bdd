package zio.bdd.core

import zio.*

case class Product(id: String, name: String, price: Double, stock: Int)
case class Cart(items: Map[String, Int] = Map.empty) {
  def addItem(productId: String, quantity: Int): Cart =
    copy(items = items + (productId -> (items.getOrElse(productId, 0) + quantity)))

  def total(products: Map[String, Product]): Double =
    items.map { case (productId, qty) =>
      products.get(productId).map(_.price * qty).getOrElse(0.0)
    }.sum
}
case class Order(cart: Cart, total: Double)
case class Payment(order: Order, status: String)

trait ProductCatalog {
  def getProduct(id: String): ZIO[Any, Throwable, Option[Product]]
  def updateStock(id: String, quantity: Int): ZIO[Any, Nothing, Unit]
}

object TestProductCatalog {
  class TestProductCatalog extends ProductCatalog {
    private val products = scala.collection.mutable.Map[String, Product]()

    def getProduct(id: String): ZIO[Any, Throwable, Option[Product]] =
      ZIO.succeed(products.get(id))

    def updateStock(id: String, quantity: Int): ZIO[Any, Nothing, Unit] = ZIO.succeed {
      products.get(id) match {
        case Some(product) => products.update(id, product.copy(stock = quantity))
        case None          => products.update(id, Product(id, s"Product $id", 10.0, quantity))
      }
    }
  }

  val layer: ZLayer[Any, Nothing, ProductCatalog] = ZLayer.succeed(new TestProductCatalog)
}

trait ShoppingCart {
  def createCart: ZIO[Any, Nothing, Cart]
  def addToCart(cart: Cart, productId: String, quantity: Int): ZIO[Any, Nothing, Cart]
  def getCart: ZIO[Any, Nothing, Cart]
}

object TestShoppingCart {
  val layer: ZLayer[Any, Nothing, ShoppingCart] = ZLayer.fromZIO(
    Ref.make(Cart()).map { cartRef =>
      new ShoppingCart {
        def createCart: ZIO[Any, Nothing, Cart] = ZIO.succeed(Cart())

        def addToCart(cart: Cart, productId: String, quantity: Int): ZIO[Any, Nothing, Cart] = {
          val updatedCart = cart.addItem(productId, quantity)
          cartRef.set(updatedCart).as(updatedCart)
        }

        def getCart: ZIO[Any, Nothing, Cart] = cartRef.get
      }
    }
  )
}

trait OrderService {
  def placeOrder(cart: Cart): ZIO[ProductCatalog, Throwable, Order]
  def getOrder: ZIO[Any, Nothing, Option[Order]]
}

object TestOrderService {
  val layer: ZLayer[ProductCatalog, Nothing, OrderService] = ZLayer.fromZIO(
    Ref.make(Option.empty[Order]).map { orderRef =>
      new OrderService {
        def placeOrder(cart: Cart): ZIO[ProductCatalog, Throwable, Order] =
          for {
            catalog <- ZIO.service[ProductCatalog]
            products <- ZIO
                          .collectAll(cart.items.keys.map(id => catalog.getProduct(id).map(_.map(p => (id, p)))).toList)
                          .map(_.flatten.toMap)
            total = cart.total(products)
            order = Order(cart, total)
            _    <- orderRef.set(Some(order))
          } yield order

        def getOrder: ZIO[Any, Nothing, Option[Order]] = orderRef.get
      }
    }
  )
}

trait PaymentGateway {
  def processPayment(order: Order, amount: Double): ZIO[Any, Nothing, Payment]
  def getPayments: ZIO[Any, Nothing, List[Payment]]
}

object TestPaymentGateway {
  val layer: ZLayer[Any, Nothing, PaymentGateway] = ZLayer.fromZIO(
    Ref.make(List.empty[Payment]).map { paymentsRef =>
      new PaymentGateway {
        def processPayment(order: Order, amount: Double): ZIO[Any, Nothing, Payment] = {
          val payment = Payment(order, "Success")
          paymentsRef.update(payment :: _).as(payment)
        }
        def getPayments: ZIO[Any, Nothing, List[Payment]] = paymentsRef.get
      }
    }
  )
}
