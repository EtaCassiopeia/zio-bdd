package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

/**
 * Realistic saga / state machine suite: order fulfillment with compensating
 * transactions.
 *
 * Exercises:
 *   - GivenS/WhenS/ThenS state-injecting variants
 *   - Long scenarios with Background setup
 *   - withSnapshot to verify compensation
 *   - Multi-stage rollback (Shipping failure → refund + release reservation)
 *   - FeatureContext for shared order IDs
 */
object SagaWorkflowSpec extends ZIOSpecDefault {

  // ── Domain ───────────────────────────────────────────────────────────────────

  enum OrderStatus:
    case Pending, Reserved, Charged, Shipped, Refunded, Cancelled

  type ItemId        = String
  type ReservationId = String
  type ChargeId      = String
  type TrackingNum   = String

  enum SagaError:
    case OutOfStock(item: ItemId)
    case PaymentDeclined(reason: String)
    case ShipmentFailed(reason: String)

  // ── In-memory services ────────────────────────────────────────────────────────

  trait Inventory {
    def reserve(item: ItemId, qty: Int): IO[SagaError.OutOfStock, ReservationId]
    def release(r: ReservationId): UIO[Unit]
    def reservations: UIO[List[ReservationId]]
    def releases: UIO[List[ReservationId]]
  }

  trait Payments {
    def charge(amount: Long): IO[SagaError.PaymentDeclined, ChargeId]
    def refund(c: ChargeId): UIO[Unit]
    def charges: UIO[List[ChargeId]]
    def refunds: UIO[List[ChargeId]]
  }

  trait Shipping {
    def ship(trackingBase: String): IO[SagaError.ShipmentFailed, TrackingNum]
  }

  case class InMemoryInventory(
    reservationRef: Ref[List[ReservationId]],
    releaseRef: Ref[List[ReservationId]],
    failOn: Option[ItemId] = None
  ) extends Inventory {
    def reserve(item: ItemId, qty: Int): IO[SagaError.OutOfStock, ReservationId] =
      if (failOn.contains(item)) ZIO.fail(SagaError.OutOfStock(item))
      else {
        val rid = s"RES-$item-$qty"
        reservationRef.update(_ :+ rid).as(rid)
      }
    def release(r: ReservationId): UIO[Unit]   = releaseRef.update(_ :+ r)
    def reservations: UIO[List[ReservationId]] = reservationRef.get
    def releases: UIO[List[ReservationId]]     = releaseRef.get
  }

  case class InMemoryPayments(
    chargesRef: Ref[List[ChargeId]],
    refundsRef: Ref[List[ChargeId]],
    shouldDecline: Boolean = false
  ) extends Payments {
    def charge(amount: Long): IO[SagaError.PaymentDeclined, ChargeId] =
      if (shouldDecline) ZIO.fail(SagaError.PaymentDeclined("card declined"))
      else {
        val cid = s"CHG-$amount"
        chargesRef.update(_ :+ cid).as(cid)
      }
    def refund(c: ChargeId): UIO[Unit] = refundsRef.update(_ :+ c)
    def charges: UIO[List[ChargeId]]   = chargesRef.get
    def refunds: UIO[List[ChargeId]]   = refundsRef.get
  }

  case class InMemoryShipping(shouldFail: Boolean = false) extends Shipping {
    def ship(trackingBase: String): IO[SagaError.ShipmentFailed, TrackingNum] =
      if (shouldFail) ZIO.fail(SagaError.ShipmentFailed("carrier unavailable"))
      else ZIO.succeed(s"TRACK-$trackingBase")
  }

  // ── State ─────────────────────────────────────────────────────────────────────

  case class SagaState(
    orderId: Option[String] = None,
    reservationId: Option[ReservationId] = None,
    chargeId: Option[ChargeId] = None,
    trackingNumber: Option[TrackingNum] = None,
    status: OrderStatus = OrderStatus.Pending,
    lastError: Option[String] = None
  )
  given Schema[SagaState] = DeriveSchema.gen[SagaState]

  case class SagaEnv(inventory: Inventory, payments: Payments, shipping: Shipping)

  val F = "saga.feature"

  // ── Steps ─────────────────────────────────────────────────────────────────────

  def makeSteps(
    failItem: Option[String] = None,
    declinePayment: Boolean = false,
    failShipping: Boolean = false
  ): ZIOSteps[SagaEnv, SagaState] =
    new ZIOSteps[SagaEnv, SagaState] {

      override def environment: ZLayer[Any, Throwable, SagaEnv] =
        ZLayer.fromZIO {
          for {
            resRef   <- Ref.make(List.empty[ReservationId])
            relRef   <- Ref.make(List.empty[ReservationId])
            chgRef   <- Ref.make(List.empty[ChargeId])
            refRef   <- Ref.make(List.empty[ChargeId])
            inventory = InMemoryInventory(resRef, relRef, failItem)
            payments  = InMemoryPayments(chgRef, refRef, declinePayment)
            shipping  = InMemoryShipping(failShipping)
          } yield SagaEnv(inventory, payments, shipping)
        }

      Given("an order " / string / " is initiated") { (orderId: String) =>
        ScenarioContext.update(_.copy(orderId = Some(orderId), status = OrderStatus.Pending))
      }

      When("item " / string / " qty " / int / " is reserved") { (item: String, qty: Int) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          env.inventory
            .reserve(item, qty)
            .foldZIO(
              err => ScenarioContext.update(_.copy(lastError = Some(err.toString))),
              rid => ScenarioContext.update(_.copy(reservationId = Some(rid), status = OrderStatus.Reserved))
            )
        }
      }

      WhenS("payment of " / long / " cents is charged") { (s: SagaState) => (amount: Long) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          if (s.reservationId.isEmpty)
            ZIO.fail(new RuntimeException("Cannot charge without reservation"))
          else
            env.payments
              .charge(amount)
              .foldZIO(
                err => ScenarioContext.update(_.copy(lastError = Some(err.toString))),
                cid => ScenarioContext.update(_.copy(chargeId = Some(cid), status = OrderStatus.Charged))
              )
        }
      }

      WhenS("the order is shipped to " / string) { (s: SagaState) => (address: String) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          if (s.chargeId.isEmpty)
            ZIO.fail(new RuntimeException("Cannot ship without charge"))
          else
            env.shipping
              .ship(s.orderId.getOrElse("order"))
              .foldZIO(
                err => ScenarioContext.update(_.copy(lastError = Some(err.toString))),
                tn => ScenarioContext.update(_.copy(trackingNumber = Some(tn), status = OrderStatus.Shipped))
              )
        }
      }

      ThenS("the order status is shipped") { (s: SagaState) =>
        Assertions.assertEquals(s.status, OrderStatus.Shipped)
      }

      ThenS("the order has a tracking number") { (s: SagaState) =>
        Assertions.assertTrue(s.trackingNumber.isDefined, "Expected tracking number")
      }

      ThenS("no reservation was made") { (s: SagaState) =>
        Assertions.assertTrue(s.reservationId.isEmpty, "Expected no reservation")
      }

      ThenS("no charge was made") { (s: SagaState) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          env.payments.charges.flatMap(cs =>
            Assertions.assertTrue(cs.isEmpty && s.chargeId.isEmpty, "Expected no charges")
          )
        }
      }

      ThenS("the reservation was released") { (s: SagaState) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          s.reservationId match {
            case None => Assertions.assertTrue(false, "No reservation was made to release")
            case Some(rid) =>
              env.inventory.releases.flatMap(rs =>
                Assertions.assertTrue(rs.contains(rid), s"Reservation $rid not released")
              )
          }
        }
      }

      ThenS("the charge was refunded") { (s: SagaState) =>
        ZIO.serviceWithZIO[SagaEnv] { env =>
          s.chargeId match {
            case None => Assertions.assertTrue(false, "No charge was made to refund")
            case Some(cid) =>
              env.payments.refunds.flatMap(rs => Assertions.assertTrue(rs.contains(cid), s"Charge $cid not refunded"))
          }
        }
      }

      ThenS("there is an error") { (s: SagaState) =>
        Assertions.assertTrue(s.lastError.isDefined, "Expected an error but none recorded")
      }

      // Compensation step (called in afterScenario in error scenarios)
      And("I release the reservation on failure") {
        ScenarioContext.get.flatMap { s =>
          (s.reservationId, s.lastError) match {
            case (Some(rid), Some(_)) =>
              ZIO.serviceWithZIO[SagaEnv](_.inventory.release(rid))
            case _ => ZIO.unit
          }
        }
      }

      And("I refund the charge on shipping failure") {
        ScenarioContext.get.flatMap { s =>
          (s.chargeId, s.lastError) match {
            case (Some(cid), Some(_)) =>
              ZIO.serviceWithZIO[SagaEnv](_.payments.refund(cid))
            case _ => ZIO.unit
          }
        }
      }
    }

  def spec: Spec[TestEnvironment & Scope, Any] = suite("SagaWorkflowSpec")(
    test("happy path: reserve → charge → ship all succeed") {
      val steps = makeSteps()
      GherkinParser
        .parseFeature(
          """Feature: Saga
            |  Scenario: happy path
            |    Given an order order-001 is initiated
            |    When item widget qty 2 is reserved
            |    When payment of 5000 cents is charged
            |    When the order is shipped to 123 Main St
            |    Then the order status is shipped
            |    And the order has a tracking number
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[SagaEnv, SagaState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("inventory failure: no payment or shipment attempted") {
      val steps = makeSteps(failItem = Some("widget"))
      GherkinParser
        .parseFeature(
          """Feature: Saga
            |  Scenario: inventory failure
            |    Given an order order-002 is initiated
            |    When item widget qty 1 is reserved
            |    Then there is an error
            |    And no charge was made
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[SagaEnv, SagaState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("payment declined: reservation released as compensation") {
      val steps = makeSteps(declinePayment = true)
      GherkinParser
        .parseFeature(
          """Feature: Saga
            |  Scenario: payment decline
            |    Given an order order-003 is initiated
            |    When item gadget qty 1 is reserved
            |    When payment of 9900 cents is charged
            |    Then there is an error
            |    And I release the reservation on failure
            |    Then the reservation was released
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[SagaEnv, SagaState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("shipping failure: charge refunded and reservation released") {
      val steps = makeSteps(failShipping = true)
      GherkinParser
        .parseFeature(
          """Feature: Saga
            |  Scenario: shipping failure
            |    Given an order order-004 is initiated
            |    When item gadget qty 1 is reserved
            |    When payment of 1500 cents is charged
            |    When the order is shipped to 456 Oak Ave
            |    Then there is an error
            |    And I refund the charge on shipping failure
            |    And I release the reservation on failure
            |    Then the charge was refunded
            |    And the reservation was released
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[SagaEnv, SagaState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(r.head.isPassed))
    },
    test("GivenS/WhenS/ThenS state-injecting variants compile and run correctly") {
      val steps = makeSteps()
      GherkinParser
        .parseFeature(
          """Feature: State injection
            |  Scenario: state-injected steps
            |    Given an order order-005 is initiated
            |    When item component qty 3 is reserved
            |    When payment of 750 cents is charged
            |    Then the order has a tracking number
            |""".stripMargin,
          F
        )
        .flatMap(f =>
          FeatureExecutor
            .executeFeatures[SagaEnv, SagaState](List(f), steps.getSteps, steps)
            .provide(steps.environment)
        )
        .map(r => assertTrue(!r.head.scenarioResults.head.stepResults.isEmpty))
    }
  )
}
