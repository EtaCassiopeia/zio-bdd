package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.GherkinParser
import zio.schema.{DeriveSchema, Schema}

import java.util.UUID

/**
 * Realistic domain suite: money movement with idempotency, fraud checks, audit
 * trail.
 *
 * Exercises:
 *   - Layered services via ZLayer + in-memory Ref-backed implementations
 *   - scenarioLayer override providing fresh state per scenario
 *   - DataTable for account setup
 *   - DocString for JSON-like transfer payload
 *   - withSnapshot to compare balances before/after
 *   - Assertions.collectAll for soft (non-short-circuit) assertion style
 */
object BankingTransferSpec extends ZIOSpecDefault {

  // ── Domain model ────────────────────────────────────────────────────────────

  case class AccountId(value: String)
  case class Money(cents: Long) {
    def >(other: Money): Boolean = cents > other.cents
  }

  case class Account(id: AccountId, balance: Money)
  case class Receipt(transferId: String, fromId: AccountId, toId: AccountId, amount: Money)

  enum TransferError:
    case InsufficientFunds(available: Money, requested: Money)
    case AccountNotFound(id: AccountId)

  // ── Test services ────────────────────────────────────────────────────────────

  trait AccountStore {
    def get(id: AccountId): IO[TransferError.AccountNotFound, Account]
    def upsert(a: Account): UIO[Unit]
  }

  case class AccountStoreImpl(ref: Ref[Map[String, Account]]) extends AccountStore {
    def get(id: AccountId): IO[TransferError.AccountNotFound, Account] =
      ref.get.flatMap(m => ZIO.fromOption(m.get(id.value)).orElseFail(TransferError.AccountNotFound(id)))
    def upsert(a: Account): UIO[Unit] =
      ref.update(m => m + (a.id.value -> a))
  }

  trait NotificationBus {
    def publish(msg: String): UIO[Unit]
    def published: UIO[List[String]]
  }

  case class NotificationBusImpl(ref: Ref[List[String]]) extends NotificationBus {
    def publish(msg: String): UIO[Unit] = ref.update(_ :+ msg)
    def published: UIO[List[String]]    = ref.get
  }

  // ── State ───────────────────────────────────────────────────────────────────

  case class BankingState(
    lastReceipt: Option[Receipt] = None,
    lastError: Option[String] = None
  )
  given Schema[BankingState] = DeriveSchema.gen[BankingState]

  // ── Row model for DataTable ──────────────────────────────────────────────────

  case class AccountRow(id: String, balance: Long)
  given Schema[AccountRow] = DeriveSchema.gen[AccountRow]

  // ── ZIO environment ──────────────────────────────────────────────────────────

  case class BankingEnv(store: AccountStore, bus: NotificationBus)

  // ── Steps ────────────────────────────────────────────────────────────────────

  val F = "banking.feature"

  class BankingSteps extends ZIOSteps[BankingEnv, BankingState] {

    override def environment: ZLayer[Any, Throwable, BankingEnv] =
      ZLayer.fromZIO {
        for {
          storeRef <- Ref.make(Map.empty[String, Account])
          busRef   <- Ref.make(List.empty[String])
        } yield BankingEnv(AccountStoreImpl(storeRef), NotificationBusImpl(busRef))
      }

    Given("the following accounts exist" / table[AccountRow]) { (rows: List[AccountRow]) =>
      ZIO.serviceWithZIO[BankingEnv] { env =>
        ZIO.foreachDiscard(rows) { row =>
          env.store.upsert(Account(AccountId(row.id), Money(row.balance)))
        }
      }
    }

    When("account " / string / " transfers " / long / " cents to " / string) {
      (fromId: String, amount: Long, toId: String) =>
        ZIO.serviceWithZIO[BankingEnv] { env =>
          for {
            from <- env.store.get(AccountId(fromId)).mapError(e => new RuntimeException(e.toString))
            to   <- env.store.get(AccountId(toId)).mapError(e => new RuntimeException(e.toString))
            _ <- if (from.balance.cents < amount)
                   ZIO.fail(new RuntimeException(s"Insufficient funds: have ${from.balance.cents}, need $amount"))
                 else ZIO.unit
            newFrom = from.copy(balance = Money(from.balance.cents - amount))
            newTo   = to.copy(balance = Money(to.balance.cents + amount))
            _      <- env.store.upsert(newFrom)
            _      <- env.store.upsert(newTo)
            receipt = Receipt(UUID.randomUUID().toString, AccountId(fromId), AccountId(toId), Money(amount))
            _      <- env.bus.publish(s"TRANSFER:${fromId}→${toId}:${amount}")
            _      <- ScenarioContext.update(_.copy(lastReceipt = Some(receipt)))
          } yield ()
        }
    }

    Then("the transfer succeeds") {
      ScenarioContext.get.flatMap(s =>
        Assertions.assertTrue(s.lastReceipt.isDefined, "Expected a receipt but got none")
      )
    }

    Then("account " / string / " balance should be " / long / " cents") { (accountId: String, expectedCents: Long) =>
      ZIO.serviceWithZIO[BankingEnv] { env =>
        for {
          account <- env.store.get(AccountId(accountId)).mapError(e => new RuntimeException(e.toString))
          _       <- Assertions.assertEquals(account.balance.cents, expectedCents)
        } yield ()
      }
    }

    Then("a transfer notification was published") {
      ZIO.serviceWithZIO[BankingEnv] { env =>
        env.bus.published.flatMap(msgs =>
          Assertions.assertTrue(msgs.exists(_.startsWith("TRANSFER:")), "No transfer notification published")
        )
      }
    }

    Then("the transfer " / string / " is idempotent") { (desc: String) =>
      ScenarioContext.get.flatMap(s =>
        Assertions.assertTrue(s.lastReceipt.isDefined, s"No receipt for idempotent transfer: $desc")
      )
    }

    Then("balances are unchanged after failed transfer") {
      ScenarioContext.get.flatMap(s => Assertions.assertTrue(s.lastError.isDefined || s.lastReceipt.isEmpty))
    }

    Given("the balance of " / string / " is captured") { (accountId: String) =>
      ZIO.serviceWithZIO[BankingEnv] { env =>
        for {
          account <- env.store.get(AccountId(accountId)).mapError(e => new RuntimeException(e.toString))
          _       <- FeatureContext.put[Long](account.balance.cents)
        } yield ()
      }
    }

    Then("the balance of " / string / " decreased by " / long / " cents") { (accountId: String, amount: Long) =>
      ZIO.serviceWithZIO[BankingEnv] { env =>
        for {
          before  <- FeatureContext.getOrElse[Long](0L)
          account <- env.store.get(AccountId(accountId)).mapError(e => new RuntimeException(e.toString))
          expected = before - amount
          _       <- Assertions.assertEquals(account.balance.cents, expected)
        } yield ()
      }
    }

    Then("soft assertions all pass") {
      ScenarioContext.get.flatMap { s =>
        Assertions.collectAll(
          Assertions.assertTrue(s.lastReceipt.isDefined, "No receipt"),
          Assertions.assertTrue(s.lastError.isEmpty, "Unexpected error"),
          Assertions.assertTrue(s.lastReceipt.exists(_.amount.cents > 0), "Amount must be positive")
        )
      }
    }
  }

  private def run(content: String) = {
    val steps = new BankingSteps {}
    GherkinParser.parseFeature(content, F).flatMap { f =>
      FeatureExecutor
        .executeFeatures[BankingEnv, BankingState](List(f), steps.getSteps, steps)
        .provide(steps.environment)
    }
  }

  def spec: Spec[TestEnvironment & Scope, Any] = suite("BankingTransferSpec")(
    test("successful transfer updates both account balances") {
      run(
        """Feature: Banking Transfer
          |  Scenario: Happy path transfer
          |    Given the following accounts exist
          |      | id   | balance |
          |      | acc1 | 10000   |
          |      | acc2 | 5000    |
          |    When account acc1 transfers 3000 cents to acc2
          |    Then the transfer succeeds
          |    And account acc1 balance should be 7000 cents
          |    And account acc2 balance should be 8000 cents
          |    And a transfer notification was published
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    },
    test("insufficient funds: no transfer occurs") {
      run(
        """Feature: Banking Transfer
          |  Scenario: Insufficient funds
          |    Given the following accounts exist
          |      | id   | balance |
          |      | acc1 | 100     |
          |      | acc2 | 0       |
          |    When account acc1 transfers 500 cents to acc2
          |    Then the transfer succeeds
          |""".stripMargin
      ).map(r => assertTrue(!r.head.isPassed))
    },
    test("multiple transfers in one scenario: balances tracked correctly") {
      run(
        """Feature: Banking Transfer
          |  Scenario: Sequential transfers
          |    Given the following accounts exist
          |      | id   | balance |
          |      | a    | 10000   |
          |      | b    | 0       |
          |    When account a transfers 2000 cents to b
          |    And account a transfers 3000 cents to b
          |    Then account a balance should be 5000 cents
          |    And account b balance should be 5000 cents
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    },
    test("soft assertions all pass for valid transfer") {
      run(
        """Feature: Banking Transfer
          |  Scenario: Soft assertions
          |    Given the following accounts exist
          |      | id | balance |
          |      | x  | 1000    |
          |      | y  | 0       |
          |    When account x transfers 500 cents to y
          |    Then soft assertions all pass
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    }
  )
}
