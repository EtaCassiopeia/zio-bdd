package zio.bdd.core

import zio.*
import zio.bdd.core.report.{ConsoleReporter, Reporter}
import zio.bdd.gherkin.*
import zio.test.*

object ScenarioRunnerTest extends ZIOSpecDefault {
  val testEnv
    : ZLayer[Any, Nothing, ProductCatalog & ShoppingCart & OrderService & PaymentGateway & LogCollector & Reporter] =
    TestProductCatalog.layer ++
      TestShoppingCart.layer ++
      (TestProductCatalog.layer >>> TestOrderService.layer) ++
      TestPaymentGateway.layer ++
      LogCollector.live ++
      ZLayer.succeed(ConsoleReporter)

  def spec: Spec[TestEnvironment & Scope, Any] = suite("ScenarioRunner")(
    test("run valid scenario with background") {
      val content = """
                      |Feature: Shopping Cart Management
                      |  Background:
                      |    Given a product "P1" exists with name "Book" price 15.99 and stock 10
                      |    Given an empty shopping cart exists
                      |  Scenario: Adding items and placing order
                      |    When the user adds 2 of product "P1" to the cart
                      |    When the user places the order
                      |    And the payment is processed
                      |    Then the order total should be 31.98
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 6,
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 15.99 and stock 10""",
        results.head(0).output == (),
        results.head(1).step == "an empty shopping cart exists",
        results.head(1).output.isInstanceOf[Cart],
        results.head(2).step == """the user adds 2 of product "P1" to the cart""",
        results.head(2).output.isInstanceOf[Cart],
        results.head(3).step == "the user places the order",
        results.head(3).output.isInstanceOf[Order],
        results.head(4).step == "the payment is processed",
        results.head(4).output.isInstanceOf[Payment],
        results.head(5).step == "the order total should be 31.98",
        results.head(5).output == ()
      )
    },
    test("run scenario outline with examples") {
      val content = """
                      |Feature: Product Catalog
                      |  Scenario Outline: Adding multiple products
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds {quantity:Int} of product "P1" to the cart
                      |    Then the order total should be {total:Float}
                      |  Examples:
                      |    | quantity | total |
                      |    | 2        | 20.00 |
                      |    | 3        | 30.00 |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results.forall(_.length == 4),
        results.forall(_.forall(_.succeeded)),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 2 of product "P1" to the cart""",
        results.head(3).step == "the order total should be 20.00",
        results(1)(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results(1)(1).step == "an empty shopping cart exists",
        results(1)(2).step == """the user adds 3 of product "P1" to the cart""",
        results(1)(3).step == "the order total should be 30.00"
      )
    },
    test("run scenario with retry on failure") {
      val content = """
                      |Feature: Retry Test
                      |  @retry(3)
                      |  Scenario: Retry on failure
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 10 of product "P1" to the cart
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head(0).succeeded,
        results.head(1).succeeded,
        !results.head(2).succeeded,
        results.head(2).error.map(_.getMessage).contains("Insufficient stock for P1"),
        feature.scenarios.head.metadata.retryCount == 3
      )
    },
    test("run scenario with repeat") {
      val content = """
                      |Feature: Repeat Test
                      |  @repeat(2)
                      |  Scenario: Repeat execution
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 6, // 3 steps x 2 repeats
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 1 of product "P1" to the cart""",
        results.head(3).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(4).step == "an empty shopping cart exists",
        results.head(5).step == """the user adds 1 of product "P1" to the cart""",
        feature.scenarios.head.metadata.repeatCount == 2
      )
    },
    test("run scenario with ignore tag") {
      val content = """
                      |Feature: Ignore Test
                      |  @ignore
                      |  Scenario: Ignored scenario
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                  """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        feature.scenarios.length == 1,
        feature.scenarios.head.metadata.isIgnored,
        feature.scenarios.head.name == "Ignored scenario",
        feature.scenarios.head.steps.length == 3,
        results.length == 1,
        results.head.isEmpty
      )
    },
    test("fail on unmatched step") {
      val content = """
                      |Feature: Unmatched Step Test
                      |  Scenario: Unmatched step
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    When an undefined step runs
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 2,
        results.head(0).succeeded,
        !results.head(1).succeeded,
        results.head(1).error.map(_.getMessage).contains("No step definition matches"),
        results.head(1).step == "an undefined step runs"
      )
    },
    test("fail on invalid input") {
      val content = """
                      |Feature: Invalid Input Test
                      |  Scenario: Invalid input type
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds -1 of product "P1" to the cart
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head(0).succeeded,
        results.head(1).succeeded,
        !results.head(2).succeeded,
        results.head(2).error.map(_.getMessage).contains("Insufficient stock for P1"),
        results.head(2).step == """the user adds -1 of product "P1" to the cart"""
      )
    },
    test("run empty scenario") {
      val content = """
                      |Feature: Empty Test
                      |  Scenario: Empty scenario
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.isEmpty
      )
    },
    test("run scenario with flaky tag") {
      val content = """
                      |Feature: Flaky Test
                      |  @flaky
                      |  Scenario: Flaky scenario
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 1 of product "P1" to the cart""",
        feature.scenarios.head.metadata.isFlaky
      )
    },
    test("run multiple scenarios") {
      val content = """
                      |Feature: Multi Scenario Test
                      |  Background:
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |  Scenario: Simple order
                      |    When the user adds 2 of product "P1" to the cart
                      |    Then the order total should be 20.00
                      |  Scenario: Complex order
                      |    When the user adds 1 of product "P1" to the cart
                      |    When the user places the order
                      |    And the payment is processed
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results.head.length == 4, // Simple order: 2 background + 2 scenario steps
        results(1).length == 5,   // Complex order: 2 background + 3 scenario steps
        results.forall(_.forall(_.succeeded)),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 2 of product "P1" to the cart""",
        results.head(3).step == "the order total should be 20.00",
        results(1)(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results(1)(1).step == "an empty shopping cart exists",
        results(1)(2).step == """the user adds 1 of product "P1" to the cart""",
        results(1)(3).step == "the user places the order",
        results(1)(4).step == "the payment is processed"
      )
    },
    test("run simple given-when-then without placeholders") {
      val content = """
                      |Feature: Simple Test
                      |  Scenario: Basic cart action
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 3,
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 1 of product "P1" to the cart"""
      )
    },
    test("run scenario with given-and-when-and-then") {
      val content = """
                      |Feature: Given-And-When-And-Then Test
                      |  Scenario: Full cart flow
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                      |    And the user places the order
                      |    And the payment is processed
                      |    Then the order total should be 10.00
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 6,
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 1 of product "P1" to the cart""",
        results.head(3).step == "the user places the order",
        results.head(4).step == "the payment is processed",
        results.head(5).step == "the order total should be 10.00"
      )
    },
    test("run scenario with background and multiple examples") {
      val content = """
                      |Feature: Background Multi Example Test
                      |  Background:
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |  Scenario Outline: Multi cart actions
                      |    When the user adds {quantity:Int} of product "P1" to the cart
                      |    Then the order total should be {total:Float}
                      |  Examples:
                      |    | quantity | total |
                      |    | 2        | 20.00 |
                      |    | 3        | 30.00 |
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 2)
      } yield assertTrue(
        results.length == 2,
        results.forall(_.length == 4),
        results.forall(_.forall(_.succeeded)),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 2 of product "P1" to the cart""",
        results.head(3).step == "the order total should be 20.00",
        results(1)(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results(1)(1).step == "an empty shopping cart exists",
        results(1)(2).step == """the user adds 3 of product "P1" to the cart""",
        results(1)(3).step == "the order total should be 30.00"
      )
    },
    test("run complex scenario") {
      val content = """
                      |Feature: Complex Test
                      |  Background:
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    And an empty shopping cart exists
                      |  @retry(2) @flaky @repeat(3)
                      |  Scenario Outline: Full feature test
                      |    When the user adds {quantity:Int} of product "P1" to the cart
                      |    And the user places the order
                      |    Then the order total should be {total:Float}
                      |  Examples:
                      |    | quantity | total |
                      |    | 2        | 20.00 |
                    """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 15, // 5 steps x 3 repeats
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 2 of product "P1" to the cart""",
        results.head(3).step == "the user places the order",
        results.head(4).step == "the order total should be 20.00",
        results.head(5).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",  // Repeat 2
        results.head(9).step == "the order total should be 20.00",                                     // Repeat 2 end
        results.head(10).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""", // Repeat 3
        results.head(14).step == "the order total should be 20.00",                                    // Repeat 3 end
        feature.scenarios.head.metadata.retryCount == 2,
        feature.scenarios.head.metadata.isFlaky,
        feature.scenarios.head.metadata.repeatCount == 3
      )
    },
    test("run scenario with ScenarioContext production and consumption") {
      val content = """
                      |Feature: Context Transition Test
                      |  Scenario: Cart to Context Transition
                      |    Given a product "P1" exists with name "Book" price 10.00 and stock 5
                      |    Given an empty shopping cart exists
                      |    When the user adds 1 of product "P1" to the cart
                      |    And the current cart is set in the context
                      |    And the cart contains 2 of product "P1"
                      |    And the cart is retrieved from the context
                      |    When the user places the order
                      |    Then the order total should be 30.00
                """.stripMargin
      for {
        feature <- GherkinParser.parseFeature(content)
        results <- ScenarioRunner.runScenarios(ShoppingCartSteps, feature, 1)
      } yield assertTrue(
        results.length == 1,
        results.head.length == 8,
        results.head.forall(_.succeeded),
        results.head(0).step == """a product "P1" exists with name "Book" price 10.00 and stock 5""",
        results.head(1).step == "an empty shopping cart exists",
        results.head(2).step == """the user adds 1 of product "P1" to the cart""",
        results.head(3).step == "the current cart is set in the context",
        results.head(3).output.isInstanceOf[ScenarioContext],
        results.head(4).step == """the cart contains 2 of product "P1"""",
        results.head(4).output.isInstanceOf[ScenarioContext],
        results.head(5).step == "the cart is retrieved from the context",
        results.head(6).step == "the user places the order",
        results.head(7).step == "the order total should be 30.00"
      )
    }
  ).provideLayer(testEnv)
}
