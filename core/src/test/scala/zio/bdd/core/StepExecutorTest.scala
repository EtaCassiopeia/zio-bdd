package zio.bdd.core

import zio.*
import zio.bdd.core.report.ConsoleReporter
import zio.test.*
import zio.bdd.gherkin.{StepType, Step => GherkinStep}
import izumi.reflect.Tag
import zio.test.Assertion._

object StepExecutorTest extends ZIOSpecDefault {

  def makeExecutor[R](
    steps: ZIOSteps[R] = ZIOSteps.empty[R]
  ): ZIO[Scope, Nothing, StepExecutor[R]] =
    ZIO.scoped {
      for {
        stackRef     <- Ref.make(Chunk.empty[StepRecord])
        logCollector <- LogCollector.live.build
      } yield StepExecutor("test-scenario", steps, stackRef, ConsoleReporter, logCollector.get)
    }

  val testEnv: ZLayer[Any, Nothing, TestEnvironment & LogCollector] =
    ZLayer.make[TestEnvironment & LogCollector](
      testEnvironment,
      LogCollector.live
    )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("StepExecutor utilities")(
    test("convertToRegex handles basic Gherkin placeholders") {
      (for {
        executor <- makeExecutor()
      } yield {
        val patterns = Map(
          "the user adds {int} items" -> s"^the user adds (-?\\d+) items$$".r,
          "price is {float}"          -> s"^price is (-?\\d+\\.\\d+)$$".r,
          "enabled is {boolean}"      -> s"^enabled is (true|false)$$".r,
          "name is {string}"          -> s"^name is (.+)$$".r,
          "value is {double}"         -> s"^value is (-?\\d+\\.\\d+)$$".r,
          "plain text"                -> s"^plain text$$".r
        )
        val results = patterns.map { case (input, expected) =>
          val result = StepUtils.convertToRegex(input)
          result.pattern.pattern == expected.pattern.pattern
        }
        assertTrue(results.forall(identity))
      }): ZIO[TestEnvironment & Scope, Nothing, TestResult]
    },
    test("extractParams extracts parameters from Gherkin placeholders") {
      (for {
        executor <- makeExecutor()
      } yield {
        val testCases = List(
          ("the user adds {int} items", "the user adds 5 items")       -> List(5),
          ("the user adds {value:int} items", "the user adds 5 items") -> List(5),
          ("price is {float}", "price is 12.34")                       -> List(12.34f),
          ("price is {price:float}", "price is 12.34")                 -> List(12.34f),
          ("enabled is {boolean}", "enabled is true")                  -> List(true),
          ("enabled is {enabled:boolean}", "enabled is true")          -> List(true),
          ("name is {string}", "name is John Doe")                     -> List("John Doe"),
          ("name is {name:string}", "name is John Doe")                -> List("John Doe"),
          ("value is {double}", "value is 1.23")                       -> List(1.23d),
          ("value is {value:double}", "value is 1.23")                 -> List(1.23d),
          ("plain text", "plain text")                                 -> List.empty[Any]
        )
        val results = testCases.map { case ((patternString, line), expected) =>
          val pattern     = StepUtils.convertToRegex(patternString)
          val (values, _) = StepUtils.extractParams(pattern, line, patternString)
          values == expected
        }
        assertTrue(results.forall(identity))
      }): ZIO[TestEnvironment & Scope, Nothing, TestResult]
    },
    test("StepExecutor executes step with alternation syntax 'the account (is|is not) active'") {
      val steps = new ZIOSteps.Default[Any] {
        Given("^the account (is|is not) active$") { (status: String) =>
          ZIO.succeed(s"Account status: $status")
        }
      }
      for {
        executor <- makeExecutor(steps)
        step      = GherkinStep(StepType.GivenStep, "the account is active", Some("test.feature"), Some(1))
        result   <- executor.executeStep("test-feature", "test-scenario", step)
      } yield assertTrue(
        result.succeeded,
        result.output == "Account status: is"
      )
    },
    test("convertToRegex and extractParams handle alternation syntax and regex placeholders") {
      (for {
        executor <- makeExecutor()
      } yield {
        val testCases = List(
          ("^the account (is|is not) active$", "the account is active", List("is")),
          ("^the account (is|is not) active$", "the account is not active", List("is not")),
          ("^user (\\w+) logged in$", "user alice logged in", List("alice")),
          ("^added (\\d+) items$", "added 42 items", List("42")),
          ("^price is (\\d+\\.\\d+)$", "price is 12.34", List("12.34")),
          ("^enabled is (true|false)$", "enabled is true", List("true")),
          ("^name is (.+)$", "name is John Doe", List("John Doe")),
          ("^value is (-?\\d+\\.\\d+)$", "value is 1.23", List("1.23")),
          ("^plain text$", "plain text", List.empty[Any]),
          ("^the user (\\w+) (is|is not) active$", "the user alice is active", List("alice", "is"))
        )
        val results = testCases.zipWithIndex.map { case ((patternString, line, expected), testIdx) =>
          val regex       = StepUtils.convertToRegex(patternString)
          val (values, _) = StepUtils.extractParams(regex, line, patternString)
          val isEqual     = values.length == expected.length && values.zip(expected).forall(_ == _)
          println(
            s"Test $testIdx: Pattern: $patternString, Line: $line, Regex: $regex, Expected: $expected (${expected
                .map(_.getClass.getSimpleName)}), Result: $values (${values.map(_.getClass.getSimpleName)}), Equal: $isEqual"
          )
          isEqual
        }
        assertTrue(results.forall(identity))
      }): ZIO[TestEnvironment & Scope, Nothing, TestResult]
    },
    test("extractParams parses named placeholders with correct types") {
      (for {
        executor <- makeExecutor()
      } yield {
        val patternString =
          "a product {productId:String} exists with name {name:String} price {price:Float} and stock {stock:Int}"
        val line           = "a product P1 exists with name Book price 15.99 and stock 10"
        val regex          = StepUtils.convertToRegex(patternString)
        val (values, tags) = StepUtils.extractParams(regex, line, patternString)

        val expectedValues = List("P1", "Book", 15.99f, 10)
        val expectedTypes  = List("String", "String", "Float", "Integer") // Integer for Int at runtime

        println(
          s"Pattern: $patternString, Line: $line, Result: $values, Types: ${values.map(_.getClass.getSimpleName)}"
        )

        assertTrue(
          values == expectedValues,
          values.map(_.getClass.getSimpleName) == expectedTypes
        )
      }): ZIO[TestEnvironment & Scope, Nothing, TestResult]
    },
    test("convertToRegex and extractParams handle negative numbers") {
      (for {
        executor <- makeExecutor()
      } yield {
        val testCases = List(
          ("value is {int}", "value is -10", List(-10)),
          ("price is {float}", "price is -12.34", List(-12.34f)),
          ("value is {double}", "value is -5.67", List(-5.67d))
        )
        val results = testCases.map { case (patternString, line, expected) =>
          val regex       = StepUtils.convertToRegex(patternString)
          val (values, _) = StepUtils.extractParams(regex, line, patternString)
          values == expected && values.head.getClass.getSimpleName == expected.head.getClass.getSimpleName
        }
        assertTrue(results.forall(identity))
      }): ZIO[TestEnvironment & Scope, Nothing, TestResult]
    }
  ).provideSome[Scope](testEnv)
}
