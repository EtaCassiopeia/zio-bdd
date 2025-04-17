package zio.bdd.core.step

import zio.test.*
import zio.test.Assertion.*
import zio.bdd.gherkin.{DataTable, DataTableRow, StepType}
import zio.schema.{DeriveSchema, Schema}
import zio.{RIO, ZIO, ZLayer}

object StepExpressionSpec extends ZIOSpecDefault with DefaultTypedExtractor {

  case class User(name: String, age: Int)
  implicit val userSchema: Schema[User] = DeriveSchema.gen[User]

  def createInput(text: String, table: Option[DataTable] = None): StepInput = StepInput(text, table)

  def createExpression(parts: StepPart*): StepExpression[Tuple] = StepExpression(parts.toList)

  def spec = suite("StepExpression")(
    suite("extract values from step text")(
      test("extract single string parameter") {
        val expr   = createExpression(Literal("hello "), Extractor(string))
        val input  = createInput("hello world")
        val result = expr.extract(input)
        assert(result)(isSome(equalTo(Tuple1("world"))))
      },
      test("extract multiple parameters") {
        val expr = createExpression(
          Literal("user "),
          Extractor(string),
          Literal(" has age "),
          Extractor(int)
        )
        val input  = createInput("user Alice has age 30")
        val result = expr.extract(input)
        assert(result)(isSome(equalTo(("Alice", 30))))
      },
      test("extract with quoted string") {
        val expr   = createExpression(Literal("say "), Extractor(string))
        val input  = createInput("say \"hello world\"")
        val result = expr.extract(input)
        assert(result)(isSome(equalTo(Tuple1("hello world"))))
      },
      test("fail on mismatched text") {
        val expr   = createExpression(Literal("hello "), Extractor(string))
        val input  = createInput("goodbye world")
        val result = expr.extract(input)
        assert(result)(isNone)
      },
      test("extract with table parameter") {
        val dataTable = DataTable(
          headers = List("name", "age"),
          rows = List(DataTableRow(List("Alice", "30")), DataTableRow(List("Bob", "25")))
        )
        val input    = createInput("users are", Some(dataTable))
        val expr     = createExpression(Literal("users are"), Extractor(table[User]))
        val result   = expr.extract(input)
        val expected = List(User("Alice", 30), User("Bob", 25))
        assert(result)(isSome(equalTo(Tuple1(expected))))
      },
      test("fail on missing table") {
        val input  = createInput("users are")
        val expr   = createExpression(Literal("users are"), Extractor(table[User]))
        val result = expr.extract(input)
        assert(result)(isNone)
      }
    ),
    suite("StepDef")(
      test("tryExecute matches and extracts parameters") {
        val stepDef = StepDefImpl[Any, Unit, Tuple1[String]](
          StepType.GivenStep,
          StepExpression(List(Literal("hello "), Extractor(string))),
          { case Tuple1(name) => ZIO.succeed(println(s"Hello, $name!")) }
        )
        val input  = createInput("hello world")
        val result = stepDef.tryExecute(input)
        assert(result)(isSome(anything))
      },
      test("tryExecute fails on mismatch") {
        val stepDef = StepDefImpl[Any, Unit, Tuple1[String]](
          StepType.GivenStep,
          StepExpression(List(Literal("hello "), Extractor(string))),
          { case Tuple1(name) => ZIO.succeed(println(s"Hello, $name!")) }
        )
        val input  = createInput("goodbye world")
        val result = stepDef.tryExecute(input)
        assert(result)(isNone)
      }
    ),
    suite("ZIOSteps integration")(
      test("register and extract steps") {
        val steps = new ZIOSteps[Any, Unit] {
          Given("a step with param " / string) { (param: String) =>
            ZIO.succeed(println(s"Param: $param"))
          }
        }
        val stepDefs = steps.getSteps
        assert(stepDefs)(hasSize(equalTo(1)))
        val stepDef = stepDefs.head.asInstanceOf[StepDefImpl[Any, Unit, Tuple1[String]]]
        val input   = createInput("a step with param hello")
        val result  = stepDef.tryExecute(input)
        assert(result)(isSome(anything))
      }
    )
  )
}
