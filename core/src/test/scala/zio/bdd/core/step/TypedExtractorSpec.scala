package zio.bdd.core.step

import zio.test.*
import zio.test.Assertion.*
import zio.schema.{DeriveSchema, Schema}
import zio.bdd.gherkin.{DataTable, DataTableRow}

object TypedExtractorSpec extends ZIOSpecDefault with DefaultTypedExtractor {

  case class User(name: String, age: Int)
  implicit val userSchema: Schema[User] = DeriveSchema.gen[User]

  case class Item(name: String, price: Double)
  implicit val itemSchema: Schema[Item] = DeriveSchema.gen[Item]

  def spec = suite("TypedExtractors")(
    suite("string extractor")(
      test("extract unquoted string") {
        val groups = List("hello")
        val result = string.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo(("hello", 1))))
      },
      test("extract quoted string and remove quotes") {
        val groups = List("\"hello world\"")
        val result = string.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo(("hello world", 1))))
      },
      test("extract empty string") {
        val groups = List("")
        val result = string.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo(("", 1))))
      },
      test("fail on out-of-bounds index") {
        val groups = List()
        val result = string.extract(StepInput("", None), groups, 0)
        assert(result)(isLeft(equalTo("Expected string at group 0")))
      },
      test("trim whitespace from unquoted string") {
        val groups = List("  hello  ")
        val result = string.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo(("hello", 1))))
      }
    ),
    suite("int extractor")(
      test("extract integer") {
        val groups = List("42")
        val result = int.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo((42, 1))))
      },
      test("fail on non-integer") {
        val groups = List("not a number")
        val result = int.extract(StepInput("", None), groups, 0)
        assert(result)(isLeft(equalTo("Expected int at group 0")))
      },
      test("fail on out-of-bounds index") {
        val groups = List()
        val result = int.extract(StepInput("", None), groups, 0)
        assert(result)(isLeft(equalTo("Expected int at group 0")))
      }
    ),
    suite("double extractor")(
      test("extract double") {
        val groups = List("3.14")
        val result = double.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo((3.14, 1))))
      },
      test("fail on non-double") {
        val groups = List("not a double")
        val result = double.extract(StepInput("", None), groups, 0)
        assert(result)(isLeft(equalTo("Expected double at group 0")))
      },
      test("extract negative double") {
        val groups = List("-2.5")
        val result = double.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo((-2.5, 1))))
      }
    ),
    suite("long extractor")(
      test("extract long") {
        val groups = List("1234567890")
        val result = long.extract(StepInput("", None), groups, 0)
        assert(result)(isRight(equalTo((1234567890L, 1))))
      },
      test("fail on non-long") {
        val groups = List("not a long")
        val result = long.extract(StepInput("", None), groups, 0)
        assert(result)(isLeft(equalTo("Expected long at group 0")))
      }
    ),
    suite("table extractor")(
      test("extract list of users") {
        val dataTable = DataTable(
          headers = List("name", "age"),
          rows = List(
            DataTableRow(List("Alice", "30")),
            DataTableRow(List("Bob", "25"))
          )
        )
        val input     = StepInput("", Some(dataTable))
        val extractor = table[User]
        val result    = extractor.extract(input, List(), 0)
        val expected  = List(User("Alice", 30), User("Bob", 25))
        assert(result)(isRight(equalTo((expected, 0))))
      },
      test("extract list of items with double") {
        val dataTable = DataTable(
          headers = List("name", "price"),
          rows = List(DataTableRow(List("Laptop", "999.99")))
        )
        val input     = StepInput("", Some(dataTable))
        val extractor = table[Item]
        val result    = extractor.extract(input, List(), 0)
        val expected  = List(Item("Laptop", 999.99))
        assert(result)(isRight(equalTo((expected, 0))))
      },
      test("fail on missing columns") {
        val dataTable = DataTable(
          headers = List("name"),
          rows = List(DataTableRow(List("Alice")))
        )
        val input     = StepInput("", Some(dataTable))
        val extractor = table[User]
        val result    = extractor.extract(input, List(), 0)
        assert(result)(isLeft(equalTo("Missing field age in row")))
      },
      test("fail on invalid data") {
        val dataTable = DataTable(
          headers = List("name", "age"),
          rows = List(DataTableRow(List("Alice", "not a number")))
        )
        val input     = StepInput("", Some(dataTable))
        val extractor = table[User]
        val result    = extractor.extract(input, List(), 0)
        assert(result)(isLeft(equalTo("Invalid int: not a number")))
      },
      test("fail on empty data table") {
        val dataTable = DataTable(
          headers = List("name", "age"),
          rows = List()
        )
        val input     = StepInput("", Some(dataTable))
        val extractor = table[User]
        val result    = extractor.extract(input, List(), 0)
        assert(result)(isLeft(equalTo("Empty data table")))
      },
      test("fail when no data table is provided") {
        val input     = StepInput("", None)
        val extractor = table[User]
        val result    = extractor.extract(input, List(), 0)
        assert(result)(isLeft(equalTo("Data table expected but not provided")))
      },
      test("fail on non-record schema") {
        implicit val intSchema: Schema[Int] = Schema.primitive[Int]
        val extractor                       = table[Int]
        val dataTable = DataTable(
          headers = List("value"),
          rows = List(DataTableRow(List("42")))
        )
        val input  = StepInput("", Some(dataTable))
        val result = extractor.extract(input, List(), 0)
        assert(result)(isLeft(equalTo("TableExtractor expects a Record schema for T")))
      }
    )
  )
}
