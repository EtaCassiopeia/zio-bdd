package zio.bdd.core

import zio.test._
import zio.test.Assertion._
import zio.Chunk
import zio.bdd.gherkin.{DataTable, DataTableRow}

object DataTableMapperSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)

  override def spec: Spec[TestEnvironment, Any] =
    suite("DataTableMapperSpec")(
      suite("mapping flat case classes")(
        test("should correctly map a DataTable to a Chunk of Person instances") {
          val dt = DataTable(
            headers = List("name", "age"),
            rows = List(
              DataTableRow(List("Alice", "25")),
              DataTableRow(List("Bob", "30"))
            )
          )
          val mapper = summon[DataTableMapper[Person]]
          val result = mapper.map(dt)

          assertTrue(result == Right(Chunk(Person("Alice", 25), Person("Bob", 30))))
        },
        test("should fail when a header is missing") {
          val dt = DataTable(
            headers = List("name"), // Missing "age"
            rows = List(
              DataTableRow(List("Alice", "25")),
              DataTableRow(List("Bob", "30"))
            )
          )
          val mapper = summon[DataTableMapper[Person]]
          val result = mapper.map(dt)

          assert(result)(
            isLeft(equalTo("Header 'age' not found in DataTable"))
          )
        },
        test("should fail when a cell value is invalid") {
          val dt = DataTable(
            headers = List("name", "age"),
            rows = List(
              DataTableRow(List("Alice", "25")),
              DataTableRow(List("Bob", "invalid")) // Invalid Int
            )
          )
          val mapper = summon[DataTableMapper[Person]]
          val result = mapper.map(dt)

          assert(result)(
            isLeft(equalTo("Invalid Int: invalid"))
          )
        },
        test("should fail when row has fewer cells than headers") {
          val dt = DataTable(
            headers = List("name", "age"),
            rows = List(
              DataTableRow(List("Alice", "25")),
              DataTableRow(List("Bob")) // Missing age
            )
          )
          val mapper = summon[DataTableMapper[Person]]
          val result = mapper.map(dt)

          assert(result)(
            isLeft(equalTo("Index 1 out of bounds for row"))
          )
        },
        test("should handle empty DataTable with no rows") {
          val dt = DataTable(
            headers = List("name", "age"),
            rows = List()
          )
          val mapper = summon[DataTableMapper[Person]]
          val result = mapper.map(dt)

          assertTrue(result == Right(Chunk.empty[Person]))
        }
      ),
      suite("additional flat case class scenarios")(
        test("should map a case class with multiple fields of different types") {
          case class Employee(id: Int, name: String, salary: Double, active: Boolean)
          val dt = DataTable(
            headers = List("id", "name", "salary", "active"),
            rows = List(
              DataTableRow(List("1", "Alice", "50000.0", "true")),
              DataTableRow(List("2", "Bob", "60000.5", "false"))
            )
          )
          val mapper = summon[DataTableMapper[Employee]]
          val result = mapper.map(dt)

          assertTrue(
            result == Right(
              Chunk(
                Employee(1, "Alice", 50000.0, true),
                Employee(2, "Bob", 60000.5, false)
              )
            )
          )
        }
      )
    )
}
