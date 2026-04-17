package zio.bdd.core.step

import zio.test.*
import zio.test.Assertion.*
import zio.schema.{DeriveSchema, Schema}
import zio.bdd.gherkin.{DataTable, DataTableRow}
import zio.*

object TypedExtractorSpec extends ZIOSpecDefault with DefaultTypedExtractor {

  case class User(name: String, age: Int)
  given Schema[User] = DeriveSchema.gen[User]

  case class Item(name: String, price: Double)
  given Schema[Item] = DeriveSchema.gen[Item]

  case class Provision(accountOpenDate: String, instrumentClass: String)
  given Schema[Provision] = DeriveSchema.gen[Provision]

  private def input(text: String = "", tbl: Option[DataTable] = None, doc: Option[String] = None) =
    StepInput(text, tbl, doc)

  private def tblInput(headers: List[String], rows: List[List[String]]) =
    input(tbl = Some(DataTable(headers, rows.map(DataTableRow.apply))))

  private val stringExtractor = suite("string extractor")(
    test("extracts unquoted word") {
      assert(string.extract(input(), List("hello"), 0))(isRight(equalTo(("hello", 1))))
    },
    test("extracts unquoted phrase") {
      assert(string.extract(input(), List("hello world"), 0))(isRight(equalTo(("hello world", 1))))
    },
    test("strips double-quotes from a quoted value") {
      assert(string.extract(input(), List("\"hello world\""), 0))(isRight(equalTo(("hello world", 1))))
    },
    test("extracts an empty string") {
      assert(string.extract(input(), List(""), 0))(isRight(equalTo(("", 1))))
    },
    test("trims surrounding whitespace from unquoted value") {
      assert(string.extract(input(), List("  hello  "), 0))(isRight(equalTo(("hello", 1))))
    },
    test("advances group index by 1") {
      assert(string.extract(input(), List("a", "b", "c"), 1))(isRight(equalTo(("b", 2))))
    },
    test("fails when group index is out of bounds") {
      assert(string.extract(input(), List(), 0))(isLeft(containsString("Expected string")))
    }
  )

  private val wordExtractor = suite("word extractor")(
    test("extracts a single word") {
      assert(word.extract(input(), List("deposit"), 0))(isRight(equalTo(("deposit", 1))))
    },
    test("pattern does not match whitespace") {
      assertTrue(!word.pattern.contains("\\s"))
    }
  )

  private val intExtractor = suite("int extractor")(
    test("extracts a positive integer") {
      assert(int.extract(input(), List("42"), 0))(isRight(equalTo((42, 1))))
    },
    test("extracts a negative integer") {
      assert(int.extract(input(), List("-7"), 0))(isRight(equalTo((-7, 1))))
    },
    test("extracts zero") {
      assert(int.extract(input(), List("0"), 0))(isRight(equalTo((0, 1))))
    },
    test("fails on a decimal value") {
      assert(int.extract(input(), List("3.14"), 0))(isLeft(anything))
    },
    test("fails on non-numeric text") {
      assert(int.extract(input(), List("not a number"), 0))(isLeft(containsString("Expected int")))
    },
    test("fails when group index is out of bounds") {
      assert(int.extract(input(), List(), 0))(isLeft(anything))
    }
  )

  private val doubleExtractor = suite("double extractor")(
    test("extracts a decimal value") {
      assert(double.extract(input(), List("3.14"), 0))(isRight(equalTo((3.14, 1))))
    },
    test("extracts a negative decimal") {
      assert(double.extract(input(), List("-2.5"), 0))(isRight(equalTo((-2.5, 1))))
    },
    test("extracts an integer as a double") {
      assert(double.extract(input(), List("42"), 0))(isRight(equalTo((42.0, 1))))
    },
    test("fails on non-numeric text") {
      assert(double.extract(input(), List("not a double"), 0))(isLeft(containsString("Expected double")))
    }
  )

  private val longExtractor = suite("long extractor")(
    test("extracts a large long value") {
      assert(long.extract(input(), List("9876543210"), 0))(isRight(equalTo((9876543210L, 1))))
    },
    test("extracts a negative long") {
      assert(long.extract(input(), List("-100"), 0))(isRight(equalTo((-100L, 1))))
    },
    test("fails on non-numeric text") {
      assert(long.extract(input(), List("not a long"), 0))(isLeft(containsString("Expected long")))
    }
  )

  private val booleanExtractor = suite("boolean extractor")(
    test("extracts 'true'") {
      assert(boolean.extract(input(), List("true"), 0))(isRight(equalTo((true, 1))))
    },
    test("extracts 'false'") {
      assert(boolean.extract(input(), List("false"), 0))(isRight(equalTo((false, 1))))
    },
    test("extracts 'True' (capitalised)") {
      assert(boolean.extract(input(), List("True"), 0))(isRight(equalTo((true, 1))))
    },
    test("extracts 'FALSE' (uppercase)") {
      assert(boolean.extract(input(), List("FALSE"), 0))(isRight(equalTo((false, 1))))
    },
    test("fails on 'maybe'") {
      assert(boolean.extract(input(), List("maybe"), 0))(isLeft(anything))
    },
    test("fails on empty string") {
      assert(boolean.extract(input(), List(""), 0))(isLeft(anything))
    }
  )

  private val bigDecimalExtractor = suite("bigDecimal extractor")(
    test("extracts an exact decimal value without floating-point loss") {
      assert(bigDecimal.extract(input(), List("12345.67"), 0))(
        isRight(equalTo((BigDecimal("12345.67"), 1)))
      )
    },
    test("extracts a large financial amount") {
      assert(bigDecimal.extract(input(), List("9999999.99"), 0))(
        isRight(equalTo((BigDecimal("9999999.99"), 1)))
      )
    },
    test("extracts zero") {
      assert(bigDecimal.extract(input(), List("0.00"), 0))(isRight(equalTo((BigDecimal("0.00"), 1))))
    },
    test("fails on non-numeric text") {
      assert(bigDecimal.extract(input(), List("not-a-number"), 0))(isLeft(anything))
    }
  )

  private val uuidExtractor = suite("uuid extractor")(
    test("extracts a well-formed UUID") {
      val id = "550e8400-e29b-41d4-a716-446655440000"
      assert(uuid.extract(input(), List(id), 0))(
        isRight(hasField("_1", _._1.toString, equalTo(id)))
      )
    },
    test("fails on a non-UUID string") {
      assert(uuid.extract(input(), List("not-a-uuid"), 0))(isLeft(anything))
    }
  )

  private val restExtractor = suite("rest extractor")(
    test("captures the full remainder including spaces") {
      assert(rest.extract(input(), List("Hello World How Are You"), 0))(
        isRight(equalTo(("Hello World How Are You", 1)))
      )
    }
  )

  private val tableExtractor = suite("table extractor")(
    test("extracts a list of Users from a two-column table") {
      val t      = tblInput(List("name", "age"), List(List("Alice", "30"), List("Bob", "25")))
      val result = table[User].extract(t, Nil, 0)
      assert(result)(isRight(equalTo((List(User("Alice", 30), User("Bob", 25)), 0))))
    },
    test("extracts a list of Items with a double column") {
      val t      = tblInput(List("name", "price"), List(List("Laptop", "999.99")))
      val result = table[Item].extract(t, Nil, 0)
      assert(result)(isRight(equalTo((List(Item("Laptop", 999.99)), 0))))
    },
    test("fails when a required column is missing from the table") {
      val t      = tblInput(List("name"), List(List("Alice")))
      val result = table[User].extract(t, Nil, 0)
      assert(result)(isLeft(equalTo("Missing field age in row")))
    },
    test("fails when a cell value cannot be converted to the expected type") {
      val t      = tblInput(List("name", "age"), List(List("Alice", "not-a-number")))
      val result = table[User].extract(t, Nil, 0)
      assert(result)(isLeft(equalTo("Invalid int: not-a-number")))
    },
    test("fails when the table has no data rows") {
      val t      = tblInput(List("name", "age"), Nil)
      val result = table[User].extract(t, Nil, 0)
      assert(result)(isLeft(equalTo("Empty data table")))
    },
    test("fails when no table is provided") {
      val result = table[User].extract(input(), Nil, 0)
      assert(result)(isLeft(equalTo("Data table expected but not provided")))
    },
    test("fails when schema is not a Record (primitive)") {
      val result = table[Int].extract(tblInput(List("x"), List(List("1"))), Nil, 0)
      assert(result)(isLeft(containsString("Record schema")))
    }
  )

  private val tableMappingExtractor = suite("tableWithMapping extractor")(
    test("maps display-name headers to Scala field names") {
      val mapping = Map("Account Open Date" -> "accountOpenDate", "Instrument Class" -> "instrumentClass")
      val t       = tblInput(List("Account Open Date", "Instrument Class"), List(List("2025-01-01", "SimpleSavings")))
      val result  = tableWithMapping[Provision](mapping).extract(t, Nil, 0)
      assert(result)(isRight(hasField("_1", _._1, equalTo(List(Provision("2025-01-01", "SimpleSavings"))))))
    },
    test("unmapped headers are passed through as-is (identity mapping for non-listed columns)") {
      // If a column is NOT in the mapping, use the header verbatim (matches field name directly)
      val mapping = Map("Account Open Date" -> "accountOpenDate")
      // instrumentClass is not in mapping — should be passed through verbatim if it matches a field
      val t      = tblInput(List("Account Open Date", "instrumentClass"), List(List("2025-01-01", "SimpleSavings")))
      val result = tableWithMapping[Provision](mapping).extract(t, Nil, 0)
      assert(result)(isRight(anything))
    }
  )

  private val docStringExtractor = suite("docString extractor")(
    test("extracts content from a StepInput that carries a doc string") {
      val stepInput = input(doc = Some("{\n  \"key\": \"value\"\n}"))
      val result    = docString.extract(stepInput, Nil, 0)
      assert(result)(isRight(hasField("_1", _._1, containsString("key"))))
    },
    test("fails when no doc string is present") {
      val result = docString.extract(input(), Nil, 0)
      assert(result)(isLeft(containsString("Doc string")))
    }
  )

  // Defined at object scope to avoid case-class redefinition inside tests
  case class AnnotatedRow(
    @ColumnName("Account Open Date") accountOpenDate: String = "",
    @ColumnName("Instrument Class") instrumentClass: String = ""
  )
  given Schema[AnnotatedRow] = DeriveSchema.gen[AnnotatedRow]

  private val columnNameExtractor = suite("@ColumnName annotation")(
    test("table[T] uses @ColumnName value to match feature file headers") {
      val tbl = DataTable(
        headers = List("Account Open Date", "Instrument Class"),
        rows = List(DataTableRow(List("2025-01-01", "SimpleSavings")))
      )
      val result = table[AnnotatedRow].extract(input(tbl = Some(tbl)), Nil, 0)
      assert(result)(isRight(equalTo((List(AnnotatedRow("2025-01-01", "SimpleSavings")), 0))))
    },
    test("tableWithMapping explicit map takes priority over @ColumnName") {
      val tbl = DataTable(
        headers = List("OpenDate", "InstrClass"),
        rows = List(DataTableRow(List("2025-06-01", "HomeLoan")))
      )
      val extractor = tableWithMapping[AnnotatedRow](
        Map(
          "OpenDate"   -> "accountOpenDate",
          "InstrClass" -> "instrumentClass"
        )
      )
      val result = extractor.extract(input(tbl = Some(tbl)), Nil, 0)
      assert(result)(isRight(equalTo((List(AnnotatedRow("2025-06-01", "HomeLoan")), 0))))
    },
    test("unrecognised header fails with Missing field error") {
      val tbl = DataTable(
        headers = List("wrong-header", "also-wrong"),
        rows = List(DataTableRow(List("v1", "v2")))
      )
      val result = table[AnnotatedRow].extract(input(tbl = Some(tbl)), Nil, 0)
      assert(result)(isLeft(containsString("Missing field")))
    }
  )

  // ── Property: group index threading ──────────────────────────────────────

  private val groupIndexProps = suite("Property: group index threading")(
    test("consecutive extractors each advance the index by 1") {
      // Simulate: "user Alice has age 30" with two extractors at indices 0 and 1
      val groups = List("Alice", "30")
      for {
        r1 <- ZIO.fromEither(string.extract(input(), groups, 0)).map(_._2)
        r2 <- ZIO.fromEither(int.extract(input(), groups, r1)).map(_._2)
      } yield assertTrue(r1 == 1, r2 == 2)
    },
    test("all scalar extractors advance index by exactly 1") {
      val cases = List(
        string.extract(input(), List("v"), 0).map(_._2),
        int.extract(input(), List("1"), 0).map(_._2),
        double.extract(input(), List("1.0"), 0).map(_._2),
        long.extract(input(), List("1"), 0).map(_._2),
        boolean.extract(input(), List("true"), 0).map(_._2),
        bigDecimal.extract(input(), List("1.0"), 0).map(_._2),
        word.extract(input(), List("w"), 0).map(_._2),
        rest.extract(input(), List("r"), 0).map(_._2)
      )
      ZIO.foreach(cases)(e => ZIO.fromEither(e)).map { indices =>
        assertTrue(indices.forall(_ == 1))
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("TypedExtractors")(
    stringExtractor,
    wordExtractor,
    intExtractor,
    doubleExtractor,
    longExtractor,
    booleanExtractor,
    bigDecimalExtractor,
    uuidExtractor,
    restExtractor,
    tableExtractor,
    tableMappingExtractor,
    docStringExtractor,
    groupIndexProps,
    columnNameExtractor
  )
}
