package zio.bdd.core.step

import zio.schema.{DynamicValue, Schema, StandardType}

import java.math.{BigDecimal => JBigDecimal}
import java.util.UUID

trait TypedExtractor[A] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (A, Int)]
  def pattern: String
}

trait DefaultTypedExtractor {

  /**
   * Matches any text, optionally double-quoted. Quotes are stripped from the
   * captured value.
   */
  val string: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map { s =>
          val trimmed = s.trim
          val unquoted =
            if (trimmed.startsWith("\"") && trimmed.endsWith("\""))
              trimmed.substring(1, trimmed.length - 1)
            else trimmed
          (unquoted, groupIndex + 1)
        }
        .toRight(s"Expected string at group $groupIndex")

    def pattern: String = "(\".*\"|.*)"
  }

  /** Matches a single word (no whitespace). */
  val word: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map(s => (s.trim, groupIndex + 1))
        .toRight(s"Expected word at group $groupIndex")

    def pattern: String = "(\\S+)"
  }

  /** Matches the entire remainder of the step text (greedy). */
  val rest: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map(s => (s, groupIndex + 1))
        .toRight(s"Expected rest-of-line at group $groupIndex")

    def pattern: String = "(.+)"
  }

  /** Matches non-negative integers. */
  val int: TypedExtractor[Int] = new TypedExtractor[Int] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Int, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toInt).toOption)
        .map(i => (i, groupIndex + 1))
        .toRight(s"Expected int at group $groupIndex")
    def pattern: String = "(-?\\d+)"
  }

  /** Matches integer and decimal numbers. */
  val double: TypedExtractor[Double] = new TypedExtractor[Double] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Double, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toDouble).toOption)
        .map(d => (d, groupIndex + 1))
        .toRight(s"Expected double at group $groupIndex")
    def pattern: String = "([-+]?[0-9]*\\.?[0-9]+)"
  }

  /** Matches long integers. */
  val long: TypedExtractor[Long] = new TypedExtractor[Long] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Long, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toLong).toOption)
        .map(l => (l, groupIndex + 1))
        .toRight(s"Expected long at group $groupIndex")
    def pattern: String = "(-?\\d+)"
  }

  /** Matches `true` or `false` (case-insensitive). */
  val boolean: TypedExtractor[Boolean] = new TypedExtractor[Boolean] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Boolean, Int)] =
      groups
        .lift(groupIndex)
        .flatMap { s =>
          s.trim.toLowerCase match {
            case "true"  => Some(true)
            case "false" => Some(false)
            case _       => None
          }
        }
        .map(b => (b, groupIndex + 1))
        .toRight(s"Expected boolean (true/false) at group $groupIndex")

    def pattern: String = "(true|false|True|False|TRUE|FALSE)"
  }

  /**
   * Matches decimal numbers as BigDecimal (exact precision — use for financial
   * values).
   */
  val bigDecimal: TypedExtractor[BigDecimal] = new TypedExtractor[BigDecimal] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (BigDecimal, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(BigDecimal(new JBigDecimal(s.trim))).toOption)
        .map(d => (d, groupIndex + 1))
        .toRight(s"Expected BigDecimal at group $groupIndex")

    def pattern: String = "([-+]?[0-9]*\\.?[0-9]+)"
  }

  /** Matches UUID strings (8-4-4-4-12 hex format). */
  val uuid: TypedExtractor[UUID] = new TypedExtractor[UUID] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (UUID, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(UUID.fromString(s.trim)).toOption)
        .map(u => (u, groupIndex + 1))
        .toRight(s"Expected UUID at group $groupIndex")

    def pattern: String = "([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})"
  }

  /**
   * Extracts a typed data table. Column headers must match the Scala field
   * names of T exactly.
   */
  def table[T](using schema: Schema[T]): TypedExtractor[List[T]] = TableExtractor(schema, Map.empty)

  /**
   * Extracts a typed data table with an explicit header-name mapping. Useful
   * when the feature file uses display names (e.g. "Account Open Date") that
   * differ from the Scala field names (e.g. "accountOpenDate").
   */
  def tableWithMapping[T](headerMap: Map[String, String])(using schema: Schema[T]): TypedExtractor[List[T]] =
    TableExtractor(schema, headerMap)

  /**
   * Extracts the doc string argument (triple-quoted text) of a step as a raw
   * String.
   */
  val docString: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      input.docString.map(ds => (ds, groupIndex)).toRight("Doc string expected but not provided")

    def pattern: String = ""
  }

  /**
   * Embed a raw regex fragment as a `TypedExtractor[String]`.
   *
   * The caller is responsible for ensuring `pat` contains **exactly one**
   * top-level capturing group `(...)`. The captured text is returned as a
   * `String`; if the group is optional (e.g. `(foo)?`) the result is `""` when
   * it does not match.
   *
   * Use this when existing extractors don't cover the pattern you need — most
   * commonly for optional literals and simple alternations:
   *
   * {{{
   * // Optional "jackson " prefix — replaces two separate step registrations:
   * Given("a valid " / regex("(jackson )?") / "provisioned account") { (prefix: String) =>
   *   val useJackson = prefix.nonEmpty
   *   ...
   * }
   *
   * // Capture either word:
   * Given("account is " / regex("(active|inactive)")) { (status: String) =>
   *   ...
   * }
   * }}}
   *
   * @param pat
   *   A regex string with exactly one top-level capturing group.
   */
  def regex(pat: String): TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map(s => (Option(s).getOrElse(""), groupIndex + 1))
        .toRight(s"Expected regex match at group $groupIndex (pattern: $pat)")

    def pattern: String = pat
  }

  /**
   * Matches any one of the supplied string alternatives and returns the matched
   * value.
   *
   * Generates the pattern `(alt0|alt1|...)` where each alternative is
   * regex-quoted. The matched alternative is returned as a `String`.
   *
   * {{{
   * // Replaces separate "is provisioned" / "is not provisioned" step registrations:
   * Then("the account " / oneOf("is", "is not") / " provisioned") { (condition: String) =>
   *   val expectPresent = condition == "is"
   *   ...
   * }
   *
   * // Longer alternative list:
   * When("remove for previous " / oneOf("withdraw Post", "deposit Post", "Remove", "EOD")) {
   *   (txKind: String) => ...
   * }
   * }}}
   */
  def oneOf(alternatives: String*): TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map(s => (s, groupIndex + 1))
        .toRight(s"Expected one of [${alternatives.mkString(", ")}] at group $groupIndex")

    // Alternatives are sorted longest-first so the regex engine tries the longest match first,
    // preventing "is" from matching before "is not" in overlapping sets.
    def pattern: String =
      "(" + alternatives.sortBy(-_.length).map(java.util.regex.Pattern.quote).mkString("|") + ")"
  }

  /**
   * Matches an optional literal text and returns `Some(text)` when present,
   * `None` when absent.
   *
   * Generates the pattern `(text)?` where `text` is regex-quoted.
   *
   * {{{
   * // Replaces two separate step registrations for the with/without suffix:
   * Given("a valid SimulationBranch request body without fork point" /
   *       optional(" with the same simulationId")) { (suffix: Option[String]) =>
   *   // suffix.isDefined tells you which variant matched
   *   ...
   * }
   * }}}
   *
   * See also `optionalClause` (in project-local extractors) which returns
   * `Boolean` instead of `Option[String]` for callers that only need the
   * presence flag.
   */
  def optional(text: String): TypedExtractor[Option[String]] = new TypedExtractor[Option[String]] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Option[String], Int)] = {
      val captured = groups.lift(groupIndex).flatMap(s => Option(s).filter(_.nonEmpty))
      Right((captured, groupIndex + 1))
    }

    def pattern: String = s"(${java.util.regex.Pattern.quote(text)})?"
  }
}

/**
 * Typed extractor for data tables. Uses Schema to map header row to case-class
 * fields.
 *
 * Header-to-field mapping priority (highest first):
 *   1. Explicit `headerMap` passed to `tableWithMapping[T]` 2.
 *      `@ColumnName("Header Text")` annotation on the case class field 3. Exact
 *      Scala field name (default)
 */
final class TableExtractor[T](schema: Schema[T], headerMap: Map[String, String]) extends TypedExtractor[List[T]] {

  // Build a reverse mapping: header-text → Scala-field-name
  // combining explicit headerMap with @ColumnName annotations from the schema.
  private lazy val effectiveMapping: Map[String, String] = {
    val annotationMap: Map[String, String] = schema match {
      case rec: zio.schema.Schema.Record[T] =>
        rec.fields.flatMap { field =>
          // Look for @ColumnName annotation on the field
          field.annotations.collectFirst { case cn: ColumnName =>
            cn.name -> field.name
          }
        }.toMap
      case _ => Map.empty
    }
    // explicit headerMap overrides @ColumnName which overrides identity
    annotationMap ++ headerMap
  }

  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (List[T], Int)] =
    input.table match {
      case Some(dataTable) if dataTable.rows.nonEmpty =>
        // Apply header mapping: feature header → Scala field name
        val headers  = dataTable.headers.map(h => effectiveMapping.getOrElse(h, h))
        val dataRows = dataTable.rows.map(_.cells)
        schema match {
          case recordSchema: Schema.Record[T] =>
            val fieldSchemas = recordSchema.fields.toList.map(f => f.name -> f.schema).toMap
            val items = dataRows.map { row =>
              val record = headers.zip(row).toMap
              val fieldValues = fieldSchemas.foldLeft[Either[String, List[(String, DynamicValue)]]](Right(Nil)) {
                case (acc, (fieldName, fieldSchema)) =>
                  record.get(fieldName) match {
                    case Some(value) =>
                      stringToDynamicValue(fieldSchema, value).flatMap(dv => acc.map(list => (fieldName, dv) :: list))
                    case None => Left(s"Missing field $fieldName in row")
                  }
              }
              fieldValues.map { fvList =>
                val orderedFvList = fvList.reverse
                val fvListMap     = scala.collection.immutable.ListMap(orderedFvList: _*)
                DynamicValue.Record(recordSchema.id, fvListMap)
              }.flatMap(dv => recordSchema.fromDynamic(dv).left.map(_.toString))
            }
            items
              .foldLeft[Either[String, List[T]]](Right(Nil)) { (acc, item) =>
                for { list <- acc; i <- item } yield i :: list
              }
              .map(_.reverse)
              .map(t => (t, groupIndex))
          case _ => Left("TableExtractor expects a Record schema for T")
        }
      case Some(_) => Left("Empty data table")
      case None    => Left("Data table expected but not provided")
    }

  private def stringToDynamicValue(schema: Schema[_], str: String): Either[String, DynamicValue] = {
    @scala.annotation.tailrec
    def unwrapLazy(s: Schema[_]): Schema[_] = s match {
      case Schema.Lazy(lazySchema) => unwrapLazy(lazySchema())
      case other                   => other
    }

    unwrapLazy(schema) match {
      case Schema.Primitive(standardType, _) =>
        standardType match {
          case StandardType.StringType =>
            Right(DynamicValue.Primitive(str, StandardType.StringType))
          case StandardType.IntType =>
            str.toIntOption.map(i => DynamicValue.Primitive(i, StandardType.IntType)).toRight(s"Invalid int: $str")
          case StandardType.LongType =>
            str.toLongOption.map(l => DynamicValue.Primitive(l, StandardType.LongType)).toRight(s"Invalid long: $str")
          case StandardType.DoubleType =>
            str.toDoubleOption
              .map(d => DynamicValue.Primitive(d, StandardType.DoubleType))
              .toRight(s"Invalid double: $str")
          case StandardType.BoolType =>
            str.trim.toLowerCase match {
              case "true"  => Right(DynamicValue.Primitive(true, StandardType.BoolType))
              case "false" => Right(DynamicValue.Primitive(false, StandardType.BoolType))
              case _       => Left(s"Invalid boolean: $str")
            }
          case StandardType.BigDecimalType =>
            scala.util
              .Try(new JBigDecimal(str.trim))
              .toEither
              .left
              .map(_ => s"Invalid BigDecimal: $str")
              .map(bd => DynamicValue.Primitive(bd, StandardType.BigDecimalType))
          case _ => Left(s"Unsupported primitive type: $standardType for value: $str")
        }
      case other => Left(s"Only primitive schemas are supported for table cells, got: ${other.getClass.getSimpleName}")
    }
  }

  def pattern: String = ""
}
