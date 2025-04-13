package zio.bdd.core.step

import zio.schema.{DynamicValue, Schema, StandardType}

trait TypedExtractor[A] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (A, Int)]
  def pattern: String
}

trait DefaultTypedExtractor {
  val string: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map { s =>
          val trimmed = s.trim
          val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
            trimmed.substring(1, trimmed.length - 1)
          } else trimmed
          (unquoted, groupIndex + 1)
        }
        .toRight(s"Expected string at group $groupIndex")

    def pattern: String = "(\".*\"|.*)"
  }

  val int: TypedExtractor[Int] = new TypedExtractor[Int] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Int, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toInt).toOption)
        .map(i => (i, groupIndex + 1))
        .toRight(s"Expected int at group $groupIndex")
    def pattern: String = "(\\d+)"
  }

  val double: TypedExtractor[Double] = new TypedExtractor[Double] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Double, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toDouble).toOption)
        .map(d => (d, groupIndex + 1))
        .toRight(s"Expected double at group $groupIndex")
    def pattern: String = "([-+]?[0-9]*\\.?[0-9]+)"
  }

  val long: TypedExtractor[Long] = new TypedExtractor[Long] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Long, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toLong).toOption)
        .map(l => (l, groupIndex + 1))
        .toRight(s"Expected long at group $groupIndex")
    def pattern: String = "(\\d+)"
  }

  def table[T](implicit schema: Schema[T]): TypedExtractor[List[T]] = TableExtractor(schema)
}

case class TableExtractor[T](schema: Schema[T]) extends TypedExtractor[List[T]] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (List[T], Int)] =
    input.table match {
      case Some(dataTable) if dataTable.rows.nonEmpty =>
        val headers  = dataTable.headers
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
              }.flatMap { dynamicValue =>
                recordSchema.fromDynamic(dynamicValue).left.map(_.toString)
              }
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
          case StandardType.StringType => Right(DynamicValue.Primitive(str, StandardType.StringType))
          case StandardType.IntType =>
            str.toIntOption.map(i => DynamicValue.Primitive(i, StandardType.IntType)).toRight(s"Invalid int: $str")
          case StandardType.LongType =>
            str.toLongOption.map(l => DynamicValue.Primitive(l, StandardType.LongType)).toRight(s"Invalid long: $str")
          case StandardType.DoubleType =>
            str.toDoubleOption
              .map(d => DynamicValue.Primitive(d, StandardType.DoubleType))
              .toRight(s"Invalid double: $str")
          case _ => Left(s"Unsupported primitive type: $standardType")
        }
      case other => Left(s"Only primitive schemas are supported for table cells, got: ${other.getClass.getSimpleName}")
    }
  }

  def pattern: String = ""
}
