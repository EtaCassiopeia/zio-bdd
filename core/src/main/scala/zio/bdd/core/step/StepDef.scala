package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.DataTable
import zio.schema.{DynamicValue, Schema, StandardType}

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.quoted.*

trait State[S] {
  def get: UIO[S]
  def update(f: S => S): UIO[Unit]
}

object State {
  def get[S: Tag]: ZIO[State[S], Nothing, S] = ZIO.serviceWithZIO[State[S]](_.get)
  def update[S: Tag](f: S => S): ZIO[State[S], Nothing, Unit] = ZIO.serviceWithZIO[State[S]](_.update(f))

  def layer[S: Tag](fiberRef: FiberRef[S]): ZLayer[Any, Nothing, State[S]] =
    ZLayer.succeed(new State[S] {
      def get: UIO[S] = fiberRef.get
      def update(f: S => S): UIO[Unit] = fiberRef.update(f)
    })
}

case class StepInput(text: String, table: Option[DataTable] = None)

trait TypedExtractor[A] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (A, Int)]
  def regexPart: String
}

object TypedExtractor {
  implicit val string: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups.lift(groupIndex).map(s => (s.trim, groupIndex + 1)).toRight(s"Expected string at group $groupIndex")
    def regexPart: String = "(.*)"
  }

  implicit val int: TypedExtractor[Int] = new TypedExtractor[Int] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Int, Int)] =
      groups.lift(groupIndex).flatMap(s => scala.util.Try(s.toInt).toOption).map(i => (i, groupIndex + 1))
        .toRight(s"Expected int at group $groupIndex")
    def regexPart: String = "(\\d+)"
  }

  implicit val double: TypedExtractor[Double] = new TypedExtractor[Double] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Double, Int)] =
      groups.lift(groupIndex).flatMap(s => scala.util.Try(s.toDouble).toOption).map(d => (d, groupIndex + 1))
        .toRight(s"Expected double at group $groupIndex")
    def regexPart: String = "([-+]?[0-9]*\\.?[0-9]+)"
  }

  implicit val long: TypedExtractor[Long] = new TypedExtractor[Long] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Long, Int)] =
      groups.lift(groupIndex).flatMap(s => scala.util.Try(s.toLong).toOption).map(l => (l, groupIndex + 1))
        .toRight(s"Expected long at group $groupIndex")
    def regexPart: String = "(\\d+)"
  }

  implicit def table[T](implicit schema: Schema[T]): TypedExtractor[List[T]] = TableExtractor(schema)
}

case class TableExtractor[T](schema: Schema[T]) extends TypedExtractor[List[T]] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (List[T], Int)] =
    input.table match {
      case Some(dataTable) if dataTable.rows.nonEmpty =>
        val headers = dataTable.headers
        val dataRows = dataTable.rows.map(_.cells)
        schema match {
          case recordSchema: Schema.Record[T] =>
            val fieldSchemas = recordSchema.fields.toList.map(f => f.name -> f.schema).toMap
            val items: List[Either[String, T]] = dataRows.map { row =>
              val record = headers.zip(row).toMap
              val fieldValues: Either[String, List[(String, DynamicValue)]] =
                fieldSchemas.foldLeft[Either[String, List[(String, DynamicValue)]]](Right(Nil)) {
                  case (acc, (fieldName, fieldSchema)) =>
                    record.get(fieldName) match {
                      case Some(value) =>
                        stringToDynamicValue(fieldSchema, value).flatMap { dv =>
                          acc.map(list => (fieldName, dv) :: list)
                        }
                      case None => Left(s"Missing field $fieldName in row")
                    }
                }
              fieldValues.map { fvList =>
                val orderedFvList = fvList.reverse
                val fvListMap = ListMap(orderedFvList: _*)
                DynamicValue.Record(recordSchema.id, fvListMap)
              }.flatMap { dynamicValue =>
                recordSchema.fromDynamic(dynamicValue).left.map(_.toString)
              }
            }
            items
              .foldLeft[Either[String, List[T]]](Right(Nil)) { (acc, item) =>
                for {
                  list <- acc
                  i <- item
                } yield i :: list
              }
              .map(_.reverse)
              .map(t => (t, groupIndex))
          case _ => Left("TableExtractor expects a Record schema for T")
        }
      case Some(_) => Left("Empty data table")
      case None => Left("Data table expected but not provided")
    }

  private def stringToDynamicValue(schema: Schema[_], str: String): Either[String, DynamicValue] = schema match {
    case Schema.Primitive(standardType, _) =>
      standardType match {
        case StandardType.StringType => Right(DynamicValue.Primitive(str, StandardType.StringType))
        case StandardType.IntType =>
          str.toIntOption.map(i => DynamicValue.Primitive(i, StandardType.IntType)).toRight(s"Invalid int: $str")
        case StandardType.LongType =>
          str.toLongOption.map(l => DynamicValue.Primitive(l, StandardType.LongType)).toRight(s"Invalid long: $str")
        case _ => Left(s"Unsupported primitive type: $standardType")
      }
    case _ => Left("Only primitive schemas are supported for table cells")
  }

  def regexPart: String = ""
}

trait StepPattern[T] {
  def regex: String
  def segments: List[Either[String, TypedExtractor[_]]]
  def extract(input: StepInput): Option[T]
}

case class StepPattern0(regex: String, segments: List[Either[String, TypedExtractor[_]]]) extends StepPattern[Unit] {
  def extract(input: StepInput): Option[Unit] =
    if (regex.r.findFirstIn(input.text).isDefined) Some(()) else None
}

case class StepPattern1[T](regex: String, segments: List[Either[String, TypedExtractor[_]]], e1: TypedExtractor[T]) extends StepPattern[T] {
    def extract(input: StepInput): Option[T] = {
        val matcher = regex.r.pattern.matcher(input.text)
        if (matcher.matches()) {
        val groups = (1 to matcher.groupCount()).map(matcher.group).toList
        e1.extract(input, groups, 0).toOption.map(_._1)
        } else None
    }
}

case class StepPattern2[T1, T2](regex: String, segments: List[Either[String, TypedExtractor[_]]], e1: TypedExtractor[T1], e2: TypedExtractor[T2]) extends StepPattern[(T1, T2)] {
def extract(input: StepInput): Option[(T1, T2)] = {
    val matcher = regex.r.pattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      for {
        (a, idx1) <- e1.extract(input, groups, 0).toOption
        (b, _)    <- e2.extract(input, groups, idx1).toOption
      } yield (a, b)
    } else None
  }
}

case class StepPattern3[T1, T2, T3](regex: String, segments: List[Either[String, TypedExtractor[_]]], e1: TypedExtractor[T1], e2: TypedExtractor[T2], e3: TypedExtractor[T3]) extends StepPattern[(T1, T2, T3)] {
def extract(input: StepInput): Option[(T1, T2, T3)] = {
    val matcher = regex.r.pattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      for {
        (a, idx1) <- e1.extract(input, groups, 0).toOption
        (b, idx2) <- e2.extract(input, groups, idx1).toOption
        (c, _)    <- e3.extract(input, groups, idx2).toOption
      } yield (a, b, c)
    } else None
  }
}

class StepPatternBuilder(
                          val segments: List[Either[String, TypedExtractor[_]]] = Nil,
                          val regexParts: List[String] = Nil,
                          val extractors: List[TypedExtractor[_]] = Nil
                        ) {
  def /[A](extractor: TypedExtractor[A]): StepPatternBuilder =
    new StepPatternBuilder(
      segments :+ Right(extractor),
      regexParts :+ extractor.regexPart,
      extractors :+ extractor
    )

  def /(literal: String): StepPatternBuilder =
    new StepPatternBuilder(
      segments :+ Left(literal),
      regexParts :+ java.util.regex.Pattern.quote(literal),
      extractors
    )

  def build: (String, List[Either[String, TypedExtractor[_]]], List[TypedExtractor[_]]) =
    (regexParts.mkString, segments, extractors)
}

object StepPatternBuilder {
  implicit def stringToStepPatternBuilder(str: String): StepPatternBuilder = {
    new StepPatternBuilder(List(Left(str)), List(str), Nil)
  }
}

trait StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]]
}

case class StepDefImpl[R, S, T](pattern: StepPattern[T], f: T => RIO[R with State[S], Unit])
  extends StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]] =  {
    println(s"pattern ${pattern.regex}")
    println(pattern.segments)
    pattern.extract(input).map(f)
  }
}

trait StepRegistry[R, S] {
  def register(step: StepDef[R, S]): UIO[Unit]
  def findStep(input: StepInput): Task[RIO[R with State[S], Unit]]
}

object StepRegistry {
  def layer[R: Tag, S: Tag]: ZLayer[Any, Nothing, StepRegistry[R, S]] =
    ZLayer {
      for {
        ref <- Ref.make(List.empty[StepDef[R, S]])
      } yield new StepRegistry[R, S] {
        def register(step: StepDef[R, S]): UIO[Unit] = ref.update(step :: _)

        def findStep(input: StepInput): Task[RIO[R with State[S], Unit]] =
          ref.get.flatMap { steps =>
            val attempts = ZIO.foreach(steps)(step => ZIO.attempt(step.tryExecute(input)))
            attempts.map(_.collectFirst { case Some(effect) => effect }).flatMap {
              case Some(effect) => ZIO.succeed(effect)
              case None => ZIO.fail(new Exception("No matching step found"))
            }
          }
      }
    }

  def findStep[R: Tag, S: Tag](input: StepInput): ZIO[StepRegistry[R, S], Throwable, RIO[R with State[S], Unit]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input))
}

object Given {
  def apply[R: Tag, S: Tag, T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
    StepMacro.stepDef[R, S, T](builder)(f)
}

object When {
  def apply[R: Tag, S: Tag, T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
    StepMacro.stepDef[R, S, T](builder)(f)
}

object Then {
  def apply[R: Tag, S: Tag, T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
    StepMacro.stepDef[R, S, T](builder)(f)
}

