package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.Hooks
import zio.bdd.gherkin.DataTable
import zio.schema.{DynamicValue, Schema, StandardType}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions
import scala.quoted.*

trait State[S] {
  def get: UIO[S]
  def update(f: S => S): UIO[Unit]
}

object State {
  def get[S: Tag]: ZIO[State[S], Nothing, S]                  = ZIO.serviceWithZIO[State[S]](_.get)
  def update[S: Tag](f: S => S): ZIO[State[S], Nothing, Unit] = ZIO.serviceWithZIO[State[S]](_.update(f))

  def layer[S: Tag](fiberRef: FiberRef[S]): ZLayer[Any, Nothing, State[S]] =
    ZLayer.succeed(new State[S] {
      def get: UIO[S]                  = fiberRef.get
      def update(f: S => S): UIO[Unit] = fiberRef.update(f)
    })
}

case class StepInput(text: String, table: Option[DataTable] = None)

trait TypedExtractor[A] {
  def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (A, Int)]
  def pattern: String
}

object TypedExtractor {

  val string: TypedExtractor[String] = new TypedExtractor[String] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (String, Int)] =
      groups
        .lift(groupIndex)
        .map { s =>
          val trimmed = s.trim
          val unquoted = if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
            trimmed.substring(1, trimmed.length - 1) // Remove quotes
          } else {
            trimmed
          }
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
                val fvListMap     = ListMap(orderedFvList: _*)
                DynamicValue.Record(recordSchema.id, fvListMap)
              }.flatMap { dynamicValue =>
                recordSchema.fromDynamic(dynamicValue).left.map(_.toString)
              }
            }
            items
              .foldLeft[Either[String, List[T]]](Right(Nil)) { (acc, item) =>
                for {
                  list <- acc
                  i    <- item
                } yield i :: list
              }
              .map(_.reverse)
              .map(t => (t, groupIndex))
          case _ => Left("TableExtractor expects a Record schema for T")
        }
      case Some(_) => Left("Empty data table")
      case None    => Left("Data table expected but not provided")
    }

  private def stringToDynamicValue(schema: Schema[_], str: String): Either[String, DynamicValue] = {
    @tailrec
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
      case other =>
        Left(s"Only primitive schemas are supported for table cells, got: ${other.getClass.getSimpleName} for $str")
    }
  }

  def pattern: String = ""
}

sealed trait StepPart
case class Literal(value: String)                     extends StepPart
case class Extractor[T](extractor: TypedExtractor[T]) extends StepPart

case class StepExpression[Out <: Tuple](parts: List[StepPart]) {
  private def regex: String = parts.map {
    case Literal(s)   => Pattern.quote(s)
    case Extractor(e) => e.pattern
  }.mkString

  def extract(input: StepInput): Option[Out] = {
    val pattern = regex.r.pattern
    val matcher = pattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      extractValues(input, groups, 0, parts.collect { case Extractor(e) => e })
    } else {
      None
    }
  }

  private def extractValues(
    input: StepInput,
    groups: List[String],
    index: Int,
    extractors: List[TypedExtractor[_]]
  ): Option[Out] =
    extractors match {
      case Nil => Some(EmptyTuple.asInstanceOf[Out])
      case head :: tail =>
        head.extract(input, groups, index) match {
          case Right((value, nextIndex)) =>
            extractValues(input, groups, nextIndex, tail).map { rest =>
              (value *: rest).asInstanceOf[Out]
            }
          case Left(_) => None
        }
    }
}

extension [Out <: Tuple](se: StepExpression[Out])
  def /(literal: String): StepExpression[Out] =
    StepExpression(se.parts :+ Literal(literal))
  def /[T](extractor: TypedExtractor[T]): StepExpression[Tuple.Concat[Out, Tuple1[T]]] =
    StepExpression(se.parts :+ Extractor(extractor))

implicit def stringToStepExpression(str: String): StepExpression[EmptyTuple] =
  StepExpression(List(Literal(str)))

trait StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]]
}

case class StepDefImpl[R, S, Out <: Tuple](stepExpr: StepExpression[Out], f: Out => RIO[R with State[S], Unit])
    extends StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]] =
    stepExpr.extract(input).map(f)
}

trait ZIOSteps[R, S] extends Hooks[R] with GeneratedStepMethods[R, S] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  private val steps: mutable.ListBuffer[StepDef[R, S]] = mutable.ListBuffer.empty

  def getSteps: List[StepDef[R, S]] = steps.toList

  def register(step: StepDef[R, S]): Unit =
    steps += step

  def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
}

trait StepRegistry[R, S] {
  def findStep(input: StepInput): Task[RIO[R with State[S], Unit]]
}

case class StepRegistryLive[R, S](steps: List[StepDef[R, S]]) extends StepRegistry[R, S] {
  def findStep(input: StepInput): Task[RIO[R with State[S], Unit]] =
    ZIO
      .fromOption(steps.iterator.map(_.tryExecute(input)).find(_.isDefined).flatten)
      .orElseFail(new Exception("No matching step found"))
}

object StepRegistry {
  def layer[R: Tag, S: Tag](steps: List[StepDef[R, S]]): ZLayer[Any, Nothing, StepRegistry[R, S]] =
    ZLayer.succeed(StepRegistryLive(steps))

  def findStep[R: Tag, S: Tag](input: StepInput): ZIO[StepRegistry[R, S], Throwable, RIO[R with State[S], Unit]] =
    ZIO.serviceWithZIO[StepRegistry[R, S]](_.findStep(input))
}

trait GeneratedStepMethods[R, S] { self: ZIOSteps[R, S] =>
  def Given(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def Given[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }

  def Given[A, B](stepExpr: StepExpression[(A, B)])(f: (A, B) => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: ((A, B)) => RIO[R with State[S], Unit] = { case (a, b) => f(a, b) }
    self.register(StepDefImpl[R, S, (A, B)](stepExpr, adaptedF))
  }

  def When(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def When[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }

  def Then(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, EmptyTuple](stepExpr, adaptedF))
  }

  def Then[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
    self.register(StepDefImpl[R, S, Tuple1[A]](stepExpr, adaptedF))
  }
}
