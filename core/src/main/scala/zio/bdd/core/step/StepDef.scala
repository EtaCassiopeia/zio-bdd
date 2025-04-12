package zio.bdd.core.step

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.Hooks
import zio.bdd.gherkin.DataTable
import zio.schema.{DynamicValue, Schema, StandardType}

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
  def regexPart: String
}

object TypedExtractor {

  implicit val string: TypedExtractor[String] = new TypedExtractor[String] {
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

    def regexPart: String = "(\".*\"|.*)" // Match quoted strings explicitly
  }

  implicit val int: TypedExtractor[Int] = new TypedExtractor[Int] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Int, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toInt).toOption)
        .map(i => (i, groupIndex + 1))
        .toRight(s"Expected int at group $groupIndex")
    def regexPart: String = "(\\d+)"
  }

  implicit val double: TypedExtractor[Double] = new TypedExtractor[Double] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Double, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toDouble).toOption)
        .map(d => (d, groupIndex + 1))
        .toRight(s"Expected double at group $groupIndex")
    def regexPart: String = "([-+]?[0-9]*\\.?[0-9]+)"
  }

  implicit val long: TypedExtractor[Long] = new TypedExtractor[Long] {
    def extract(input: StepInput, groups: List[String], groupIndex: Int): Either[String, (Long, Int)] =
      groups
        .lift(groupIndex)
        .flatMap(s => scala.util.Try(s.toLong).toOption)
        .map(l => (l, groupIndex + 1))
        .toRight(s"Expected long at group $groupIndex")
    def regexPart: String = "(\\d+)"
  }

  implicit def table[T](implicit schema: Schema[T]): TypedExtractor[List[T]] = TableExtractor(schema)
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

case class StepPattern1[T](regex: String, segments: List[Either[String, TypedExtractor[_]]], e1: TypedExtractor[T])
    extends StepPattern[T] {
  def extract(input: StepInput): Option[T] = {
    val matcher = regex.r.pattern.matcher(input.text)
    if (matcher.matches()) {
      val groups = (1 to matcher.groupCount()).map(matcher.group).toList
      e1.extract(input, groups, 0).toOption.map(_._1)
    } else None
  }
}

case class StepPattern2[T1, T2](
  regex: String,
  segments: List[Either[String, TypedExtractor[_]]],
  e1: TypedExtractor[T1],
  e2: TypedExtractor[T2]
) extends StepPattern[(T1, T2)] {
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

case class StepPattern3[T1, T2, T3](
  regex: String,
  segments: List[Either[String, TypedExtractor[_]]],
  e1: TypedExtractor[T1],
  e2: TypedExtractor[T2],
  e3: TypedExtractor[T3]
) extends StepPattern[(T1, T2, T3)] {
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
      regexParts :+ literal,
      extractors
    )

  def build: (String, List[Either[String, TypedExtractor[_]]], List[TypedExtractor[_]]) =
    (regexParts.mkString, segments, extractors)
}

object StepPatternBuilder {
  implicit def stringToStepPatternBuilder(str: String): StepPatternBuilder =
    new StepPatternBuilder(List(Left(str)), List(str), Nil)
}

trait StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]]
}

case class StepDefImpl[R, S, T](pattern: StepPattern[T], f: T => RIO[R with State[S], Unit]) extends StepDef[R, S] {
  def tryExecute(input: StepInput): Option[RIO[R with State[S], Unit]] =
    pattern.extract(input).map(f)
}

trait ZIOSteps[R, S] extends Hooks[R] with GeneratedStepMethods[R, S] {
  type Step[I, O] = I => ZIO[R, Throwable, O]

  // Mutable list to collect steps during initialization
  private val steps: mutable.ListBuffer[StepDef[R, S]] = mutable.ListBuffer.empty

  // Retrieve the collected steps
  def getSteps: List[StepDef[R, S]] = steps.toList

  // Register a step by adding it to the list
  def register(step: StepDef[R, S]): Unit =
    steps += step

  // Environment required for step execution
  def environment: ZLayer[Any, Any, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Any, R]]
}

object ZIOSteps {
  trait Default[R, S] extends ZIOSteps[R, S]
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

//trait GeneratedStepMethods[R, S] { self: ZIOSteps [R, S] =>
//  def Given[T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
//    StepMacro.stepDef[R, S, T](builder, f, self)
//
//  def Given[T1, T2](builder: StepPatternBuilder)(f: (T1, T2) => RIO[R & State[S], Unit]): Unit = {
//    val adaptedF: ((T1, T2)) => RIO[R & State[S], Unit] = {
//      case (t1, t2) => f(t1, t2)
//    }
//    StepMacro.stepDef[R, S, (T1, T2)](builder, adaptedF, self)
//  }
//
//  def When[T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
//    StepMacro.stepDef[R, S, T](builder, f, self)
//
//  def Then[T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit =
//    StepMacro.stepDef[R, S, T](builder, f,self)
//}

trait GeneratedStepMethods[R, S] { self: ZIOSteps[R, S] =>
  // For no parameters
  def Given(builder: StepPatternBuilder)(f: => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 0) {
      throw new Exception(s"Expected 0 parameters but got ${extractors.length} extractors")
    }
    val pattern                                      = StepPattern0(regex, segments)
    val adaptedF: Unit => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, Unit](pattern, adaptedF))
  }

  def Given[T1](builder: StepPatternBuilder)(f: T1 => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 1) {
      throw new Exception(s"Expected 1 parameter but got ${extractors.length} extractors")
    }
    val e1      = extractors(0).asInstanceOf[TypedExtractor[T1]]
    val pattern = StepPattern1[T1](regex, segments, e1)
    self.register(StepDefImpl[R, S, T1](pattern, f))
  }

  def Given[T1, T2](builder: StepPatternBuilder)(f: (T1, T2) => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 2) {
      throw new Exception(s"Expected 2 parameters but got ${extractors.length} extractors")
    }
    val e1                                                 = extractors(0).asInstanceOf[TypedExtractor[T1]]
    val e2                                                 = extractors(1).asInstanceOf[TypedExtractor[T2]]
    val pattern                                            = StepPattern2[T1, T2](regex, segments, e1, e2)
    val adaptedF: ((T1, T2)) => RIO[R with State[S], Unit] = tup => f(tup._1, tup._2)
    self.register(StepDefImpl[R, S, (T1, T2)](pattern, adaptedF))
  }

  def Given[T1, T2, T3](builder: StepPatternBuilder)(f: (T1, T2, T3) => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 3) {
      throw new Exception(s"Expected 3 parameters but got ${extractors.length} extractors")
    }
    val e1                                                     = extractors(0).asInstanceOf[TypedExtractor[T1]]
    val e2                                                     = extractors(1).asInstanceOf[TypedExtractor[T2]]
    val e3                                                     = extractors(2).asInstanceOf[TypedExtractor[T3]]
    val pattern                                                = StepPattern3[T1, T2, T3](regex, segments, e1, e2, e3)
    val adaptedF: ((T1, T2, T3)) => RIO[R with State[S], Unit] = tup => f(tup._1, tup._2, tup._3)
    self.register(StepDefImpl[R, S, (T1, T2, T3)](pattern, adaptedF))
  }

  def When(builder: StepPatternBuilder)(f: => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 0) {
      throw new Exception(s"Expected 0 parameters but got ${extractors.length} extractors")
    }
    val pattern                                      = StepPattern0(regex, segments)
    val adaptedF: Unit => RIO[R with State[S], Unit] = _ => f
    self.register(StepDefImpl[R, S, Unit](pattern, adaptedF))
  }

  def When[T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 1) {
      throw new Exception(s"Expected 1 parameter but got ${extractors.length} extractors, ${regex}")
    }
    val e1      = extractors(0).asInstanceOf[TypedExtractor[T]]
    val pattern = StepPattern1[T](regex, segments, e1)
    self.register(StepDefImpl[R, S, T](pattern, f))
  }

  def Then[T](builder: StepPatternBuilder)(f: T => RIO[R with State[S], Unit]): Unit = {
    val (regex, segments, extractors) = builder.build
    if (extractors.length != 1) {
      throw new Exception(s"Expected 1 parameter but got ${extractors.length} extractors")
    }
    val e1      = extractors(0).asInstanceOf[TypedExtractor[T]]
    val pattern = StepPattern1[T](regex, segments, e1)
    self.register(StepDefImpl[R, S, T](pattern, f))
  }
}
