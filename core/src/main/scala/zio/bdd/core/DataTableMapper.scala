package zio.bdd.core

import zio.Chunk
import zio.bdd.gherkin.DataTable

import scala.compiletime.{constValueTuple, summonAll}
import scala.deriving.Mirror
import scala.util.Try

object Utils {
  def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
    list.foldRight[Either[E, List[A]]](Right(Nil)) { (e, acc) =>
      for {
        a  <- e
        as <- acc
      } yield a :: as
    }
}

// Type class for converting strings to specific types
trait FromString[T] {
  def fromString(s: String): Either[String, T]
}

object FromString {
  implicit val string: FromString[String]   = s => Right(s)
  implicit val int: FromString[Int]         = s => Try(s.toInt).toEither.left.map(_ => s"Invalid Int: $s")
  implicit val double: FromString[Double]   = s => Try(s.toDouble).toEither.left.map(_ => s"Invalid Double: $s")
  implicit val boolean: FromString[Boolean] = s => Try(s.toBoolean).toEither.left.map(_ => s"Invalid Boolean: $s")
}

// Type class for mapping DataTable to T
trait DataTableMapper[T] {
  def map(dataTable: DataTable): Either[String, Chunk[T]]
}

object DataTableMapper {

  // Mapper for case classes using Mirror API
  inline given derived[T](using m: Mirror.Of[T]): DataTableMapper[T] = new DataTableMapper[T] {
    def map(dataTable: DataTable): Either[String, Chunk[T]] =
      inline m match {
        case p: Mirror.ProductOf[T] =>
          // Extract field names from the product type
          val labels     = constValueTuple[p.MirroredElemLabels]
          val fieldNames = labels.toList.asInstanceOf[List[String]]

          // Map headers to their indices
          val headerToIndex = dataTable.headers.zipWithIndex.toMap
          val fieldIndices = fieldNames.map { name =>
            headerToIndex.get(name).toRight(s"Header '$name' not found in DataTable")
          }

          // Sequence the indices and proceed with derivation
          Utils.sequence(fieldIndices).flatMap { indices =>
            val fromStrings =
              summonAll[Tuple.Map[p.MirroredElemTypes, FromString]].toList.asInstanceOf[List[FromString[_]]]
            val mappings = indices.zip(fromStrings)
            val mappedRows = dataTable.rows.map { row =>
              val cells = row.cells
              val values = mappings.map { case (idx, fs) =>
                if (idx >= cells.length) Left(s"Index $idx out of bounds for row")
                else fs.asInstanceOf[FromString[Any]].fromString(cells(idx))
              }
              Utils.sequence(values).map(vals => p.fromProduct(Tuple.fromArray(vals.toArray)))
            }
            Utils.sequence(mappedRows).map(Chunk.fromIterable)
          }
      }
  }
}
