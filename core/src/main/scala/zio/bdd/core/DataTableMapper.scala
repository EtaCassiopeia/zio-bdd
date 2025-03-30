package zio.bdd.core

import zio.Chunk
import zio.bdd.gherkin.DataTable
import izumi.reflect.Tag

import scala.compiletime.{constValueTuple, summonAll}
import scala.deriving.Mirror
import scala.util.Try

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

  /**
   * Derives a DataTableMapper for product types (e.g., case classes) using
   * Scala's Mirror API. Maps a DataTable to a Chunk[T] by matching table
   * headers to field names and converting cell values.
   */
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
          sequence(fieldIndices).flatMap { indices =>
            val fromStrings =
              summonAll[Tuple.Map[p.MirroredElemTypes, FromString]].toList.asInstanceOf[List[FromString[_]]]
            val mappings = indices.zip(fromStrings)
            val mappedRows = dataTable.rows.map { row =>
              val cells = row.cells
              val values = mappings.map { case (idx, fs) =>
                if (idx >= cells.length) Left(s"Index $idx out of bounds for row")
                else fs.asInstanceOf[FromString[Any]].fromString(cells(idx))
              }
              sequence(values).map(vals => p.fromProduct(Tuple.fromArray(vals.toArray)))
            }
            sequence(mappedRows).map(Chunk.fromIterable)
          }
      }

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
      list.foldRight[Either[E, List[A]]](Right(Nil)) { (e, acc) =>
        for {
          a  <- e
          as <- acc
        } yield a :: as
      }
  }

  /**
   * A type class for extracting the element type Tag from a container type Tag
   * (e.g., Tag[List[T]] -> Tag[T]). Useful when a step’s input type is a
   * collection, and we need to map a DataTable to the element type.
   */
  trait ElementTagExtractor[C[_]] {

    /**
     * Extracts the Tag of the element type T from a Tag of a container type
     * C[T].
     *
     * @param tag
     *   The Tag of the container type (e.g., Tag[List[T]]).
     * @tparam T
     *   The element type, which must have an implicit Tag available.
     * @return
     *   The Tag of the element type T.
     */
    def extractElementTag[T: Tag](tag: Tag[C[T]]): Tag[T]
  }

  object ElementTagExtractor {

    /**
     * Provides an ElementTagExtractor for List, enabling extraction of Tag[T]
     * from Tag[List[T]]. Used in conjunction with DataTableMapper to map
     * DataTables to collections of elements.
     */
    implicit val listExtractor: ElementTagExtractor[List] = new ElementTagExtractor[List] {
      def extractElementTag[T: Tag](tag: Tag[List[T]]): Tag[T] = implicitly[Tag[T]]
    }

    /**
     * Summons an implicit ElementTagExtractor for the given container type C.
     *
     * @param extractor
     *   The implicit ElementTagExtractor instance.
     * @tparam C
     *   The container type (e.g., List).
     * @return
     *   The ElementTagExtractor instance.
     */
    def apply[C[_]](implicit extractor: ElementTagExtractor[C]): ElementTagExtractor[C] = extractor
  }

  /**
   * Retrieves a DataTableMapper for a type T given its Tag. This is a utility
   * to summon mappers dynamically when type information is provided via Tags,
   * as in the ZIOSteps where input types are captured as Tags.
   *
   * @param tag
   *   The Tag of the type T.
   * @param mapper
   *   The implicit DataTableMapper for T.
   * @tparam T
   *   The type to map the DataTable to.
   * @return
   *   The DataTableMapper for T.
   */
  def mapperForTag[T](tag: Tag[T])(implicit mapper: DataTableMapper[T]): DataTableMapper[T] = mapper

  /**
   * Retrieves a DataTableMapper for the element type T of a List[T], given a
   * Tag[List[T]]. This is used in steps where the input is a List[T] and a
   * DataTable needs to be mapped to Chunk[T], such as in Gherkin steps with
   * table arguments.
   *
   * @param listTag
   *   The Tag of the List[T] type.
   * @param extractor
   *   The implicit ElementTagExtractor for List.
   * @param mapper
   *   The implicit DataTableMapper for the element type T.
   * @tparam T
   *   The element type of the List, which must have an implicit Tag.
   * @return
   *   The DataTableMapper for the element type T.
   */
  def mapperForListElement[T: Tag](listTag: Tag[List[T]])(implicit
    extractor: ElementTagExtractor[List],
    mapper: DataTableMapper[T]
  ): DataTableMapper[T] = {
    val elementTag = extractor.extractElementTag(listTag)
    mapperForTag(elementTag)
  }
}
