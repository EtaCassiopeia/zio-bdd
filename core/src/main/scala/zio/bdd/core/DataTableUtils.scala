package zio.bdd.core

import izumi.reflect.Tag
import zio.bdd.gherkin.DataTable

object DataTableUtils {

  /**
   * Maps a DataTable to a List[T] given a Tag[List[T]]. Automatically summons
   * the DataTableMapper[T] and performs the mapping.
   *
   * @param listTag
   *   The Tag of the List[T] type.
   * @param dataTable
   *   The DataTable to map.
   * @tparam T
   *   The element type of the List, which must have an implicit Tag and
   *   DataTableMapper.
   * @return
   *   Either[String, List[T]] - Right with the mapped List[T], or Left with an
   *   error message.
   */
  def mapDataTableToList[T: Tag: DataTableMapper](
    listTag: Tag[List[T]],
    dataTable: DataTable
  ): Either[String, List[T]] = {
    val mapper = DataTableMapper.mapperForListElement(listTag)
    mapper.map(dataTable).map(_.toList)
  }
}
