package zio.bdd.core.step

import zio.bdd.gherkin.DataTable

case class StepInput(text: String, table: Option[DataTable] = None)
