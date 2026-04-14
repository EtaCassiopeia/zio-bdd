package zio.bdd.core.step

import zio.bdd.gherkin.DataTable

case class StepInput(
  text: String,
  table: Option[DataTable] = None,
  /** Doc string argument from a triple-quoted block in the feature file. */
  docString: Option[String] = None
)
