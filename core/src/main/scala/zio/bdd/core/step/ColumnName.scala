package zio.bdd.core.step

import scala.annotation.StaticAnnotation

/**
 * Annotation for mapping a Gherkin table header name to a case class field.
 *
 * When a case class field is annotated with `@ColumnName`, the `table[T]`
 * extractor uses the annotation value as the header name rather than the Scala
 * field name. This is an alternative to `tableWithMapping[T](map)` that keeps
 * the mapping co-located with the type definition.
 *
 * {{{
 * case class ProvisionDetails(
 *   @ColumnName("Account Open Date")  accountOpenDate: String,
 *   @ColumnName("Instrument Class")   instrumentClass: String,
 *   @ColumnName("Source Timestamp")   sourceTimestamp: String
 * )
 * }}}
 *
 * With this annotation, the Gherkin table:
 * {{{
 *   | Account Open Date | Instrument Class | Source Timestamp |
 *   | 2025-01-01        | SimpleSavings    | 2025-01-01       |
 * }}}
 *
 * is correctly deserialized to `ProvisionDetails("2025-01-01", "SimpleSavings",
 * "2025-01-01")`.
 *
 * @param name
 *   the exact header string as it appears in the Gherkin table
 */
final class ColumnName(val name: String) extends StaticAnnotation
