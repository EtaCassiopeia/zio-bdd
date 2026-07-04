package zio.bdd.core

import zio.schema.Schema

/**
 * Typeclass for providing a default value of type T.
 */
trait Default[T]:
  def default: T

object Default:

  /**
   * Derive Default[T] from Schema[T]'s default value. Fails fast at suite
   * startup if no default.
   */
  transparent inline given schemaDefault[T](using schema: Schema[T]): Default[T] =
    fromSchema(schema)

  // The Default instance is built here (a regular method), not inline, so the anonymous class is
  // compiled once rather than duplicated at every `schemaDefault` call site (Scala 3 E197).
  private def fromSchema[T](schema: Schema[T]): Default[T] =
    new Default[T]:
      val default: T = schema.defaultValue match
        case Right(value) => value
        case Left(error) =>
          throw new IllegalStateException(
            s"Cannot derive Default for ${schema.getClass.getSimpleName}: $error\n" +
              s"Hint: add default values to all fields of your state case class, or provide an explicit Default[T] instance."
          )

  /** Create a Default[T] from an explicit value — no Schema required. */
  def fromValue[T](value: => T): Default[T] = new Default[T]:
    lazy val default: T = value

  /** Alias for fromValue. Preferred spelling in companion-object style. */
  def from[T](value: => T): Default[T] = fromValue(value)

  /** Summon the Default[T] instance in scope. */
  def apply[T](using d: Default[T]): Default[T] = d
