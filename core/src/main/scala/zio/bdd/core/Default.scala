package zio.bdd.core

import zio.schema.Schema

trait Default[T] {
  def default: T
}

object Default {

  /** Derives a Default[T] instance from a Schema[T]. */
  implicit def schemaDefault[T](implicit schema: Schema[T]): Default[T] = new Default[T] {
    def default: T = schema.defaultValue match {
      case Right(value) => value
      case Left(error)  => throw new Exception(s"Failed to derive default for ${schema}: $error")
    }
  }

  /** Summon an instance of Default[T] for a given type T. */
  def apply[T](implicit instance: Default[T]): Default[T] = instance
}
