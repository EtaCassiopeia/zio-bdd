package zio.bdd.core

import zio.*

trait Transformer[A, B] {
  def transform(a: A): ZIO[Any, Throwable, B]
}

object Transformer {
  implicit val stringToInt: Transformer[String, Int] = new Transformer[String, Int] {
    def transform(s: String): ZIO[Any, Throwable, Int] = ZIO.attempt(s.toInt)
  }

  implicit val stringToDouble: Transformer[String, Double] = new Transformer[String, Double] {
    def transform(s: String): ZIO[Any, Throwable, Double] = ZIO.attempt(s.toDouble)
  }

  def apply[A, B](implicit transformer: Transformer[A, B]): Transformer[A, B] = transformer
}
