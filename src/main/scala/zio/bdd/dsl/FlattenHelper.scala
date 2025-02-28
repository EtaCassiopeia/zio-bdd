package zio.bdd.dsl

object FlattenHelper {
  def flatten[T](value: T): Any = value match {
    case () => () // Unit stays Unit
    case (a, b) => {
      val fa = flatten(a)
      val fb = flatten(b)
      (fa, fb) match {
        case ((), ()) => ()
        case ((), fb) => fb
        case (fa, ()) => fa
        case (fa, fb) => (fa, fb)
      }
    }
    case other => other // Preserve non-tuple, non-Unit values
  }
}
