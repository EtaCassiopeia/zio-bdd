package zio.bdd.core.step

import zio.*

/**
 * Typeclass expressing that environment `R` contains service `A`.
 *
 * This is the foundation for reusable step modules that don't couple to a
 * concrete `R` type — they only require that `R` contains the services they
 * need.
 *
 * Usage:
 * {{{
 * // A reusable HTTP step library — doesn't need to know the full R
 * trait HttpSteps[R](using HasService[HttpClient, R]) { self: ZIOSteps[R, ?] =>
 *   Given("GET " / string / " returns " / int) { (url: String, expected: Int) =>
 *     ZIO.serviceWithZIO[HttpClient](_.get(url).map(_.status)).flatMap { actual =>
 *       Assertions.assertEquals(actual, expected)
 *     }
 *   }
 * }
 *
 * // A suite that includes the reusable steps
 * @Suite(...)
 * object MySuite extends ZIOSteps[HttpClient & DynamoDB, AppState]
 *     with HttpSteps[HttpClient & DynamoDB]   // works because HasService[HttpClient, R] is satisfied
 *     with DynamoSteps[HttpClient & DynamoDB]
 * }}}
 *
 * The evidence `HasService[A, R]` is derived automatically when `R` includes
 * `A` as a component of its environment intersection.
 */
sealed trait HasService[A, R]

object HasService:
  /** When R = A exactly, the evidence is trivially satisfied. */
  given identity[A]: HasService[A, A] = new HasService[A, A] {}

  /** When R includes A in an intersection (R = A & B & ...). */
  given intersection[A, B, R](using HasService[A, R]): HasService[A, B & R] =
    new HasService[A, B & R] {}

  /** Flip: A & B satisfies HasService[A, A & B] */
  given leftIntersection[A, B]: HasService[A, A & B] =
    new HasService[A, A & B] {}

/**
 * Extension methods on `ZIO` that use `HasService` to access a service from `R`
 * without requiring the full `R` type to be known.
 *
 * Steps can use `ZIO.service[HttpClient]` directly — use `HasService` as a
 * constraint to document and enforce which services a step module requires.
 */
object HasServiceOps:
  /** Access a service `A` from environment `R`. */
  def withService[A: zio.Tag]: ZIO[A, Nothing, A] = ZIO.service[A]
