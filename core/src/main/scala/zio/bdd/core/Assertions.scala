package zio.bdd.core

import zio.*
import zio.test.Assertion
import scala.reflect.ClassTag

import zio.test.Assertion.*

object Assertions {
  private def evaluate[A](value: A, assertion: Assertion[A], message: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      if (!assertion.test(value)) throw new AssertionError(message)
    }

  /**
   * Assert a boolean condition inside a step body.
   *
   * IMPORTANT: Always use this with `<-` in a for-comprehension, NOT with
   * `.map`:
   *
   * Correct:
   * {{{
   *   Then("the greeting should be " / string) { (expected: String) =>
   *     ScenarioContext.get.flatMap(s => assertTrue(s.greeting == expected, s"Expected $expected, got ${s.greeting}"))
   *   }
   * }}}
   *
   * Wrong (assertion is silently discarded — the ZIO is never executed):
   * {{{
   *   ScenarioContext.get.map(s => assertTrue(s.greeting == expected))  // BUG
   * }}}
   */
  def assertTrue(condition: Boolean, message: String = "Assertion failed"): ZIO[Any, Throwable, Unit] =
    evaluate(condition, isTrue, message)

  def assertEquals[A](actual: A, expected: A, message: String = "Values not equal"): ZIO[Any, Throwable, Unit] =
    evaluate(actual, equalTo(expected), s"$message: expected $expected, got $actual")

  def assertThrows[E <: Throwable: ClassTag](
    zio: ZIO[Any, E, Any],
    message: String = "Expected exception not thrown"
  ): ZIO[Any, Nothing, Unit] =
    zio.either.map {
      case Left(_)  => ()
      case Right(_) => throw new AssertionError(message)
    }

  def assertZIO[A](
    actual: A,
    assertion: Assertion[A],
    message: String = "Assertion failed"
  ): ZIO[Any, Throwable, Unit] =
    evaluate(actual, assertion, message)

  def assertSome[A](actual: Option[A], message: String = "Expected Some, got None"): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isSome, message)

  def assertSomeEquals[A](
    actual: Option[A],
    expected: A,
    message: String = "Option value mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isSome(equalTo(expected)), s"$message: expected Some($expected), got $actual")

  def assertNone[A](actual: Option[A], message: String = "Expected None, got Some"): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isNone, message)

  def assertRight[A](actual: Either[?, A], message: String = "Expected Right, got Left"): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isRight, message)

  def assertRightEquals[A](
    actual: Either[?, A],
    expected: A,
    message: String = "Either value mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isRight(equalTo(expected)), s"$message: expected Right($expected), got $actual")

  def assertLeft[A](actual: Either[A, ?], message: String = "Expected Left, got Right"): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isLeft, message)

  def assertLeftEquals[A](
    actual: Either[A, ?],
    expected: A,
    message: String = "Either value mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, isLeft(equalTo(expected)), s"$message: expected Left($expected), got $actual")

  def assertContainsAll[A](
    actual: Iterable[A],
    expected: Iterable[A],
    message: String = "Collection mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual.toList, hasSameElements(expected.toList), message)

  def assertContainsSubset[A](
    actual: Iterable[A],
    expected: Iterable[A],
    message: String = "Collection subset mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, containsAllElements(expected), message)

  def assertHasField[A, B](
    actual: A,
    fieldName: String,
    getter: A => B,
    assertion: Assertion[B],
    message: String = "Field assertion failed"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, hasField(fieldName, getter, assertion), message)

  def assertFieldEquals[A, B](
    actual: A,
    fieldName: String,
    getter: A => B,
    expected: B,
    message: String = "Field value mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertHasField(
      actual,
      fieldName,
      getter,
      equalTo(expected),
      s"$message: expected $fieldName to be $expected, got ${getter(actual)}"
    )

  def assertNested[A, B](
    actual: A,
    path: String,
    getter: A => B,
    assertion: Assertion[B],
    message: String = "Nested assertion failed"
  ): ZIO[Any, Throwable, Unit] =
    assertZIO(actual, Assertion.assertion(path)(a => assertion.test(getter(a))), message)

  def assertNestedEquals[A, B](
    actual: A,
    path: String,
    getter: A => B,
    expected: B,
    message: String = "Nested value mismatch"
  ): ZIO[Any, Throwable, Unit] =
    assertNested(
      actual,
      path,
      getter,
      equalTo(expected),
      s"$message: expected $path to be $expected, got ${getter(actual)}"
    )

  /**
   * Run all assertions and collect every failure into a single
   * `MultipleAssertionError`.
   *
   * Unlike chaining with `*>`, this does NOT short-circuit on the first failure
   * — every assertion is always evaluated. Use this when a step needs to verify
   * several independent conditions and you want a single report showing all
   * failures at once.
   *
   * {{{
   *   Then("the response is well-formed") {
   *     ScenarioContext.get.flatMap { s =>
   *       Assertions.collectAll(
   *         Assertions.assertEquals(s.http.statusCode, 200),
   *         Assertions.assertTrue(s.http.body.nonEmpty),
   *         Assertions.assertTrue(s.http.body.contains("ledgerSequenceNumber"))
   *       )
   *     }
   *   }
   * }}}
   */
  def collectAll(assertions: ZIO[Any, Throwable, Unit]*): ZIO[Any, Throwable, Unit] =
    ZIO.foreach(assertions.toList)(_.either).flatMap { results =>
      val failures = results.collect { case Left(t) => Option(t.getMessage).getOrElse(t.getClass.getName) }
      if (failures.isEmpty) ZIO.unit
      else ZIO.fail(new MultipleAssertionError(failures))
    }

  private def containsAllElements[A](expected: Iterable[A]): Assertion[Iterable[A]] =
    Assertion.assertion("containsAllElements") { actual =>
      expected.forall(e => actual.exists(_ == e))
    }
}

/** Thrown by `Assertions.collectAll` when one or more sub-assertions fail. */
final class MultipleAssertionError(val failures: List[String])
    extends AssertionError(
      s"${failures.length} assertion(s) failed:\n" + failures.mkString("  - ", "\n  - ", "")
    )
