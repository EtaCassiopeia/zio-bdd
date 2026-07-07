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
   * Retry `effect` until it succeeds or `maxTime` elapses, for asserting on
   * eventually-consistent systems (feature-flag propagation, async consumers,
   * mock reload) without a blind `ZIO.sleep`.
   *
   * On timeout the last underlying failure is surfaced — not a generic "timed
   * out" — because `retry` re-emits the effect's final error once the schedule
   * is exhausted. The effect stays interruptible, so an outer `stepTimeout` (or
   * any enclosing timeout) is a hard cap; keep `maxTime` ≤ that timeout.
   *
   * Only typed failures are retried. A defect (an exception thrown outside a
   * `ZIO.attempt` boundary, surfacing as `Cause.Die`) propagates on the first
   * attempt and is NOT retried — wrap effects that may throw in `ZIO.attempt`
   * so the throw becomes a retryable typed failure. The `assert*` helpers
   * already do this.
   *
   * {{{
   *   When("flag propagation is observed") {
   *     eventually(
   *       getFlagStatus.filterOrFail(_ == "ON")(RuntimeException("flag not ON yet")),
   *       maxTime = 8.seconds
   *     )
   *   }
   * }}}
   */
  def eventually[R, A](
    effect: ZIO[R, Throwable, A],
    maxTime: Duration = 10.seconds,
    schedule: Schedule[Any, Any, Any] = Schedule.exponential(50.millis).jittered && Schedule.spaced(500.millis)
  ): ZIO[R, Throwable, A] =
    effect.retry(schedule && Schedule.upTo(maxTime))

  /**
   * Probe a value with `fetch`, run `assertion` against it, and retry the pair
   * until the assertion holds or `maxTime` elapses. On timeout the last
   * assertion failure is surfaced (see [[eventually]]).
   *
   * {{{
   *   Then("the balance is eventually settled") {
   *     eventuallyAssert(fetchBalance)(b => assertEquals(b, expected))
   *   }
   * }}}
   */
  def eventuallyAssert[R, A](
    fetch: ZIO[R, Throwable, A]
  )(
    assertion: A => ZIO[Any, Throwable, Unit],
    maxTime: Duration = 10.seconds
  ): ZIO[R, Throwable, Unit] =
    eventually(fetch.flatMap(assertion), maxTime)

  /**
   * The inverse of [[eventually]]: assert that `condition` *stays* true for the
   * whole `duration`, re-probing every `interval`. Fails fast on the first
   * violation, surfacing that underlying failure — a transient breach that
   * recovers mid-`interval` between probes can still be missed, so pick an
   * `interval` fine enough for the property under test.
   *
   * The wait stays interruptible, so an outer `stepTimeout` (or any enclosing
   * timeout) is a hard cap; keep `duration` ≤ that timeout. As with
   * [[eventually]], only typed failures count as a violation — a defect from
   * `condition` (an exception thrown outside a `ZIO.attempt` boundary)
   * propagates immediately.
   *
   * {{{
   *   Then("the balance stays zero after the cancelled transaction") {
   *     during(ScenarioContext.get.flatMap(s => assertEquals(s.balance, 0)), duration = 2.seconds)
   *   }
   * }}}
   */
  def during[R](
    condition: ZIO[R, Throwable, Unit],
    duration: Duration,
    interval: Duration = 200.millis
  ): ZIO[R, Throwable, Unit] =
    condition.repeat(Schedule.spaced(interval) && Schedule.upTo(duration)).unit

  // ── Collection quantifiers ─────────────────────────────────────────────────
  // Assert a predicate over the elements of a collection (every / some / no /
  // exactly-n / between). The predicate is soft-evaluated against every element
  // so a single failure message can list all offenders with their indices.

  private def labelTag(label: String): String = if (label.nonEmpty) s"[$label] " else ""

  // Run `pred` against every element, recording (index, element, failure message
  // when the predicate failed — None when it satisfied).
  private def probeAll[A](
    collection: Iterable[A],
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Nothing, List[(Int, A, Option[String])]] =
    ZIO.foreach(collection.toList.zipWithIndex) { case (a, i) =>
      pred(a).either.map {
        case Right(_) => (i, a, None)
        case Left(t)  => (i, a, Some(Option(t.getMessage).getOrElse(t.getClass.getName)))
      }
    }

  private def renderElems[A](elems: List[(Int, A, Option[String])]): String =
    elems.map { case (i, a, msg) => s"  - [index $i] $a" + msg.fold("")(m => s" ($m)") }.mkString("\n")

  /**
   * Assert every element of `collection` satisfies `pred`. An empty collection
   * passes vacuously.
   */
  def assertForAll[A](collection: Iterable[A], label: String = "")(
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Throwable, Unit] =
    probeAll(collection, pred).flatMap { probed =>
      val offenders = probed.filter(_._3.isDefined)
      if (offenders.isEmpty) ZIO.unit
      else
        ZIO.fail(
          new AssertionError(
            s"${labelTag(label)}assertForAll failed: ${offenders.size} of ${probed.size} element(s) did not satisfy the predicate:\n${renderElems(offenders)}"
          )
        )
    }

  /** Assert at least one element of `collection` satisfies `pred`. */
  def assertExists[A](collection: Iterable[A], label: String = "")(
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Throwable, Unit] =
    probeAll(collection, pred).flatMap { probed =>
      if (probed.exists(_._3.isEmpty)) ZIO.unit
      else {
        val detail = if (probed.nonEmpty) s":\n${renderElems(probed)}" else ""
        ZIO.fail(
          new AssertionError(
            s"${labelTag(label)}assertExists failed: no element of ${probed.size} satisfied the predicate$detail"
          )
        )
      }
    }

  /**
   * Assert no element of `collection` satisfies `pred`. Named to avoid a clash
   * with `assertNone[A](actual: Option[A])`, which asserts an `Option` is
   * empty.
   */
  def assertNoneSatisfy[A](collection: Iterable[A], label: String = "")(
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Throwable, Unit] =
    probeAll(collection, pred).flatMap { probed =>
      val satisfying = probed.filter(_._3.isEmpty)
      if (satisfying.isEmpty) ZIO.unit
      else
        ZIO.fail(
          new AssertionError(
            s"${labelTag(label)}assertNoneSatisfy failed: ${satisfying.size} element(s) satisfied the predicate:\n${renderElems(satisfying)}"
          )
        )
    }

  /** Assert exactly `n` elements of `collection` satisfy `pred`. */
  def assertExactly[A](n: Int, collection: Iterable[A], label: String = "")(
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Throwable, Unit] =
    probeAll(collection, pred).flatMap { probed =>
      val satisfying = probed.filter(_._3.isEmpty)
      if (satisfying.size == n) ZIO.unit
      else
        ZIO.fail(
          new AssertionError(
            s"${labelTag(label)}assertExactly($n) failed: ${satisfying.size} of ${probed.size} element(s) satisfied the predicate:\n${renderElems(satisfying)}"
          )
        )
    }

  /**
   * Assert between `min` and `max` (inclusive) elements of `collection` satisfy
   * `pred`.
   */
  def assertBetween[A](min: Int, max: Int, collection: Iterable[A], label: String = "")(
    pred: A => ZIO[Any, Throwable, Unit]
  ): ZIO[Any, Throwable, Unit] =
    probeAll(collection, pred).flatMap { probed =>
      val satisfying = probed.filter(_._3.isEmpty)
      val count      = satisfying.size
      if (count >= min && count <= max) ZIO.unit
      else {
        val detail = if (satisfying.nonEmpty) s":\n${renderElems(satisfying)}" else ""
        ZIO.fail(
          new AssertionError(
            s"${labelTag(label)}assertBetween($min, $max) failed: $count of ${probed.size} element(s) satisfied the predicate$detail"
          )
        )
      }
    }

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
