package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for Assertions.collectAll and MultipleAssertionError (#37 soft
 * assertions).
 */
object AssertionsSpec extends ZIOSpecDefault {

  private val collectAllTests = suite("Assertions.collectAll")(
    test("all passing assertions succeeds") {
      Assertions
        .collectAll(
          Assertions.assertTrue(1 == 1),
          Assertions.assertEquals("a", "a"),
          Assertions.assertTrue(List(1, 2).nonEmpty)
        )
        .as(assertCompletes)
    },
    test("empty assertion list succeeds") {
      Assertions.collectAll().as(assertCompletes)
    },
    test("single failing assertion fails with MultipleAssertionError") {
      Assertions
        .collectAll(
          Assertions.assertTrue(false, "condition was false")
        )
        .either
        .map {
          case Left(e: MultipleAssertionError) =>
            assertTrue(e.failures.length == 1, e.failures.head.contains("condition was false"))
          case Left(other) =>
            assertTrue(false)
          case Right(_) =>
            assertTrue(false)
        }
    },
    test("mixed passing and failing assertions collects all failures") {
      Assertions
        .collectAll(
          Assertions.assertTrue(true),
          Assertions.assertTrue(false, "first failure"),
          Assertions.assertEquals("x", "y", "second failure"),
          Assertions.assertTrue(true)
        )
        .either
        .map {
          case Left(e: MultipleAssertionError) =>
            assertTrue(
              e.failures.length == 2,
              e.failures.exists(_.contains("first failure")),
              e.failures.exists(_.contains("second failure"))
            )
          case Left(other) =>
            assertTrue(false)
          case Right(_) =>
            assertTrue(false)
        }
    },
    test("failure message includes count") {
      Assertions
        .collectAll(
          Assertions.assertTrue(false, "a"),
          Assertions.assertTrue(false, "b"),
          Assertions.assertTrue(false, "c")
        )
        .either
        .map {
          case Left(e: MultipleAssertionError) =>
            assertTrue(
              e.getMessage.contains("3 assertion(s) failed"),
              e.failures.length == 3
            )
          case _ =>
            assertTrue(false)
        }
    },
    test("each sub-assertion in a failing collectAll runs independently") {
      var counter = 0
      val bump    = ZIO.succeed(counter += 1)

      Assertions
        .collectAll(
          bump *> Assertions.assertTrue(false, "a"),
          bump *> Assertions.assertTrue(false, "b"),
          bump *> Assertions.assertTrue(false, "c")
        )
        .either
        .map { _ =>
          assertTrue(counter == 3)
        }
    }
  )

  private val multipleAssertionErrorTests = suite("MultipleAssertionError")(
    test("stores all failure messages") {
      val e = new MultipleAssertionError(List("x", "y"))
      assertTrue(e.failures == List("x", "y"))
    },
    test("getMessage includes all failures") {
      val e = new MultipleAssertionError(List("alpha", "beta"))
      assertTrue(
        e.getMessage.contains("alpha"),
        e.getMessage.contains("beta"),
        e.getMessage.contains("2 assertion(s) failed")
      )
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("Assertions")(
    collectAllTests,
    multipleAssertionErrorTests
  )
}
