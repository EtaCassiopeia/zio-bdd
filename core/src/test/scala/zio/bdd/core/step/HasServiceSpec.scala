package zio.bdd.core.step

import zio.*
import zio.test.*
import zio.test.Assertion.*

/**
 * Tests for the HasService typeclass — environment capability constraints for
 * reusable step modules.
 *
 * HasService[A, R] expresses "environment R contains service A" and is derived
 * automatically from environment intersection types.
 */
object HasServiceSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment & Scope, Any] = suite("HasService")(
    suite("Implicit derivation")(
      test("identity: HasService[A, A] is satisfied") {
        val ev = summon[HasService[String, String]]
        assertTrue(ev != null)
      },
      test("left intersection: HasService[A, A & B] is satisfied") {
        val ev = summon[HasService[String, String & Int]]
        assertTrue(ev != null)
      },
      test("right intersection via transitivity: HasService[A, B & A] is satisfied") {
        val ev = summon[HasService[String, Int & String]]
        assertTrue(ev != null)
      },
      test("deeper intersection: HasService[A, B & C & A] is satisfied") {
        val ev = summon[HasService[Double, Int & Boolean & Double]]
        assertTrue(ev != null)
      }
    ),
    suite("HasServiceOps")(
      test("withService[A] retrieves the service from environment A") {
        for {
          value <- HasServiceOps.withService[String].provide(ZLayer.succeed("hello"))
        } yield assertTrue(value == "hello")
      },
      test("withService[A] works inside an A & B environment") {
        for {
          s <- HasServiceOps
                 .withService[String]
                 .provide(
                   ZLayer.succeed("world") ++ ZLayer.succeed(42)
                 )
        } yield assertTrue(s == "world")
      }
    )
  )
}
