package zio.bdd.core.step

import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*
import zio.{Scope, UIO, ZIO, ZLayer}

object StepRegistrySpec extends ZIOSpecDefault with DefaultTypedExtractor {

  private def makeStep[T <: Tuple](t: StepType, expr: StepExpression[T], f: T => ZIO[Any, Throwable, Unit]) =
    StepDefImpl[Any, Unit, T](t, expr, f)

  private val givenUser = makeStep(
    StepType.GivenStep,
    StepExpression(List(Literal("user "), Extractor(string))),
    { case Tuple1(name) => ZIO.debug(s"user: $name") }
  )

  private val whenAge = makeStep(
    StepType.WhenStep,
    StepExpression(List(Literal("age is "), Extractor(int))),
    { case Tuple1(age) => ZIO.debug(s"age: $age") }
  )

  // This And step acts as universal glue for any keyword
  private val andRole = makeStep(
    StepType.AndStep,
    StepExpression(List(Literal("role is "), Extractor(string))),
    { case Tuple1(role) => ZIO.debug(s"role: $role") }
  )

  private def registry(steps: List[StepDef[Any, Unit]]) = StepRegistryLive[Any, Unit](steps)
  private def si(text: String)                          = StepInput(text)

  private val stateLayer = ZLayer.succeed(new State[Unit] {
    def get: UIO[Unit]                     = ZIO.unit
    def update(f: Unit => Unit): UIO[Unit] = ZIO.unit
  })

  private val lookup = suite("Step lookup")(
    test("exact keyword type match returns the step effect") {
      val reg = registry(List(givenUser))
      for {
        effect <- reg.findStep(StepType.GivenStep, si("user Alice"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    },
    test("And step is used as fallback when exact keyword type is not found") {
      // There is no ThenStep — the And step should fill in
      val reg = registry(List(givenUser, andRole))
      for {
        effect <- reg.findStep(StepType.ThenStep, si("role is admin"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    },
    test("And step is also found when looked up directly as AndStep") {
      val reg = registry(List(andRole))
      for {
        effect <- reg.findStep(StepType.AndStep, si("role is admin"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    },
    test("step with an int extractor extracts the value and executes") {
      val reg = registry(List(whenAge))
      for {
        effect <- reg.findStep(StepType.WhenStep, si("age is 30"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    }
  )

  // ── No-match error ────────────────────────────────────────────────────────

  private val noMatch = suite("No-match error")(
    test("findStep returns Left(NoMatchingStep) when no step matches") {
      val reg    = registry(List(givenUser))
      val result = reg.findStep(StepType.GivenStep, si("unknown step"))
      assertZIO(result.either)(isLeft(isSubtype[NoMatchingStep](anything)))
    },
    test("NoMatchingStep error message contains a code snippet") {
      val reg = registry(List(givenUser))
      for {
        err <- reg.findStep(StepType.GivenStep, si("unknown step")).either
      } yield err match {
        case Left(NoMatchingStep(_, _, snippet)) =>
          assertTrue(snippet.contains("Given"), snippet.nonEmpty)
        case _ =>
          assertTrue(false)
      }
    },
    test("no fallback when step text does not match any registered step") {
      val reg    = registry(List(givenUser, whenAge)) // no matching text
      val result = reg.findStep(StepType.ThenStep, si("unknown step xyz"))
      assertZIO(result.either)(isLeft(anything))
    }
  )

  private val crossKeyword = suite("Cross-keyword fallback (Cucumber semantics)")(
    test("When step can be found when resolved as Given (via And inheritance)") {
      // Feature: Given ... / And a post request is sent → effective type = Given
      // But step is registered as When — should still match
      val whenPost = makeStep(
        StepType.WhenStep,
        StepExpression(List(Literal("a post request is sent"))),
        _ => ZIO.unit
      )
      val reg = registry(List(whenPost))
      for {
        effect <- reg.findStep(StepType.GivenStep, si("a post request is sent"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    },
    test("Then step can be found when looked up as When") {
      val thenStatus = makeStep(
        StepType.ThenStep,
        StepExpression(List(Literal("the status is 200"))),
        _ => ZIO.unit
      )
      val reg = registry(List(thenStatus))
      for {
        effect <- reg.findStep(StepType.WhenStep, si("the status is 200"))
        _      <- effect.provide(stateLayer ++ Scope.default)
      } yield assertCompletes
    },
    test("cross-keyword fallback does not apply when step text does not match at all") {
      val whenPost = makeStep(
        StepType.WhenStep,
        StepExpression(List(Literal("a post request is sent"))),
        _ => ZIO.unit
      )
      val reg = registry(List(whenPost))
      assertZIO(reg.findStep(StepType.GivenStep, si("completely different step")).either)(
        isLeft(anything)
      )
    }
  )

  private val ambiguity = suite("Ambiguity detection")(
    test("two steps with identical patterns return AmbiguousStep") {
      val step1 = makeStep(StepType.GivenStep, StepExpression(List(Literal("duplicate step"))), _ => ZIO.unit)
      val step2 = makeStep(StepType.GivenStep, StepExpression(List(Literal("duplicate step"))), _ => ZIO.unit)
      val reg   = registry(List(step1, step2))
      assertZIO(reg.findStep(StepType.GivenStep, si("duplicate step")).either)(
        isLeft(isSubtype[AmbiguousStep](anything))
      )
    },
    test("AmbiguousStep error lists both matching pattern descriptions") {
      val step1 = makeStep(StepType.GivenStep, StepExpression(List(Literal("ambiguous"))), _ => ZIO.unit)
      val step2 = makeStep(StepType.GivenStep, StepExpression(List(Literal("ambiguous"))), _ => ZIO.unit)
      val reg   = registry(List(step1, step2))
      for {
        err <- reg.findStep(StepType.GivenStep, si("ambiguous")).either
      } yield err match {
        case Left(AmbiguousStep(_, _, patterns)) => assertTrue(patterns.length == 2)
        case _                                   => assertTrue(false)
      }
    },
    test("non-ambiguous steps with similar prefixes are not flagged") {
      val step1 = makeStep(StepType.GivenStep, StepExpression(List(Literal("user Alice"))), _ => ZIO.unit)
      val step2 = makeStep(StepType.GivenStep, StepExpression(List(Literal("user Bob"))), _ => ZIO.unit)
      val reg   = registry(List(step1, step2))
      // "user Alice" matches only step1 — no ambiguity
      assertZIO(reg.findStep(StepType.GivenStep, si("user Alice")).either)(isRight(anything))
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepRegistry")(
    lookup,
    noMatch,
    ambiguity,
    crossKeyword
  )
}
