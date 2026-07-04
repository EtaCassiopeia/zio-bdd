package zio.bdd.core.step

import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*
import zio.{Ref, Scope, UIO, ZIO, ZLayer}

object StepRegistrySpec extends ZIOSpecDefault with DefaultTypedExtractor {

  private def makeStep[T <: Tuple](t: StepType, expr: StepExpression[T], f: T => ZIO[Any, Throwable, Unit]) =
    StepDefImpl[Any, Unit, T](t, expr, f)

  private val givenUser = makeStep(
    StepType.GivenStep,
    StepExpression(List(Literal("user "), Extractor(string))),
    (t: Tuple1[String]) => ZIO.debug(s"user: ${t._1}")
  )

  private val whenAge = makeStep(
    StepType.WhenStep,
    StepExpression(List(Literal("age is "), Extractor(int))),
    (t: Tuple1[Int]) => ZIO.debug(s"age: ${t._1}")
  )

  // This And step acts as universal glue for any keyword
  private val andRole = makeStep(
    StepType.AndStep,
    StepExpression(List(Literal("role is "), Extractor(string))),
    (t: Tuple1[String]) => ZIO.debug(s"role: ${t._1}")
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

  // ── Specificity: overlapping prefixes (#186) ────────────────────────────────

  // The 2-arg step from the issue: its first `{string}` `.*` fallback can greedily swallow
  // the middle of a line meant for the 6-arg step (up to the final ` returns text `), so
  // both full-match. The more specific (more literal) definition must win, not the one with
  // fewest extractors.
  private def stubShort(winner: Ref[String]) = makeStep(
    StepType.GivenStep,
    StepExpression[(String, String)](
      List(Literal("a stub GET "), Extractor(string), Literal(" returns text "), Extractor(string))
    ),
    (_: (String, String)) => winner.set("short")
  )

  private def stubLong(winner: Ref[String]) = makeStep(
    StepType.GivenStep,
    StepExpression[(String, String, String, String, String, String)](
      List(
        Literal("a stub GET "),
        Extractor(string),
        Literal(" requiring query "),
        Extractor(string),
        Literal(" = "),
        Extractor(string),
        Literal(" and query "),
        Extractor(string),
        Literal(" = "),
        Extractor(string),
        Literal(" returns text "),
        Extractor(string)
      )
    ),
    (_: (String, String, String, String, String, String)) => winner.set("long")
  )

  private val longLine =
    "a stub GET \"/q\" requiring query \"a\" = \"1\" and query \"b\" = \"2\" returns text \"matched\""

  // Wires `steps` into a registry, runs the definition matched for `input`, and returns
  // which one fired — each step writes its label to the shared `winner` Ref when executed.
  private def winnerAfter(steps: Ref[String] => List[StepDef[Any, Unit]], stepType: StepType, input: String) =
    for {
      winner <- Ref.make("none")
      effect <- registry(steps(winner)).findStep(stepType, si(input))
      _      <- effect.provide(stateLayer ++ Scope.default)
      who    <- winner.get
    } yield who

  private val specificity = suite("Specificity (overlapping prefixes)")(
    test("longer more specific overlapping step wins over shorter one (#186)") {
      winnerAfter(w => List(stubShort(w), stubLong(w)), StepType.GivenStep, longLine)
        .map(who => assertTrue(who == "long"))
    },
    test("winner is independent of registration order (#186)") {
      winnerAfter(w => List(stubLong(w), stubShort(w)), StepType.GivenStep, longLine)
        .map(who => assertTrue(who == "long"))
    },
    test("fully-literal step still wins over a wildcard string variant (#186)") {
      def steps(w: Ref[String]) = List(
        makeStep(
          StepType.ThenStep,
          StepExpression[Tuple1[String]](List(Literal("the response body is "), Extractor(string))),
          (_: Tuple1[String]) => w.set("wildcard")
        ),
        makeStep(
          StepType.ThenStep,
          StepExpression[EmptyTuple](List(Literal("the response body is not empty"))),
          (_: EmptyTuple) => w.set("literal")
        )
      )
      winnerAfter(steps, StepType.ThenStep, "the response body is not empty")
        .map(who => assertTrue(who == "literal"))
    },
    // Guards the secondary tie-break key: when two matches pin the same number of literal
    // characters, the one with fewer extractors must still win. Here both have literalLen 3;
    // the wildcard's `{string}` matches the empty tail, so only `-extractorCount` separates
    // them. Without the secondary key this would be a false ambiguity.
    test("on a literal-length tie, fewer extractors still wins (#186)") {
      def steps(w: Ref[String]) = List(
        makeStep(
          StepType.GivenStep,
          StepExpression[Tuple1[String]](List(Literal("abc"), Extractor(string))),
          (_: Tuple1[String]) => w.set("wildcard")
        ),
        makeStep(
          StepType.GivenStep,
          StepExpression[EmptyTuple](List(Literal("abc"))),
          (_: EmptyTuple) => w.set("literal")
        )
      )
      winnerAfter(steps, StepType.GivenStep, "abc").map(who => assertTrue(who == "literal"))
    },
    // Two structurally different patterns that tie on both keys (each: 1 literal char,
    // 1 extractor) and both full-match must still be reported as ambiguous, not silently
    // resolved by registration order.
    test("structurally different steps that tie on specificity are ambiguous (#186)") {
      val prefix = makeStep(
        StepType.GivenStep,
        StepExpression[Tuple1[String]](List(Literal("a"), Extractor(string))),
        (_: Tuple1[String]) => ZIO.unit
      )
      val suffix = makeStep(
        StepType.GivenStep,
        StepExpression[Tuple1[String]](List(Extractor(string), Literal("b"))),
        (_: Tuple1[String]) => ZIO.unit
      )
      val reg = registry(List(prefix, suffix))
      assertZIO(reg.findStep(StepType.GivenStep, si("ab")).either)(
        isLeft(isSubtype[AmbiguousStep](anything))
      )
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("StepRegistry")(
    lookup,
    noMatch,
    ambiguity,
    crossKeyword,
    specificity
  )
}
