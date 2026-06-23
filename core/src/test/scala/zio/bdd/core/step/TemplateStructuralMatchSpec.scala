package zio.bdd.core.step

import izumi.reflect.Tag
import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*
import zio.ZIO

/**
 * Issue #97: structurally matching a `@property` step template — one that still
 * has `<col>` placeholders in place of real values — against the registered
 * StepDefs, to find which extractor (and its `Tag[_]`) governs each
 * placeholder. Unlike `StepRegistry.findStep`, no real conforming value is
 * required at this stage; only literal/extractor positions need to align.
 */
object TemplateStructuralMatchSpec extends ZIOSpecDefault with DefaultTypedExtractor {

  private def makeStep[T <: Tuple](t: StepType, expr: StepExpression[T]) =
    StepDefImpl[Any, Unit, T](t, expr, (_: T) => ZIO.unit)

  private def registry(steps: List[StepDef[Any, Unit]]) = StepRegistryLive[Any, Unit](steps)

  // ── StepExpression.structuralMatch (pure, no registry involved) ──────────

  private val exprLevel = suite("StepExpression.structuralMatch")(
    test("single-placeholder step resolves the placeholder's column name and Tag") {
      val expr = StepExpression(List(Literal("balance is "), Extractor(int)))
      assertTrue(expr.structuralMatch("balance is <amount>").contains(List("amount" -> Tag[Int])))
    },
    test("multi-placeholder step resolves all placeholders in left-to-right order") {
      val expr = StepExpression(
        List(Literal("an account with balance "), Extractor(int), Literal(" and limit "), Extractor(long))
      )
      assertTrue(
        expr
          .structuralMatch("an account with balance <balance> and limit <limit>")
          .contains(List("balance" -> Tag[Int], "limit" -> Tag[Long]))
      )
    },
    test("step with no placeholders structurally matches a no-extractor template trivially") {
      val expr = StepExpression(List(Literal("a clean database")))
      assertTrue(expr.structuralMatch("a clean database").contains(Nil))
    },
    test("step with no placeholders does not apply when the StepExpression has an extractor") {
      // "balance is 100" has no `<col>` placeholder — there's nothing to supply the
      // extractor's type, so this must not spuriously match.
      val expr = StepExpression(List(Literal("balance is "), Extractor(int)))
      assertTrue(expr.structuralMatch("balance is 100").isEmpty)
    },
    test("mismatched literal text does not match even when extractor positions would align") {
      val expr = StepExpression(List(Literal("foo "), Extractor(int)))
      assertTrue(expr.structuralMatch("bar <x>").isEmpty)
    },
    test("trailing literal text after the last extractor must also align") {
      val expr = StepExpression(List(Literal("amount "), Extractor(int), Literal(" exactly")))
      assertTrue(expr.structuralMatch("amount <amount> exactly").contains(List("amount" -> Tag[Int])))
      assertTrue(expr.structuralMatch("amount <amount> roughly").isEmpty)
    }
  )

  // ── StepRegistry.resolveTemplateColumns (registry lookup + disambiguation) ─

  private val registryLevel = suite("StepRegistry.resolveTemplateColumns")(
    test("resolves a single-placeholder template to the matching StepDef's column/Tag") {
      val step = makeStep(StepType.GivenStep, StepExpression(List(Literal("balance is "), Extractor(int))))
      val reg  = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "balance is <amount>"))(
        equalTo(List("amount" -> Tag[Int]))
      )
    },
    test("resolves a multi-placeholder template across two extractors") {
      val step = makeStep(
        StepType.GivenStep,
        StepExpression(List(Literal("balance "), Extractor(int), Literal(" limit "), Extractor(long)))
      )
      val reg = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "balance <balance> limit <limit>"))(
        equalTo(List("balance" -> Tag[Int], "limit" -> Tag[Long]))
      )
    },
    test("a template with no placeholders resolves to an empty column list against a literal StepDef") {
      val step = makeStep(StepType.GivenStep, StepExpression(List(Literal("a clean database"))))
      val reg  = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "a clean database"))(equalTo(Nil))
    },
    test("a template with no placeholders does not match a StepDef that requires one") {
      val step = makeStep(StepType.GivenStep, StepExpression(List(Literal("balance is "), Extractor(int))))
      val reg  = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "balance is 100").either)(
        isLeft(isSubtype[NoMatchingTemplate](anything))
      )
    },
    test("no structurally matching StepDef fails with NoMatchingTemplate") {
      val step = makeStep(StepType.GivenStep, StepExpression(List(Literal("balance is "), Extractor(int))))
      val reg  = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "completely unrelated <col>").either)(
        isLeft(isSubtype[NoMatchingTemplate](anything))
      )
    },
    test("two structurally identical StepDefs with different extractor types are ambiguous") {
      // Same literal skeleton and extractor count — structural matching can't see that one
      // extractor produces Int and the other Long, since the placeholder has no real value yet.
      val intStep  = makeStep(StepType.GivenStep, StepExpression(List(Literal("amount is "), Extractor(int))))
      val longStep = makeStep(StepType.GivenStep, StepExpression(List(Literal("amount is "), Extractor(long))))
      val reg      = registry(List(intStep, longStep))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "amount is <amount>").either)(
        isLeft(isSubtype[AmbiguousTemplate](anything))
      )
    },
    test("AmbiguousTemplate error lists both structurally matching pattern descriptions") {
      val intStep  = makeStep(StepType.GivenStep, StepExpression(List(Literal("amount is "), Extractor(int))))
      val longStep = makeStep(StepType.GivenStep, StepExpression(List(Literal("amount is "), Extractor(long))))
      val reg      = registry(List(intStep, longStep))
      for {
        err <- reg.resolveTemplateColumns(StepType.GivenStep, "amount is <amount>").either
      } yield err match {
        case Left(AmbiguousTemplate(_, _, patterns)) => assertTrue(patterns.length == 2)
        case _                                       => assertTrue(false)
      }
    },
    test("disambiguation prefers the structurally matching StepDef with fewest extractors") {
      // "exact balance" (0 extractors) is more specific than "balance is <col>" (1 extractor);
      // only the literal one structurally matches a no-placeholder template, so no ambiguity.
      val literalStep = makeStep(StepType.GivenStep, StepExpression(List(Literal("exact balance"))))
      val genericStep = makeStep(StepType.GivenStep, StepExpression(List(Literal("balance is "), Extractor(int))))
      val reg         = registry(List(literalStep, genericStep))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "exact balance"))(equalTo(Nil))
    },
    test("falls back to And steps when the exact keyword has no structural match") {
      val andStep = makeStep(StepType.AndStep, StepExpression(List(Literal("role is "), Extractor(string))))
      val reg     = registry(List(andStep))
      assertZIO(reg.resolveTemplateColumns(StepType.ThenStep, "role is <role>"))(
        equalTo(List("role" -> Tag[String]))
      )
    }
  )

  def spec: Spec[TestEnvironment, Any] = suite("Template structural matching (#97)")(
    exprLevel,
    registryLevel
  )
}
