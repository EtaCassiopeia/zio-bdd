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
    },
    test("the same column name repeated with the same extractor type yields both occurrences in order") {
      val expr = StepExpression(
        List(Literal("transfer "), Extractor(int), Literal(" to cover a fee of "), Extractor(int))
      )
      assertTrue(
        expr
          .structuralMatch("transfer <amount> to cover a fee of <amount>")
          .contains(List("amount" -> Tag[Int], "amount" -> Tag[Int]))
      )
    },
    test("the same column name repeated with different extractor types still structurally aligns") {
      // structuralMatch is purely positional — it doesn't judge type consistency across
      // occurrences of the same name. StepRegistry.resolveTemplateColumns is what rejects this.
      val expr = StepExpression(List(Literal("from "), Extractor(int), Literal(" to "), Extractor(string)))
      assertTrue(
        expr
          .structuralMatch("from <amount> to <amount>")
          .contains(List("amount" -> Tag[Int], "amount" -> Tag[String]))
      )
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
    },
    test("the same column governed by two different extractor types fails with ConflictingColumnType") {
      val step = makeStep(
        StepType.GivenStep,
        StepExpression(List(Literal("from "), Extractor(int), Literal(" to "), Extractor(string)))
      )
      val reg = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "from <amount> to <amount>").either)(
        isLeft(isSubtype[ConflictingColumnType](anything))
      )
    },
    test("the same column repeated with the same extractor type succeeds with both occurrences") {
      val step = makeStep(
        StepType.GivenStep,
        StepExpression(List(Literal("transfer "), Extractor(int), Literal(" twice as "), Extractor(int)))
      )
      val reg = registry(List(step))
      assertZIO(reg.resolveTemplateColumns(StepType.GivenStep, "transfer <amount> twice as <amount>"))(
        equalTo(List("amount" -> Tag[Int], "amount" -> Tag[Int]))
      )
    }
  )

  // ── Concurrency: a StepRegistryLive is immutable after construction, so resolving
  // many templates against the same registry from parallel fibers must be as safe as
  // findStep already is — this is the lookup #98/#99 will call once per scenario, and
  // scenarios run under featureParallelism/scenarioParallelism (see ConcurrencySpec).

  private val concurrencySuite = suite("concurrent resolution")(
    test("200 concurrent resolveTemplateColumns calls on the same registry all return the same result") {
      val step = makeStep(
        StepType.GivenStep,
        StepExpression(List(Literal("balance "), Extractor(int), Literal(" limit "), Extractor(long)))
      )
      val reg      = registry(List(step))
      val expected = List("balance" -> Tag[Int], "limit" -> Tag[Long])
      for {
        results <- ZIO.foreachPar((1 to 200).toList)(_ =>
                     reg.resolveTemplateColumns(StepType.GivenStep, "balance <balance> limit <limit>")
                   )
      } yield assertTrue(results.forall(_ == expected))
    },
    test("concurrent calls resolving different templates against a shared registry don't cross-contaminate") {
      val balanceStep = makeStep(StepType.GivenStep, StepExpression(List(Literal("balance "), Extractor(int))))
      val nameStep    = makeStep(StepType.GivenStep, StepExpression(List(Literal("name "), Extractor(string))))
      val reg         = registry(List(balanceStep, nameStep))
      for {
        results <- ZIO.foreachPar((1 to 100).toList) { i =>
                     if (i % 2 == 0)
                       reg.resolveTemplateColumns(StepType.GivenStep, "balance <amount>").map(_ -> "balance")
                     else
                       reg.resolveTemplateColumns(StepType.GivenStep, "name <who>").map(_ -> "name")
                   }
      } yield assertTrue(
        results.forall {
          case (cols, "balance") => cols == List("amount" -> Tag[Int])
          case (cols, "name")    => cols == List("who" -> Tag[String])
          case _                 => false
        }
      )
    }
  )

  def spec: Spec[TestEnvironment, Any] = suite("Template structural matching (#97)")(
    exprLevel,
    registryLevel,
    concurrencySuite
  )
}
