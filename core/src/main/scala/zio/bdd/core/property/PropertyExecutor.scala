package zio.bdd.core.property

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepRegistry, TemplateLookupError, ZIOSteps}
import zio.bdd.core.{Default, ScenarioExecutor, ScenarioResult, StepResult}
import zio.bdd.gherkin.{PropertyTag, Scenario, Step, StepType}
import zio.test.{Gen, Sized, TestRandom}

/**
 * Executes `@property` scenarios by sampling values from the `HasGen[T]`
 * registry and running the scenario body for each sample.
 *
 * Does NOT reuse `zio.test.check` — zio-bdd steps fail with `Throwable` while
 * `check` expects a `TestResult`/`BoolAlgebra` return.
 *
 * v1 algorithm:
 *   1. Resolve each column → `HasGen` instance. 2. If a replay record exists
 *      for this scenario, run the stored counterexample first. 3. Loop
 *      `samples` times, substituting sampled values into step placeholders. 4.
 *      On failure: persist the failing seed + values, annotate the result. 5.
 *      Emit one `ScenarioResult` for the whole property run.
 *
 * Full shrink-tree walking is deferred to v2. v1 records the seed that
 * triggered the failure so developers can replay it exactly.
 */
object PropertyExecutor:

  case class ColumnGen(name: String, gen: Gen[Any, Any], label: String)

  /**
   * What running one sample produced — distinguishes a genuine property
   * falsification from an infrastructure problem (a failing `beforeScenario`
   * hook, an unimplemented/`@pending` step) that happened to occur while
   * running a sample.
   *
   * This distinction matters because `Errored` results must NOT be persisted to
   * `PropertyFailureStore` or rendered as a `[counterexample]` — doing so would
   * misreport "the environment is broken" or "this step isn't implemented yet"
   * as "this property is falsified by these generated values", which is a
   * different (and misleading) claim. See the regression tests in
   * `PropertyExecutorSpec` for both failure modes.
   */
  private enum SampleOutcome:
    case Passed
    case Falsified(result: ScenarioResult)
    case Errored(result: ScenarioResult)

  private object SampleOutcome:
    def from(result: ScenarioResult): SampleOutcome =
      if (result.isPassed) Passed
      else if (result.setupError.isDefined || result.hasPending) Errored(result)
      else Falsified(result)

  /**
   * Separator between `col=val (gen)` entries in a `[counterexample]` step
   * pattern. A plain comma would be ambiguous — a domain type's default
   * `toString` (e.g. a case class) commonly contains one (`Address(Main
   * St,London)`) — so this uses a token vanishingly unlikely to appear in
   * generated text instead of escaping (and thus visually mangling) the values.
   * `PrettyReporter.counterexampleTable` splits on this same separator.
   */
  val counterexampleEntrySep = " ‖ "

  def run[R: Tag, S: Tag: Default](
    scenario: Scenario,
    suite: ZIOSteps[R, S],
    genLookup: ColumnGenLookup,
    flags: Map[String, String] = Map.empty,
    dryRun: Boolean = false
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    scenario.propertyConfig match
      case None =>
        ZIO.succeed(
          ScenarioResult(
            scenario,
            Nil,
            setupError = Some(
              Cause.fail(
                new IllegalStateException(
                  s"PropertyExecutor called on scenario without @property config: ${scenario.name}"
                )
              )
            )
          )
        )
      case Some(config) =>
        runProperty(scenario, config, suite, genLookup, flags, dryRun)

  private def runProperty[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    lookup: ColumnGenLookup,
    flags: Map[String, String],
    dryRun: Boolean
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    resolveColumnGens[R, S](scenario, lookup).flatMap {
      case Left(err) =>
        ZIO.succeed(ScenarioResult(scenario, Nil, setupError = Some(Cause.fail(new RuntimeException(err)))))
      case Right(colGens) =>
        if (dryRun)
          // --dry-run validates step matching without executing step bodies. Sampling the
          // configured `samples` count for real would defeat that — instead, validate column
          // resolution (already done above) and run exactly one dry-run sample to confirm
          // every step pattern matches, without looping or touching the failure store.
          runDryRunSample(scenario, suite, colGens, flags)
        else
          for {
            seed      <- config.seed.map(ZIO.succeed).getOrElse(Random.nextLong)
            replayRec <- if (config.replay) PropertyFailureStore.read(scenario) else ZIO.none
            result <- replayRec match
                        case Some(rec) => handleReplay(scenario, config, suite, colGens, rec, seed, flags)
                        case None      => runFreshSamples(scenario, config, suite, colGens, seed, flags)
          } yield result
    }

  private def runDryRunSample[R: Tag, S: Tag: Default](
    scenario: Scenario,
    suite: ZIOSteps[R, S],
    colGens: List[ColumnGen],
    flags: Map[String, String]
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    sampleColumnValues(colGens, seed = 0L, sampleIdx = 0).flatMap { colValues =>
      val substituted = substituteSteps(scenario, colValues.toMap)
      val meta        = zio.bdd.gherkin.ScenarioMetadata.from(substituted, flags)
      val layer       = if (flags.isEmpty) suite.scenarioLayer(meta) else suite.flagLayer(meta, flags)
      ScenarioExecutor
        .executeScenario[R, S](substituted, suite, dryRun = true, flagValues = flags)
        .provideSomeLayer[R & StepRegistry[R, S]](layer.orDie)
        .map { result =>
          val genSummary = colGens.map(cg => s"${cg.name} (${cg.label})").mkString(", ")
          val summaryStep = Step(
            stepType = StepType.GivenStep,
            pattern = s"[property] dry-run: step patterns validated — generators: $genSummary"
          )
          result.copy(
            // stableId preserves the pre-rename id so logs collected during this run (tagged
            // with the original scenario id by FeatureExecutor's logAnnotate, before this
            // rename happens) are still found by reporters looking up by `scenario.id`.
            scenario = scenario.copy(name = s"${scenario.name} [dry-run]", stableId = Some(scenario.id)),
            stepResults = StepResult(summaryStep, Right(())) +: result.stepResults
          )
        }
    }

  private def handleReplay[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    colGens: List[ColumnGen],
    rec: PropertyFailureStore.FailureRecord,
    freshSeed: Long,
    flags: Map[String, String]
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    val replayValues = colGens.map(cg => cg.name -> rec.shrunkValues.getOrElse(cg.name, ""))
    runOneSample(scenario, suite, replayValues, flags).flatMap {
      case SampleOutcome.Passed =>
        // The previously-falsifying sample now passes — the bug is fixed. Clear the stale
        // failure record and proceed with a full fresh batch of samples instead of just
        // reporting the one-sample replay as the whole scenario result.
        PropertyFailureStore.clear(scenario) *> runFreshSamples(scenario, config, suite, colGens, freshSeed, flags)
      case SampleOutcome.Falsified(failResult) =>
        // Still failing the same way.
        ZIO.succeed(
          buildFailureResult(
            scenario = scenario,
            seed = rec.seed,
            sampleIndex = rec.sampleIndex,
            shrunkValues = rec.shrunkValues,
            generatorLabels = colGens.map(cg => cg.name -> cg.label).toMap,
            originalResult = failResult,
            isReplay = true
          )
        )
      case SampleOutcome.Errored(result) =>
        // The replay run hit a setup error or a pending step, not the original falsification —
        // that's an infra/environment problem, not evidence the bug still reproduces (or
        // doesn't). Surface it as-is and leave the failure record untouched so a later run can
        // still attempt the real replay once the environment/step is fixed.
        ZIO.succeed(result)
    }

  private def runFreshSamples[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    colGens: List[ColumnGen],
    seed: Long,
    flags: Map[String, String]
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    ZIO.logAnnotate("propertyScenario", scenario.name) {
      // Run up to `config.samples` iterations. Short-circuit on the first non-Passed
      // outcome. State is (sampleIndex, Option[terminal result]); we stop once that's Some —
      // for either a genuine falsification (persisted to the failure store, wrapped as a
      // [counterexample]) or an infra error (surfaced as-is, NOT persisted — see SampleOutcome).
      ZIO
        .iterate((0, Option.empty[ScenarioResult]))(s => s._2.isEmpty && s._1 < config.samples) { s =>
          val idx = s._1
          sampleColumnValues(colGens, seed, idx).flatMap { colValues =>
            runOneSample(scenario, suite, colValues, flags).flatMap {
              case SampleOutcome.Passed =>
                ZIO.succeed((idx + 1, None))
              case SampleOutcome.Errored(result) =>
                ZIO.succeed((idx + 1, Some(result)))
              case SampleOutcome.Falsified(failResult) =>
                val shrunkValues    = colValues.toMap
                val generatorLabels = colGens.map(cg => cg.name -> cg.label).toMap
                PropertyFailureStore
                  .write(scenario, seed, idx, shrunkValues, generatorLabels)
                  .as(
                    (
                      idx + 1,
                      Some(
                        buildFailureResult(
                          scenario,
                          seed,
                          idx,
                          shrunkValues,
                          generatorLabels,
                          failResult,
                          isReplay = false
                        )
                      )
                    )
                  )
            }
          }
        }
        .map { s =>
          s._2 match {
            case Some(terminalResult) => terminalResult
            case None                 => buildPassResult(scenario, seed, s._1, colGens)
          }
        }
    }

  // ── Result builders ──────────────────────────────────────────────────────

  /**
   * Pass result: one synthetic "summary" step surfaces the generator labels and
   * sample count in the `<steps>` block so CI dashboards and the Cucumber
   * Reports plugin show something informative instead of an empty step list.
   */
  private def buildPassResult(
    scenario: Scenario,
    seed: Long,
    sampleCount: Int,
    colGens: List[ColumnGen]
  ): ScenarioResult =
    val genSummary = colGens.map(cg => s"${cg.name} (${cg.label})").mkString(", ")
    val label      = s"$sampleCount samples passed, seed=$seed"
    val summaryStep = Step(
      stepType = StepType.GivenStep,
      pattern = s"[property] $label — generators: $genSummary"
    )
    ScenarioResult(
      // stableId: see the matching comment in runDryRunSample.
      scenario = scenario.copy(name = s"${scenario.name} [$label]", stableId = Some(scenario.id)),
      stepResults = List(StepResult(summaryStep, Right(())))
    )

  /**
   * Failure result: prepends a synthetic "[counterexample]" step to the real
   * step results from the failing sample run.
   *
   * This means the `<steps>` block in JUnit XML shows:
   *   1. A failed "[counterexample] col=val (gen), ..." step carrying the
   *      summary message — visible in the Jenkins/GitHub Actions step detail.
   *      2. The real Given/When/Then steps from the falsifying sample, with the
   *      exact step that failed marked as "failed" and subsequent steps as
   *      "skipped".
   *
   * The failure is carried by the counterexample step's outcome (not
   * setupError), so ScenarioResult.error resolves through the normal
   * stepResults path and the JUnit <failure message=...> attribute is populated
   * correctly.
   */
  private def buildFailureResult(
    scenario: Scenario,
    seed: Long,
    sampleIndex: Int,
    shrunkValues: Map[String, String],
    generatorLabels: Map[String, String],
    originalResult: ScenarioResult,
    isReplay: Boolean
  ): ScenarioResult =
    val replayNote = if (isReplay) " [replayed from failure store]" else ""
    val summary    = s"Falsified after ${sampleIndex + 1} samples (seed=$seed)$replayNote"

    // Column values are arbitrary user-generated strings — a domain type's default toString
    // (e.g. a case class: `Address(Main St,London)`) commonly contains a literal comma, which
    // would make a plain ", "-joined list ambiguous to split back into entries. `entrySep` is
    // a token vanishingly unlikely to appear in generated text, so entries split unambiguously
    // without needing to escape (and thus visually mangle) the values themselves; kept in sync
    // with PrettyReporter.counterexampleTable, which parses this exact format.
    val counterexampleLabel = shrunkValues.toList
      .sortBy(_._1)
      .map { case (col, v) => s"$col=$v (${generatorLabels.getOrElse(col, "?")})" }
      .mkString(PropertyExecutor.counterexampleEntrySep)
    val counterexampleStep = Step(
      stepType = StepType.GivenStep,
      pattern = s"[counterexample] $counterexampleLabel"
    )
    val counterexampleResult = StepResult(
      step = counterexampleStep,
      outcome = Left(
        Cause.fail(
          new PropertyFalsifiedException(
            s"$summary\nMinimal counterexample: $counterexampleLabel"
          )
        )
      )
    )

    originalResult.copy(
      // stableId: see the matching comment in runDryRunSample.
      scenario = scenario.copy(name = s"${scenario.name} [seed=$seed]", stableId = Some(scenario.id)),
      stepResults = counterexampleResult +: originalResult.stepResults,
      // Error is now carried by the first StepResult, not setupError.
      // ScenarioResult.error already walks stepResults first, so this is consistent.
      setupError = None
    )

  /**
   * Run the scenario once with the given column → string value substitution.
   */
  private def runOneSample[R: Tag, S: Tag: Default](
    scenario: Scenario,
    suite: ZIOSteps[R, S],
    colValues: List[(String, String)],
    flags: Map[String, String]
  ): ZIO[R & StepRegistry[R, S], Nothing, SampleOutcome] =
    val substituted = substituteSteps(scenario, colValues.toMap)
    val meta        = zio.bdd.gherkin.ScenarioMetadata.from(substituted, flags)
    val layer       = if (flags.isEmpty) suite.scenarioLayer(meta) else suite.flagLayer(meta, flags)
    ScenarioExecutor
      .executeScenario[R, S](substituted, suite, flagValues = flags)
      .provideSomeLayer[R & StepRegistry[R, S]](layer.orDie)
      .map(SampleOutcome.from)

  /** Substitute `<placeholder>` in step patterns with sampled string values. */
  private def substituteSteps(scenario: Scenario, values: Map[String, String]): Scenario =
    val placeholderRe = "<([^>]+)>".r
    val substitutedSteps = scenario.steps.map { step =>
      val newPattern = placeholderRe.replaceAllIn(
        step.pattern,
        m => java.util.regex.Matcher.quoteReplacement(values.getOrElse(m.group(1), m.matched))
      )
      step.copy(pattern = newPattern)
    }
    scenario.copy(steps = substitutedSteps, propertyConfig = None)

  /**
   * Sample one string value per column using the column's `Gen`, seeded
   * deterministically from `(seed, sampleIndex, columnIndex)` so each sample in
   * a run is distinct, each column within a sample is independent — even when
   * two columns share the exact same `Gen` (e.g. two `HasGen[Int]` columns must
   * not always be equal) — while the whole run remains reproducible from `seed`
   * alone.
   */
  private def sampleColumnValues(
    colGens: List[ColumnGen],
    seed: Long,
    sampleIdx: Int
  ): UIO[List[(String, String)]] =
    ZIO.foreach(colGens.zipWithIndex) { case (cg, colIdx) =>
      sampleGenToString(cg.gen, mixSeed(seed, sampleIdx, colIdx)).map(v => cg.name -> v)
    }

  /**
   * Mix the property seed with the sample and column index into one
   * well-distributed seed.
   */
  private def mixSeed(seed: Long, sampleIdx: Int, colIdx: Int): Long =
    val bySample = seed ^ (sampleIdx.toLong * 6364136223846793005L + 1442695040888963407L)
    bySample ^ (colIdx.toLong * 2685821657736338717L + 1L)

  private def sampleGenToString(gen: Gen[Any, Any], seed: Long): UIO[String] =
    (for {
      _ <- TestRandom.setSeed(seed)
      v <- gen.sample.map(_.value.toString).runHead
    } yield v.getOrElse(""))
      .provideLayer(Sized.default ++ TestRandom.deterministic)

  // ── Column generator resolution ──────────────────────────────────────────
  //
  // Resolution order for each `<col>` placeholder:
  //   1. Named override from the column header (`| col: genName |`)
  //   2. The suite's explicit `columnGenLookup`
  //   3. Automatic type-based lookup: find the Tag[_] the column's extractor
  //      produces (StepRegistry.resolveTemplateColumns, #97) and resolve it via
  //      HasGen.resolveByTag (#98)
  //   4. Setup error
  //
  // Tiers 1-2 are pure (no StepRegistry needed). Tier 3 only runs for columns
  // still unresolved after 1-2, and only queries StepRegistry for steps that
  // actually mention one of those columns — a suite that already wires every
  // column explicitly (tiers 1-2 resolve everything) never touches tier 3 at
  // all, so existing fully-explicit suites see zero behavior or performance
  // change.

  private def resolveColumnGens[R: Tag, S: Tag](
    scenario: Scenario,
    lookup: ColumnGenLookup
  ): URIO[StepRegistry[R, S], Either[String, List[ColumnGen]]] = {
    val placeholderRe = "<([^>]+)>".r
    val allPlaceholders = scenario.steps.flatMap { step =>
      placeholderRe.findAllMatchIn(step.pattern).map(_.group(1)).toList
    }.distinct

    // A named header override (`| col: genName |`) that doesn't resolve against HasGen's
    // named registry is a setup error, not a silent fall-through to tiers 2-3 — falling
    // through would let a typo'd generator name go unnoticed and quietly resolve to a
    // *different* generator (columnGenLookup's, or the automatic by-type one) instead of
    // erroring, which defeats the point of naming an override at all.
    def explicit(col: String): Either[String, Option[HasGen[?]]] =
      scenario.columnGens.get(col) match
        case Some(genName) =>
          HasGen
            .resolve(genName)
            .map(hg => Some(hg))
            .toRight(
              s"Column '$col' specifies generator '$genName' via header override " +
                s"(`| $col: $genName |`), but no generator is registered under that name. " +
                s"""Register it with `HasGen.named("$genName")(...)`."""
            )
        case None => Right(lookup.byColumn(col))

    val explicitOutcomes: Map[String, Either[String, Option[HasGen[?]]]] =
      allPlaceholders.map(col => col -> explicit(col)).toMap

    val namedOverrideErrors: List[String] = explicitOutcomes.values.collect { case Left(e) => e }.toList
    val erroredCols: Set[String]          = explicitOutcomes.collect { case (col, Left(_)) => col }.toSet

    val explicitResolved: Map[String, HasGen[?]] =
      explicitOutcomes.collect { case (col, Right(Some(hg))) => col -> hg }
    val unresolved = allPlaceholders.filterNot(col => explicitResolved.contains(col) || erroredCols.contains(col))

    val byType: URIO[StepRegistry[R, S], Map[String, Either[String, HasGen[?]]]] =
      if (unresolved.isEmpty) ZIO.succeed(Map.empty)
      else resolveByType[R, S](scenario, unresolved.toSet)

    byType.map { byTypeResolved =>
      val resolved = allPlaceholders.filterNot(erroredCols.contains).map { col =>
        explicitResolved
          .get(col)
          .map(Right(_))
          .getOrElse(
            byTypeResolved.getOrElse(
              col,
              Left(s"No HasGen registered for column '$col'. Register `given HasGen[T]` for its type.")
            )
          )
          .map(hg => col -> hg)
      }
      val tierErrors = resolved.collect { case Left(e) => e }
      val errors     = namedOverrideErrors ++ tierErrors
      if (errors.nonEmpty) Left(errors.mkString("; "))
      else
        Right(resolved.collect { case Right((col, hg)) =>
          ColumnGen(col, hg.gen.asInstanceOf[Gen[Any, Any]], hg.label)
        })
    }
  }

  /**
   * Tier 3: resolve `columns` by inferring each one's type from the step
   * extractor that governs it, then looking that type up in `HasGen`'s
   * Tag-keyed registry. Only the steps mentioning one of `columns` are
   * structurally matched.
   */
  private def mentionsColumn(step: Step, col: String): Boolean = step.pattern.contains(s"<$col>")

  private def resolveByType[R: Tag, S: Tag](
    scenario: Scenario,
    columns: Set[String]
  ): URIO[StepRegistry[R, S], Map[String, Either[String, HasGen[?]]]] = {
    val relevantSteps = scenario.steps.filter(step => columns.exists(mentionsColumn(step, _)))
    ZIO
      .foreach(relevantSteps) { step =>
        StepRegistry.resolveTemplateColumns[R, S](step.stepType, step.pattern).either.map(step -> _)
      }
      .map { perStep =>
        columns.toList.map(col => col -> resolveOneColumnByType(col, perStep)).toMap
      }
  }

  private def resolveOneColumnByType(
    col: String,
    perStep: List[(Step, Either[TemplateLookupError, List[(String, Tag[?])]])]
  ): Either[String, HasGen[?]] = {
    val mentions = perStep.filter { case (step, _) => mentionsColumn(step, col) }
    // Distinct: the same step can legitimately mention `col` more than once (the same
    // sampled value substitutes into every occurrence), which would otherwise duplicate
    // entries here without indicating any real conflict.
    val seen: List[(Tag[?], String)] = mentions.collect { case (step, Right(cols)) =>
      cols.collect { case (name, tag) if name == col => tag -> step.pattern }
    }.flatten.distinct
    val distinctTags = seen.map(_._1).distinct

    distinctTags match {
      case Nil =>
        // No step told us this column's type — surface why, preferring the most direct cause.
        val message = mentions.collectFirst { case (step, Left(err)) =>
          s"Column '$col': could not determine its type from step \"${step.pattern}\" (${err.toException.getMessage})."
        }
          .getOrElse(s"No HasGen registered for column '$col'. Register `given HasGen[T]` for its type.")
        Left(message)
      case tag :: Nil =>
        HasGen.resolveByTag(tag) match {
          case Some(hg) => Right(hg)
          case None =>
            val name = typeName(tag)
            Left(
              s"Column '$col' was inferred as $name (from its step's extractor) but no HasGen is " +
                s"registered for that type. Register one via `given HasGen[$name] with ...` then " +
                s"`HasGen.registerType(HasGen[$name])`, or add an explicit `columnGenLookup` entry for '$col'."
            )
        }
      case _ =>
        val detail = seen.map { case (tag, pattern) => s"${typeName(tag)} (from \"$pattern\")" }.mkString(", ")
        Left(
          s"Column '$col' has conflicting inferred types across steps: $detail. " +
            "Use `columnGenLookup` to disambiguate."
        )
    }
  }

  /**
   * Render a Tag's underlying type name for error messages, e.g. `Tag[Money]`
   * -> `"Money"`. `tag.tag` is the underlying `LightTypeTag`, whose own
   * `toString` already renders just the type name — more robust than parsing
   * `Tag.toString`'s `"Tag[...]"` wrapper.
   */
  private def typeName(tag: Tag[?]): String = tag.tag.toString

/**
 * Thrown when a property scenario is falsified. Message contains the
 * counterexample.
 */
class PropertyFalsifiedException(message: String) extends RuntimeException(message)

/**
 * Resolves a column name to a `HasGen` instance. This is the *override* path —
 * most columns resolve automatically (named header override, then automatic
 * type-based lookup via `HasGen.resolveByTag`; see `resolveColumnGens` above).
 * Provide an entry here when you want a different generator than the automatic
 * choice, or to disambiguate a column whose inferred type conflicts across
 * steps.
 */
trait ColumnGenLookup:
  def byColumn(columnName: String): Option[HasGen[?]]

object ColumnGenLookup:
  val empty: ColumnGenLookup = _ => None
