package zio.bdd.core.property

import izumi.reflect.Tag
import zio.*
import zio.bdd.core.step.{StepRegistry, ZIOSteps}
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

  def run[R: Tag, S: Tag: Default](
    scenario: Scenario,
    suite: ZIOSteps[R, S],
    genLookup: ColumnGenLookup
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
        runProperty(scenario, config, suite, genLookup)

  private def runProperty[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    lookup: ColumnGenLookup
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    resolveColumnGens(scenario, lookup).flatMap {
      case Left(err) =>
        ZIO.succeed(ScenarioResult(scenario, Nil, setupError = Some(Cause.fail(new RuntimeException(err)))))
      case Right(colGens) =>
        for {
          seed      <- config.seed.map(ZIO.succeed).getOrElse(Random.nextLong)
          replayRec <- if (config.replay) PropertyFailureStore.read(scenario) else ZIO.none
          result <- replayRec match
                      case Some(rec) => handleReplay(scenario, config, suite, colGens, rec, seed)
                      case None      => runFreshSamples(scenario, config, suite, colGens, seed)
        } yield result
    }

  private def handleReplay[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    colGens: List[ColumnGen],
    rec: PropertyFailureStore.FailureRecord,
    freshSeed: Long
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    val replayValues = colGens.map(cg => cg.name -> rec.shrunkValues.getOrElse(cg.name, ""))
    runOneSample(scenario, suite, replayValues).flatMap {
      case None =>
        // The previously-falsifying sample now passes — the bug is fixed. Clear the stale
        // failure record and proceed with a full fresh batch of samples instead of just
        // reporting the one-sample replay as the whole scenario result.
        PropertyFailureStore.clear(scenario) *> runFreshSamples(scenario, config, suite, colGens, freshSeed)
      case Some(failResult) =>
        // Still failing
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
    }

  private def runFreshSamples[R: Tag, S: Tag: Default](
    scenario: Scenario,
    config: PropertyTag.PropertyConfig,
    suite: ZIOSteps[R, S],
    colGens: List[ColumnGen],
    seed: Long
  ): ZIO[R & StepRegistry[R, S], Nothing, ScenarioResult] =
    ZIO.logAnnotate("propertyScenario", scenario.name) {
      // Run up to `config.samples` iterations. Short-circuit on first failure.
      // State is (sampleIndex, Option[failResult]); we stop when failResult is Some.
      ZIO
        .iterate((0, Option.empty[ScenarioResult]))(s => s._2.isEmpty && s._1 < config.samples) { s =>
          val idx = s._1
          sampleColumnValues(colGens, seed, idx).flatMap { colValues =>
            runOneSample(scenario, suite, colValues).flatMap {
              case None =>
                ZIO.succeed((idx + 1, None))
              case Some(failResult) =>
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
            case Some(failResult) => failResult
            case None             => buildPassResult(scenario, seed, s._1, colGens)
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
      scenario = scenario.copy(name = s"${scenario.name} [$label]"),
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

    val counterexampleLabel = shrunkValues.toList
      .sortBy(_._1)
      .map { case (col, v) => s"$col=$v (${generatorLabels.getOrElse(col, "?")})" }
      .mkString(", ")
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
      scenario = scenario.copy(name = s"${scenario.name} [seed=$seed]"),
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
    colValues: List[(String, String)]
  ): ZIO[R & StepRegistry[R, S], Nothing, Option[ScenarioResult]] =
    val substituted = substituteSteps(scenario, colValues.toMap)
    val meta        = zio.bdd.gherkin.ScenarioMetadata.from(substituted)
    ScenarioExecutor
      .executeScenario[R, S](substituted, suite)
      .provideSomeLayer[R & StepRegistry[R, S]](suite.scenarioLayer(meta).orDie)
      .map(result => if (result.isPassed) None else Some(result))

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
   * deterministically using `seed XOR sampleIndex` so each sample in a run is
   * distinct while remaining reproducible.
   */
  private def sampleColumnValues(
    colGens: List[ColumnGen],
    seed: Long,
    sampleIdx: Int
  ): UIO[List[(String, String)]] =
    ZIO.foreach(colGens) { cg =>
      val itemSeed = seed ^ (sampleIdx.toLong * 6364136223846793005L + 1442695040888963407L)
      sampleGenToString(cg.gen, itemSeed).map(v => cg.name -> v)
    }

  private def sampleGenToString(gen: Gen[Any, Any], seed: Long): UIO[String] =
    (for {
      _ <- TestRandom.setSeed(seed)
      v <- gen.sample.map(_.value.toString).runHead
    } yield v.getOrElse(""))
      .provideLayer(Sized.default ++ TestRandom.deterministic)

  // ── Column generator resolution ──────────────────────────────────────────

  private def resolveColumnGens(
    scenario: Scenario,
    lookup: ColumnGenLookup
  ): UIO[Either[String, List[ColumnGen]]] =
    ZIO.succeed {
      val placeholderRe = "<([^>]+)>".r
      val allPlaceholders = scenario.steps.flatMap { step =>
        placeholderRe.findAllMatchIn(step.pattern).map(_.group(1)).toList
      }.distinct

      val resolved = allPlaceholders.map { col =>
        val namedGen = scenario.columnGens.get(col).flatMap(HasGen.resolve)
        namedGen.orElse(lookup.byColumn(col)) match
          case Some(hg) => Right(ColumnGen(col, hg.gen.asInstanceOf[Gen[Any, Any]], hg.label))
          case None     => Left(s"No HasGen registered for column '$col'. Register `given HasGen[T]` for its type.")
      }
      val errors = resolved.collect { case Left(e) => e }
      if (errors.nonEmpty) Left(errors.mkString("; "))
      else Right(resolved.collect { case Right(cg) => cg })
    }

/**
 * Thrown when a property scenario is falsified. Message contains the
 * counterexample.
 */
class PropertyFalsifiedException(message: String) extends RuntimeException(message)

/**
 * Resolves a column name to a `HasGen` instance. Suites provide this by mapping
 * placeholder names to the `HasGen` for the corresponding `TypedExtractor`
 * type.
 */
trait ColumnGenLookup:
  def byColumn(columnName: String): Option[HasGen[?]]

object ColumnGenLookup:
  val empty: ColumnGenLookup = _ => None
