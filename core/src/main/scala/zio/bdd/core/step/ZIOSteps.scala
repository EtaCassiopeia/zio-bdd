package zio.bdd.core.step

import zio.*
import zio.bdd.core.{Default, FeatureExecutor, FeatureResult, Hooks}
import zio.bdd.gherkin.{Feature, ScenarioMetadata}
import izumi.reflect.Tag
import scala.annotation.targetName

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Base trait for zio-bdd test suites. Extend this to define step registrations,
 * environment layers, and lifecycle hooks.
 *
 * ==Canonical composition pattern==
 *
 * Large test suites should be split across step trait files and composed at the
 * suite object level:
 *
 * {{{
 * // Step trait: the self-type constraint is critical
 * trait ProvisionSteps { self: ZIOSteps[AppEnv, AppState] =>
 *   Given("a valid provision body") { ... }
 *   When("a provision request is sent") { ... }
 *   Then("the ledger returns 200") { ... }
 * }
 *
 * // Suite object: mixes in all step traits
 * @Suite(featureDirs = Array("src/test/resources/features"))
 * object MySuite
 *     extends ZIOSteps[AppEnv, AppState]
 *     with ProvisionSteps
 *     with PostSteps
 *     with EODSteps:
 *   override def environment = AppEnv.layer
 * }}}
 *
 * ==Common compilation errors==
 *
 * '''Error: "value Given is not a member of ProvisionSteps"'''
 *
 * Cause: Missing `self: ZIOSteps[R, S] =>` self-type constraint on the step
 * trait. `Given`/`When`/`Then` are inherited from `ZIOSteps` and require the
 * self-type.
 *
 * Fix: Add `{ self: ZIOSteps[MyR, MyS] => ... }` to the trait body.
 *
 * '''Error: "type mismatch: found ZIO[R & State[S] & Scope, Throwable, Unit],
 * required ZIO[MyR & State[MyS], Throwable, Unit]"'''
 *
 * Cause: The step trait's `R` or `S` type parameter doesn't match the suite's.
 * Check that all step traits use exactly the same `R` and `S` as the suite
 * object.
 *
 * '''Error: "Ambiguous step definitions detected at suite startup"'''
 *
 * Cause: Two step definitions with identical text are registered (from
 * different trait mixins or accidentally duplicated). The framework detects
 * this at startup rather than silently running the wrong step.
 *
 * Fix: Rename or merge the duplicate step definitions.
 *
 * ==Type aliases==
 *
 * Step body helpers should be typed using the provided aliases:
 *
 * {{{
 * private def sendRequest(url: String): StepEffect = ...      // void
 * private def fetchAccount(): StepIO[Account]      = ...      // value-producing
 * }}}
 *
 * ==Resource lifecycle==
 *
 * Steps can acquire and release resources scoped to a single scenario:
 *
 * {{{
 * Given("a test database table exists") {
 *   ZIO.acquireRelease(
 *     DynamoDB.createTable(testTableName).orDie
 *   )(
 *     _ => DynamoDB.deleteTable(testTableName).orDie
 *   ).unit
 *   // Table is deleted when the scenario ends, even if steps fail
 * }
 * }}}
 *
 * ==Three-tier environment model==
 *
 * Override `globalLayer`, `featureLayer`, or `scenarioLayer` to control
 * resource sharing and per-scenario overrides:
 *
 * {{{
 * override def globalLayer     = sharedConnectionPool    // once per JVM
 * override def featureLayer    = sharedConnectionPool >>> freshSchema  // per feature
 * override def scenarioLayer(meta: ScenarioMetadata) =  // per scenario
 *   if (meta.tags.contains("use-mock")) mockHttpLayer else realHttpLayer
 * }}}
 *
 * @tparam R
 *   The ZIO environment type providing services to step bodies
 * @tparam S
 *   The scenario state type (use `TypeMap` for modular state)
 */
trait ZIOSteps[R: Tag, S: Tag: Default]
    extends Hooks[R, S]
    with GeneratedStepMethods[R, S]
    with DefaultTypedExtractor
    with StateOps[S] {

  type Step[I, O] = I => ZIO[R, Throwable, O]

  /**
   * The return type required by `Given`/`When`/`Then`/`And`/`But` step bodies.
   *
   * Use this to annotate private helper methods that are reused across steps:
   * {{{
   *   private def sendRequest(req: HttpRequest): StepEffect =
   *     for {
   *       client <- ZIO.service[HttpClient]
   *       resp   <- client.send(req)
   *       _      <- ScenarioContext.update(_.copy(lastResponse = resp))
   *     } yield ()
   *
   *   Given("a provision is sent") { sendRequest(provisionRequest) }
   * }}}
   *
   * Without this alias, the full type is `ZIO[R & State[S], Throwable, Unit]`.
   * Annotating private helpers with `StepEffect` produces clearer type errors
   * at the definition site rather than at the `Given`/`When`/`Then` call site.
   *
   * Note: `Scope` is available in the ZIO environment at runtime — steps can
   * use `ZIO.acquireRelease` without adding `Scope` to this type.
   */
  type StepEffect = ZIO[R & State[S], Throwable, Unit]

  /**
   * A parameterised variant of `StepEffect` for private helpers that produce a
   * value consumed by subsequent steps in a for-comprehension:
   *
   * {{{
   *   private def provisionAccount(body: ProvisionBody): StepIO[AccountId] =
   *     ZIO.serviceWithZIO[HttpClient](_.post("/provision", body))
   *       .map(resp => resp.accountId)
   *
   *   Given("a valid provisioned account") {
   *     for {
   *       id <- provisionAccount(ProvisionBody.default)
   *       _  <- ScenarioContext.update(_.copy(accountId = Some(id)))
   *     } yield ()
   *   }
   * }}}
   */
  type StepIO[+A] = ZIO[R & State[S], Throwable, A]

  // ── State-injecting step helpers ────────────────────────────────────────
  // These are defined directly on ZIOSteps (not in GeneratedStepMethods) so
  // they have access to the Tag[S] context bound from the class header.
  // `Tag[S]` is guaranteed by `ZIOSteps[R: Tag, S: Tag: Default]`.
  //
  // Usage:
  //   // Zero Gherkin params — state only:
  //   GivenS("a valid provision body") { s =>
  //     for { event <- gen(s.core.accountReferenceId); _ <- update(...) } yield ()
  //   }
  //
  //   // With Gherkin params — state + extracted tuple:
  //   ThenS("the ledger returns a " / string / " status code") { s => (code: String) =>
  //     assertTrue(s.http.statusCode == code.toInt, ...)
  //   }

  /** Given step (no extracted params) with state injection. */
  def GivenS(expr: StepExpression[EmptyTuple])(f: S => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, EmptyTuple](
        zio.bdd.gherkin.StepType.GivenStep,
        expr,
        (_: EmptyTuple) => State.get[S].flatMap(s => f(s))
      )
    )

  /**
   * Given step (with extracted params) with state injection. `f` is curried:
   * state first, then the Gherkin-extracted tuple.
   */
  @targetName("GivenSWithParams")
  def GivenS[A](expr: StepExpression[Tuple1[A]])(f: S => A => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, Tuple1[A]](
        zio.bdd.gherkin.StepType.GivenStep,
        expr,
        (t: Tuple1[A]) => State.get[S].flatMap(s => f(s)(t._1))
      )
    )

  @targetName("GivenSWithParams2")
  def GivenS[A, B](expr: StepExpression[(A, B)])(f: S => (A, B) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B)](
        zio.bdd.gherkin.StepType.GivenStep,
        expr,
        (t: (A, B)) => State.get[S].flatMap(s => f(s)(t._1, t._2))
      )
    )

  @targetName("GivenSWithParams3")
  def GivenS[A, B, C](expr: StepExpression[(A, B, C)])(f: S => (A, B, C) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B, C)](
        zio.bdd.gherkin.StepType.GivenStep,
        expr,
        (t: (A, B, C)) => State.get[S].flatMap(s => f(s)(t._1, t._2, t._3))
      )
    )

  /** When step (no extracted params) with state injection. */
  def WhenS(expr: StepExpression[EmptyTuple])(f: S => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, EmptyTuple](
        zio.bdd.gherkin.StepType.WhenStep,
        expr,
        (_: EmptyTuple) => State.get[S].flatMap(s => f(s))
      )
    )

  /** When step (with extracted params) with state injection. */
  @targetName("WhenSWithParams")
  def WhenS[A](expr: StepExpression[Tuple1[A]])(f: S => A => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, Tuple1[A]](
        zio.bdd.gherkin.StepType.WhenStep,
        expr,
        (t: Tuple1[A]) => State.get[S].flatMap(s => f(s)(t._1))
      )
    )

  @targetName("WhenSWithParams2")
  def WhenS[A, B](expr: StepExpression[(A, B)])(f: S => (A, B) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B)](
        zio.bdd.gherkin.StepType.WhenStep,
        expr,
        (t: (A, B)) => State.get[S].flatMap(s => f(s)(t._1, t._2))
      )
    )

  @targetName("WhenSWithParams3")
  def WhenS[A, B, C](expr: StepExpression[(A, B, C)])(f: S => (A, B, C) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B, C)](
        zio.bdd.gherkin.StepType.WhenStep,
        expr,
        (t: (A, B, C)) => State.get[S].flatMap(s => f(s)(t._1, t._2, t._3))
      )
    )

  /** Then step (no extracted params) with state injection. */
  def ThenS(expr: StepExpression[EmptyTuple])(f: S => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, EmptyTuple](
        zio.bdd.gherkin.StepType.ThenStep,
        expr,
        (_: EmptyTuple) => State.get[S].flatMap(s => f(s))
      )
    )

  /** Then step (with extracted params) with state injection. */
  @targetName("ThenSWithParams")
  def ThenS[A](expr: StepExpression[Tuple1[A]])(f: S => A => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, Tuple1[A]](
        zio.bdd.gherkin.StepType.ThenStep,
        expr,
        (t: Tuple1[A]) => State.get[S].flatMap(s => f(s)(t._1))
      )
    )

  @targetName("ThenSWithParams2")
  def ThenS[A, B](expr: StepExpression[(A, B)])(f: S => (A, B) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B)](
        zio.bdd.gherkin.StepType.ThenStep,
        expr,
        (t: (A, B)) => State.get[S].flatMap(s => f(s)(t._1, t._2))
      )
    )

  @targetName("ThenSWithParams3")
  def ThenS[A, B, C](expr: StepExpression[(A, B, C)])(f: S => (A, B, C) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B, C)](
        zio.bdd.gherkin.StepType.ThenStep,
        expr,
        (t: (A, B, C)) => State.get[S].flatMap(s => f(s)(t._1, t._2, t._3))
      )
    )

  /** And step (no extracted params) with state injection. */
  def AndS(expr: StepExpression[EmptyTuple])(f: S => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, EmptyTuple](
        zio.bdd.gherkin.StepType.AndStep,
        expr,
        (_: EmptyTuple) => State.get[S].flatMap(s => f(s))
      )
    )

  /** And step (with extracted params) with state injection. */
  @targetName("AndSWithParams")
  def AndS[A](expr: StepExpression[Tuple1[A]])(f: S => A => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, Tuple1[A]](
        zio.bdd.gherkin.StepType.AndStep,
        expr,
        (t: Tuple1[A]) => State.get[S].flatMap(s => f(s)(t._1))
      )
    )

  @targetName("AndSWithParams2")
  def AndS[A, B](expr: StepExpression[(A, B)])(f: S => (A, B) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B)](
        zio.bdd.gherkin.StepType.AndStep,
        expr,
        (t: (A, B)) => State.get[S].flatMap(s => f(s)(t._1, t._2))
      )
    )

  @targetName("AndSWithParams3")
  def AndS[A, B, C](expr: StepExpression[(A, B, C)])(f: S => (A, B, C) => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, (A, B, C)](
        zio.bdd.gherkin.StepType.AndStep,
        expr,
        (t: (A, B, C)) => State.get[S].flatMap(s => f(s)(t._1, t._2, t._3))
      )
    )

  /** But step (no extracted params) with state injection. */
  def ButS(expr: StepExpression[EmptyTuple])(f: S => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, EmptyTuple](
        zio.bdd.gherkin.StepType.ButStep,
        expr,
        (_: EmptyTuple) => State.get[S].flatMap(s => f(s))
      )
    )

  /** But step (with extracted params) with state injection. */
  @targetName("ButSWithParams")
  def ButS[A](expr: StepExpression[Tuple1[A]])(f: S => A => RIO[R & State[S] & Scope, Unit]): Unit =
    register(
      StepDefImpl[R, S, Tuple1[A]](
        zio.bdd.gherkin.StepType.ButStep,
        expr,
        (t: Tuple1[A]) => State.get[S].flatMap(s => f(s)(t._1))
      )
    )

  // ── Step registration ───────────────────────────────────────────────────

  private val steps: mutable.ListBuffer[StepDef[R, S]] = mutable.ListBuffer.empty
  // Sealed exactly once on the first getSteps call — prevents post-run registrations.
  // AtomicBoolean.compareAndSet makes the seal+validate happen-once even under concurrent access.
  private val sealed_ = new java.util.concurrent.atomic.AtomicBoolean(false)

  /**
   * Return all registered steps.
   *
   * On first call, validates the step registry for ambiguous patterns. Two
   * steps with identical expressions registered for the same step type throw
   * `AmbiguousStepDefinitionException` at suite startup — not mid-test.
   */
  private[core] def getSteps: List[StepDef[R, S]] = {
    if (sealed_.compareAndSet(false, true)) validateAmbiguity()
    steps.toList
  }

  /**
   * Protected alias for `getSteps` — allows subtraits (e.g. SuiteHooks) to
   * override `run`.
   */
  protected final def registeredSteps: List[StepDef[R, S]] = getSteps

  private[core] def register(step: StepDef[R, S]): Unit = {
    if (sealed_.get())
      throw new IllegalStateException(
        s"Cannot register step '${step.asInstanceOf[StepDefImpl[?, ?, ?]].stepExpr}' after the suite has started running. " +
          "Step registration must happen during object initialization."
      )
    steps += step
  }

  /**
   * Validate that no two registered steps have identical patterns for the same
   * step type. Called once at the first `getSteps` access (suite startup).
   */
  private def validateAmbiguity(): Unit = {
    val byTypeAndPattern = steps.toList.groupBy { case s: StepDefImpl[?, ?, ?] =>
      (s.stepType, s.stepExpr.toString)
    }
    val ambiguous = byTypeAndPattern.filter(_._2.length > 1)
    if (ambiguous.nonEmpty) {
      val details = ambiguous.map { case ((t, pat), defs) =>
        s"  $t '$pat' — registered ${defs.length} times"
      }.mkString("\n")
      throw new IllegalStateException(
        s"Ambiguous step definitions detected at suite startup. " +
          s"The following step expressions are registered multiple times:\n$details\n" +
          s"Rename or merge duplicate step definitions."
      )
    }
  }

  // ── Three-tier environment model ────────────────────────────────────────

  /**
   * Tier 1 — built once per JVM process and shared across all suite runs.
   *
   * Use this for expensive shared resources: connection pools, shared caches,
   * embedded databases that should start once and be reused.
   *
   * Default: delegates to `featureLayer`.
   */
  def globalLayer: ZLayer[Any, Throwable, R] = featureLayer

  /**
   * Tier 2 — built once per feature execution.
   *
   * Use this for per-feature state: a fresh database schema per feature, a
   * per-feature HTTP mock registration, etc.
   *
   * Default: delegates to `environment` (backward-compatible single layer).
   */
  def featureLayer: ZLayer[Any, Throwable, R] = environment

  /**
   * Tier 3 — built per scenario.
   *
   * Override to provide scenario-specific services. The scenario's
   * `ScenarioMetadata` (name, tags, file, line) is available for conditional
   * layer selection:
   *
   * {{{
   * override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, R] =
   *   if (meta.tags.contains("use-mock")) mockHttpLayer
   *   else realHttpLayer
   * }}}
   *
   * Default: delegates to `featureLayer`.
   */
  def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, R] = featureLayer

  /**
   * Called once per `@flags(key=value, ...)` tag annotation on a scenario.
   *
   * When a scenario has one or more `@flags(...)` tags, the framework expands
   * it into multiple runs — one per tag — calling `flagLayer` for each. The
   * `flags` map contains the key/value pairs parsed from that tag.
   *
   * Override to inject flag values into the environment layer for each
   * expansion:
   *
   * {{{
   * // Wire into zio-openfeature TestFeatureProvider:
   * override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]) =
   *   environment >>> OpenFeatureOverride.layer(flags)
   *
   * // Simple config override:
   * override def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]) =
   *   environment >>> FlagConfig.layer(flags)
   * }}}
   *
   * Default: delegates to `scenarioLayer(meta)` — flags are visible in metadata
   * but not injected into the environment. Override to use them.
   */
  def flagLayer(meta: ScenarioMetadata, flags: Map[String, String]): ZLayer[Any, Throwable, R] =
    scenarioLayer(meta)

  /**
   * Primary single-tier environment. Legacy entry point. Override this OR the
   * three-tier methods above — not both.
   */
  def environment: ZLayer[Any, Throwable, R] = ZLayer.empty.asInstanceOf[ZLayer[Any, Throwable, R]]

  /**
   * Override to set a default timeout for every step in this suite. `None`
   * means unlimited (default).
   *
   * {{{
   * override def stepTimeout: Option[Duration] = Some(Duration.fromSeconds(30))
   * }}}
   */
  def stepTimeout: Option[Duration] = _annotationStepTimeout

  // Internal: set by ZIOBDDFramework when the @Suite(stepTimeout=N) annotation is present.
  // A non-zero annotation value wins over None but does NOT override a subclass override of stepTimeout.
  private[core] var _annotationStepTimeout: Option[Duration] = None

  /**
   * Called by the framework to inject the annotation-configured timeout, but
   * only when the subclass has not overridden stepTimeout.
   */
  private[bdd] final def overrideStepTimeout(d: Duration): Unit =
    _annotationStepTimeout = Some(d)

  // ── Step DSL operators ──────────────────────────────────────────────────

  extension [Out <: Tuple](se: StepExpression[Out])
    def /(literal: String): StepExpression[Out] =
      StepExpression(se.parts :+ Literal(literal))
    def /[T](extractor: TypedExtractor[T]): StepExpression[Tuple.Concat[Out, Tuple1[T]]] =
      StepExpression(se.parts :+ Extractor(extractor))

  implicit def stringToStepExpression(str: String): StepExpression[EmptyTuple] =
    StepExpression(List(Literal(str)))

  /** Allows empty step bodies `Given("step") {}` to compile. */
  implicit def unitToStepEffect(u: Unit): ZIO[Any, Nothing, Unit] = ZIO.unit

  /**
   * Mark a step as pending (known-unimplemented, distinct from failed).
   */
  def pending(reason: String = "TODO"): ZIO[Any, Throwable, Unit] =
    ZIO.fail(new zio.bdd.core.PendingException(reason))

  /**
   * Snapshot helper: capture a value from state before a block, then assert
   * against it after.
   *
   * {{{
   * withSnapshot(_.accountBalance) { before =>
   *   When("a deposit is made") { ... } *>
   *   Then("balance increased") {
   *     ScenarioContext.get.flatMap(s =>
   *       Assertions.assertTrue(s.accountBalance > before)
   *     )
   *   }
   * }
   * }}}
   */
  def withSnapshot[A](lens: S => A)(
    body: A => ZIO[R & State[S] & Scope, Throwable, Unit]
  ): ZIO[R & State[S] & Scope, Throwable, Unit] =
    ScenarioContext.get.flatMap(s => body(lens(s)))

  // ── Execution ───────────────────────────────────────────────────────────

  def run(
    features: List[Feature],
    featureParallelism: Int = 1,
    scenarioParallelism: Int = 1,
    dryRun: Boolean = false
  ): ZIO[R, Nothing, List[FeatureResult]] =
    FeatureExecutor.executeFeatures[R, S](features, getSteps, this, featureParallelism, scenarioParallelism, dryRun)
}
