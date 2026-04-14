package zio.bdd.core.step

import zio.*
import zio.bdd.gherkin.StepType
import scala.compiletime.{erasedValue, summonInline}

/**
 * Convert a Tuple type to the function type expected by step body handlers.
 *
 * Resolved at compile time via Scala 3 match types — no sbt code generator
 * needed. Steps with arity > 8 produce a compile error (no silent truncation).
 */
type TupleToFn[R, S, T <: Tuple] <: Any = T match
  case EmptyTuple               => RIO[R & State[S] & Scope, Unit]
  case Tuple1[a]                => a => RIO[R & State[S] & Scope, Unit]
  case (a, b)                   => (a, b) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c)                => (a, b, c) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d)             => (a, b, c, d) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e)          => (a, b, c, d, e) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f)       => (a, b, c, d, e, f) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f, g)    => (a, b, c, d, e, f, g) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f, g, h) => (a, b, c, d, e, f, g, h) => RIO[R & State[S] & Scope, Unit]

/**
 * Like `TupleToFn` but with the scenario state `S` injected as the first
 * (curried) parameter.
 *
 * Use with `GivenS`/`WhenS`/`ThenS`/`AndS`/`ButS` to receive the current state
 * directly in the step body without a manual `s <- ScenarioContext.get`:
 *
 * {{{
 * // Without state injection (old style):
 * Given("a valid provision body") {
 *   for {
 *     s     <- ScenarioContext.get
 *     event <- generateEvent(s.core.accountReferenceId)
 *     _     <- ScenarioLens.update[AppState, ProvisionState](_.copy(payload = event.payload))
 *   } yield ()
 * }
 *
 * // With state injection (new style — s is injected, no ScenarioContext.get needed):
 * GivenS("a valid provision body") { s =>
 *   for {
 *     event <- generateEvent(s.core.accountReferenceId)
 *     _     <- ScenarioLens.update[AppState, ProvisionState](_.copy(payload = event.payload))
 *   } yield ()
 * }
 *
 * // With Gherkin parameters — curried: state first, then extracted params:
 * ThenS("the ledger returns a " / string / " status code") { s => (code: String) =>
 *   assertTrue(s.http.statusCode == code.toInt, s"Expected $code, got ${s.http.statusCode}")
 * }
 * }}}
 *
 * The state snapshot is taken at the moment the step executes (equivalent to
 * one `FiberRef.get`). If you need to re-read state after an intermediate
 * update within the same step body, call `ScenarioContext.get` explicitly for
 * the second read.
 */
type TupleToFnS[R, S, T <: Tuple] <: Any = T match
  case EmptyTuple               => S => RIO[R & State[S] & Scope, Unit]
  case Tuple1[a]                => S => a => RIO[R & State[S] & Scope, Unit]
  case (a, b)                   => S => (a, b) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c)                => S => (a, b, c) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d)             => S => (a, b, c, d) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e)          => S => (a, b, c, d, e) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f)       => S => (a, b, c, d, e, f) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f, g)    => S => (a, b, c, d, e, f, g) => RIO[R & State[S] & Scope, Unit]
  case (a, b, c, d, e, f, g, h) => S => (a, b, c, d, e, f, g, h) => RIO[R & State[S] & Scope, Unit]

/**
 * Inline step registration — replaces the sbt code generator entirely.
 *
 * Mix in `InlineStepMethods` to use the inline
 * `Given`/`When`/`Then`/`And`/`But` DSL. The arity dispatch is resolved at
 * compile time via `TupleToFn` match type.
 *
 * {{{
 * object MySuite extends ZIOSteps[R, S] with InlineStepMethods[R, S]:
 *   Given("a user " / string / " with age " / int) { (name: String, age: Int) =>
 *     ScenarioContext.update(s => s.copy(users = s.users + (name -> User(name, age))))
 *   }
 * }}}
 *
 * The generated `GeneratedStepMethods` trait (from the sbt plugin) remains
 * available for backward compatibility. Suites can use either or both.
 */
trait InlineStepMethods[R, S] { self: ZIOSteps[R, S] =>

  /**
   * Register a step of any arity (0-8) using the `TupleToFn` match type. The
   * function type `f` is resolved at the call site by the compiler.
   */
  transparent inline def step[Out <: Tuple](
    stepType: StepType,
    expr: StepExpression[Out]
  )(inline f: TupleToFn[R, S, Out]): Unit = {
    val adapted: Out => RIO[R & State[S] & Scope, Unit] =
      inline erasedValue[Out] match
        case _: EmptyTuple =>
          val fn = f.asInstanceOf[RIO[R & State[S] & Scope, Unit]]
          (_: Out) => fn
        case _: Tuple1[?] =>
          val fn = f.asInstanceOf[Any => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => fn(t.asInstanceOf[Tuple1[Any]]._1)
        case _: (?, ?) =>
          val fn = f.asInstanceOf[(Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => { val tt = t.asInstanceOf[(Any, Any)]; fn(tt._1, tt._2) }
        case _: (?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => { val tt = t.asInstanceOf[(Any, Any, Any)]; fn(tt._1, tt._2, tt._3) }
        case _: (?, ?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => { val tt = t.asInstanceOf[(Any, Any, Any, Any)]; fn(tt._1, tt._2, tt._3, tt._4) }
        case _: (?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => { val tt = t.asInstanceOf[(Any, Any, Any, Any, Any)]; fn(tt._1, tt._2, tt._3, tt._4, tt._5) }
        case _: (?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => {
            val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any)]; fn(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6)
          }
        case _: (?, ?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => {
            val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any)];
            fn(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6, tt._7)
          }
        case _: (?, ?, ?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => {
            val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any)];
            fn(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6, tt._7, tt._8)
          }
    self.register(StepDefImpl[R, S, Out](stepType, expr, adapted))
  }

  /**
   * Register a state-injecting step of any arity (0-8) using the `TupleToFnS`
   * match type. The current scenario state `S` is read once at step execution
   * time and passed as the first (curried) argument to `f`.
   *
   * The adaptation is: `State.get[S].flatMap(s => f(s)(extractedParams...))` —
   * one `FiberRef.get` per step invocation, equivalent to the manual pattern.
   */
  transparent inline def stepS[Out <: Tuple](
    stepType: StepType,
    expr: StepExpression[Out]
  )(inline f: TupleToFnS[R, S, Out]): Unit = {
    val adapted: Out => RIO[R & State[S] & Scope, Unit] =
      inline erasedValue[Out] match
        case _: EmptyTuple =>
          val fn = f.asInstanceOf[Any => RIO[R & State[S] & Scope, Unit]]
          (_: Out) => State.get[S].flatMap(s => fn(s))
        case _: Tuple1[?] =>
          val fn = f.asInstanceOf[Any => Any => RIO[R & State[S] & Scope, Unit]]
          (t: Out) => State.get[S].flatMap(s => fn(s)(t.asInstanceOf[Tuple1[Any]]._1))
        case _: (?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any)]; fn(s)(tt._1, tt._2)
            }
        case _: (?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any)]; fn(s)(tt._1, tt._2, tt._3)
            }
        case _: (?, ?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any, Any)]; fn(s)(tt._1, tt._2, tt._3, tt._4)
            }
        case _: (?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any, Any, Any)]; fn(s)(tt._1, tt._2, tt._3, tt._4, tt._5)
            }
        case _: (?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any)]; fn(s)(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6)
            }
        case _: (?, ?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any)];
              fn(s)(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6, tt._7)
            }
        case _: (?, ?, ?, ?, ?, ?, ?, ?) =>
          val fn = f.asInstanceOf[Any => (Any, Any, Any, Any, Any, Any, Any, Any) => RIO[R & State[S] & Scope, Unit]]
          (t: Out) =>
            State.get[S].flatMap { s =>
              val tt = t.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any)];
              fn(s)(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6, tt._7, tt._8)
            }
    self.register(StepDefImpl[R, S, Out](stepType, expr, adapted))
  }

  /** Given step. */
  transparent inline def Given[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFn[R, S, Out]): Unit =
    step(StepType.GivenStep, expr)(f)

  /** When step. */
  transparent inline def When[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFn[R, S, Out]): Unit =
    step(StepType.WhenStep, expr)(f)

  /** Then step. */
  transparent inline def Then[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFn[R, S, Out]): Unit =
    step(StepType.ThenStep, expr)(f)

  /** And step. */
  transparent inline def And[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFn[R, S, Out]): Unit =
    step(StepType.AndStep, expr)(f)

  /** But step. */
  transparent inline def But[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFn[R, S, Out]): Unit =
    step(StepType.ButStep, expr)(f)

  /**
   * Given step with state injection — `f` receives the current scenario state
   * as first argument.
   */
  transparent inline def GivenS[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFnS[R, S, Out]): Unit =
    stepS(StepType.GivenStep, expr)(f)

  /**
   * When step with state injection — `f` receives the current scenario state as
   * first argument.
   */
  transparent inline def WhenS[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFnS[R, S, Out]): Unit =
    stepS(StepType.WhenStep, expr)(f)

  /**
   * Then step with state injection — `f` receives the current scenario state as
   * first argument.
   */
  transparent inline def ThenS[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFnS[R, S, Out]): Unit =
    stepS(StepType.ThenStep, expr)(f)

  /**
   * And step with state injection — `f` receives the current scenario state as
   * first argument.
   */
  transparent inline def AndS[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFnS[R, S, Out]): Unit =
    stepS(StepType.AndStep, expr)(f)

  /**
   * But step with state injection — `f` receives the current scenario state as
   * first argument.
   */
  transparent inline def ButS[Out <: Tuple](expr: StepExpression[Out])(inline f: TupleToFnS[R, S, Out]): Unit =
    stepS(StepType.ButStep, expr)(f)
}
