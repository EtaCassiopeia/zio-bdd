package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * A backend registered in the conformance matrix. `layer` is a fully self-wired
 * [[MockControl]]; `capabilities`/`isolation` are what it advertises
 * (#118/#123). `available = false` parks the whole column as SKIPPED — e.g. the
 * Rift backend when Docker isn't present (gated by `RIFT_IT`), so the matrix
 * still runs WireMock in-process.
 */
final case class MockBackendUnderTest(
  name: String,
  layer: ZLayer[Any, Throwable, MockControl],
  capabilities: Set[Capability],
  isolation: Isolation,
  available: Boolean = true
)

/**
 * One portable conformance scenario: a `check` programmed against the neutral
 * [[MockControl]] (never a concrete backend), gated by the capabilities it
 * needs. A scenario requiring a capability the backend doesn't advertise is
 * SKIPPED, never FAILED — that is a negotiated gap, not a conformance breach.
 */
final case class ConformanceScenario(
  name: String,
  requires: Set[Capability],
  check: MockControl => ZIO[Scope, Throwable, Unit]
)

enum Outcome:
  case Pass, Skip, Fail

final case class Cell(scenario: String, backend: String, outcome: Outcome, detail: Option[String] = None)

/** The pass/skip/fail matrix — one cell per (scenario, backend). */
final case class Matrix(backends: List[MockBackendUnderTest], scenarios: List[ConformanceScenario], cells: List[Cell]):

  def cell(scenario: String, backend: String): Option[Cell] =
    cells.find(c => c.scenario == scenario && c.backend == backend)

  /**
   * A column is conformant iff it has zero FAIL and every SKIP is justified —
   * the backend was unavailable, or the scenario required a capability the
   * backend does not advertise (the acceptance condition "SKIPs equal the
   * un-advertised capabilities").
   */
  def conformant(backend: MockBackendUnderTest): Boolean =
    cells.filter(_.backend == backend.name).forall { c =>
      c.outcome match
        case Outcome.Fail => false
        case Outcome.Pass => true
        case Outcome.Skip =>
          !backend.available ||
          scenarios.find(_.name == c.scenario).exists(!_.requires.subsetOf(backend.capabilities))
    }

  /** A compact P/S/F grid, one row per scenario, one column per backend. */
  def render: String =
    val nameW = (scenarios.map(_.name.length) :+ 8).max
    val head  = "scenario".padTo(nameW, ' ') + "  " + backends.map(_.name).mkString("  ")
    val rows = scenarios.map { s =>
      val marks = backends.map(b => cell(s.name, b.name).map(mark).getOrElse("?").padTo(b.name.length, ' '))
      s.name.padTo(nameW, ' ') + "  " + marks.mkString("  ")
    }
    (head +: rows).mkString("\n")

  private def mark(c: Cell): String = c.outcome match
    case Outcome.Pass => "PASS"
    case Outcome.Skip => "SKIP"
    case Outcome.Fail => "FAIL"

/** Runs each scenario against each backend and assembles the [[Matrix]]. */
object ConformanceHarness:

  def run(backends: List[MockBackendUnderTest], scenarios: List[ConformanceScenario]): UIO[Matrix] =
    ZIO
      .foreach(backends)(b => runBackend(b, scenarios))
      .map(rows => Matrix(backends, scenarios, rows.flatten))

  // Stand the backend up once and run every scenario against it (each provisions
  // and tears down its own spaces). A backend that can't even *build* fails every
  // scenario; an unavailable backend (e.g. Rift with no Docker) skips them all.
  // The all-FAIL mapping is scoped to `build`, so a later teardown failure can't
  // rewrite already-computed cells; interruption propagates (it is not a verdict).
  private def runBackend(backend: MockBackendUnderTest, scenarios: List[ConformanceScenario]): UIO[List[Cell]] =
    if !backend.available then
      ZIO.succeed(scenarios.map(s => Cell(s.name, backend.name, Outcome.Skip, Some("backend unavailable"))))
    else
      ZIO.scoped {
        backend.layer.build.foldCauseZIO(
          cause =>
            if cause.isInterruptedOnly then ZIO.failCause(cause.stripFailures)
            else
              ZIO.succeed(
                scenarios.map(s =>
                  Cell(s.name, backend.name, Outcome.Fail, Some(s"backend setup failed: ${detailOf(cause)}"))
                )
              )
          ,
          env => ZIO.foreach(scenarios)(s => cellFor(env.get[MockControl], backend, s))
        )
      }

  private def cellFor(control: MockControl, backend: MockBackendUnderTest, scenario: ConformanceScenario): UIO[Cell] =
    if !scenario.requires.subsetOf(backend.capabilities) then
      val missing = (scenario.requires -- backend.capabilities).map(_.toString).mkString(", ")
      ZIO.succeed(Cell(scenario.name, backend.name, Outcome.Skip, Some(s"requires $missing")))
    else
      ZIO
        .scoped(scenario.check(control))
        .foldCauseZIO(
          // A failed or DIED check is a verdict (Fail cell); interruption is not — propagate it.
          cause =>
            if cause.isInterruptedOnly then ZIO.failCause(cause.stripFailures)
            else ZIO.succeed(Cell(scenario.name, backend.name, Outcome.Fail, Some(detailOf(cause)))),
          _ => ZIO.succeed(Cell(scenario.name, backend.name, Outcome.Pass))
        )

  // A failure carries its message; a DEFECT (a harness/check bug, not a backend
  // verdict) is labelled so a reader can tell "the backend misbehaved" from "the
  // check threw". Total — never throws on an empty cause.
  private def detailOf(cause: Cause[Throwable]): String =
    cause.failureOption
      .map(_.getMessage)
      .orElse(cause.dieOption.map(d => s"[harness defect] ${d.getClass.getName}: ${d.getMessage}"))
      .filter(_ != null)
      .getOrElse(cause.prettyPrint.linesIterator.nextOption.getOrElse("<no detail>"))
      .take(300)
