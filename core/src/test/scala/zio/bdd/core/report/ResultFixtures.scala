package zio.bdd.core.report

import zio.*
import zio.bdd.core.*
import zio.bdd.gherkin.{Feature, Scenario, Step, StepType}

import javax.xml.parsers.SAXParserFactory

/**
 * Shared test fixtures for JUnit XML reporter and formatter tests. Also
 * re-usable from PrettyReporterSpec if that spec is refactored to import these.
 */
private[report] object ResultFixtures {

  val F = "test.feature"

  /**
   * Parse XML with a fresh SAX parser per call. `scala.xml.XML` reuses a single
   * non-thread-safe parser; zio-test runs a suite's tests concurrently, so the
   * shared parser corrupts mid-parse (SAXParseException). A new parser per call
   * makes these tests safe to run in parallel.
   */
  def parseXml(content: String): scala.xml.Elem = {
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(false)
    scala.xml.XML.withSAXParser(factory.newSAXParser()).loadString(content)
  }

  def mkStep(t: StepType, p: String, lineNo: Int = 1): Step =
    Step(t, p, file = Some(F), line = Some(lineNo))

  def mkStepResult(step: Step, status: StepStatus, durationMs: Long = 10L): StepResult =
    status match
      case StepStatus.Passed    => StepResult(step, Right(()), durationMs)
      case StepStatus.Failed(c) => StepResult(step, Left(c), durationMs)
      case StepStatus.TimedOut(d, c) =>
        StepResult(step, Left(Cause.fail(new StepTimeoutException(step.pattern, d))), durationMs)
      case StepStatus.Skipped      => StepResult.skipped(step)
      case StepStatus.Pending(msg) => StepResult(step, Left(Cause.fail(new PendingException(msg))), durationMs)

  def mkScenario(name: String, steps: List[Step] = Nil, tags: List[String] = Nil, lineNo: Int = 1): Scenario =
    Scenario(name, tags, steps, Some(F), Some(lineNo))

  def mkScenarioResult(
    name: String,
    steps: List[(StepType, String, StepStatus)],
    tags: List[String] = Nil,
    durationMs: Long = 100L
  ): ScenarioResult = {
    // The formatter zips Scenario.steps with stepResults, so both must be populated and aligned.
    val gherkinSteps = steps.zipWithIndex.map { case ((t, p, _), i) => mkStep(t, p, i + 1) }
    val sc           = mkScenario(name, gherkinSteps, tags)
    val stepResults  = gherkinSteps.zip(steps).map { case (gs, (_, _, st)) => mkStepResult(gs, st) }
    ScenarioResult(sc, stepResults, duration = durationMs)
  }

  def mkIgnoredScenarioResult(name: String): ScenarioResult = {
    val sc = mkScenario(name, tags = List("ignore"))
    ScenarioResult(sc, Nil)
  }

  def mkPendingScenarioResult(name: String, reason: String = "todo"): ScenarioResult =
    mkScenarioResult(name, List((StepType.GivenStep, "pending step", StepStatus.Pending(reason))))

  def mkFeatureResult(name: String, scenarios: List[ScenarioResult], durationMs: Long = 200L): FeatureResult =
    FeatureResult(Feature(name, Nil, Nil, Some(F), Some(1)), scenarios, durationMs)
}
