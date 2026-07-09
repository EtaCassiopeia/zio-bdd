package zio.bdd.core.report

import zio.*
import zio.test.*
import zio.bdd.core.*
import zio.bdd.gherkin.StepType
import zio.stream.ZStream

import java.time.Instant
import scala.annotation.experimental

/**
 * Gate for issue #238 — JUnit XML and the live streaming reporter must surface
 * the scenario aspects (retry `attempts`, XFAIL/XPASS from `@expectedFailure`),
 * not just the effective pass/fail.
 *
 * Note (as in JUnitXMLFormatterSpec): a scala.xml.Elem is a self-referential
 * Seq, so every navigation is reduced to a plain String/Boolean BEFORE it
 * reaches assertTrue.
 *
 * Marked `@experimental` because the streaming-reporter suite below exercises
 * the `@experimental` `TestEvent`/`LiveProgressReporter` API (issue #288).
 */
@experimental
object ReporterAspectsSpec extends ZIOSpecDefault {

  import ResultFixtures.*
  import JUnitXMLFormatter.*

  private val ts = Instant.parse("2024-01-15T10:00:00Z")

  private val failingStep =
    List((StepType.GivenStep, "does a thing", StepStatus.Failed(Cause.fail(new AssertionError("boom")))))
  private val passingStep = List((StepType.GivenStep, "does a thing", StepStatus.Passed))

  private def xfail(name: String): ScenarioResult =
    mkScenarioResult(name, failingStep, tags = List("expectedFailure")).copy(expectedFailure = true)
  private def xpass(name: String): ScenarioResult =
    mkScenarioResult(name, passingStep, tags = List("expectedFailure")).copy(expectedFailure = true)
  private def retried(name: String, n: Int): ScenarioResult =
    mkScenarioResult(name, passingStep).copy(attempts = n)

  private def renderXml(scenarios: List[ScenarioResult]): String = {
    val fr = mkFeatureResult("F", scenarios)
    generateXML(buildSuite(fr.feature, fr.scenarioResults, Map.empty, ts, "Suite"))
  }

  def spec = suite("ReporterAspectsSpec")(
    suite("JUnit XML")(
      test("XPASS emits a <failure> with an actionable message, not generic 'Scenario failed' (AC1)") {
        val xml = renderXml(List(xpass("unexpected")))
        assertTrue(xml.contains("<failure"), xml.contains("remove @expectedFailure"), !xml.contains("Scenario failed"))
      },
      test("XPASS still counts as a suite failure — build fails (AC2)") {
        val doc      = parseXml(renderXml(List(xpass("unexpected"))))
        val failures = (doc \ "@failures").text
        assertTrue(failures == "1")
      },
      test("XFAIL emits <skipped> (distinguishable), not <failure>, and does not fail the build (AC3)") {
        val xml      = renderXml(List(xfail("known bug")))
        val doc      = parseXml(xml)
        val failures = (doc \ "@failures").text
        assertTrue(
          xml.contains("<skipped"),
          xml.contains("expected failure"),
          !xml.contains("<failure"),
          failures == "0"
        )
      },
      test("attempts > 1 is carried into the testcase (AC4)") {
        val doc      = parseXml(renderXml(List(retried("flaky", 3))))
        val attempts = ((doc \ "testcase").head \ "@attempts").text
        assertTrue(attempts == "3")
      },
      test("a first-try pass carries no attempts attribute (AC4)") {
        val doc      = parseXml(renderXml(List(retried("clean", 1))))
        val attempts = ((doc \ "testcase").head \ "@attempts").text
        assertTrue(attempts == "")
      },
      test("a SETUP failure under @expectedFailure is a real <failure>, never masked as XFAIL-skipped") {
        val setupFailed = mkScenarioResult("broken hook", passingStep, tags = List("expectedFailure"))
          .copy(expectedFailure = true, setupError = Some(Cause.fail(new RuntimeException("hook exploded"))))
        val xml      = renderXml(List(setupFailed))
        val failures = (parseXml(xml) \ "@failures").text
        assertTrue(xml.contains("<failure"), !xml.contains("expected failure:"), failures == "1")
      },
      test("mixed suite: XFAIL + XPASS + normal pass + normal fail — counts land correctly") {
        val normalFail = mkScenarioResult("real fail", failingStep)
        val normalPass = mkScenarioResult("real pass", passingStep)
        val doc        = parseXml(renderXml(List(xfail("known"), xpass("surprise"), normalPass, normalFail)))
        val tests      = (doc \ "@tests").text
        val failures   = (doc \ "@failures").text // XPASS + normalFail
        val skipped    = (doc \ "@skipped").text  // XFAIL
        assertTrue(tests == "4", failures == "2", skipped == "1")
      },
      test("the <testsuite skipped> attribute tallies XFAIL alongside @ignore and pending") {
        val doc =
          parseXml(renderXml(List(xfail("known"), mkIgnoredScenarioResult("skipme"), mkPendingScenarioResult("todo"))))
        val skipped = (doc \ "@skipped").text
        assertTrue(skipped == "3")
      }
    ),
    suite("LiveProgressReporter (streaming)")(
      test("surfaces XFAIL, XPASS and retry attempts in the live scenario lines (AC5)") {
        val events = List(
          TestEvent.ScenarioFinished(xfail("known"), "F"),
          TestEvent.ScenarioFinished(xpass("surprise"), "F"),
          TestEvent.ScenarioFinished(retried("flaky", 3), "F")
        )
        for
          _   <- LiveProgressReporter.consume(ZStream.fromIterable(events)).provide(LogCollector.live())
          out <- TestConsole.output
        yield
          val text = out.mkString
          assertTrue(
            text.contains("expected failure"),
            text.contains("remove @expectedFailure"),
            text.contains("3 attempts")
          )
      },
      test("SuiteFinished summary reports xfail and xpass counts") {
        val fr = mkFeatureResult(
          "F",
          List(xfail("k"), xpass("s"), mkScenarioResult("p", passingStep), mkScenarioResult("f", failingStep))
        )
        for
          _   <- LiveProgressReporter.consume(ZStream(TestEvent.SuiteFinished(List(fr)))).provide(LogCollector.live())
          out <- TestConsole.output
        yield
          val text = out.mkString
          assertTrue(text.contains("1 xfail"), text.contains("1 xpass"))
      },
      test("SuiteFinished summary suffix omits the aspect that is absent (only xfail present)") {
        val fr = mkFeatureResult("F", List(xfail("k"), mkScenarioResult("p", passingStep)))
        for
          _   <- LiveProgressReporter.consume(ZStream(TestEvent.SuiteFinished(List(fr)))).provide(LogCollector.live())
          out <- TestConsole.output
        yield
          val text = out.mkString
          assertTrue(text.contains("1 xfail"), !text.contains("xpass"))
      }
    )
  )
}
