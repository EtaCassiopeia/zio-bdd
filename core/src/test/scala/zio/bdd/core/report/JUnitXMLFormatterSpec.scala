package zio.bdd.core.report

import zio.*
import zio.test.*
import zio.bdd.core.*
import zio.bdd.gherkin.StepType

import java.time.Instant
import scala.xml.{Elem, NodeSeq}

/**
 * Tests for the pure XML emission layer (JUnitXMLFormatter):
 *   - tests/failures/skipped attribute counts match inputs
 *   - step elements rendered with correct keyword/name/status/time
 *   - failed scenario emits <failure> element with message and type
 *   - \@ignore emits <skipped/>; pending emits <skipped message="Pending:
 *     ..."/>
 *   - timed-out step emits status="timed_out"
 *   - XML special characters in names are escaped
 *   - LogCollector content partitions into system-out / system-err
 *   - classname falls back to feature name when suiteClassname is empty
 *   - JUnit4 vs JUnit5 format differences (errors=, assertions=)
 *
 * Note: a scala.xml.Elem/NodeSeq is a self-referential Seq[Node]; letting
 * zio-test's PrettyPrint render one on an assertion failure overflows the
 * stack. So every navigation is reduced to a plain String / Int / Boolean
 * BEFORE it reaches assertTrue, and the Elem is never captured by the assertion
 * macro.
 */
object JUnitXMLFormatterSpec extends ZIOSpecDefault {

  import ResultFixtures.*
  import JUnitXMLFormatter.*

  private val ts = Instant.parse("2024-01-15T10:00:00Z")

  private def buildAndRender(
    featureName: String,
    scenarios: List[ScenarioResult],
    suiteClassname: String = "MySuite",
    format: Format = Format.JUnit5
  ): Elem = {
    val fr    = mkFeatureResult(featureName, scenarios)
    val suite = buildSuite(fr.feature, fr.scenarioResults, Map.empty, ts, suiteClassname)
    parseXml(generateXML(suite, format))
  }

  // Attribute readers that yield plain values (never expose the Elem to assertTrue).
  private def attr(ns: NodeSeq, name: String): String = ns \@ name
  private def all(xml: Elem, label: String): NodeSeq  = xml \\ label

  private val countsSuite = suite("testsuite attribute counts")(
    test("tests attribute equals total scenario count") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("p", List((StepType.GivenStep, "step", StepStatus.Passed))),
          mkScenarioResult(
            "f",
            List((StepType.GivenStep, "step", StepStatus.Failed(Cause.fail(new RuntimeException("err")))))
          ),
          mkIgnoredScenarioResult("i")
        )
      )
      val tests = xml \@ "tests"
      assertTrue(tests == "3")
    },
    test("failures attribute counts only Failed scenarios") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("p", List((StepType.GivenStep, "step", StepStatus.Passed))),
          mkScenarioResult(
            "f1",
            List((StepType.GivenStep, "step", StepStatus.Failed(Cause.fail(new RuntimeException("e1")))))
          ),
          mkScenarioResult(
            "f2",
            List((StepType.GivenStep, "step", StepStatus.Failed(Cause.fail(new RuntimeException("e2")))))
          )
        )
      )
      val failures = xml \@ "failures"
      assertTrue(failures == "2")
    },
    test("skipped attribute counts @ignore + pending scenarios") {
      val xml = buildAndRender(
        "F",
        List(
          mkIgnoredScenarioResult("ignored"),
          mkPendingScenarioResult("pending"),
          mkScenarioResult("passed", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      val skipped = xml \@ "skipped"
      assertTrue(skipped == "2")
    },
    test("time attribute is formatted with 3 decimal places") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)), durationMs = 1234L)
        )
      )
      val time          = xml \@ "time"
      val hasDot        = time.contains(".")
      val threeDecimals = time.split("\\.").lastOption.exists(_.length == 3)
      assertTrue(hasDot, threeDecimals)
    }
  )

  private val testcasesSuite = suite("testcase elements")(
    test("each scenario produces exactly one testcase element") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s1", List((StepType.GivenStep, "step", StepStatus.Passed))),
          mkScenarioResult("s2", List((StepType.WhenStep, "step", StepStatus.Passed)))
        )
      )
      val caseCount = all(xml, "testcase").length
      assertTrue(caseCount == 2)
    },
    test("testcase name attribute matches scenario name") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("My Scenario", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      val name = attr(all(xml, "testcase"), "name")
      assertTrue(name == "My Scenario")
    },
    test("testcase classname is suiteClassname when provided") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        ),
        suiteClassname = "com.example.MySuite$"
      )
      val classname = attr(all(xml, "testcase"), "classname")
      assertTrue(classname == "com.example.MySuite$")
    },
    test("testcase classname falls back to feature name when suiteClassname is empty") {
      val xml = buildAndRender(
        "My Feature",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        ),
        suiteClassname = ""
      )
      val classname = attr(all(xml, "testcase"), "classname")
      assertTrue(classname == "My Feature")
    }
  )

  private val stepsSuite = suite("step elements")(
    test("each Gherkin step produces a <step> element inside <steps>") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "a user exists", StepStatus.Passed),
              (StepType.WhenStep, "the user logs in", StepStatus.Passed),
              (StepType.ThenStep, "access is granted", StepStatus.Passed)
            )
          )
        )
      )
      val stepCount = all(xml, "step").length
      assertTrue(stepCount == 3)
    },
    test("step keyword attribute matches Given/When/Then") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "setup", StepStatus.Passed),
              (StepType.WhenStep, "action", StepStatus.Passed),
              (StepType.ThenStep, "verify", StepStatus.Passed)
            )
          )
        )
      )
      val keywords = all(xml, "step").map(_ \@ "keyword").toList
      assertTrue(keywords == List("Given", "When", "Then"))
    },
    test("failed step emits status='failed'") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "a failing step", StepStatus.Failed(Cause.fail(new RuntimeException("oops"))))
            )
          )
        )
      )
      val status = attr(all(xml, "step"), "status")
      assertTrue(status == "failed")
    },
    test("skipped step emits status='skipped'") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "a step", StepStatus.Skipped)
            )
          )
        )
      )
      val status = attr(all(xml, "step"), "status")
      assertTrue(status == "skipped")
    },
    test("pending step emits status='pending'") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "a step", StepStatus.Pending("implement me"))
            )
          )
        )
      )
      val status = attr(all(xml, "step"), "status")
      assertTrue(status == "pending")
    },
    test("timed-out step emits status='timed_out'") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "a slow step", StepStatus.TimedOut(zio.Duration.fromSeconds(5), Cause.empty))
            )
          )
        )
      )
      val status = attr(all(xml, "step"), "status")
      assertTrue(status == "timed_out")
    }
  )

  private val failureSuite = suite("failure element")(
    test("failed scenario emits <failure> element with message and type='AssertionError'") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (
                StepType.GivenStep,
                "a step",
                StepStatus.Failed(Cause.fail(new RuntimeException("expected 200, got 404")))
              )
            )
          )
        )
      )
      val failure    = all(xml, "failure")
      val present    = failure.nonEmpty
      val failType   = failure \@ "type"
      val hasMessage = (failure \@ "message").contains("expected 200")
      assertTrue(present, failType == "AssertionError", hasMessage)
    },
    test("passed scenario has no <failure> element") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "a step", StepStatus.Passed)))
        )
      )
      val noFailure = all(xml, "failure").isEmpty
      assertTrue(noFailure)
    }
  )

  private val skippedSuite = suite("skipped / pending elements")(
    test("@ignore scenario emits <skipped/> with no message attribute") {
      val xml       = buildAndRender("F", List(mkIgnoredScenarioResult("ignored")))
      val skipped   = all(xml, "skipped")
      val present   = skipped.nonEmpty
      val noMessage = (skipped \@ "message").isEmpty
      assertTrue(present, noMessage)
    },
    test("pending scenario emits <skipped message='Pending: ...'/>") {
      val xml        = buildAndRender("F", List(mkPendingScenarioResult("pending", "implement this")))
      val skipped    = all(xml, "skipped")
      val present    = skipped.nonEmpty
      val hasMessage = (skipped \@ "message").contains("Pending: implement this")
      assertTrue(present, hasMessage)
    }
  )

  private val xmlEscapingSuite = suite("XML special character escaping")(
    test("scenario name with < > & is safely rendered as attributes") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("<Special> & 'Name'", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      // If the XML parses without exception, escaping was correct
      val present = all(xml, "testcase").nonEmpty
      assertTrue(present)
    },
    test("feature name with double-quote is safely rendered") {
      val xml = buildAndRender(
        "Feature \"quoted\"",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      val present = all(xml, "testsuite").nonEmpty
      assertTrue(present)
    }
  )

  private val logsSuite = suite("system-out / system-err log partitioning")(
    test("Info entries go to system-out, Error entries go to system-err") {
      val fr = mkFeatureResult(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      val scenarioId = fr.scenarioResults.head.scenario.id.toString
      val logs = Map(
        scenarioId -> CollectedLogs(
          List(
            LogEntry("info msg", java.time.Instant.now(), LogSource.Stdout, InternalLogLevel.Info, "s1"),
            LogEntry("error msg", java.time.Instant.now(), LogSource.Stderr, InternalLogLevel.Error, "s1")
          )
        )
      )
      val suite  = buildSuite(fr.feature, fr.scenarioResults, logs, ts, "Suite")
      val xml    = parseXml(generateXML(suite))
      val hasOut = all(xml, "system-out").nonEmpty
      val hasErr = all(xml, "system-err").nonEmpty
      assertTrue(hasOut, hasErr)
    },
    test("empty logs produce no system-out or system-err elements") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        )
      )
      val noOut = all(xml, "system-out").isEmpty
      val noErr = all(xml, "system-err").isEmpty
      assertTrue(noOut, noErr)
    }
  )

  private val formatSuite = suite("JUnit4 vs JUnit5 format differences")(
    test("JUnit4 adds errors attribute to testsuite") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        ),
        format = Format.JUnit4
      )
      val errors = xml \@ "errors"
      assertTrue(errors == "0")
    },
    test("JUnit5 does NOT add errors attribute to testsuite") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        ),
        format = Format.JUnit5
      )
      val noErrors = (xml \@ "errors").isEmpty
      assertTrue(noErrors)
    },
    test("JUnit4 adds assertions attribute to testcase counting passed steps") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult(
            "s",
            List(
              (StepType.GivenStep, "step1", StepStatus.Passed),
              (StepType.WhenStep, "step2", StepStatus.Passed)
            )
          )
        ),
        format = Format.JUnit4
      )
      val assertions = attr(all(xml, "testcase"), "assertions")
      assertTrue(assertions == "2")
    },
    test("JUnit5 does NOT add assertions attribute to testcase") {
      val xml = buildAndRender(
        "F",
        List(
          mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))
        ),
        format = Format.JUnit5
      )
      val noAssertions = attr(all(xml, "testcase"), "assertions").isEmpty
      assertTrue(noAssertions)
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("JUnitXMLFormatterSpec")(
    countsSuite,
    testcasesSuite,
    stepsSuite,
    failureSuite,
    skippedSuite,
    xmlEscapingSuite,
    logsSuite,
    formatSuite
  )
}
