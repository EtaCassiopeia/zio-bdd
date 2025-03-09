package zio.bdd.core.report

import zio.*
import zio.bdd.core.CollectedLogs

import java.time.Instant
import java.time.format.DateTimeFormatter
import scala.xml.{Elem, NodeSeq, PrettyPrinter}

object JUnitReporter {

  case class TestCase(
    name: String,
    succeeded: Boolean,
    logs: CollectedLogs,
    timestamp: Instant,
    duration: Long,     // in milliseconds
    assertions: Int = 0 // Number of assertions (for JUnit 4)
  )

  case class TestSuite(
    name: String,
    cases: List[TestCase],
    timestamp: Instant
  )

  sealed trait Format
  object Format {
    case object JUnit4 extends Format
    case object JUnit5 extends Format
  }

  def generateXML(suite: TestSuite, format: Format = Format.JUnit5): ZIO[Any, Nothing, String] =
    ZIO.succeed {
      format match {
        case Format.JUnit4 => generateJUnit4XML(suite)
        case Format.JUnit5 => generateJUnit5XML(suite)
      }
    }

  // JUnit 4 distinguishes failures (assertion failures) from errors (exceptions); assume no errors for now
  private def generateJUnit4XML(suite: TestSuite): String = {
    val xml = <testsuite
    name={suite.name}
    tests={suite.cases.length.toString}
    failures={suite.cases.count(!_.succeeded).toString}
    errors="0"
    timestamp={DateTimeFormatter.ISO_INSTANT.format(suite.timestamp)}>
      {suite.cases.map(toJUnit4TestCaseXML)}
    </testsuite>

    new PrettyPrinter(120, 2).format(xml)
  }

  private def generateJUnit5XML(suite: TestSuite): String = {
    val xml = <testsuite
    name={suite.name}
    tests={suite.cases.length.toString}
    failures={suite.cases.count(!_.succeeded).toString}
    timestamp={DateTimeFormatter.ISO_INSTANT.format(suite.timestamp)}>
      {suite.cases.map(toJUnit5TestCaseXML)}
    </testsuite>

    new PrettyPrinter(120, 2).format(xml)
  }

  private def toJUnit4TestCaseXML(testCase: TestCase): NodeSeq = {
    val stdout = testCase.logs.stdout.map(_.message).mkString("\n")
    val stderr = testCase.logs.stderr.map(_.message).mkString("\n")
    <testcase
    name={testCase.name}
    time={(testCase.duration / 1000.0).toString}
    assertions={testCase.assertions.toString}
    timestamp={DateTimeFormatter.ISO_INSTANT.format(testCase.timestamp)}>
      {if (!testCase.succeeded) <failure message="Test failed"/> else NodeSeq.Empty}
      {if (stdout.nonEmpty) <system-out>{stdout}</system-out> else NodeSeq.Empty}
      {if (stderr.nonEmpty) <system-err>{stderr}</system-err> else NodeSeq.Empty}
    </testcase>
  }

  private def toJUnit5TestCaseXML(testCase: TestCase): NodeSeq = {
    val stdout = testCase.logs.stdout.map(_.message).mkString("\n")
    val stderr = testCase.logs.stderr.map(_.message).mkString("\n")
    <testcase
    name={testCase.name}
    time={(testCase.duration / 1000.0).toString}
    timestamp={DateTimeFormatter.ISO_INSTANT.format(testCase.timestamp)}>
      {if (!testCase.succeeded) <failure message="Test failed"/> else NodeSeq.Empty}
      {if (stdout.nonEmpty) <system-out>{stdout}</system-out> else NodeSeq.Empty}
      {if (stderr.nonEmpty) <system-err>{stderr}</system-err> else NodeSeq.Empty}
    </testcase>
  }

  def writeToFile(suite: TestSuite, filePath: String, format: Format = Format.JUnit5): ZIO[Any, Throwable, Unit] =
    for {
      xml <- generateXML(suite, format)
      _   <- ZIO.blocking(ZIO.attempt(scala.util.Using.resource(new java.io.PrintWriter(filePath))(_.print(xml))))
    } yield ()
}
