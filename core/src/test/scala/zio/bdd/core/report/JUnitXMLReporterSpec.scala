package zio.bdd.core.report

import zio.*
import zio.test.*
import zio.bdd.core.*
import zio.bdd.gherkin.StepType

/**
 * Tests for the filesystem side of JUnitXMLReporter:
 *   - writes one TEST-<Suite>-<Feature>.xml per feature
 *   - sanitises non-alphanumeric characters in feature names
 *   - creates outputDir if it doesn't exist
 *   - when suiteClass is empty, prefix is "TEST-"
 */
object JUnitXMLReporterSpec extends ZIOSpecDefault {

  import ResultFixtures.*

  private def createTempDir: ZIO[Any, Throwable, java.io.File] =
    ZIO.attemptBlocking(
      java.nio.file.Files.createTempDirectory("zio-bdd-junit-test").toFile
    )

  private def deleteRecursive(dir: java.io.File): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      def delete(f: java.io.File): Unit = {
        if (f.isDirectory) Option(f.listFiles()).foreach(_.foreach(delete))
        f.delete()
      }
      delete(dir)
    }.orDie

  private def withTmpDir[A](f: java.io.File => ZIO[Any, Throwable, A]): ZIO[Any, Throwable, A] =
    ZIO.acquireReleaseWith(createTempDir)(deleteRecursive)(f)

  private def listXmlFiles(dir: java.io.File): List[String] =
    Option(dir.listFiles()).map(_.filter(_.getName.endsWith(".xml")).map(_.getName).toList).getOrElse(Nil)

  private val fileNamingSuite = suite("file naming")(
    test("writes one TEST-<Suite>-<Feature>.xml per feature") {
      withTmpDir { tmpDir =>
        val config   = JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "com.example.MySuite$")
        val reporter = JUnitXMLReporter(config)
        val fr1 =
          mkFeatureResult("Alpha", List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))))
        val fr2 =
          mkFeatureResult("Beta", List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))))
        for {
          _ <- reporter.report(List(fr1, fr2)).provide(LogCollector.live())
        } yield {
          val files = listXmlFiles(tmpDir)
          assertTrue(
            files.contains("TEST-MySuite-Alpha.xml"),
            files.contains("TEST-MySuite-Beta.xml"),
            files.length == 2
          )
        }
      }
    },
    test("sanitises non-alphanumeric characters in feature names (spaces, colons, slashes replaced)") {
      withTmpDir { tmpDir =>
        val config   = JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "MySuite$")
        val reporter = JUnitXMLReporter(config)
        val fr = mkFeatureResult(
          "My Feature: Special/Chars!",
          List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed))))
        )
        for {
          _ <- reporter.report(List(fr)).provide(LogCollector.live())
        } yield {
          val files = listXmlFiles(tmpDir)
          val file  = files.headOption.getOrElse("")
          assertTrue(
            files.length == 1,
            !file.contains(" "),
            !file.contains(":"),
            !file.contains("/"),
            !file.contains("!")
          )
        }
      }
    },
    test("when suiteClass is empty, prefix is 'TEST-' (no double dash)") {
      withTmpDir { tmpDir =>
        val config   = JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "")
        val reporter = JUnitXMLReporter(config)
        val fr = mkFeatureResult(
          "MyFeature",
          List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed))))
        )
        for {
          _ <- reporter.report(List(fr)).provide(LogCollector.live())
        } yield {
          val files = listXmlFiles(tmpDir)
          assertTrue(
            files.length == 1,
            files.head.startsWith("TEST-"),
            !files.head.startsWith("TEST--"),
            files.head.contains("MyFeature")
          )
        }
      }
    },
    test("suiteClass simple name strips dollar and package prefix") {
      withTmpDir { tmpDir =>
        val config =
          JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "com.example.test.ProvisionSuite$")
        val reporter = JUnitXMLReporter(config)
        val fr =
          mkFeatureResult("F", List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))))
        for {
          _ <- reporter.report(List(fr)).provide(LogCollector.live())
        } yield {
          val files = listXmlFiles(tmpDir)
          assertTrue(files.exists(_.startsWith("TEST-ProvisionSuite-")))
        }
      }
    }
  )

  private val directoryCreationSuite = suite("outputDir creation")(
    test("creates nested outputDir if it does not exist") {
      withTmpDir { tmpDir =>
        val nestedDir = new java.io.File(tmpDir, "nested/deeply/reports")
        val config    = JUnitReporterConfig(outputDir = nestedDir.getAbsolutePath, suiteClass = "Suite$")
        val reporter  = JUnitXMLReporter(config)
        val fr =
          mkFeatureResult("F", List(mkScenarioResult("s", List((StepType.GivenStep, "step", StepStatus.Passed)))))
        for {
          _ <- reporter.report(List(fr)).provide(LogCollector.live())
          // Capture filesystem state during `use`; withTmpDir's release deletes the dir,
          // and assertTrue evaluates its expressions lazily — after release would see false.
          exists = nestedDir.exists()
          isDir  = nestedDir.isDirectory
        } yield assertTrue(exists, isDir)
      }
    }
  )

  private val xmlContentSuite = suite("XML file content validity")(
    test("generated XML parses as valid XML") {
      withTmpDir { tmpDir =>
        val config   = JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "MySuite$")
        val reporter = JUnitXMLReporter(config)
        val fr = mkFeatureResult(
          "Feature",
          List(
            mkScenarioResult("passed", List((StepType.GivenStep, "step", StepStatus.Passed))),
            mkScenarioResult(
              "failed",
              List((StepType.GivenStep, "step", StepStatus.Failed(Cause.fail(new RuntimeException("oops")))))
            )
          )
        )
        for {
          _       <- reporter.report(List(fr)).provide(LogCollector.live())
          file     = listXmlFiles(tmpDir).headOption.map(f => new java.io.File(tmpDir, f)).get
          content <- ZIO.attemptBlocking(scala.io.Source.fromFile(file).mkString)
        } yield {
          // Extract attributes to plain strings before asserting. A scala.xml.Elem is a
          // self-referential Seq[Node], so letting zio-test's PrettyPrint render it on an
          // assertion failure overflows the stack — keep the Elem out of assertTrue.
          val xml          = parseXml(content)
          val testsAttr    = xml \@ "tests"
          val failuresAttr = xml \@ "failures"
          assertTrue(testsAttr == "2", failuresAttr == "1")
        }
      }
    },
    test("empty feature list produces no XML files") {
      withTmpDir { tmpDir =>
        val config   = JUnitReporterConfig(outputDir = tmpDir.getAbsolutePath, suiteClass = "Suite$")
        val reporter = JUnitXMLReporter(config)
        for {
          _ <- reporter.report(Nil).provide(LogCollector.live())
        } yield assertTrue(listXmlFiles(tmpDir).isEmpty)
      }
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("JUnitXMLReporterSpec")(
    fileNamingSuite,
    directoryCreationSuite,
    xmlContentSuite
  )
}
