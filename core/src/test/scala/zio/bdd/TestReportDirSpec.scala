package zio.bdd

import zio.test.*

import java.net.URI
import java.nio.file.{Files, Path, Paths}

/**
 * Issue #353: the junitxml report directory is derived from the suite class's
 * location through its URI, not `URL#getPath` (which yields `/C:/…` on Windows
 * and keeps percent-escapes).
 */
object TestReportDirSpec extends ZIOSpecDefault {

  private def dirFor(url: String): Option[Path] = ZIOBDDTask.testReportDirFor(URI.create(url).toURL)

  def spec = suite("ZIOBDDTask.testReportDirFor (#353)")(
    test("derives <module>/target/test-reports from a test-classes location") {
      val tmp   = Files.createTempDirectory("zio-bdd-353")
      val clazz = tmp.resolve("mod/target/scala-3.3.4/test-classes/a/B$.class")
      assertTrue(dirFor(clazz.toUri.toString).contains(tmp.resolve("mod/target/test-reports")))
    },
    test("decodes a percent-encoded space instead of keeping %20") {
      val url = "file:/home/me/My%20Project/mod/target/scala-3.3.4/test-classes/a/B$.class"
      assertTrue(
        dirFor(url).contains(Paths.get(URI.create("file:/home/me/My%20Project/mod/target/test-reports"))),
        dirFor(url).exists(_.toString.contains("My Project")),
        !dirFor(url).exists(_.toString.contains("%20"))
      )
    },
    test("accepts a URL whose path carries an unescaped space") {
      val url = new java.net.URL("file:/home/me/My Project/target/test-classes/B$.class")
      assertTrue(ZIOBDDTask.testReportDirFor(url).exists(_.toString.contains("My Project")))
    },
    test("a Windows drive-letter location converts via the URI, not the /C:/ path string") {
      // On Windows this yields C:\Users\me\My Project\target\test-reports; on other
      // platforms the same URI conversion applies. The old code built "/C:/…" by string
      // concatenation from URL#getPath, which Paths.get rejects on Windows.
      val url = "file:/C:/Users/me/My%20Project/target/scala-3.3.4/test-classes/a/B$.class"
      assertTrue(dirFor(url).contains(Paths.get(URI.create("file:/C:/Users/me/My%20Project/target/test-reports"))))
    },
    test("uses the innermost target ancestor, as the module root") {
      val url = "file:/w/target/checkout/mod/target/scala-3.3.4/test-classes/B$.class"
      assertTrue(dirFor(url).contains(Paths.get("/w/target/checkout/mod/target/test-reports")))
    },
    test("resolves a class inside a jar from the jar's own location") {
      val url = "jar:file:/w/My%20Project/target/scala-3.3.4/app-tests.jar!/a/B$.class"
      assertTrue(dirFor(url).contains(Paths.get(URI.create("file:/w/My%20Project/target/test-reports"))))
    },
    test("None when the location has no target ancestor") {
      assertTrue(dirFor("file:/w/out/classes/B$.class").isEmpty)
    },
    test("None for a non-file location") {
      assertTrue(dirFor("http://example.com/target/classes/B$.class").isEmpty)
    }
  )
}
