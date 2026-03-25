package zio.bdd

import zio.test.*
import zio.test.Assertion.*

import java.io.File
import java.nio.file.Files

object FeatureFilesSpec extends ZIOSpecDefault {

  private val classLoader = getClass.getClassLoader

  override def spec: Spec[Any, Any] = suite("FeatureFiles")(
    suite("filesystem")(
      test("finds .feature files in a directory") {
        val dir = new File(classLoader.getResource("features").toURI)
        val result = FeatureFiles(dir.getAbsolutePath, classLoader).retrieve()
        assertTrue(result.nonEmpty && result.forall(_.endsWith(".feature")))
      },
      test("finds a single .feature file") {
        val file = new File(classLoader.getResource("features/sample.feature").toURI)
        val result = FeatureFiles(file.getAbsolutePath, classLoader).retrieve()
        assertTrue(result == List(file.getAbsolutePath))
      },
      test("returns Nil for non-existent path") {
        val result = FeatureFiles("/non/existent/path", classLoader).retrieve()
        assertTrue(result.isEmpty)
      },
      test("returns Nil for empty path") {
        val result = FeatureFiles("", classLoader).retrieve()
        assertTrue(result.isEmpty)
      },
      test("returns Nil for a file without .feature extension") {
        val tmpFile = Files.createTempFile("test", ".txt").toFile
        tmpFile.deleteOnExit()
        val result = FeatureFiles(tmpFile.getAbsolutePath, classLoader).retrieve()
        assertTrue(result.isEmpty)
      }
    ),
    suite("classpath")(
      test("finds .feature files in a classpath directory") {
        val result = FeatureFiles("classpath:features", classLoader).retrieve()
        assertTrue(result.nonEmpty && result.forall(_.endsWith(".feature")))
      },
      test("finds a single .feature file from classpath") {
        val result = FeatureFiles("classpath:features/sample.feature", classLoader).retrieve()
        assertTrue(result.size == 1 && result.head.endsWith("sample.feature"))
      },
      test("returns Nil for non-existent classpath resource") {
        val result = FeatureFiles("classpath:non/existent", classLoader).retrieve()
        assertTrue(result.isEmpty)
      }
    )
  )
}
