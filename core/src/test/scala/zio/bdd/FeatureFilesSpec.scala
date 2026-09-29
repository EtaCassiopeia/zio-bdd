package zio.bdd

import zio.ZIO
import zio.bdd.gherkin.GherkinParser
import zio.test.*
import zio.test.Assertion.*

import java.io.File
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

object FeatureFilesSpec extends ZIOSpecDefault {

  private val classLoader = getClass.getClassLoader

  override def spec: Spec[Any, Any] = suite("FeatureFiles")(
    suite("filesystem")(
      test("finds .feature files in a directory") {
        val dir    = new File(classLoader.getResource("features").toURI)
        val result = FeatureFiles(dir.getAbsolutePath, classLoader).retrieve()
        assertTrue(result.nonEmpty, result.forall(_.endsWith(".feature")))
      },
      test("finds a single .feature file") {
        val file   = new File(classLoader.getResource("features/sample.feature").toURI)
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
        assertTrue(result.nonEmpty, result.forall(_.endsWith(".feature")))
      },
      test("finds a single .feature file from classpath") {
        val result = FeatureFiles("classpath:features/sample.feature", classLoader).retrieve()
        assertTrue(result.size == 1, result.head.endsWith("sample.feature"))
      },
      test("returns Nil for non-existent classpath resource") {
        val result = FeatureFiles("classpath:non/existent", classLoader).retrieve()
        assertTrue(result.isEmpty)
      }
    ),
    suite("classpath inside a jar (#354)")(
      test("discovers the .feature files directly inside a jar directory and parses them") {
        withJarLoader { loader =>
          for {
            found    <- ZIO.attempt(FeatureFiles("classpath:features/x", loader).retrieve())
            features <- ZIO.foreach(found)(loc => JarFeatures.read(loc).flatMap(GherkinParser.parseFeature(_, loc)))
          } yield assertTrue(
            found.size == 2,
            found.forall(l => l.startsWith("jar:file:") && l.contains("!/features/x/")),
            features.map(_.name) == List("Alpha", "Beta"),
            features.flatMap(_.scenarios.map(_.name)) == List("first alpha", "first beta")
          )
        }
      },
      test("discovers and parses a single .feature file inside a jar") {
        withJarLoader { loader =>
          for {
            found   <- ZIO.attempt(FeatureFiles("classpath:features/x/a b.feature", loader).retrieve())
            content <- JarFeatures.read(found.head)
            feature <- GherkinParser.parseFeature(content, found.head)
          } yield assertTrue(found.size == 1, feature.name == "Alpha", feature.file.contains(found.head))
        }
      },
      test("a non-feature entry and a missing entry yield Nil") {
        withJarLoader { loader =>
          ZIO.attempt {
            val txt     = FeatureFiles("classpath:features/x/notes.txt", loader).retrieve()
            val missing = FeatureFiles("classpath:features/nope", loader).retrieve()
            assertTrue(txt.isEmpty, missing.isEmpty)
          }
        }
      },
      test("the jar is closed after listing and reading (it can be listed again and deleted)") {
        for {
          jar    <- ZIO.attempt(writeJar())
          loader  = new URLClassLoader(Array(jar.toUri.toURL), null)
          first  <- ZIO.attempt(FeatureFiles("classpath:features/x", loader).retrieve())
          _      <- ZIO.foreachDiscard(first)(JarFeatures.read)
          second <- ZIO.attempt(FeatureFiles("classpath:features/x", loader).retrieve())
          _      <- ZIO.attempt(loader.close())
          _      <- ZIO.attempt(Files.delete(jar))
          // bound outside assertTrue: its macro mis-lowers a Java static call (java/nio/file/Files$)
          deleted = !Files.exists(jar)
        } yield assertTrue(first.size == 2, first == second, deleted)
      }
    )
  )

  // A jar (in a directory whose name contains a space) holding two features, a
  // non-feature file, and a nested directory whose feature must not be picked up.
  private def writeJar(): Path = {
    val dir = Files.createTempDirectory("zio bdd 354")
    val jar = dir.resolve("shared-tests.jar")
    val out = new JarOutputStream(Files.newOutputStream(jar))
    try {
      def put(name: String, content: String): Unit = {
        out.putNextEntry(new JarEntry(name))
        out.write(content.getBytes(StandardCharsets.UTF_8))
        out.closeEntry()
      }
      out.putNextEntry(new JarEntry("features/"))
      out.closeEntry()
      out.putNextEntry(new JarEntry("features/x/"))
      out.closeEntry()
      put("features/x/a b.feature", "Feature: Alpha\n  Scenario: first alpha\n    Given a step\n")
      put("features/x/b.feature", "Feature: Beta\n  Scenario: first beta\n    Given a step\n")
      put("features/x/notes.txt", "not a feature")
      put("features/x/nested/c.feature", "Feature: Nested\n  Scenario: nested\n    Given a step\n")
    } finally out.close()
    jar
  }

  private def withJarLoader(f: URLClassLoader => ZIO[Any, Throwable, TestResult]): ZIO[Any, Throwable, TestResult] =
    ZIO.scoped {
      ZIO
        .acquireRelease(ZIO.attempt(new URLClassLoader(Array(writeJar().toUri.toURL), null)))(l =>
          ZIO.attempt(l.close()).orDie
        )
        .flatMap(f)
    }
}
