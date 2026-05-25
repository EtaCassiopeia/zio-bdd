import sbt.*
import sbt.Keys.*

/**
 * Generates a `step-registry.json` file listing all registered step definitions.
 *
 * The JSON file follows a format that can be consumed by IDE plugins (IntelliJ Cucumber,
 * VS Code Cucumber extension) to provide step navigation and autocomplete in `.feature` files.
 *
 * Usage:
 *   sbt "generateStepRegistry"
 *
 * Output: target/zio-bdd/step-registry.json
 *
 * IDE configuration:
 *   - VS Code Cucumber extension: set `cucumber.stepDefinitions` to point at this file
 *   - IntelliJ: configure "Step definition patterns" to use the generated patterns
 *
 * The generated JSON contains:
 * {{{
 * {
 *   "steps": [
 *     { "keyword": "Given", "text": "a valid provision body", "pattern": "a valid provision body", "file": "src/.../ProvisionSteps.scala" },
 *     { "keyword": "When",  "text": "a post request is sent", "pattern": "a post request is sent" },
 *     ...
 *   ]
 * }
 * }}}
 *
 * Pattern extraction is best-effort: it scans Scala sources for `Given("...")`-style calls
 * and includes both the raw string and the inferred regex pattern for tools that support it.
 */
object StepRegistryPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    val generateStepRegistry = taskKey[File](
      "Generate step-registry.json for IDE step navigation (zio-bdd)."
    )
    val stepRegistryOutput = settingKey[File](
      "Output path for the generated step registry JSON."
    )
  }

  import autoImport.*

  override def projectSettings: Seq[Def.Setting[?]] = Seq(
    stepRegistryOutput := target.value / "zio-bdd" / "step-registry.json",

    generateStepRegistry := {
      val log      = streams.value.log
      val outFile  = stepRegistryOutput.value
      val srcDirs  = (Test / sourceDirectories).value

      IO.createDirectory(outFile.getParentFile)

      val steps = collectStepDefinitions(srcDirs, log)
      val json  = buildJson(steps)

      IO.write(outFile, json)
      log.info(s"[zio-bdd] Generated step registry: ${outFile.getAbsolutePath} (${steps.length} steps)")
      outFile
    }
  )

  // ── Step definition models ──────────────────────────────────────────────

  private case class StepEntry(
    keyword: String,
    text: String,
    pattern: String,
    file: String,
    line: Int
  )

  // ── Scanning ────────────────────────────────────────────────────────────

  /** All five step keywords plus their inferrable regex patterns from the typed DSL. */
  private val stepDslPattern =
    """(?:Given|When|Then|And|But)\s*\(\s*"([^"]+)""".r

  /** Also capture the chained extractor expressions: "literal " / string / ... */
  private val chainedPattern =
    """(Given|When|Then|And|But)\s*\(([^)]+)\)\s*\{""".r

  private def collectStepDefinitions(sourceDirs: Seq[File], log: Logger): List[StepEntry] = {
    val scalaFiles = sourceDirs.flatMap(dir =>
      if (dir.exists()) dir.allPaths.get.filter(_.getName.endsWith(".scala")).toList
      else Nil
    )

    log.info(s"[zio-bdd] Scanning ${scalaFiles.length} Scala source file(s)...")

    val collected = scalaFiles.flatMap { file =>
      val relPath = file.getAbsolutePath
      val lines   = scala.io.Source.fromFile(file).getLines().zipWithIndex.toList

      lines.flatMap { case (line, lineIdx) =>
        val lineNo = lineIdx + 1
        stepDslPattern.findAllMatchIn(line).map { m =>
          val kw   = line.stripLeading().takeWhile(c => c.isLetter)
          val text = m.group(1)
          StepEntry(
            keyword = kw,
            text    = text,
            pattern = literalToPattern(text),
            file    = relPath,
            line    = lineNo
          )
        }.toList
      }
    }
    distinctBy(collected.toList)(e => (e.keyword, e.text))
  }

  /**
   * Convert a literal step text to a regex-compatible pattern.
   * Heuristic: replace integer and decimal literal tokens with capture groups.
   */
  private def literalToPattern(text: String): String = {
    val escaped = java.util.regex.Pattern.quote(text)
    // Unescape captured portions for substitution
    text
      .replaceAll("-?\\d+\\.\\d+", "([\\\\d.+-]+)")  // decimals
      .replaceAll("-?\\d+",        "(-?\\\\d+)")       // integers
      .replaceAll("\"[^\"]*\"",    "\"([^\"]*)\"")      // quoted strings
  }

  // ── JSON serialisation ──────────────────────────────────────────────────

  private def jsonString(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""

  private def buildJson(steps: List[StepEntry]): String = {
    val entries = steps.map { s =>
      s"""  {
         |    "keyword": ${jsonString(s.keyword)},
         |    "text": ${jsonString(s.text)},
         |    "pattern": ${jsonString(s.pattern)},
         |    "file": ${jsonString(s.file)},
         |    "line": ${s.line}
         |  }""".stripMargin
    }.mkString(",\n")

    s"""{
       |  "version": "1",
       |  "generator": "zio-bdd-step-registry",
       |  "steps": [
       |$entries
       |  ]
       |}""".stripMargin
  }

  private def distinctBy[A, B](list: List[A])(f: A => B): List[A] =
    list.foldLeft((List.empty[A], Set.empty[B])) { case ((acc, seen), elem) =>
      val key = f(elem)
      if (seen.contains(key)) (acc, seen) else (acc :+ elem, seen + key)
    }._1
}
