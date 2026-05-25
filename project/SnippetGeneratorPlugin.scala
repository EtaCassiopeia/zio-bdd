import sbt.*
import sbt.Keys.*

/**
 * zio-bdd snippet generator sbt plugin.
 *
 * Reads all `.feature` files under a given directory and produces
 * copy-paste-ready zio-bdd step skeleton definitions for any step texts
 * that don't appear to have a matching step definition in the project sources.
 *
 * Usage:
 *   sbt "zioBddSnippets"                        -- uses default featureDir
 *   sbt "zioBddSnippets src/test/resources/features/my-module"
 *
 * The task prints skeletons to stdout and exits successfully — it is advisory,
 * not a build-breaking validation. Use `--dry-run` at test time for that.
 */
object SnippetGeneratorPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    val zioBddSnippets = inputKey[Unit](
      "Generate skeleton zio-bdd step definitions for undefined steps in feature files."
    )
  }

  import autoImport.*
  import complete.DefaultParsers.*

  override def projectSettings: Seq[Def.Setting[?]] = Seq(
    zioBddSnippets := {
      val args = spaceDelimited("<featureDir>").parsed
      val log  = streams.value.log
      val testSrcDirs = (Test / sourceDirectories).value
      val testResDir  = (Test / resourceDirectory).value

      runSnippets(args, log, testSrcDirs, testResDir)
    }
  )

  private def runSnippets(
    args: Seq[String],
    log: Logger,
    scalaSourceDirs: Seq[File],
    defaultFeatureDir: File
  ): Unit = {
    val featureDir = args.headOption.map(new File(_)).getOrElse(defaultFeatureDir)

    if (!featureDir.exists()) {
      log.warn("Feature directory not found: " + featureDir.getAbsolutePath + ". Pass a path: sbt zioBddSnippets path/to/features")
      return
    }

    log.info(s"Scanning: ${featureDir.getAbsolutePath}")

    val featureFiles = collectFeatureFiles(featureDir)
    if (featureFiles.isEmpty) { log.warn(s"No .feature files found under ${featureDir.getAbsolutePath}"); return }

    val allSteps = featureFiles.flatMap(parseStepsFromFeature)
    if (allSteps.isEmpty) { log.info("No steps found."); return }

    val stepTexts = collectRegisteredStepTexts(scalaSourceDirs)
    val undefined = distinctBy(allSteps.filterNot { step =>
      stepTexts.exists(t => normalise(t).contains(normalise(step.text)) ||
                            normalise(step.text).contains(normalise(t)))
    })(_.text)

    if (undefined.isEmpty) { log.info("All steps appear to have definitions. Nothing to generate."); return }

    log.info(s"Found ${undefined.size} potentially undefined step(s).")

    val byKeyword = undefined.groupBy(_.keyword)
    val output    = new StringBuilder
    output.append("// ── Generated step skeletons ──────────────────────────────────────────────\n")
    output.append("// Paste into your step trait and implement each body.\n\n")

    for (kw <- List("Given", "When", "Then", "And", "But") if byKeyword.contains(kw)) {
      val steps = byKeyword(kw)
      for (step <- steps) {
        val (exprStr, paramStr) = inferExtractors(step.text)
        output.append(s"""$kw($exprStr)$paramStr {\n  ZIO.unit // TODO implement\n}\n\n""")
      }
    }
    println(output.toString)
  }

  // ── Internal data types ─────────────────────────────────────────────────

  private case class GherkinStep(keyword: String, text: String)

  // ── Feature file parsing ────────────────────────────────────────────────

  private def collectFeatureFiles(dir: File): List[File] = {
    def recurse(f: File): List[File] =
      if (f.isDirectory) Option(f.listFiles()).toList.flatten.flatMap(recurse)
      else if (f.getName.endsWith(".feature")) List(f)
      else Nil
    recurse(dir)
  }

  private val stepKeywords = List("Given", "When", "Then", "And", "But", "*")

  private def parseStepsFromFeature(file: File): List[GherkinStep] = {
    val lines = scala.io.Source.fromFile(file).getLines().toList
    lines.flatMap { raw =>
      val stripped = raw.stripLeading()
      stepKeywords.collectFirst {
        case kw if stripped.startsWith(kw + " ") || stripped.startsWith(kw + ":") =>
          val text = stripped.drop(kw.length).dropWhile(c => c == ':' || c == ' ')
          val effectiveKw = if (kw == "*") "And" else kw
          GherkinStep(effectiveKw, text.trim)
      }
    }
  }

  // ── Scala source scanning ───────────────────────────────────────────────

  private val stepCallPattern = """(?:Given|When|Then|And|But)\("([^"]+)""".r

  private def collectRegisteredStepTexts(sourceDirs: Seq[File]): Set[String] = {
    val scalaFiles = sourceDirs.flatMap(d =>
      if (d.exists()) d.allPaths.get.filter(_.getName.endsWith(".scala")).toList
      else Nil
    )
    scalaFiles.flatMap { file =>
      val content = scala.io.Source.fromFile(file).mkString
      stepCallPattern.findAllMatchIn(content).map(_.group(1)).toList
    }.toSet
  }

  // ── Extractor inference ─────────────────────────────────────────────────

  private val uuidRe     = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}".r
  private val intRe      = "-?\\d+".r
  private val decimalRe  = "-?\\d+\\.\\d+".r
  private val quotedRe   = """"[^"]*"""".r
  private val boolRe     = "(?i)true|false".r

  /**
   * Infer the most likely typed extractor expression and parameter list.
   * Returns (exprString, paramString) where paramString is the lambda signature.
   */
  private def inferExtractors(text: String): (String, String) = {
    var remaining = text
    val parts     = List.newBuilder[String]  // expression parts: literals and extractor names
    val params    = List.newBuilder[(String, String)] // (name, type)
    var paramIdx  = 0

    def nextParamName(typ: String): String = {
      val letter = ('a' + paramIdx).toChar
      paramIdx += 1
      s"$letter"
    }

    def consume(pattern: scala.util.matching.Regex, extractor: String, scalaType: String): Unit =
      pattern.findFirstMatchIn(remaining).foreach { m =>
        val before = remaining.substring(0, m.start).trim
        if (before.nonEmpty) parts += s""""$before""""
        parts += extractor
        params += (nextParamName(scalaType) -> scalaType)
        remaining = remaining.substring(m.end).trim
      }

    // Replace tokens from most specific to least
    // Repeated until no more matches
    var changed = true
    while (changed) {
      val prev = remaining
      if (uuidRe.findFirstIn(remaining).isDefined)    consume(uuidRe,    "/ uuid",       "java.util.UUID")
      if (boolRe.findFirstIn(remaining).isDefined)    consume(boolRe,    "/ boolean",    "Boolean")
      if (decimalRe.findFirstIn(remaining).isDefined) consume(decimalRe, "/ bigDecimal", "BigDecimal")
      if (intRe.findFirstIn(remaining).isDefined)     consume(intRe,     "/ int",        "Int")
      if (quotedRe.findFirstIn(remaining).isDefined)  consume(quotedRe,  "/ string",     "String")
      changed = remaining != prev
    }
    if (remaining.nonEmpty) parts += s""""${remaining.trim}""""

    val allParts = parts.result()
    val allParams = params.result()

    val exprStr = if (allParts.isEmpty) s""""$text""""
                  else allParts.mkString(" ")

    val paramStr =
      if (allParams.isEmpty) ""
      else {
        val ps = allParams.map { case (n, t) => s"($n: $t)" }.mkString(", ")
        if (allParams.length == 1) s" { ${allParams.head._1}: ${allParams.head._2} =>"
        else s" { ($ps) =>"
      }

    val closeSig = if (allParams.nonEmpty) "" else " {"
    (exprStr, if (paramStr.nonEmpty) paramStr else " {")
  }

  private def normalise(s: String): String = s.toLowerCase.replaceAll("\\s+", " ").trim

  private def distinctBy[A, B](list: List[A])(f: A => B): List[A] =
    list.foldLeft((List.empty[A], Set.empty[B])) { case ((acc, seen), elem) =>
      val key = f(elem)
      if (seen.contains(key)) (acc, seen) else (acc :+ elem, seen + key)
    }._1
}
