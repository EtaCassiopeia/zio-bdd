package zio.bdd.gherkin

import zio.*

import java.io.File
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break

/** Metadata about a scenario passed to hooks. */
case class ScenarioMetadata(
  name: String,
  tags: List[String],
  file: Option[String],
  line: Option[Int],
  /**
   * Flag values injected for this scenario run via @flags(k=v) tag expansion.
   */
  flagValues: Map[String, String] = Map.empty
)

object ScenarioMetadata {
  def from(scenario: Scenario, flagValues: Map[String, String] = Map.empty): ScenarioMetadata =
    ScenarioMetadata(scenario.name, scenario.tags, scenario.file, scenario.line, flagValues)
}

/**
 * Parses `@flags(key=value, key2=value2)` tags from Gherkin scenarios.
 *
 * When a scenario has one or more `@flags(...)` tags, the test framework
 * expands it into multiple runs — one per `@flags(...)` annotation — each with
 * a different `Map[String, String]` injected into the scenario's environment
 * layer.
 *
 * Syntax: `@flags(rateLimiting=true, featureX=false)` → `Map("rateLimiting" ->
 * "true", "featureX" -> "false")` `@flags(rateLimiting)` → `Map("rateLimiting"
 * -> "true")` (boolean shorthand)
 *
 * Multiple `@flags(...)` tags on the same scenario produce a matrix expansion:
 * `@flags(a=true) @flags(a=false)` → two runs, one with a=true, one with
 * a=false
 */
object FlagsTag:
  private val tagPattern = """^flags\(([^)]+)\)$""".r

  /** Parse a single tag string into an optional flag map. */
  def parse(tag: String): Option[Map[String, String]] =
    tagPattern.findFirstMatchIn(tag.trim.stripPrefix("@")).map { m =>
      m.group(1)
        .split(",")
        .flatMap { kv =>
          kv.trim.split("=", 2) match
            case Array(k, v) if k.trim.nonEmpty => Some(k.trim -> v.trim)
            case Array(k) if k.trim.nonEmpty    => Some(k.trim -> "true")
            case _                              => None
        }
        .toMap
    }

  /**
   * Extract all flag maps from a list of tags. Returns an empty list when no
   * `@flags(...)` tags are present (scenario runs once, as normal).
   */
  def extractAll(tags: List[String]): List[Map[String, String]] =
    tags.flatMap(parse)

case class Feature(
  name: String,
  tags: List[String] = Nil,
  scenarios: List[Scenario],
  file: Option[String] = None,
  line: Option[Int] = None
) {
  def id: Int            = s"feature:$name:${file.getOrElse("unknown")}:${line.getOrElse(0)}".hashCode
  def isIgnored: Boolean = tags.exists(_.equalsIgnoreCase("ignore"))

  def prettyString(indentLevel: Int = 0): String = {
    val indent       = "  " * indentLevel
    val tagsStr      = if (tags.nonEmpty) indent + tags.map("@" + _).mkString(" ") + "\n" else ""
    val featureLine  = indent + "Feature: " + name + "\n"
    val scenariosStr = scenarios.map(_.prettyString(indentLevel + 1)).mkString("\n")
    tagsStr + featureLine + scenariosStr
  }
}

case class Scenario(
  name: String,
  tags: List[String] = Nil,
  steps: List[Step],
  file: Option[String] = None,
  line: Option[Int] = None
) {
  def id: Int            = s"scenario:$name:${file.getOrElse("unknown")}:${line.getOrElse(0)}".hashCode
  def isIgnored: Boolean = tags.exists(_.equalsIgnoreCase("ignore"))

  def prettyString(indentLevel: Int): String = {
    val indent       = "  " * indentLevel
    val tagsStr      = if (tags.nonEmpty) indent + tags.map("@" + _).mkString(" ") + "\n" else ""
    val scenarioLine = indent + "Scenario: " + name + "\n"
    val stepsStr     = steps.map(_.prettyString(indentLevel + 1)).mkString("\n")
    tagsStr + scenarioLine + stepsStr
  }
}

enum StepType {
  case GivenStep, WhenStep, ThenStep, ButStep, AndStep
}

case class DataTableRow(cells: List[String]) {
  def prettyString(indentLevel: Int, widths: List[Int]): String = {
    val indent = "  " * indentLevel
    val paddedCells = cells.zip(widths).map { case (cell, width) =>
      cell.padTo(width, ' ').mkString
    }
    indent + "| " + paddedCells.mkString(" | ") + " |"
  }
}

case class DataTable(headers: List[String], rows: List[DataTableRow]) {
  def prettyString(indentLevel: Int): String = {
    val indent     = "  " * indentLevel
    val numColumns = headers.size
    val columnWidths = (0 until numColumns).map { j =>
      val headerLength  = headers(j).length
      val maxCellLength = rows.map(row => if (j < row.cells.length) row.cells(j).length else 0).maxOption.getOrElse(0)
      Math.max(headerLength, maxCellLength)
    }.toList
    val headerRow = indent + "| " + headers
      .zip(columnWidths)
      .map { case (cell, width) => cell.padTo(width, ' ').mkString }
      .mkString(" | ") + " |"
    val dataRows = rows.map(_.prettyString(indentLevel, columnWidths))
    (headerRow +: dataRows).mkString("\n")
  }
}

case class Step(
  stepType: StepType,
  pattern: String,
  dataTable: Option[DataTable] = None,
  file: Option[String] = None,
  line: Option[Int] = None,
  docString: Option[String] = None
) {
  def id: Int = s"step:${stepType}:$pattern:${file.getOrElse("")}:${line.getOrElse(0)}".hashCode

  override def toString: String =
    s"${stepType.toString.replace("Step", "")} $pattern ${file.zip(line).map { case (f, l) => s"($f:$l)" }.getOrElse("")}"

  def prettyString(indentLevel: Int): String = {
    val indent = "  " * indentLevel
    val kw = stepType match {
      case StepType.GivenStep => "Given"; case StepType.WhenStep => "When"
      case StepType.ThenStep  => "Then"; case StepType.ButStep   => "But"
      case StepType.AndStep   => "And"
    }
    val stepLine     = indent + kw + " " + pattern + "\n"
    val dataTableStr = dataTable.map(_.prettyString(indentLevel + 1)).getOrElse("")
    val docStringStr = docString.map(s => s"""$indent  \"\"\"\n$s\n$indent  \"\"\"\n""").getOrElse("")
    stepLine + dataTableStr + docStringStr
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// Line-oriented Gherkin parser.
//
// Design rationale:
//   The fastparse MultiLineWhitespace approach causes structural ambiguity
//   because Gherkin is line-oriented: keywords are significant only at the
//   start of a line (after optional indentation), and empty lines are legal
//   everywhere as separators. A character-level PEG parser with global
//   whitespace consumption fights that structure.
//
//   This parser pre-processes the content into a list of typed Line tokens,
//   then applies a state-machine parser over that token list. This is the
//   approach used by the official Cucumber Gherkin library itself.
//
//   Benefits over fastparse MultiLineWhitespace:
//   - No trailing-newline sensitivity (BUG-01)
//   - No keyword prefix-match issue (BUG-06)
//   - No "Feature: \n" eating next line as name (BUG-04)
//   - Feature description paragraphs tolerated naturally (BUG-05)
//   - Background steps carry data tables/doc strings correctly (BUG-02)
//   - BOM stripped at preprocessing level (BUG-03)
//   - CRLF handled at preprocessing level
//   - \| escape in table cells parsed correctly (GAP-02)
//   - Backtick doc string delimiter (GAP-01)
//   - Scenarios: alias for Examples: (BUG-07)
//   - Doc string indentation stripped per spec (BUG-08)
//   - Feature with no scenarios is valid (GAP-04)
// ─────────────────────────────────────────────────────────────────────────────
object GherkinParser {

  // ── Preprocessing ──────────────────────────────────────────────────────────

  /**
   * Normalise raw file content before tokenising.
   *
   *   1. Strip BOM (U+FEFF) if present — common on Windows / tool-generated
   *      files. 2. Normalise CRLF → LF. 3. Strip `# language: XX` directive
   *      (language support is a future feature; we still parse English keywords
   *      only, but we do not fail on the directive). 4. Strip other comment
   *      lines (line starting with `#` after optional whitespace).
   */
  private def preprocessContent(content: String): String = {
    val withoutBom = if (content.startsWith("﻿")) content.drop(1) else content
    val normalised = withoutBom.replace("\r\n", "\n").replace("\r", "\n")
    normalised
  }

  // ── Line-level token types ─────────────────────────────────────────────────

  private sealed trait LineToken
  private case object EmptyLine                                                  extends LineToken
  private case class CommentLine(text: String)                                   extends LineToken
  private case class TagsLine(tags: List[String], lineNo: Int)                   extends LineToken
  private case class FeatureLine(name: String, lineNo: Int)                      extends LineToken
  private case class RuleLine(name: String, lineNo: Int)                         extends LineToken
  private case class BackgroundLine(lineNo: Int)                                 extends LineToken
  private case class ScenarioLine(name: String, isOutline: Boolean, lineNo: Int) extends LineToken
  private case class StepLine(keyword: StepType, text: String, lineNo: Int)      extends LineToken
  private case class TableRowLine(cells: List[String], lineNo: Int)              extends LineToken
  private case class DocStringDelimiter(indent: Int, delim: String, lineNo: Int) extends LineToken
  private case class DocStringContent(text: String, lineNo: Int)                 extends LineToken
  private case class ExamplesLine(name: String, tags: List[String], lineNo: Int) extends LineToken
  private case class DescriptionLine(text: String, lineNo: Int)                  extends LineToken

  private val stepKeywords = Map(
    "Given" -> StepType.GivenStep,
    "When"  -> StepType.WhenStep,
    "Then"  -> StepType.ThenStep,
    "But"   -> StepType.ButStep,
    "And"   -> StepType.AndStep,
    "*"     -> StepType.AndStep // wildcard step bullet
  )

  // Tags: @word, @word-with-dash, @word.with.dot, @retry(3), @flags(k=v, k2=v2), unicode, etc.
  // Tags: @word, @word-with-dash, @word.with.dot, @retry(3), @flags(k=v, k2=v2), unicode, etc.
  // Two forms supported:
  //   - Without parentheses: [^\s@(]+ — stops at space, @, or (
  //   - With parentheses: identifier + balanced (...) — content may contain spaces and commas
  // This ensures @flags(key=value, key2=value2) is captured as a single tag token.
  private val tagPattern = "(?U)@([^\\s@(]+(?:\\([^)]*\\))?)".r

  /** Tokenise a single source line. lineNo is 1-based. */
  private def tokeniseLine(raw: String, lineNo: Int, inDocString: Option[(Int, String)]): LineToken = {
    val stripped = raw.stripLeading() // strip leading whitespace for keyword detection
    val indent   = raw.length - stripped.length

    // When inside a doc string, check only for the closing delimiter
    inDocString match {
      case Some((dsIndent, delim)) =>
        if (stripped.startsWith(delim)) DocStringDelimiter(indent, delim, lineNo)
        else DocStringContent(raw, lineNo)
      case None =>
        if (stripped.isEmpty) EmptyLine
        else if (stripped.startsWith("#")) CommentLine(stripped)
        else if (stripped.startsWith("Feature:")) FeatureLine(stripped.stripPrefix("Feature:").trim, lineNo)
        else if (stripped.startsWith("Rule:")) RuleLine(stripped.stripPrefix("Rule:").trim, lineNo)
        else if (stripped.startsWith("Background:")) BackgroundLine(lineNo)
        else if (isScenarioKeyword(stripped)) parseScenarioLine(stripped, lineNo)
        else if (isExamplesKeyword(stripped)) parseExamplesLine(stripped, raw, lineNo)
        else if (stripped.startsWith("|")) parseTableRow(stripped, lineNo)
        else if (stripped.startsWith("\"\"\"")) DocStringDelimiter(indent, "\"\"\"", lineNo)
        else if (stripped.startsWith("```")) DocStringDelimiter(indent, "```", lineNo)
        else if (stripped.startsWith("@")) TagsLine(tagPattern.findAllMatchIn(stripped).map(_.group(1)).toList, lineNo)
        else
          stepKeywords.collectFirst {
            case (kw, st) if startsWithKeyword(stripped, kw) =>
              StepLine(st, stripped.drop(kw.length).stripLeading().dropWhile(_ == ':').strip(), lineNo)
          }.getOrElse(DescriptionLine(raw.trim, lineNo))
    }
  }

  /** True when the stripped line starts with a scenario/outline keyword. */
  private def isScenarioKeyword(s: String): Boolean =
    Seq("Scenario Outline:", "Scenario Template:", "Scenario:", "Example:").exists(s.startsWith) ||
      s.startsWith("Scenario Outline") || s.startsWith("Scenario Template")

  private def isExamplesKeyword(s: String): Boolean =
    s.startsWith("Examples") || s.startsWith("Scenarios")

  private def startsWithKeyword(line: String, kw: String): Boolean =
    line.startsWith(kw) &&
      (line.length == kw.length || line(kw.length) == ' ' || line(kw.length) == ':' || line(kw.length) == '\t')

  private def parseScenarioLine(s: String, lineNo: Int): ScenarioLine = {
    val isOutline = s.startsWith("Scenario Outline") || s.startsWith("Scenario Template")
    val name =
      if (s.startsWith("Scenario Outline:")) s.stripPrefix("Scenario Outline:").trim
      else if (s.startsWith("Scenario Template:")) s.stripPrefix("Scenario Template:").trim
      else if (s.startsWith("Scenario:")) s.stripPrefix("Scenario:").trim
      else if (s.startsWith("Example:")) s.stripPrefix("Example:").trim
      else if (s.startsWith("Scenario")) s.stripPrefix("Scenario").trim.dropWhile(_ == ':').trim
      else s.trim
    ScenarioLine(name, isOutline, lineNo)
  }

  private def parseExamplesLine(stripped: String, raw: String, lineNo: Int): LineToken = {
    // Examples may appear inside a scenario after steps — the tokeniser can't know that
    // so we just always emit ExamplesLine here; the parser will attach it correctly.
    // Capture any tags on the SAME line before the keyword (unusual but valid per spec)
    val tagsBefore = tagPattern.findAllMatchIn(stripped.takeWhile(_ != 'E').takeWhile(_ != 'S')).map(_.group(1)).toList
    val name =
      if (stripped.startsWith("Examples:")) stripped.stripPrefix("Examples:").trim
      else if (stripped.startsWith("Scenarios:")) stripped.stripPrefix("Scenarios:").trim
      else if (stripped.startsWith("Examples")) stripped.stripPrefix("Examples").trim.dropWhile(_ == ':').trim
      else stripped.stripPrefix("Scenarios").trim.dropWhile(_ == ':').trim
    ExamplesLine(name, tagsBefore, lineNo)
  }

  /** Parse a table row, handling `\|` escape sequences. */
  private def parseTableRow(s: String, lineNo: Int): TableRowLine = {
    // s is already stripped of leading whitespace, starts with '|'
    val inner = if (s.startsWith("|")) s.drop(1) else s
    val end   = if (inner.endsWith("|")) inner.dropRight(1) else inner
    // Split on | but not \|
    val cells = splitOnPipe(end).map(c => unescapeCell(c.trim))
    TableRowLine(cells, lineNo)
  }

  private def splitOnPipe(s: String): List[String] = {
    val buf = new StringBuilder
    val acc = List.newBuilder[String]
    var i   = 0
    while (i < s.length) {
      if (s(i) == '\\' && i + 1 < s.length && s(i + 1) == '|') {
        buf.append("\\|"); i += 2
      } else if (s(i) == '|') {
        acc += buf.toString; buf.clear(); i += 1
      } else {
        buf.append(s(i)); i += 1
      }
    }
    acc += buf.toString
    acc.result()
  }

  private def unescapeCell(s: String): String =
    s.replace("\\|", "|").replace("\\\\", "\\").replace("\\n", "\n")

  // ── Tokeniser ──────────────────────────────────────────────────────────────

  /**
   * Tokenise the full content into a sequence of typed LineTokens. Handles doc
   * string state machine to avoid mis-tokenising content lines.
   */
  private def tokenise(content: String): IndexedSeq[LineToken] = {
    val lines                           = content.split('\n')
    var docState: Option[(Int, String)] = None // (indent, delimiter) when inside a doc string
    lines.zipWithIndex.map { case (raw, idx) =>
      val lineNo = idx + 1
      val token  = tokeniseLine(raw, lineNo, docState)
      token match {
        case DocStringDelimiter(indent, delim, _) =>
          docState match {
            case None    => docState = Some((indent, delim)) // opening delimiter
            case Some(_) => docState = None                  // closing delimiter
          }
        case _ => ()
      }
      token
    }.toIndexedSeq
  }

  // ── Token-stream parser ────────────────────────────────────────────────────

  /**
   * Cursor over the token stream. Provides look-ahead and consuming operations.
   */
  private final class TokenCursor(tokens: IndexedSeq[LineToken]) {
    private var pos       = 0
    private val warningsB = List.newBuilder[ParseWarning]

    /** Record a non-fatal lint finding encountered during parsing. */
    def warn(w: ParseWarning): Unit  = warningsB += w
    def warnings: List[ParseWarning] = warningsB.result()

    def peek: Option[LineToken]           = if (pos < tokens.length) Some(tokens(pos)) else None
    def peekAt(n: Int): Option[LineToken] = tokens.lift(pos + n)
    def isEOF: Boolean                    = pos >= tokens.length
    def advance(): Unit                   = pos += 1
    def position: Int                     = pos
    def reset(p: Int): Unit               = pos = p

    /** Skip empty lines and comments. */
    def skipBlanks(): Unit =
      while (peek.exists { case EmptyLine | CommentLine(_) => true; case _ => false }) advance()

    /** Consume the next token if it matches the predicate, returning it. */
    def consume[A](pf: PartialFunction[LineToken, A]): Option[A] =
      peek.collect(pf).map { r =>
        advance(); r
      }

    /** Accumulate all leading tag lines, returning the merged tag list. */
    def consumeTagLines(): List[String] = {
      val buf      = List.newBuilder[String]
      var continue = true
      while (continue) {
        skipBlanks()
        peek match {
          case Some(TagsLine(ts, _)) => buf ++= ts; advance()
          case _                     => continue = false
        }
      }
      buf.result()
    }
  }

  // ── Parse errors ───────────────────────────────────────────────────────────

  private case class ParseError(message: String, lineNo: Int, file: String)
      extends RuntimeException(
        s"Parse error at $file:$lineNo — $message"
      )

  /**
   * A non-fatal lint finding raised while parsing. Always logged; in strict
   * mode it is promoted to a parse failure. Currently the only source is an
   * unrecognized line appearing after a scenario's steps have begun — almost
   * always a misspelled step keyword that would otherwise be silently dropped.
   */
  private case class ParseWarning(message: String, lineNo: Int, file: String) {
    def render: String = s"$file:$lineNo — $message"
  }

  // ── Internal AST (before expansion) ────────────────────────────────────────

  private case class RawStep(
    stepType: StepType,
    text: String,
    lineNo: Int,
    dataTable: Option[DataTable] = None,
    docString: Option[String] = None
  )

  private case class RawExamplesBlock(
    name: String,
    tags: List[String],
    headers: List[String],
    rows: List[List[String]],
    lineNo: Int
  )

  private case class RawScenario(
    name: String,
    tags: List[String],
    steps: List[RawStep],
    examplesBlocks: List[RawExamplesBlock],
    isOutline: Boolean,
    lineNo: Int
  )

  private case class RawBackground(steps: List[RawStep], lineNo: Int)
  private case class RawRule(name: String, background: Option[RawBackground], scenarios: List[RawScenario], lineNo: Int)

  // ── Token-level parsers ────────────────────────────────────────────────────

  /** Parse step arguments (data table or doc string) following a step line. */
  private def parseStepArgs(cursor: TokenCursor, file: String): (Option[DataTable], Option[String]) = {
    cursor.skipBlanks()
    cursor.peek match {
      case Some(TableRowLine(_, _)) =>
        val table = parseDataTable(cursor, file)
        (Some(table), None)
      case Some(DocStringDelimiter(delimIndent, _, _)) =>
        cursor.advance() // consume opening delimiter
        val docStr = parseDocString(cursor, delimIndent, file)
        (None, Some(docStr))
      case _ => (None, None)
    }
  }

  private def parseDataTable(cursor: TokenCursor, file: String): DataTable = {
    val rows = List.newBuilder[List[String]]
    while (cursor.peek.exists(_.isInstanceOf[TableRowLine])) {
      cursor.peek.collect { case TableRowLine(cells, _) => cells }.foreach { cells =>
        rows += cells
        cursor.advance()
      }
      cursor.skipBlanks()
    }
    val allRows = rows.result()
    allRows match {
      case Nil         => DataTable(Nil, Nil)
      case hdr :: rest => DataTable(hdr, rest.map(DataTableRow.apply))
    }
  }

  /**
   * Parse doc string content. Strips the common indentation prefix
   * (spec-compliant) rather than `.trim`. The closing delimiter was already
   * consumed by the tokeniser state machine.
   */
  private def parseDocString(cursor: TokenCursor, delimIndent: Int, file: String): String = {
    val lines  = List.newBuilder[String]
    var closed = false
    while (!cursor.isEOF && !closed) {
      cursor.peek match {
        case Some(DocStringDelimiter(_, _, _)) =>
          cursor.advance() // consume closing delimiter
          closed = true
        case Some(DocStringContent(raw, _)) =>
          lines += raw
          cursor.advance()
        case Some(EmptyLine) =>
          lines += ""
          cursor.advance()
        case _ =>
          closed = true // malformed but tolerate
      }
    }
    val contentLines = lines.result()
    stripCommonIndent(contentLines, delimIndent)
  }

  /**
   * Strip `delimIndent` spaces of common indentation from all lines. Per the
   * Gherkin spec, the indentation of the opening `"""` is removed from content.
   */
  private def stripCommonIndent(lines: List[String], delimIndent: Int): String = {
    val prefix = " " * delimIndent
    lines.map { line =>
      if (line.startsWith(prefix)) line.drop(delimIndent)
      else if (line.forall(_.isWhitespace)) ""
      else line
    }.mkString("\n").stripTrailing()
  }

  /**
   * Parse a list of steps (Given/When/Then/But/And/wildcard) from the cursor.
   */
  private def parseSteps(cursor: TokenCursor, file: String): List[RawStep] = {
    val steps    = List.newBuilder[RawStep]
    var seenStep = false
    var continue = true
    while (continue) {
      cursor.skipBlanks()
      cursor.peek match {
        case Some(StepLine(kw, text, lineNo)) =>
          cursor.advance()
          val (table, doc) = parseStepArgs(cursor, file)
          steps += RawStep(kw, text, lineNo, table, doc)
          seenStep = true
        case Some(DescriptionLine(text, lineNo)) =>
          // A description/narrative line is valid Gherkin only as a block's leading text
          // (before the first step) — e.g. "As a ... / I want ...". Once steps have begun,
          // an unrecognized line is almost always a misspelled step keyword; surface it as a
          // lint warning instead of silently dropping it. Leading descriptions stay silent.
          if (seenStep)
            cursor.warn(
              ParseWarning(s"""unrecognized line ignored (misspelled step keyword?): "$text"""", lineNo, file)
            )
          cursor.advance()
        case _ => continue = false
      }
    }
    steps.result()
  }

  /** Parse one Examples / Scenarios block. */
  private def parseExamplesBlock(cursor: TokenCursor, prePendedTags: List[String], file: String): RawExamplesBlock =
    cursor.peek match {
      case Some(ExamplesLine(name, lineTags, lineNo)) =>
        cursor.advance()
        cursor.skipBlanks()
        val table = cursor.peek match {
          case Some(TableRowLine(_, _)) => parseDataTable(cursor, file)
          case _                        => DataTable(Nil, Nil)
        }
        RawExamplesBlock(
          name = name,
          tags = (prePendedTags ++ lineTags).distinct,
          headers = table.headers,
          rows = table.rows.map(_.cells),
          lineNo = lineNo
        )
      case other =>
        throw ParseError(s"Expected Examples/Scenarios keyword, got: $other", 0, file)
    }

  /** Parse a scenario (including outline + examples). */
  private def parseScenario(cursor: TokenCursor, tags: List[String], file: String): RawScenario =
    cursor.peek match {
      case Some(ScenarioLine(name, isOutline, lineNo)) =>
        cursor.advance()
        val steps = parseSteps(cursor, file)
        // Collect examples blocks (multiple allowed)
        val examples = List.newBuilder[RawExamplesBlock]
        var cont     = true
        while (cont) {
          cursor.skipBlanks()
          // Tags may appear before an Examples block. Tentatively consume them, but if no Examples block follows
          // they belong to the NEXT scenario — restore the cursor so they aren't silently swallowed (which would
          // strip that scenario's tags). See parseScenario tag-stripping regression.
          val mark   = cursor.position
          val exTags = cursor.consume { case TagsLine(ts, _) => ts }.getOrElse(Nil)
          cursor.skipBlanks()
          cursor.peek match {
            case Some(ExamplesLine(_, _, _)) =>
              examples += parseExamplesBlock(cursor, exTags, file)
            case _ =>
              cursor.reset(mark)
              cont = false
          }
        }
        RawScenario(name, tags, steps, examples.result(), isOutline, lineNo)
      case other =>
        throw ParseError(s"Expected Scenario/Example keyword, got: $other", 0, file)
    }

  /** Parse a Background block. */
  private def parseBackground(cursor: TokenCursor, file: String): RawBackground =
    cursor.peek match {
      case Some(BackgroundLine(lineNo)) =>
        cursor.advance()
        RawBackground(parseSteps(cursor, file), lineNo)
      case other =>
        throw ParseError(s"Expected Background keyword, got: $other", 0, file)
    }

  /** Parse a Rule block (optional inner background + ≥0 scenarios). */
  private def parseRule(cursor: TokenCursor, tags: List[String], file: String): RawRule =
    cursor.peek match {
      case Some(RuleLine(name, lineNo)) =>
        cursor.advance()
        cursor.skipBlanks()
        val bg = cursor.peek match {
          case Some(BackgroundLine(_)) => Some(parseBackground(cursor, file))
          case _                       => None
        }
        val scenarios = List.newBuilder[RawScenario]
        var cont      = true
        while (cont) {
          cursor.skipBlanks()
          val scTags = cursor.consumeTagLines()
          cursor.skipBlanks()
          cursor.peek match {
            case Some(ScenarioLine(_, _, _)) =>
              scenarios += parseScenario(cursor, scTags, file)
            case _ => cont = false
          }
        }
        RawRule(name, bg, scenarios.result(), lineNo)
      case other =>
        throw ParseError(s"Expected Rule keyword, got: $other", 0, file)
    }

  // ── Expansion ─────────────────────────────────────────────────────────────

  private def toStep(r: RawStep, file: String): Step =
    Step(r.stepType, r.text, r.dataTable, Some(file), Some(r.lineNo), r.docString)

  private def expandScenario(
    rawSc: RawScenario,
    backgroundSteps: List[RawStep],
    file: String
  ): List[Scenario] = {
    val bgSteps  = backgroundSteps.map(toStep(_, file))
    val scSteps  = rawSc.steps.map(toStep(_, file))
    val allSteps = bgSteps ++ scSteps

    if (rawSc.examplesBlocks.isEmpty) {
      List(Scenario(rawSc.name, rawSc.tags, allSteps, Some(file), Some(rawSc.lineNo)))
    } else {
      rawSc.examplesBlocks.flatMap { block =>
        val combinedTags = (rawSc.tags ++ block.tags).distinct
        val baseLabel    = if (block.name.nonEmpty) s"${rawSc.name} - ${block.name}" else rawSc.name

        if (block.headers.isEmpty || block.rows.isEmpty) {
          // Outline with no example rows — emit once as a placeholder with no param substitution
          List(Scenario(s"$baseLabel - (no examples)", combinedTags, allSteps, Some(file), Some(rawSc.lineNo)))
        } else {
          block.rows.zipWithIndex.map { case (rowValues, i) =>
            val data       = block.headers.zip(rowValues).toMap
            val paramBg    = backgroundSteps.map(expandStep(_, data, file))
            val paramSteps = rawSc.steps.map(expandStep(_, data, file))
            Scenario(
              name = s"$baseLabel - Example ${i + 1}",
              tags = combinedTags,
              steps = paramBg.collect { case Right(s) => s } ++ paramSteps.collect { case Right(s) => s },
              file = Some(file),
              line = Some(rawSc.lineNo)
            )
          }
        }
      }
    }
  }

  private def expandStep(rawStep: RawStep, data: Map[String, String], file: String): Either[String, Step] = {
    val placeholderRe = "<([^>]+)>".r
    var newText       = rawStep.text
    boundary {
      for (m <- placeholderRe.findAllMatchIn(rawStep.text)) {
        val key = m.group(1)
        data.get(key) match {
          case Some(value) => newText = newText.replace(s"<$key>", value)
          case None        => break(Left(s"Missing value for placeholder '<$key>' in step: ${rawStep.text}"))
        }
      }
      Right(Step(rawStep.stepType, newText, rawStep.dataTable, Some(file), Some(rawStep.lineNo), rawStep.docString))
    }
  }

  // ── Top-level feature parser ───────────────────────────────────────────────

  private def parseFeatureFromTokens(cursor: TokenCursor, file: String): Either[Throwable, Feature] =
    try {
      cursor.skipBlanks()

      // Feature-level tags
      val featureTags = cursor.consumeTagLines()
      cursor.skipBlanks()

      // Feature keyword
      val (featureName, featureLineNo) = cursor.consume { case FeatureLine(n, l) => (n, l) }
        .getOrElse(throw ParseError("Expected 'Feature:' keyword", 0, file))

      // Optional description paragraph (narrative text before first Background/Scenario/Rule/@)
      cursor.skipBlanks()
      while (cursor.peek.exists(_.isInstanceOf[DescriptionLine])) cursor.advance()
      cursor.skipBlanks()

      // Optional feature-level Background
      val featureBackground = cursor.peek match {
        case Some(BackgroundLine(_)) => Some(parseBackground(cursor, file))
        case _                       => None
      }

      // Zero or more Rule blocks OR zero or more Scenario blocks (mixed is not spec-valid but we tolerate it)
      val scenarios = List.newBuilder[Scenario]

      var cont = true
      while (cont && !cursor.isEOF) {
        cursor.skipBlanks()
        val pendingTags = cursor.consumeTagLines()
        cursor.skipBlanks()
        cursor.peek match {
          case Some(RuleLine(_, _)) =>
            val rule   = parseRule(cursor, pendingTags, file)
            val ruleBg = rule.background.map(_.steps).getOrElse(Nil)
            rule.scenarios.foreach { sc =>
              scenarios ++= expandScenario(sc, featureBackground.map(_.steps).getOrElse(Nil) ++ ruleBg, file)
            }
          case Some(ScenarioLine(_, _, _)) =>
            val sc = parseScenario(cursor, pendingTags, file)
            scenarios ++= expandScenario(sc, featureBackground.map(_.steps).getOrElse(Nil), file)
          case Some(FeatureLine(_, _)) =>
            // Second Feature keyword — stop (multi-feature files are not parsed by this function)
            cont = false
          case Some(ExamplesLine(_, _, lineNo)) =>
            // Examples outside a scenario — report but skip
            cursor.advance()
          case None => cont = false
          case _    => cont = false
        }
      }

      Right(Feature(featureName.trim, featureTags, scenarios.result(), Some(file), Some(featureLineNo)))
    } catch {
      case pe: ParseError => Left(pe)
      case e: Exception   => Left(e)
    }

  // ── Public API ─────────────────────────────────────────────────────────────

  /**
   * Parse a single feature.
   *
   * Lint warnings (e.g. a line that looks like a misspelled step keyword) are
   * always logged via `ZIO.logWarning`. When `strict` is `true`, the presence
   * of any warning fails the parse instead of merely logging it.
   */
  def parseFeature(
    content: String,
    file: String = "unknown.feature",
    strict: Boolean = false
  ): ZIO[Any, Throwable, Feature] = {
    val normalised = preprocessContent(content)
    val tokens     = tokenise(normalised)
    val cursor     = new TokenCursor(tokens)
    parseFeatureFromTokens(cursor, file) match {
      case Left(err) => ZIO.fail(err)
      case Right(feature) =>
        val warnings = cursor.warnings
        ZIO.foreachDiscard(warnings)(w => ZIO.logWarning(w.render)) *>
          (if (strict && warnings.nonEmpty)
             ZIO.fail(
               ParseError(
                 s"${warnings.size} lint warning(s); first: ${warnings.head.message}",
                 warnings.head.lineNo,
                 file
               )
             )
           else ZIO.succeed(feature))
    }
  }

  def parseFeatureFile(file: File, strict: Boolean = false): ZIO[Any, Throwable, Feature] =
    ZIO.scoped {
      ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(file, "UTF-8"))).flatMap { source =>
        val content = source.mkString // preserve the trailing newline
        parseFeature(content, file.getAbsolutePath, strict)
      }
    }

  def loadFeatures(
    directory: File,
    failFast: Boolean = false,
    strict: Boolean = false
  ): ZIO[Any, Throwable, List[Feature]] =
    for {
      files <- ZIO.attempt {
                 Option(directory.listFiles())
                   .map(_.filter(_.getName.endsWith(".feature")).toList)
                   .getOrElse(Nil)
               }.orDie
      features <- if (files.isEmpty) {
                    ZIO.logWarning(s"No .feature files found in ${directory.getAbsolutePath}").as(Nil)
                  } else {
                    ZIO
                      .foreachPar(files) { file =>
                        parseFeatureFile(file, strict).foldZIO(
                          failure = err =>
                            if (failFast) ZIO.fail(err)
                            else ZIO.logError(s"Skipping ${file.getAbsolutePath}: ${err.getMessage}").as(None),
                          success = f => ZIO.succeed(Some(f))
                        )
                      }
                      .withParallelism(files.length min 4)
                      .map(_.flatten.toList)
                      .tap(fs => ZIO.logInfo(s"Loaded ${fs.length} features from ${directory.getAbsolutePath}"))
                  }
    } yield features
}
