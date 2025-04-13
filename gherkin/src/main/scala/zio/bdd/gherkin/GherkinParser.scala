package zio.bdd.gherkin

import zio.*
import fastparse.*
import fastparse.MultiLineWhitespace.*
import java.io.File
import scala.io.Source

case class Feature(
  name: String,
  tags: List[String] = Nil,
  background: List[Step] = Nil,
  scenarios: List[Scenario],
  file: Option[String] = None,
  line: Option[Int] = None
)

case class Scenario(
  name: String,
  tags: List[String] = Nil,
  steps: List[Step],
  examples: List[ExampleRow],
  file: Option[String] = None,
  line: Option[Int] = None
) {
  def isIgnored: Boolean = tags.exists(_.contains("ignore"))
}

enum StepType {
  case GivenStep
  case WhenStep
  case ThenStep
  case AndStep
}

case class DataTableRow(cells: List[String])

case class DataTable(headers: List[String], rows: List[DataTableRow])

case class Step(
  stepType: StepType,
  pattern: String,
  dataTable: Option[DataTable] = None,
  file: Option[String] = None,
  line: Option[Int] = None
) {
  def id: String = file.zip(line).map { case (f, l) => s"$f:$l" }.getOrElse("unknown")

  override def toString: String =
    s"${stepType.toString.replace("Step", "")} $pattern ${file.zip(line).map { case (f, l) => s"($f:$l)" }.getOrElse("")}"
}

case class ExampleRow(data: Map[String, String])

object GherkinParser {
  // Context to track file and content for line numbers
  case class ParseContext(file: String, content: String) {
    def lineAt(index: Int): Int = content.take(index).count(_ == '\n') + 1
  }

  // Whitespace parser: handles spaces, tabs, newlines, carriage returns
  def ws(using P[?]): P[Unit] = P(CharIn(" \t\n\r").rep)

  // Tag parser: captures tags like @retry(3), @flaky, @ignore or user-defined tags to organise features and scenarios
  def tag(using P[?]): P[String] = P("@" ~ CharsWhile(c => c.isLetterOrDigit || c == '_' || c == '(' || c == ')').!)

  // Keyword parser: handles Gherkin keywords with optional colon
  def keyword(using P[?]): P[String] = P(
    ("Feature" | "Background" | "Scenario Outline" | "Scenario" | "Given" | "When" | "Then" | "And" | "Examples") ~ (":".?)
  ).!.map(_.stripSuffix(":"))

  // Text parser: captures text until newline, including typed placeholders like {name:String}
  def text(using P[?]): P[String] = P(CharsWhile(_ != '\n').!)

  // Cell parser: captures text between '|' characters, trims whitespace
  def cell(using P[?]): P[String] = P(CharsWhile(_ != '|').!).map(_.trim)

  // Row parser: parses a table row starting and ending with '|'
  def row(using P[?]): P[List[String]] = P("|" ~/ cell.rep(sep = "|") ~ "|").map(_.toList)

  // Tags parser: captures zero or more tags
  def tags(using P[?]): P[List[String]] = P(tag.rep.map(_.toList))

  // Background parser: captures background steps
  def background(ctx: ParseContext)(using P[?]): P[List[Step]] =
    P("Background" ~ ":" ~/ ws ~ step(ctx).rep).map(_.toList)

  // DataTable parser: captures a table with headers and rows
  def dataTableParser(using P[?]): P[DataTable] = {
    import NoWhitespace._
    def cell: P[String] = P(CharsWhile(c => !"|\n".contains(c)).!).map(_.trim)

    def row: P[DataTableRow] = P(CharsWhile(_.isWhitespace).? ~ "|" ~ cell.rep(1, sep = "|") ~ "|" ~ "\n").map {
      cells => DataTableRow(cells.toList)
    }

    P(row.rep(1)).map { rows =>
      val headerRow = rows.head
      val dataRows  = rows.tail
      DataTable(headerRow.cells, dataRows.toList)
    }
  }

  // Step parser: captures step type as StepType and pattern separately
  def step(ctx: ParseContext)(using P[?]): P[Step] =
    P(Index ~ ("Given" | "When" | "Then" | "And").! ~ ":".? ~/ text ~ dataTableParser.?).map {
      case (idx, stepTypeStr, pattern, dataTableOpt) =>
        val stepType = stepTypeStr match {
          case "Given" => StepType.GivenStep
          case "When"  => StepType.WhenStep
          case "Then"  => StepType.ThenStep
          case "And"   => StepType.AndStep
        }
        Step(stepType, pattern.trim, dataTableOpt, Some(ctx.file), Some(ctx.lineAt(idx)))
    }

  // Examples parser: parses the Examples section with header and data rows
  def examples(using P[?]): P[List[ExampleRow]] =
    P("Examples" ~ ":" ~/ ws ~ row ~ (ws ~ Index ~ row).rep).map { case (header, rows) =>
      rows.map { case (idx, row) =>
        ExampleRow(header.zip(row).toMap)
      }.toList
    }

  // Scenario parser: supports both Scenario and Scenario Outline with tags
  def scenario(ctx: ParseContext)(using P[?]): P[Scenario] =
    P(
      tags ~ Index ~ (("Scenario" ~ !("Outline" ~ ":")) | "Scenario Outline") ~ ":" ~/ text ~ ws ~ step(
        ctx
      ).rep ~ examples.?
    ).map { case (tags, idx, name, steps, examplesOpt) =>
      Scenario(
        name.trim,
        tags,
        steps.toList,
        examplesOpt.getOrElse(Nil),
        Some(ctx.file),
        Some(ctx.lineAt(idx))
      )
    }

  // Feature parser: parses feature name, optional background, and one or more scenarios
  def feature(ctx: ParseContext)(using P[?]): P[Feature] =
    P(tags ~ "Feature" ~ ":" ~/ Index ~ text ~ ws ~ background(ctx).? ~ scenario(ctx).rep(1)).map {
      case (tags, idx, name, bgOpt, scenarios) =>
        Feature(name.trim, tags, bgOpt.getOrElse(Nil), scenarios.toList, Some(ctx.file), Some(ctx.lineAt(idx)))
    }

  // Top-level parser: parses the entire Gherkin content
  def gherkin(ctx: ParseContext)(using P[?]): P[Feature] =
    P(Start ~/ ws ~ feature(ctx) ~ ws ~ End)

  // Preprocess content to remove comments (lines starting with #)
  private def preprocessContent(content: String): String =
    content.linesIterator
      .map(line => if (line.trim.startsWith("#")) "" else line)
      .mkString("\n")

  // Helper method to parse content, converting Parsed.Failure to ZIO failure
  def parseFeature(content: String, file: String = "unknown.feature"): ZIO[Any, Throwable, Feature] =
    ZIO.fromEither {
      val cleanedContent = preprocessContent(content)
      val ctx            = ParseContext(file, cleanedContent)
      parse(cleanedContent, p => gherkin(ctx)(using p)) match {
        case Parsed.Success(feature, _) => Right(feature)
        case Parsed.Failure(label, index, extra) =>
          val inputSnippet = cleanedContent.linesIterator.drop(ctx.lineAt(index) - 1).nextOption() match {
            case Some(line) => s"near: '$line' (index $index)"
            case None       => "at end of input"
          }
          Left(
            new Exception(
              s"Failed to parse Gherkin content at index $index: expected $label\nInput: $inputSnippet\nTrace: ${extra.trace().msg}"
            )
          )
      }
    }

  // Parse a single feature file
  def parseFeatureFile(file: File): ZIO[Any, Throwable, Feature] =
    ZIO.scoped {
      ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(file))).flatMap { source =>
        val content = source.getLines().mkString("\n")
        parseFeature(content, file.getAbsolutePath)
      }
    }

  // Load all feature files from a directory
  def loadFeatures(directory: File): ZIO[Any, Throwable, List[Feature]] =
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
                      .foreachPar(files)(parseFeatureFile)
                      .withParallelism(files.length min 4)
                      .tap(features =>
                        ZIO.logInfo(s"Loaded ${features.length} features from ${directory.getAbsolutePath}")
                      )
                  }
    } yield features
}
