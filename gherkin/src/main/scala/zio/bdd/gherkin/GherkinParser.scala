package zio.bdd.gherkin

import zio.*
import fastparse.*
import fastparse.MultiLineWhitespace.*
import zio.bdd.core.{ExampleRow, Feature, Scenario, ScenarioMetadata}

import java.io.File
import scala.io.Source

object GherkinParser {
  // Whitespace parser: handles spaces, tabs, newlines, carriage returns
  def ws(using P[_]): P[Unit] = P(CharIn(" \t\n\r").rep)

  // Tag parser: captures tags like @Retry(3), @Flaky
  def tag(using P[_]): P[String] = P("@" ~ CharsWhile(c => c.isLetterOrDigit || c == '_' || c == '(' || c == ')').!)

  // Keyword parser: handles Gherkin keywords with optional colon
  def keyword(using P[_]): P[String] = P(
    ("Feature" | "Background" | "Scenario Outline" | "Scenario" | "Given" | "When" | "Then" | "And" | "Examples") ~ (":".?)
  ).!.map(_.stripSuffix(":"))

  // Text parser: captures text until newline
  def text(using P[_]): P[String] = P(CharsWhile(_ != '\n').!)

  // Cell parser: captures text between '|' characters, trims whitespace
  def cell(using P[_]): P[String] = P(CharsWhile(_ != '|').!).map(_.trim)

  // Row parser: parses a table row starting and ending with '|'
  def row(using P[_]): P[List[String]] = P("|" ~/ cell.rep(sep = "|") ~ "|").map(_.toList)

  // Tags parser: captures zero or more tags
  def tags(using P[_]): P[List[String]] = P(tag.rep.map(_.toList))

  // Background parser: captures background steps
  def background(using P[_]): P[List[String]] = P("Background" ~ ":" ~/ ws ~ step.rep).map(_.toList)

  // Step parser: parses steps with optional colon
  def step(using P[_]): P[String] = P(("Given" | "When" | "Then" | "And") ~ ":".? ~/ text ~ ws).map(_.trim)

  // Examples parser: parses the Examples section with header and data rows
  def examples(using P[_]): P[List[ExampleRow]] =
    P("Examples" ~ ":" ~/ ws ~ row ~ (ws ~ row).rep).map { case (header, rows) =>
      rows.map(row => ExampleRow(header.zip(row).toMap)).toList
    }

  // Scenario parser: supports both Scenario and Scenario Outline with tags
  def scenario(using P[_]): P[Scenario] =
    P(tags ~ (("Scenario" ~ !("Outline" ~ ":")) | "Scenario Outline") ~ ":" ~/ text ~ ws ~ step.rep ~ examples.?).map {
      case (tags, name, steps, examplesOpt) =>
        val metadata = parseMetadata(tags)
        Scenario(name.trim, steps.toList, examplesOpt.getOrElse(Nil), metadata)
    }

  // Feature parser: parses feature name, optional background, and one or more scenarios
  def feature(using P[_]): P[Feature] =
    P("Feature" ~ ":" ~/ text ~ ws ~ background.? ~ scenario.rep(1)).map { case (name, bgOpt, scenarios) =>
      Feature(name.trim, bgOpt.getOrElse(Nil), scenarios.toList)
    }

  // Top-level parser: parses the entire Gherkin content
  def gherkin(using P[_]): P[Feature] =
    P(Start ~/ ws ~ feature ~ ws ~ End)

  // Helper method to parse content, converting Parsed.Failure to ZIO failure
  def parseFeature(content: String): ZIO[Any, Throwable, Feature] =
    ZIO.fromEither {
      parse(content, p => gherkin(using p)) match {
        case Parsed.Success(feature, _) => Right(feature)
        case Parsed.Failure(label, index, extra) =>
          val inputSnippet = content.linesIterator.drop(index / content.length).nextOption() match {
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

  // Parse metadata from tags
  private def parseMetadata(tags: List[String]): ScenarioMetadata = {
    val retryCount  = tags.collectFirst { case s"Retry($n)" => n.toInt }.getOrElse(0)
    val isFlaky     = tags.contains("Flaky")
    val repeatCount = tags.collectFirst { case s"Repeat($n)" => n.toInt }.getOrElse(1)
    ScenarioMetadata(retryCount, isFlaky, repeatCount)
  }

  // Parse a single feature file with ZIO resource management and detailed error handling
  def parseFeatureFile(file: File): ZIO[Any, Throwable, Feature] =
    ZIO.scoped {
      ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(file))).flatMap { source =>
        val content = source.getLines().mkString("\n")
        parseFeature(content)
      }
    }

  // Load all feature files from a directory with ZIO concurrency and logging
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
