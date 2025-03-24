package zio.bdd.core

import scala.util.matching.Regex
import izumi.reflect.Tag

object StepUtils {
  def convertToRegex(pattern: String): Regex =
    if (pattern.startsWith("^") && pattern.endsWith("$")) {
      // Regular expression pattern, use as-is
      pattern.r
    } else {
      // Gherkin-style pattern, wrap with ^ and $ and convert placeholders
      val baseRegex = pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(-?\\d+)")
        .replace("{float}", "(-?\\d+\\.\\d+)")
        .replace("{double}", "(-?\\d+\\.\\d+)")
        .replace("{boolean}", "(true|false)")
        .replaceAll("\\{[^:]+:[^}]+\\}", "(.+)")
      s"^$baseRegex$$".r
    }

  def extractParams(pattern: Regex, line: String, patternString: String): (List[Any], List[Tag[Any]]) = {
    val trimmedLine = line.trim
    val matchResult = pattern.findFirstMatchIn(trimmedLine)
    val subgroups   = matchResult.map(_.subgroups).getOrElse(Nil)

    val (placeholders, isRegexPattern) = if (patternString.startsWith("^") && patternString.endsWith("$")) {
      // Regular expression pattern, extract capturing groups
      val placeholderPattern = """\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\)""".r
      (placeholderPattern.findAllIn(patternString).toList, true)
    } else {
      // Gherkin-style pattern, extract {} placeholders
      val tokens             = patternString.split("\\s+")
      val placeholderPattern = """\{[^:]+(?::[^}]+)?\}""".r
      val placeholders       = tokens.filter(token => placeholderPattern.pattern.matcher(token).matches()).toList
      (placeholders, false)
    }

    if (subgroups.isEmpty || placeholders.isEmpty || subgroups.length != placeholders.length) {
      (List(), List())
    } else {
      val typeMap: Map[String, (String => Any, Tag[_])] = Map(
        "int"     -> ((s: String) => s.toInt, Tag[Int]),
        "string"  -> ((s: String) => s.trim, Tag[String]),
        "float"   -> ((s: String) => s.toFloat, Tag[Float]),
        "double"  -> ((s: String) => s.toDouble, Tag[Double]),
        "boolean" -> ((s: String) => s.toBoolean, Tag[Boolean])
      )

      val (values, tags) = subgroups
        .zip(placeholders)
        .map { case (param, placeholder) =>
          if (isRegexPattern) {
            // Regex pattern: treat capturing groups as String
            (param, Tag[String].asInstanceOf[Tag[Any]])
          } else {
            // Gherkin-style: parse type from placeholder
            val placeholderType = placeholder.stripPrefix("{").stripSuffix("}").split(":").last.toLowerCase
            val (parser, tag)   = typeMap.getOrElse(placeholderType, typeMap("string"))
            val parsedValue     = parser(param)
            (parsedValue, tag.asInstanceOf[Tag[Any]])
          }
        }
        .unzip

      (values, tags)
    }
  }
}
