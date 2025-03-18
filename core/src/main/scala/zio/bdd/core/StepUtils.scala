package zio.bdd.core

import scala.util.matching.Regex

object StepUtils {
  def convertToRegex(pattern: String): Regex = {
    val baseRegex = if (pattern.contains("{") || pattern.contains("}")) {
      pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(-?\\d+)")
        .replace("{float}", "(-?\\d+\\.\\d+)")
        .replace("{double}", "(-?\\d+\\.\\d+)")
        .replace("{boolean}", "(true|false)")
        .replaceAll("\\{[^:]+:[^}]+\\}", "(.+)")
    } else {
      pattern
    }

    // Add anchors only if not already present
    val startsWithAnchor = baseRegex.startsWith("^")
    val endsWithAnchor   = baseRegex.endsWith("$")
    val anchoredPattern = (startsWithAnchor, endsWithAnchor) match {
      case (true, true)   => baseRegex
      case (true, false)  => s"$baseRegex$$"
      case (false, true)  => s"^$baseRegex"
      case (false, false) => s"^$baseRegex$$"
    }

    anchoredPattern.r
  }

  def extractParams(pattern: Regex, line: String, patternString: String): List[Any] = {
    val trimmedLine  = line.trim
    val matchResult  = pattern.findFirstMatchIn(trimmedLine)
    val subgroups    = matchResult.map(_.subgroups).getOrElse(Nil)
    val placeholders = patternString.split("\\s+").filter(_.matches("\\{[^:]+(:[^}]+)?\\}")).toList

    if (subgroups.isEmpty) {
      List()
    } else if (placeholders.isEmpty) {
      subgroups
    } else if (subgroups.length != placeholders.length) {
      List()
    } else {
      val params = subgroups.zip(placeholders).map { case (param, placeholder) =>
        val parsed = parseParam(param, placeholder)
        parsed
      }
      params
    }
  }

  def parseParam(param: String, placeholder: String): Any = {
    val parts           = placeholder.stripPrefix("{").stripSuffix("}").split(":")
    val placeholderType = if (parts.length > 1) parts(1).toLowerCase else parts(0).toLowerCase
    placeholderType match {
      case "int"     => param.toInt
      case "float"   => param.toFloat
      case "double"  => param.toDouble
      case "boolean" => param.toBoolean
      case "string"  => param.trim
      case _         => param.trim
    }
  }
}
