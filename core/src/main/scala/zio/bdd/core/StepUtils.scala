package zio.bdd.core

import scala.util.matching.Regex

object StepUtils {
  def convertToRegex(pattern: String): Regex =
    if (pattern.contains("{") || pattern.contains("}")) {
      println(s"Converting pattern: $pattern")
      val result = pattern
        .replace("{string}", "(.+)")
        .replace("{int}", "(\\d+)")
        .replace("{float}", "(\\d+\\.\\d+)")
        .replace("{double}", "(\\d+\\.\\d+)")
        .replace("{boolean}", "(true|false)")
        .replaceAll("\\{[^:]+:[^}]+\\}", "(.+)")
        .r
      println(s"Converted pattern: $result")
      result
    } else {
      pattern.r
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
      println(s"Mismatch: subgroups=$subgroups, placeholders=$placeholders")
      List()
    } else {
      val params = subgroups.zip(placeholders).map { case (param, placeholder) =>
        val parsed = parseParam(param, placeholder)
        println(s"Parsed: $param -> $parsed (${parsed.getClass.getSimpleName}) for $placeholder")
        parsed
      }
      println(s"Extracted params: $params ${params.map(_.getClass.getSimpleName).mkString(",")}")
      params
    }
  }

  def parseParam(param: String, placeholder: String): Any = {
    val parts           = placeholder.stripPrefix("{").stripSuffix("}").split(":")
    val placeholderType = if (parts.length > 1) parts(1).toLowerCase else parts(0).toLowerCase
    println(s"Parsing param: $param, placeholder: $placeholder, type: $placeholderType")
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
