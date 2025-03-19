package zio.bdd.core.report

import zio.bdd.core.TestError

object ErrorExplainer {
  def explain(error: TestError): String = error match {
    case TestError.GenericError(msg, cause, trace) =>
      s"Error: $msg${cause.map(c => s"\nCaused by: ${c.getMessage}").getOrElse("")}${trace.map(t => s"\nTrace: $t").getOrElse("")}"
    case TestError.TypeMismatch(expected, actual, input, cause, trace) =>
      s"Type mismatch: expected $expected, got $actual for input $input${cause
          .map(c => s"\nCaused by: ${c.getMessage}")
          .getOrElse("")}${trace.map(t => s"\nTrace: $t").getOrElse("")}"
    case TestError.MissingStep(step, cause, trace) =>
      s"Missing step definition for: $step${cause.map(c => s"\nCaused by: ${c.getMessage}").getOrElse("")}${trace
          .map(t => s"\nTrace: $t")
          .getOrElse("")}"
  }
}
