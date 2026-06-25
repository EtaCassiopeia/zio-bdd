package zio.bdd.core.step

/**
 * A runtime-accurate summary of a registered step definition, produced by
 * `ZIOSteps.allDefinitions`. Consumed by external tooling (LSP server, IntelliJ
 * plugin) to populate accurate step indexes without static source scanning.
 *
 * @param keyword
 *   BDD keyword (Given, When, Then, And, But)
 * @param pattern
 *   The full regex the step matches against — same pattern `StepRegistry` uses
 *   at runtime, so extractor types are correctly reflected (e.g. a `/ int /`
 *   extractor produces `\-?\d+` rather than the literal string "int").
 * @param displayText
 *   Human-readable representation for UI (e.g. `"the cart has " / int / " items"`).
 */
case class StepSummary(keyword: String, pattern: String, displayText: String)
