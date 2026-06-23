# Running Tests in zio-bdd Example

This README provides instructions to run the tests for the `zio-bdd` example located in `example/src/test/scala/zio/bdd/example/SimpleSpec.scala` and `example/src/test/resources/features/simple.feature`.

## Prerequisites

- **Scala 3**: Ensure you have Scala 3 installed.
- **SBT**: Use SBT 1.9.x or later.
- **Dependencies**: Add the following to your `build.sbt`:
  ```scala
  libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "1.0.0" % Test
  libraryDependencies += "dev.zio" %% "zio" % "2.1.17"
  Test / testFrameworks += new TestFramework("zio.bdd.ZIOBDDFramework")
  ```

## Test Setup

The example includes a feature file with tagged scenarios:
- Feature: `Simple Greeting` (`@core @greeting`)
- Scenarios:
    - `Greet a user` (`@positive @smoke`)
    - `Greet an empty user` (`@ignore`)
    - `Greet a different user` (`@negative`)

## Running Tests

Use the following SBT commands from the project root to run the tests:

### 1. Run All Tests
```bash
sbt "example/test"
```
- Runs all scenarios filtered by `@Suite(includeTags = Array("positive"))`, executing only `@positive` scenarios by default.

> `testOnly` must be scoped to the `example` project (`example/testOnly ...`) — the root
> project only aggregates `core` and `gherkin`, so an unscoped `testOnly` from the repo root
> finds no tests at all.

### 2. Filter by Including Tags
```bash
sbt "example/testOnly zio.bdd.example.SimpleSpec -- --include-tags smoke"
```
- Runs only scenarios with `@smoke` (`Greet a user`). `--include-tags` fully replaces the
  `@Suite`'s `includeTags = Array("positive")` for this run.

### 3. Filter by Excluding Tags
```bash
sbt "example/testOnly zio.bdd.example.SimpleSpec -- --exclude-tags ignore"
```
- `--include-tags` and `--exclude-tags` are resolved independently: since this command doesn't
  set `--include-tags`, the `@Suite`'s `includeTags = Array("positive")` is still in effect.
  Only `Greet a user` runs; `Greet a different user` (`@negative`) stays excluded.

### 4. Combine Include and Exclude Filters
```bash
sbt "example/testOnly zio.bdd.example.SimpleSpec -- --include-tags positive --exclude-tags flaky"
```
- Runs `@positive` scenarios excluding `@flaky` (`Greet a user` — none of the scenarios carry
  `@flaky` here, so this behaves the same as `--include-tags positive` alone).

### 5. Test Feature-Level Tags
```bash
sbt "example/testOnly zio.bdd.example.SimpleSpec -- --include-tags core"
```
- The `@core` tag on the `Feature:` line is inherited by every scenario, so this matches
  `Greet a user` and `Greet a different user`. `Greet an empty user` still shows as `IGNORED` —
  `@ignore` is a built-in status tag handled separately from `--include-tags`/`--exclude-tags`
  filtering, so it's never run regardless of which tags are included.

### 6. Debug Mode (Verbose Output)
```bash
sbt --debug "example/test"
```
- Runs all tests with detailed logging.

## Notes
- Ensure the feature file is at `example/src/test/resources/features/simple.feature`.
- `--include-tags`/`--exclude-tags` on the command line each independently replace the
  corresponding `@Suite` annotation field *only if supplied*; an omitted CLI flag falls back to
  the `@Suite` value for that field. They do not merge with or fully override the annotation as
  a pair.
- `@ignore` always marks a scenario `IGNORED`, independent of `--include-tags`/`--exclude-tags`.
