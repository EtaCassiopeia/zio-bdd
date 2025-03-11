# Running Tests in zio-bdd Example

This README provides instructions to run the tests for the `zio-bdd` example located in `example/src/test/scala/zio/bdd/example/SimpleSpec.scala` and `example/src/test/resources/features/simple.feature`.

## Prerequisites

- **Scala 3**: Ensure you have Scala 3 installed.
- **SBT**: Use SBT 1.9.x or later.
- **Dependencies**: Add the following to your `build.sbt`:
  ```scala
  libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "0.1.0" % Test // Not yet published, use local build
  libraryDependencies += "dev.zio" %% "zio" % "2.1.16"
  Test / testFrameworks += new TestFramework("zio.bdd.core.ZIOBDDFramework")
  ```

## Test Setup

The example includes a feature file with tagged scenarios:
- Feature: `Simple Greeting` (`@core @greeting`)
- Scenarios:
    - `Greet a user` (`@positive @smoke`)
    - `Greet an empty user` (`@negative @ignore`)
    - `Greet a different user` (`@positive @flaky @retry(2)`)

## Running Tests

Use the following SBT commands from the project root to run the tests:

### 1. Run All Tests
```bash
sbt "example/test"
```
- Runs all scenarios filtered by `@Suite(includeTags = Array("positive"))`, executing only `@positive` scenarios by default.

### 2. Filter by Including Tags
```bash
sbt "testOnly zio.bdd.example.SimpleSpec -- --include-tags smoke"
```
- Runs only scenarios with `@smoke` (e.g., `Greet a user`).

### 3. Filter by Excluding Tags
```bash
sbt "testOnly zio.bdd.example.SimpleSpec -- --exclude-tags ignore"
```
- Runs scenarios without `@ignore` (e.g., `Greet a user` and `Greet a different user`).

### 4. Combine Include and Exclude Filters
```bash
sbt "testOnly zio.bdd.example.SimpleSpec -- --include-tags positive --exclude-tags flaky"
```
- Runs `@positive` scenarios excluding `@flaky` (e.g., `Greet a user`).

### 5. Test Feature-Level Tags
```bash
sbt "testOnly zio.bdd.example.SimpleSpec -- --include-tags core"
```
- Runs all scenarios because the feature has `@core`.

### 6. Debug Mode (Verbose Output)
```bash
sbt --debug "example/test"
```
- Runs all tests with detailed logging.

## Notes
- Ensure the feature file is at `example/src/test/resources/features/simple.feature`.
- Tag filters override the `@Suite` configuration when specified via the command line.
