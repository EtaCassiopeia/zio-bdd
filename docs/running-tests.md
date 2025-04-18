
# Running Tests in zio-bdd

This page explains how to run your `zio-bdd` tests, including configuration options, command-line arguments, and reporting formats. Understanding these options will help you integrate `zio-bdd` into your development and CI/CD workflows effectively.

## Basic Test Execution

To run all tests defined in your `@Suite`-annotated specs, use the standard SBT test command:

```bash
sbt test
```

This command discovers and executes all scenarios in the feature files specified in your `@Suite` configurations.

## Configuring the Test Suite

The `@Suite` annotation on your `ZIOSteps` subclass allows you to configure various aspects of test execution:

```scala
@Suite(
  featureDir = "src/test/resources/features",
  reporters = Array("pretty", "junitxml"),
  parallelism = 1,
  includeTags = Array("positive"),
  excludeTags = Array("ignore"),
  logLevel = "debug"
)
object MySpec extends ZIOSteps[Env, State] {
  // ...
}
```

- **featureDir**: Directory containing `.feature` files.
- **reporters**: Output formats (e.g., `"pretty"`, `"junitxml"`).
- **parallelism**: Number of scenarios to run concurrently.
- **includeTags**: Only run scenarios with these tags.
- **excludeTags**: Skip scenarios with these tags.
- **logLevel**: Logging verbosity (e.g., `"debug"`, `"info"`).

## Command-Line Options

You can override `@Suite` settings or provide additional filters via command-line arguments when running tests.

### Tag Filters

- **Include Tags**: Run only scenarios with specific tags.
  ```bash
  sbt "testOnly * -- --include-tags smoke"
  ```
- **Exclude Tags**: Skip scenarios with specific tags.
  ```bash
  sbt "testOnly * -- --exclude-tags ignore"
  ```
- **Combine Filters**: Use both include and exclude tags.
  ```bash
  sbt "testOnly * -- --include-tags positive --exclude-tags flaky"
  ```

### Feature File Selection

- **Run Specific Feature**: Execute only a particular feature file.
  ```bash
  sbt "testOnly * -- --feature-file src/test/resources/features/greeting.feature"
  ```

### Reporting

- **Specify Reporters**: Choose output formats.
  ```bash
  sbt "testOnly * -- --reporters pretty,junitxml"
  ```

### Logging

- **Set Log Level**: Adjust verbosity.
  ```bash
  sbt "testOnly * -- --log-level debug"
  ```

## Reporters

`zio-bdd` supports multiple reporters for test output:

- **pretty**: Human-readable console output.
- **junitxml**: JUnit XML format for CI integration.

You can specify multiple reporters in the `@Suite` annotation or via the command line.

## Parallelism

Control the number of scenarios running concurrently:

- **In @Suite**: Set `parallelism` to a positive integer.
- **Command Line**: Override with `--parallelism N`.

Use parallelism to speed up test execution, but ensure your tests are isolated to avoid interference.

## Best Practices

- **Tag Wisely**: Use tags to categorize tests (e.g., `@smoke`, `@integration`).
- **CI Integration**: Leverage JUnit XML for CI tools like Jenkins or GitHub Actions.
- **Debugging**: Increase log level to `"debug"` for detailed step execution logs.

## Next Steps

- Explore advanced configurations in [Advanced Features](advanced-features.md).
- See practical examples in [Examples](examples.md).
