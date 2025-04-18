# JUnit XML Reporter Reference

This document provides an overview of the two JUnit XML report formats—Legacy (JUnit 4 and earlier) and Jupiter (JUnit 5 / Open Test Reporting).

## Overview

The framework aims to generate test reports in two distinct XML formats:
- **Legacy Format**: A widely adopted, suite-based structure compatible with most CI tools (e.g., Jenkins, Maven Surefire).
- **Jupiter Format**: An event-driven, hierarchical structure introduced with JUnit 5, designed for modern testing frameworks but less supported by legacy tools.

Supporting both formats ensures broad compatibility while preparing for future adoption of the Jupiter standard. This section details each format’s structure, components, and considerations for mapping Gherkin-based test results.

---

## Legacy JUnit XML Format (JUnit 4 and Earlier)

The Legacy JUnit XML format is a flat, hierarchical structure originally popularized by the Ant JUnit task. It’s the de facto standard for test reporting in many CI/CD pipelines.

### Structure

```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="Test run" tests="8" failures="1" errors="1" skipped="1" assertions="20" time="16.082687" timestamp="2021-04-02T15:48:23">
  <testsuite name="Tests.Registration" tests="8" failures="1" errors="1" skipped="1" assertions="20" time="16.082687" timestamp="2021-04-02T15:48:23" file="tests/registration.code">
    <properties>
      <property name="version" value="1.774"/>
    </properties>
    <system-out>Data written to standard out.</system-out>
    <system-err>Data written to standard error.</system-err>
    <testcase name="testCase1" classname="Tests.Registration" assertions="2" time="2.436" file="tests/registration.code" line="24"/>
    <testcase name="testCase5" classname="Tests.Registration" assertions="2" time="2.902412" file="tests/registration.code" line="202">
      <failure message="Expected value did not match." type="AssertionError">
        <!-- Stack trace -->
      </failure>
    </testcase>
  </testsuite>
</testsuites>
```

### Components

1. **`<testsuites>` (Optional Root Element)**
    - Represents the entire test run, aggregating multiple test suites.
    - **Attributes**:
        - `name`: Descriptive name of the test run (e.g., "Test run").
        - `tests`: Total number of tests.
        - `failures`: Total failed tests.
        - `errors`: Total errored tests (unexpected exceptions).
        - `skipped`: Total skipped tests.
        - `assertions`: Total assertions (optional).
        - `time`: Total execution time in seconds.
        - `timestamp`: ISO 8601 timestamp (e.g., "2021-04-02T15:48:23").
    - **Notes**: Omitted if there’s only one `<testsuite>`, which then becomes the root.

2. **`<testsuite>`**
    - Groups related tests (e.g., a Gherkin feature).
    - **Attributes**: Same as `<testsuites>` but scoped to the suite, plus:
        - `file`: Source file path (e.g., "tests/registration.code").
    - **Children**:
        - `<properties>`: Metadata key-value pairs.
        - `<system-out>`: Standard output for the suite.
        - `<system-err>`: Standard error for the suite.
        - Multiple `<testcase>` elements.

3. **`<testcase>`**
    - Represents an individual test (e.g., a Gherkin scenario).
    - **Attributes**:
        - `name`: Test name (e.g., "Scenario: Valid Registration").
        - `classname`: Parent suite/class name (e.g., "Tests.Registration").
        - `assertions`: Number of assertions (optional).
        - `time`: Execution time in seconds.
        - `file`: Source file path (optional).
        - `line`: Line number in source file (optional).
    - **Children**:
        - `<skipped>`: Indicates a skipped test (`message` optional).
        - `<failure>`: Assertion failure (`message`, `type`, stack trace).
        - `<error>`: Unexpected error (`message`, `type`, stack trace).
        - `<system-out>`: Test-specific output.
        - `<system-err>`: Test-specific error output.
        - `<properties>`: Test-specific metadata (optional).

4. **`<properties>`**
    - Stores metadata at suite or test level.
    - Contains `<property>` elements with `name` and `value` attributes or text content.

### Characteristics
- **Hierarchy**: Flat, with suites containing test cases.
- **Results**: Implicitly successful unless marked with `<skipped>`, `<failure>`, or `<error>`.
- **Compatibility**: Broadly supported by CI tools (e.g., Jenkins, GitLab CI).

---

## Jupiter JUnit XML Format (JUnit 5 / Open Test Reporting)

The Jupiter format, part of the Open Test Reporting initiative, is an event-based structure introduced with JUnit 5. It’s designed for flexibility and framework-agnostic reporting.

### Structure

```xml
<?xml version="1.0"?>
<e:events xmlns:e="http://opentest4j.org/opentest-reporting" xmlns:junit="http://junit.org/junit5">
  <e:started id="1" name="JUnit Jupiter" time="2022-10-07T10:57:29.193634500Z">
    <metadata>
      <junit:uniqueId>[engine:junit-jupiter]</junit:uniqueId>
      <junit:legacyReportingName>JUnit Jupiter</junit:legacyReportingName>
      <junit:type>CONTAINER</junit:type>
    </metadata>
  </e:started>
  <e:started id="2" name="HelloTest" parentId="1" time="2022-10-07T10:57:29.247574500Z">
    <metadata>
      <junit:uniqueId>[engine:junit-jupiter]/[class:com.example.HelloTest]</junit:uniqueId>
      <junit:legacyReportingName>com.example.HelloTest</junit:legacyReportingName>
      <junit:type>CONTAINER</junit:type>
    </metadata>
    <sources>
      <java:classSource className="com.example.HelloTest"/>
    </sources>
  </e:started>
  <e:started id="3" name="testOne()" parentId="2" time="2022-10-07T10:57:29.269523200Z">
    <metadata>
      <junit:uniqueId>[engine:junit-jupiter]/[class:com.example.HelloTest]/[method:testOne()]</junit:uniqueId>
      <junit:legacyReportingName>testOne()</junit:legacyReportingName>
      <junit:type>TEST</junit:type>
    </metadata>
    <sources>
      <java:methodSource className="com.example.HelloTest" methodName="testOne" methodParameterTypes=""/>
    </sources>
  </e:started>
  <e:finished id="3" time="2022-10-07T10:57:29.307527100Z">
    <result status="SUCCESSFUL"/>
  </e:finished>
  <e:finished id="2" time="2022-10-07T10:57:29.325526800Z">
    <result status="SUCCESSFUL"/>
  </e:finished>
  <e:finished id="1" time="2022-10-07T10:57:29.331528600Z">
    <result status="SUCCESSFUL"/>
  </e:finished>
</e:events>
```

### Components

1. **`<e:events>` (Root Element)**
    - Encapsulates all test execution events.
    - Uses namespaces (e.g., `e:` for events, `junit:` for JUnit-specific metadata).

2. **`<e:started>`**
    - Marks the start of a test entity (container or test).
    - **Attributes**:
        - `id`: Unique event identifier.
        - `name`: Display name (e.g., "testOne()").
        - `parentId`: Links to parent container’s `id`.
        - `time`: ISO 8601 start timestamp.
    - **Children**:
        - `<metadata>`:
            - `<junit:uniqueId>`: Unique identifier (e.g., "[engine:junit-jupiter]/[class:com.example.HelloTest]").
            - `<junit:legacyReportingName>`: Legacy-compatible name.
            - `<junit:type>`: "CONTAINER" or "TEST".
        - `<sources>`: Source info (e.g., `<java:classSource>` or `<java:methodSource>`).

3. **`<e:finished>`**
    - Marks the end of a test entity.
    - **Attributes**:
        - `id`: Matches the corresponding `<e:started>` event.
        - `time`: ISO 8601 finish timestamp.
    - **Children**:
        - `<result>`:
            - `status`: "SUCCESSFUL", "FAILED", "SKIPPED", or "ABORTED".
            - `<java:throwable>` (if failed): Exception details with `type` and stack trace.

### Characteristics
- **Hierarchy**: Deep nesting via `parentId`.
- **Results**: Explicit status in `<result>`.
- **Compatibility**: Limited support in legacy CI tools; requires adapters.

---

## Mapping Gherkin to JUnit Formats

Our framework uses extended Gherkin syntax (features, scenarios, steps). Here’s how these map to each format:

### Legacy Format
- **Feature**: `<testsuite>` (e.g., `name="Feature: User Registration"`).
- **Scenario**: `<testcase>` (e.g., `name="Scenario: Valid Registration"`).
- **Steps**: Aggregated into the `<testcase>` result (e.g., a step failure becomes `<failure>`).
- **Metadata**: Use `<properties>` for tags or ZIO-specific data.
- **Output**: Capture logs in `<system-out>`/`<system-err>`.

### Jupiter Format
- **Feature**: Container `<e:started>`/`<e:finished>` pair (e.g., `name="Feature: User Registration"`).
- **Scenario**: Nested container or test event (e.g., `name="Scenario: Valid Registration"`).
- **Steps**: Individual test events or aggregated into the scenario result.
- **Metadata**: Use `<metadata>` and `<sources>` for traceability.

---

## Implementation Considerations

- **Dual Support**: Generate both formats or allow configuration to select one.
- **Legacy Compatibility**: Ensure Jupiter reports include `<junit:legacyReportingName>` for partial backward compatibility.

---

## References

- [Legacy Format Example](https://github.com/testmoapp/junitxml)
- [Jupiter Format Example](https://howtodoinjava.com/junit5/xml-reports/)
- JUnit 5 Documentation: [Open Test Reporting](https://junit.org/junit5/docs/current/user-guide/#running-tests-listeners-opentest)
