# Gherkin Syntax Reference

zio-bdd implements a line-oriented Gherkin parser. This document is the complete syntax
reference for every construct the parser accepts. If it is not listed here, it is not
supported.

---

## File structure

A feature file is a UTF-8 text file with the `.feature` extension. Every file contains
exactly one `Feature:` block. The parser handles:

- UTF-8 BOM at the start of the file — stripped transparently.
- Windows CRLF line endings — normalised to LF before parsing.
- `# language: en` directive lines — stripped without error (only English keywords
  are currently recognised; multi-language keyword sets are not implemented).
- Trailing newline optional — files that end without a final newline are accepted.

The general structure of a file from top to bottom is:

```
[feature-level tags]
Feature: <name>
  [description paragraph]
  [Background:]
  [Rule: blocks | Scenario blocks...]
```

---

## Keywords

### Feature

```gherkin
Feature: Payment Processing
```

`Feature:` is the only mandatory keyword. Everything else is optional. The feature name
is the text on the same line after `Feature:`. It may be empty.

A description paragraph — free-form prose lines — may follow `Feature:` before the first
`Background:`, `Rule:`, or `Scenario:`. Description text is silently ignored; it is not
stored or validated.

A feature with no scenarios is valid. This is useful for documenting planned behaviour
before writing step definitions.

---

### Background

```gherkin
Background:
  Given the database is seeded
  And the audit log is empty
```

`Background:` precedes the scenarios in a feature and contains steps that are
automatically prepended to every scenario's step list. The background steps are
indistinguishable from ordinary steps once expanded — they carry the same types
(`Given`, `When`, `Then`, etc.) and can have data tables and doc strings.

**Rule inner Background:** A `Rule:` block may contain its own `Background:`. Inner
background steps are appended after the feature-level background steps:

```gherkin
Feature: Account Lifecycle

  Background:
    Given the system is running         # prepended to every scenario

  Rule: EOD must be idempotent

    Background:
      Given a provisioned account       # prepended only to this rule's scenarios

    Scenario: EOD is idempotent
      When EOD runs twice
      Then the state is unchanged
      # step list: "the system is running", "a provisioned account", "When...", "Then..."
```

There is at most one `Background:` per feature and one per `Rule:` block.

---

### Scenario

```gherkin
Scenario: Happy path login
  Given a valid user exists
  When the user submits credentials
  Then a session token is returned
```

`Scenario:` introduces a single test case. The name is the text after the colon on the
same line. The name may be empty.

**Alias:** `Example:` is accepted as a synonym for `Scenario:`.

```gherkin
Example: Happy path login
  Given ...
```

---

### Rule

```gherkin
Rule: Accounts must be active before posting

  Scenario: posting to an inactive account fails
    Given an inactive account
    When a post request is sent
    Then a 422 response is returned
```

`Rule:` groups related scenarios under a business-rule heading. The rule name is free
text. A `Rule:` block may contain:

- An optional inner `Background:`.
- Zero or more `Scenario:` / `Scenario Outline:` blocks.

Rules may not be nested. The feature may contain any mix of top-level scenarios and
`Rule:` blocks; however, mixing them in the same file is not spec-valid Gherkin, though
the parser tolerates it.

---

### Scenario Outline / Scenario Template

```gherkin
Scenario Outline: Post a <type> transaction
  Given a provisioned account
  When a <type> post of <amount> is sent
  Then the balance changes by <amount>
```

`Scenario Outline:` (alias: `Scenario Template:`) is a parameterised scenario. Angle-
bracket placeholders — `<columnName>` — are substituted with values from the `Examples:`
table. Each data row in the `Examples:` table produces one expanded `Scenario`.

The expanded scenario name is:

```
<outline name> - Example <N>
```

or, when the `Examples:` block has a name:

```
<outline name> - <examples name> - Example <N>
```

If a placeholder used in a step has no matching column header in the `Examples:` table
the step is silently dropped from the expanded scenario — no step-not-found error or
warning is raised.

---

### Examples / Scenarios

```gherkin
Examples:
  | type       | amount |
  | deposit    | 100    |
  | withdrawal | 50     |
```

`Examples:` (alias: `Scenarios:`) follows a `Scenario Outline:` block. It is a data
table where the first row is the header and every subsequent row produces one scenario
expansion. Column names must match the `<placeholder>` names in the outline.

**Named blocks:** an `Examples:` block may have a name:

```gherkin
Examples: Happy path
  | user  |
  | Alice |
```

**Multiple blocks:** a single outline may have multiple `Examples:` blocks. Each block
produces its own set of expanded scenarios independently.

**Per-block tags:** tags placed before an `Examples:` block are applied to all scenarios
expanded from that block. They are merged with the tags from the enclosing `Scenario
Outline:`.

```gherkin
@smoke
Scenario Outline: Validate <x>
  Given <x> is present

@regression
Examples: Block A
  | x |
  | a |

@slow
Examples: Block B
  | x |
  | b |
```

Scenarios from Block A carry `smoke` and `regression`. Scenarios from Block B carry
`smoke` and `slow`.

**`@property(...)` tag — generative Examples:** when an `Examples:` block (or its parent
`Scenario Outline:`) carries a `@property(...)` tag AND the block has no data rows
(header only), the block triggers property-based testing instead of literal expansion.
The framework samples values from the `HasGen[T]` registry for each column and runs the
scenario N times.

```gherkin
@property(samples=500, seed=42)
Scenario Outline: Balance is never negative
  Given an account with balance <balance>
  When I withdraw <amount>
  Then the balance is not negative

  Examples:
    | balance | amount |
```

Column headers may carry a `: generatorName` suffix to select a named generator override
for that column:

```gherkin
  Examples:
    | balance | amount: smallAmounts |
```

See [Property-Based Testing](property-testing.md) for the full reference.

---

## Steps

Steps are the executable lines inside a scenario. They begin with one of the five
keywords:

| Keyword | Type recorded |
|---------|---------------|
| `Given` | `GivenStep`   |
| `When`  | `WhenStep`    |
| `Then`  | `ThenStep`    |
| `And`   | `AndStep`     |
| `But`   | `ButStep`     |

The wildcard bullet `*` is also accepted and records as `AndStep`:

```gherkin
* the system is running
* the database is seeded
```

**Step text** is the remainder of the line after the keyword and any whitespace. Text is
stripped of leading and trailing whitespace.

**Word-boundary enforcement:** keywords are only recognised at the start of a stripped
line, and only when followed by whitespace, a colon, or end of line. Text that *starts
with* a keyword word is not mis-parsed:

```gherkin
Then Givenness matters   # → ThenStep, pattern "Givenness matters"
Then Andromeda galaxy    # → ThenStep, pattern "Andromeda galaxy"
```

**Colon variant:** `Given: a step` (colon immediately after keyword) is accepted.

---

## Tags

Tags appear on a line by themselves, before the element they annotate. Multiple tags may
appear on one line or across multiple consecutive lines — all are merged:

```gherkin
@smoke @regression
@priority-high
Scenario: Provision an account
```

Produces tags: `smoke`, `regression`, `priority-high`.

**Placement:**
- Before `Feature:` — feature-level tags. These are NOT inherited by scenarios.
- Before a `Rule:` — rule tags are discarded entirely; the parser never stores them
  (`RawRule` has no tags field) and they are not propagated to scenarios.
- Before `Scenario:` / `Scenario Outline:` — scenario-level tags.
- Before `Examples:` — applied to all scenarios expanded from that block.

**Tag format:** a tag is `@` followed by one or more non-whitespace characters. Dashes,
dots, underscores, parentheses, digits, and Unicode letters are all accepted:

```gherkin
@spec-1.4.7
@retry(3)
@flaky
@日本語
```

**`@ignore` tag:** when a scenario (or feature) has the `ignore` tag (case-insensitive),
it is recorded as ignored and its step bodies are not executed.

```gherkin
@ignore
Scenario: Not yet implemented
  Given a step that does not exist yet
```

**`@flags(k=v)` tag:** a special built-in tag that triggers flag-matrix expansion. See
[testing-flags.md](testing-flags.md) for the full reference.

---

## Data tables

A data table follows a step on the next indented lines. The first row is the header; all
subsequent rows are data rows.

```gherkin
Given the following users exist:
  | name  | role  | active |
  | Alice | admin | true   |
  | Bob   | user  | false  |
```

**Cell handling:**
- Leading and trailing whitespace in each cell is stripped.
- An empty cell `|  |` is stored as an empty string `""`.
- Column count is determined by the header row; data rows with fewer columns are
  padded with empty strings.

**Pipe escape:** a literal `|` inside a cell is written as `\|`:

```gherkin
Given the regex pattern:
  | pattern  |
  | a\|b\|c  |
# cell value: a|b|c
```

Other escape sequences inside cell text:

| Written | Stored as |
|---------|-----------|
| `\|`    | `\|` → `|`   |
| `\\`    | `\\` → `\`   |
| `\n`    | `\n` → newline character |

**`@ColumnName` annotation:** when a step definition uses the `@ColumnName` annotation on
a case-class field, the framework maps table columns to fields by name. See
[step-dsl.md](step-dsl.md) for details.

---

## Doc strings

A doc string attaches a multi-line text block to a step. Two delimiter styles are
supported:

**Triple-quote (`"""`):**

```gherkin
Given the request body:
  """
  {
    "accountId": "abc-123",
    "amount": 1000
  }
  """
Then done
```

**Triple-backtick (`` ``` ``):**

```gherkin
Given the SQL query:
  ```
  SELECT * FROM accounts WHERE active = true
  ```
Then the result set is non-empty
```

**Indentation stripping:** the leading whitespace equal to the indentation of the opening
delimiter is stripped from every content line. Relative indentation inside the block is
preserved. Empty lines within the block are preserved as empty strings.

The doc string is stored in `Step.docString: Option[String]` and is accessible in step
bodies.

---

## Comments

Lines beginning with `#` (after optional leading whitespace) are stripped before parsing.
Comments may appear anywhere — before `Feature:`, between steps, inside tables, between
scenarios.

```gherkin
# This is a top-level comment
Feature: Payment Processing

  # Background comment
  Background:
    Given the system is running  # inline comments are NOT supported — this text
                                 # "the system is running  # inline..." is the step text
```

**Inline comments are not supported.** Only full-line comments (where `#` is the first
non-whitespace character) are stripped. Text after a step keyword that contains `#` is
part of the step text.

---

## Complete example

```gherkin
# project-level comment
# language: en

@project @smoke
Feature: Account Lifecycle
  As a financial system I want to manage accounts.

  Background:
    Given the system is running
    And the following instrument classes exist:
      | Class         | Version |
      | SimpleSavings | 1.0.0   |

  @provision @smoke
  Scenario: Provision an account
    When a provision request is sent
    Then the ledger returns a 200 status code

  @post
  Scenario Outline: Post a <type> transaction
    Given a provisioned account
    When a <type> post of <amount> is sent
    Then the balance changes by <amount>

  Examples: Deposits
    | type    | amount |
    | deposit | 1000   |
    | deposit | 5000   |

  @smoke
  Examples: Withdrawals
    | type       | amount |
    | withdrawal | 500    |

  Rule: EOD must be idempotent

    Background:
      Given a provisioned account with a balance

    Scenario: EOD run twice is idempotent
      When EOD is run on 2025-01-01
      And EOD is run again on 2025-01-01
      Then the account state is unchanged
```

---

## What is NOT supported

The following Gherkin features are intentionally absent. Attempting to use them will
either be silently ignored or cause a parse failure.

| Unsupported feature | Behaviour |
|---------------------|-----------|
| Multiple `Feature:` blocks in one file | Parser stops at the second `Feature:` keyword; only the first feature is returned |
| Non-English keywords (`Fonctionnalité:`, `Szene:`, etc.) | `# language:` directive is stripped but only English keywords are recognised |
| Inline comments (`Given a step # comment`) | `#` inside a step line is part of the step text |
| `@ColumnName` derivation without annotation | Plain case-class mapping requires explicit `@ColumnName` on fields |
| `DocString` content type hints (`` ```json ``) | The type hint after `` ``` `` is captured as part of the delimiter but not stored separately |
| Nested `Rule:` blocks | Not parsed as nested; the inner `Rule:` is promoted to a sibling top-level rule, so its scenarios get only the feature's `Background:`, not the outer rule's |
| Multi-feature files | Not supported; use separate `.feature` files |
| `Ability:` / `Business Need:` as aliases for `Feature:` | Not recognised |
| Step argument ordering constraints | The parser places a data table or doc string on the immediately following step; two arguments on one step are not supported |
