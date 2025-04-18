---
layout: default
title: Home
---
# zio-bdd

**zio-bdd** is a Behavior-Driven Development (BDD) testing framework built for Scala 3 and ZIO. This framework enables developers to write expressive, type-safe, and concurrent tests using Gherkin syntax, integrated seamlessly with ZIO's powerful effect system.

## Overview

`zio-bdd` bridges the gap between human-readable specifications and executable tests. It supports Gherkin feature files, type-safe step definitions, scenario-scoped state management with `ScenarioContext`, and integration with ZIO services seamlessly. Whether you're new to BDD or an experienced ZIO user, this documentation will guide you through every aspect of using `zio-bdd` effectively.

## Table of Contents

- [Getting Started](getting-started.md) - Set up and write your first test.
- [Gherkin Syntax Reference](gherkin-reference.md) - Understand Gherkin and its support in `zio-bdd`.
- [Step Definitions](step-definitions.md) - Define steps with type extractors and manage state.
- [Running Tests](running-tests.md) - Configure and execute tests from the command line.
- [Advanced Features](advanced-features.md) - Leverage services, layers, and advanced configurations.
- [Examples](examples.md) - Explore practical use cases.
- [Contributing](contributing.md) - Learn how to contribute to `zio-bdd`.

## Why zio-bdd?

- **Type Safety**: Leverage Scala 3’s type system for robust step definitions.
- **Concurrency**: Run tests concurrently with ZIO fibers.
- **Modularity**: Integrate with ZIO services and manage state effortlessly.
- **Readability**: Write tests in Gherkin, understandable by technical and non-technical stakeholders alike.

Start your journey with the [Getting Started](getting-started.md) guide to see `zio-bdd` in action!

For the latest updates and version information, check the [GitHub repository](https://github.com/EtaCassiopeia/zio-bdd).