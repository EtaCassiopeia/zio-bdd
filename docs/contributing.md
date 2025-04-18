
# Contributing to zio-bdd

We welcome contributions to `zio-bdd`! This guide outlines how to get involved, from reporting issues to submitting pull requests. Your contributions help make `zio-bdd` better for the entire community.

## Getting Started

### Reporting Issues

If you encounter bugs, have feature requests, or need clarification, please open an issue on the [GitHub repository](https://github.com/EtaCassiopeia/zio-bdd). When reporting issues:

- **Search First**: Check if the issue already exists.
- **Be Detailed**: Provide steps to reproduce, expected behavior, and actual behavior.
- **Include Versions**: Mention your Scala, ZIO, and `zio-bdd` versions.

### Suggesting Features

Feature suggestions are welcome! When proposing a new feature:

- **Explain the Use Case**: Describe the problem it solves.
- **Consider Alternatives**: Mention any workarounds or existing solutions.
- **Be Open to Discussion**: Engage with the community to refine the idea.

## Development Workflow

### Setting Up the Environment

1. **Fork the Repository**: Create your own fork on GitHub.
2. **Clone Your Fork**:
   ```bash
   git clone https://github.com/your-username/zio-bdd.git
   ```
3. **Install Dependencies**: Ensure you have Scala 3 and SBT installed.
4. **Build the Project**:
   ```bash
   sbt compile
   ```

### Making Changes

1. **Create a Branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```
2. **Write Code**: Follow the coding standards and add tests for new features.
3. **Run Tests**:
   ```bash
   sbt test
   ```
4. **Commit Changes**: Use clear, descriptive commit messages.
5. **Push to Your Fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

### Submitting a Pull Request

1. **Open a Pull Request**: From your fork to the main repository.
2. **Describe Your Changes**: Explain what you did and why.
3. **Link to Issues**: If applicable, reference related issues.
4. **Wait for Review**: Be responsive to feedback and make necessary adjustments.

## Coding Standards

- **Follow Scala Style**: Adhere to the [Scala Style Guide](https://docs.scala-lang.org/style/).
- **Use ZIO Best Practices**: Write idiomatic ZIO code.
- **Document Code**: Add Scaladoc comments for public APIs.
- **Test Thoroughly**: Ensure new features are covered by tests.

## Community Guidelines

- **Be Respectful**: Treat all contributors with kindness and respect.
- **Stay Constructive**: Provide feedback that helps improve the project.
- **Collaborate**: Work together to solve problems and implement features.

## License

By contributing to `zio-bdd`, you agree that your contributions will be licensed under the Apache License 2.0.

Thank you for contributing to `zio-bdd`! Your efforts help advance BDD testing in the Scala and ZIO ecosystems.
