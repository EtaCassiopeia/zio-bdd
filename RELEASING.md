# Releasing zio-bdd

This document describes how to publish a new release of zio-bdd to Maven Central via Sonatype.

## Prerequisites

- GPG key configured for signing (used by `sbt-ci-release`).
- Sonatype credentials in `~/.sbt/1.0/sonatype.sbt` or as environment variables:
  ```
  SONATYPE_USERNAME=<user>
  SONATYPE_PASSWORD=<token>
  ```
- The `sonatypeCentralHost` is set in `build.sbt` via `sonatypeCredentialHost`.

## Pre-release checklist

1. All tests pass: `sbt clean test`
2. No scalafmt violations: `sbt scalafmtCheckAll`
3. No compilation warnings: `sbt compile` (check output for `-Wunused:imports` warnings)
4. `CHANGELOG.md` updated with the new version entry (move `[Unreleased]` → `[X.Y.Z] — <date>`).
5. Docs install snippets bumped to the new version — run `./scripts/bump-doc-versions.sh X.Y.Z`
   (idempotent; only touches `io.github.etacassiopeia` coordinates). Optional: the release workflow
   opens this bump as a PR afterward if you skip it here (see step 4 below).
6. All new public APIs are documented.

## Versioning

zio-bdd follows [Semantic Versioning](https://semver.org/):
- MAJOR: breaking API changes (binary-incompatible).
- MINOR: new features, backward-compatible.
- PATCH: bug fixes, backward-compatible.

## Release steps

1. **Update version** (sbt-ci-release derives the version from git tags). The tag **must be
   signed** — the release workflow's `verify-tag-signature` job rejects an unsigned, lightweight,
   or unknown-key tag before anything publishes (it validates the tag against the committed public
   keys in `.github/release-signing-keys.asc`), so an unsigned tag no longer cuts a release:
   ```bash
   git tag -s v1.0.0 -m "v1.0.0"
   ```
   Set `git config --global tag.gpgSign true` so tags sign by default and `git tag` **fails loudly**
   if signing can't run (e.g. no `GPG_TTY`), rather than silently creating an unsigned tag. To add or
   rotate a signer, append their ASCII-armored **public** key to `.github/release-signing-keys.asc`
   (`gpg --armor --export <fingerprint> >> .github/release-signing-keys.asc`).

2. **Push the tag** to trigger the release workflow:
   ```bash
   git push origin v1.0.0
   ```
   The GitHub Actions workflow (`.github/workflows/release.yml`) runs these jobs on the tag:
   - **verify-tag-signature** — rejects the tag unless a key in `.github/release-signing-keys.asc`
     signed it.
   - **publish** — one job on JDK 17. It first asserts that the `zio-bdd-mock-conformance` POM's
     compile scope is SPI-only (#331), then `sbt ci-release` signs and publishes the whole
     aggregate to Sonatype Central. There is no separate JDK-21 preview job since #285.
   - **github-release** — creates the GitHub Release with auto-generated notes; `--latest` for a
     plain `vX.Y.Z`, `--prerelease` for a hyphenated `vX.Y.Z-RCn`. (#275)
   - **docs-version-bump** — opens a `docs/bump-X.Y.Z` PR syncing the README/docs install snippets
     to the new version (skipped for pre-releases). (#282)

3. **Verify on Maven Central** (can take 10-30 minutes to propagate):
   - `https://search.maven.org/artifact/io.github.etacassiopeia/zio-bdd_3`

4. **Merge the docs-bump PR** if step 5 of the checklist was skipped — it's docs-only, so a repo
   admin can merge it directly (a bot-opened PR does not re-trigger CI, which is expected). The
   GitHub Release is already created by the workflow — no manual step.

5. **Prepend breaking changes to the GitHub Release notes** when the release has any. The workflow's
   `--generate-notes` lists merged PR titles only, so a coordinate change or a JDK-floor move is
   invisible there. Copy the release's `Breaking changes` and `Security` sections from
   `CHANGELOG.md` above the generated notes:
   ```bash
   # everything in the version's section before its first `### Added` heading
   awk '/^## \[X\.Y\.Z\]/{f=1;next} f&&/^### Added/{exit} f' CHANGELOG.md > breaking.md
   gh release view vX.Y.Z --json body --jq .body > generated.md
   cat breaking.md generated.md > notes.md
   gh release edit vX.Y.Z --notes-file notes.md
   ```

## Local test publish

To test the publish flow without pushing to Central:
```bash
sbt publishLocal
```

To verify the artifact resolves from a sibling project, add to that project's `build.sbt`:
```scala
libraryDependencies += "io.github.etacassiopeia" %% "zio-bdd" % "1.0.0" % Test
```
Then run `sbt update` in the sibling project.

## Snapshot releases

SNAPSHOT versions are published automatically on every push to `master` if
`sbt-ci-release` is configured for snapshots. The snapshot version is derived
from the last tag + commit hash: `1.0.1-SNAPSHOT`.

## Module structure

| Artifact                   | Contents                                                    | JDK floor |
|----------------------------|-------------------------------------------------------------|-----------|
| `zio-bdd-gherkin`          | Gherkin parser                                              | 11        |
| `zio-bdd`                  | Core runner, step DSL, reporters, hooks                     | 11        |
| `zio-bdd-mock`             | Portable `MockControl` SPI                                  | 11        |
| `zio-bdd-rift`             | Rift adapter: container, `connect`, and embedded (via `rift-scala-zio`) | 17 |
| `zio-bdd-wiremock`         | In-process WireMock adapter                                 | 11        |
| `zio-bdd-mock-conformance` | Conformance scenario sets + `ConformanceHarness` for adapters | 11      |

`zio-bdd-rift-embedded`, `zio-bdd-rift-embedded-jdk21`, and `zio-bdd-rift-embedded-natives` were
retired in 1.5.0 (#285) and must not reappear in a release.

