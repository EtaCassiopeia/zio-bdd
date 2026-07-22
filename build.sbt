import xerial.sbt.Sonatype.sonatypeCentralHost
import StepGeneratorPlugin.autoImport.*
import com.typesafe.tools.mima.core.*

// ---- Rift adapter, re-based onto the rift-scala SDK (#285) ----------------------------------
// Pre-#285, `zio-bdd-rift` drove Rift over hand-rolled admin HTTP (the zio HTTP client), and a separate
// `zio-bdd-rift-embedded(-jdk21)` + `zio-bdd-rift-embedded-natives` artifact set drove an in-process
// engine over a hand-written Panama FFM bridge to a downloaded `librift_ffi` cdylib — two protocol
// implementations, a JDK-21/22 class-file split, and ~250 lines of native-library download/mirror/
// checksum plumbing in this file. #285 re-bases everything onto the official `rift-scala-zio` SDK
// (io.github.achird-labs), which owns BOTH transports (container admin HTTP and in-process FFM) behind
// one typed `rift.zio.Rift` service — so there is one adapter (`RiftMockControl`), one published
// artifact (`zio-bdd-rift`), and this file carries no native-download/mirror code at all: the embedded
// engine + natives are ordinary `rift-java-embedded`/`rift-java-natives` runtime dependencies resolved
// like any other jar (see the `rift` project below), not assets this build fetches and packages itself.
//
// Single source of truth for the pinned Rift release (#195): the container image tag
// (`Rift.DefaultImage`, via the generated `RiftBuildInfo` below) derives from it, so a version bump
// touches exactly one line.
val riftVersion = "0.14.0"

// The official Scala 3 SDK the adapters are built on (#285). `rift-scala-zio` -> `rift-scala-bridge`
// pins `rift-java-core` at riftJavaVersion; the embedded engine + natives jars this build adds at Test
// scope MUST stay on that same rift-java version — they meet across the FFI/ABI boundary, so a split
// between the compile-side facade and the runtime engine is exactly the mismatch to avoid.
val riftScalaVersion = "0.1.1"
val riftJavaVersion  = "0.1.3"

inThisBuild(
  List(
    organization := "io.github.etacassiopeia",
    homepage     := Some(url("https://github.com/EtaCassiopeia/zio-bdd")),
    scalaVersion := "3.3.4",
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        id = "etacassiopeia",
        name = "Mohsen Zainalpour",
        email = "zainalpour@gmail.com",
        url = url("https://github.com/EtaCassiopeia")
      )
    ),
    sonatypeCredentialHost := sonatypeCentralHost,
    sonatypeRepository     := "https://s01.oss.sonatype.org/service/local",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:imports"
    )
  )
)

// Java-version floor for the published modules (#205). Without a pinned `-release`, Scala 3 targets
// the build JDK's class-file version, so a release cut on JDK 22 produced class-66 bytecode that even
// JDK 21 cannot load. These modules use no Java-17+ API, so pin class 55 — loadable on Java 11+ —
// regardless of the JDK that cuts the release.
lazy val javaFloorSettings: Seq[Def.Setting[_]] = Seq(
  Compile / scalacOptions += "-release:11",
  Compile / javacOptions ++= Seq("--release", "11")
)

// The `rift` module's floor moves 11 -> 17 (#285): its SDK dependency (`rift-scala-zio` -> `bridge` ->
// `rift-java-core`) is JDK-17 bytecode (class file 61), so a JDK-11 consumer cannot load it regardless
// of what `-release` this module itself compiles with. Every other module (core, gherkin, mock,
// wiremock, conformance) is unaffected and stays on `javaFloorSettings`.
lazy val javaFloor17Settings: Seq[Def.Setting[_]] = Seq(
  Compile / scalacOptions += "-release:17",
  Compile / javacOptions ++= Seq("--release", "17")
)

lazy val commonDependencies = Seq(
  "dev.zio" %% "zio"                   % "2.1.17",
  "dev.zio" %% "zio-schema"            % "1.6.6",
  "dev.zio" %% "zio-schema-derivation" % "1.6.6",
  "dev.zio" %% "zio-logging"           % "2.5.0",
  "dev.zio" %% "zio-test"              % "2.1.17",
  "dev.zio" %% "zio-test-sbt"          % "2.1.17" % Test
)

// Binary-compatibility checking. The baseline is established at 1.0.0: until a 1.x is published,
// `previousStableVersion` is a 0.x tag and is filtered out (MiMa is a no-op), so the 1.0.0 release
// itself is not checked against 0.1.0. From 1.0.0 onward, 1.0.x / 1.1 are verified against the
// latest stable 1.x release.
lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := previousStableVersion.value
    // Only check against a final 1.x release — skip 0.x and pre-releases (RC/M/SNAPSHOT, which
    // contain a '-'), so the 1.0.0 release itself is not checked against 0.1.0 or 1.0.0-RCx.
    .filter(v => v.startsWith("1.") && !v.contains("-"))
    .map(organization.value %% moduleName.value % _)
    .toSet,
  // Property-based testing (#91) added new defaulted fields to `Scenario` / the internal
  // `RawExamplesBlock`, a new defaulted `genLookup` parameter on `FeatureExecutor`, and
  // replaced several `PrettyReporter.Color` case objects with `val` aliases of the same type.
  // All of these are source-compatible but not binary-compatible against 1.0.0 — expected for
  // a minor feature release; filtered rather than worked around.
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.copy"),
    // #290: PropertyConfig gained a trailing `inertKeys: Set[String] = Set.empty` field
    // (warn on parsed-but-inert shrink/maxShrinks/verbose tags). Additive with a default —
    // source-compatible; only the synthetic apply/this/copy signatures changed.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.PropertyTag#PropertyConfig.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.PropertyTag#PropertyConfig.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.PropertyTag#PropertyConfig.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeature"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeatures"),
    // #291: the test-only `reset` on FeatureContext/Stage was renamed `resetForTest` to
    // stop it reading as a production reset path (it never was — clearing is via
    // freshScope/locallyScoped). `private[bdd]` compiles to public bytecode, so the rename
    // drops the old name from the binary API.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureContext.reset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.Stage.reset"),
    // #225: ScenarioResult gained a trailing `attempts: Int = 1` field (retry-tag support).
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.copy"),
    // #238: TestCaseRecord gains an `attempts` field (retry aspect in JUnit XML) — additive, defaulted.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.copy"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.Yellow"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleGreen"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleCyan"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleYellow"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleRed"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleCyan$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleGreen$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleRed$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleYellow$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$Yellow$"),
    // #96 added a `Tag[A]` member to `TypedExtractor[A]` (a prerequisite for type-based
    // HasGen discovery in property testing, #99) so `table`/`tableWithMapping` gained a
    // `Tag[T]` context bound and `TableExtractor` gained a `Tag[List[T]]` constructor param.
    // Source-compatible, not binary-compatible against 1.0.0.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.table"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.tableWithMapping"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.TableExtractor.this"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.TypedExtractor.tag"),
    // #97 added `resolveTemplateColumns` to `StepRegistry` — structurally matches a
    // `@property` step template (placeholders still present) against the registry to find
    // which extractor governs each `<col>`. Source-compatible, not binary-compatible against 1.0.0.
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.StepRegistry.resolveTemplateColumns"),
    // #108 added `focused: Boolean = false` to `BDDTestConfig`, enabling the --focused CLI flag
    // which suppresses @ignore noise from the report when running a single scenario from the IDE.
    // Binary-incompatible with 1.0.0 (case class apply/copy/constructor changed).
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.copy")
  )
)

// What the root aggregates — i.e. what `test`/`publishSigned`/`ci-release` operate on. Pre-#285 this
// branched on the build JDK (the JDK-21/22 dual embedded-artifact split); the SDK re-base collapses
// the whole stack onto a single JDK-17 floor for `rift` (see `javaFloor17Settings`), so every module
// aggregates unconditionally — one `sbt test` / one release job builds all of it.
lazy val aggregatedProjects: Seq[Project] = Seq(core, gherkin, mock, rift, wiremock, conformance)

lazy val root = (project in file("."))
  .aggregate(aggregatedProjects.map(p => LocalProject(p.id)): _*)
  .settings(
    name                  := "zio-bdd-root",
    description           := "A ZIO-based BDD testing framework for Scala 3",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty // not published — MiMa is a no-op here
  )
  .dependsOn(core, gherkin)

lazy val core = (project in file("core"))
  .dependsOn(gherkin, mock)
  .settings(
    name := "zio-bdd",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.scala-sbt"           % "test-interface" % "1.0" % "provided",
      "org.scala-lang.modules" %% "scala-xml"      % "2.3.0",
      "dev.zio"                %% "zio-streams"    % "2.1.17",
      "dev.zio"                %% "izumi-reflect"  % "3.0.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    stepMethodGeneratorSettings,
    javaFloorSettings,
    mimaSettings
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "zio-bdd-gherkin",
    libraryDependencies ++= commonDependencies,
    javaFloorSettings,
    mimaSettings
  )

// Portable MockControl SPI (#110). Standalone, backend-neutral: no dependency on
// any adapter (Rift, WireMock) — adapters depend on this module, never the reverse.
lazy val mock = (project in file("mock"))
  .settings(
    name := "zio-bdd-mock",
    libraryDependencies ++= commonDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    javaFloorSettings,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Compile-checked documentation examples (mdoc, #283). NOT published and NOT aggregated into the root,
// so `sbt test` / `ci-release` ignore it — run `sbt docs/mdoc` to regenerate docs/verified-examples.md
// from mdoc-src/ and fail the build if any canonical copy-paste snippet drifts from the real API. The
// other docs/*.md stay plain Markdown (mostly illustrative fragments that don't compile standalone).
lazy val docs = (project in file("docs-mdoc"))
  .enablePlugins(MdocPlugin)
  // conformance is on the classpath so the third-party-adapter conformance example (#332) compiles
  // against the real ConformanceHarness/scenario-set API. Its Compile scope is `mock` only (the
  // bundled adapters are Test-scope), so this pulls in no backend and stays JDK-11-clean (#330).
  .dependsOn(core, mock, conformance)
  .settings(
    name                  := "zio-bdd-docs",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty,
    mdocIn                := file("mdoc-src"),
    mdocOut               := file("docs"),
    // Snippets show idiomatic imports for copy-paste; don't fail the doc build on an unused one.
    scalacOptions ~= (_.filterNot(_ == "-Wunused:imports"))
  )

// Rift adapter (#113, re-based onto the official rift-scala SDK for #285): implements the portable
// MockControl SPI over `rift.zio.Rift` — the SDK owns both transports (container admin HTTP, and the
// in-process embedded engine over stable Panama FFM), so this is now ONE adapter and ONE published
// artifact, collapsing what used to be `zio-bdd-rift` + `zio-bdd-rift-embedded(-jdk21)` +
// `zio-bdd-rift-embedded-natives`. Depends on `mock`, never the reverse. JDK floor 17 (not the
// `javaFloorSettings` 11 the rest of the build uses): the SDK's `bridge` module links
// `rift-java-core`, which is JDK-17 bytecode (class file 61) — a JDK-11 consumer cannot load it
// regardless of what `-release` this module itself compiles with.
lazy val rift = Project("rift", file("rift"))
  .dependsOn(mock)
  .settings(
    name := "zio-bdd-rift",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "io.github.achird-labs" %% "rift-scala-zio" % riftScalaVersion,
      // `rift-java-testcontainers` is `% Optional` on the SDK's own `bridge` module (so a consumer who
      // never calls `Rift.container`/`Rift.managed` doesn't inherit org.testcontainers/Docker) — but
      // `Rift.managed` is a documented zio-bdd-rift entry point (used unconditionally, not opt-in), so
      // THIS module depends on it directly at Compile scope, mirroring the pre-#285 adapter's
      // always-available `testcontainers-scala-core` dependency.
      "io.github.achird-labs" % "rift-java-testcontainers" % riftJavaVersion,
      // The embedded engine + natives are RUNTIME `ServiceLoader` dependencies (`rift.zio.Rift.embedded`
      // resolves them at call time, not compile time) — Test-scoped here so `rift/test` exercises the
      // embedded suites; an application wiring `EmbeddedRift.layer` adds them itself (DESIGN.md §5.2).
      "io.github.achird-labs" % "rift-java-embedded" % riftJavaVersion % Test
    ) ++ RiftNatives.currentClassifier
      // No published `rift-java-natives` classifier for this host (Windows, musl, ppc64le, ...):
      // omit the Test dependency entirely rather than failing `sbt compile` outright.
      // `EmbeddedRift.available` degrades to `false` and the embedded suites skip, gracefully.
      .map(c => ("io.github.achird-labs" % "rift-java-natives" % riftJavaVersion % Test).classifier(c))
      .toSeq,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Generate RiftBuildInfo from `riftVersion` (the single source of truth, #195) so Rift.DefaultImage
    // derives its image tag from the same val — no hand-maintained literal.
    Compile / sourceGenerators += Def.task {
      val out = (Compile / sourceManaged).value / "zio" / "bdd" / "mock" / "rift" / "RiftBuildInfo.scala"
      IO.write(
        out,
        s"""package zio.bdd.mock.rift
           |
           |// GENERATED from `riftVersion` in build.sbt (#195) — do not edit. The single source of truth
           |// for the pinned Rift release; Rift.DefaultImage derives its image tag from it.
           |private[rift] object RiftBuildInfo:
           |  val riftVersion: String = "$riftVersion"
           |""".stripMargin
      )
      Seq(out)
    }.taskValue,
    javaFloor17Settings,
    // The embedded suites load a native library — fork so `--enable-native-access` applies to a clean
    // JVM, not the sbt shell's own.
    Test / fork := true,
    Test / javaOptions += "--enable-native-access=ALL-UNNAMED",
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// WireMock adapter (#122): the in-process, pure-JVM, zero-Docker provider with
// Correlated isolation by default. Implements the portable MockControl SPI over
// an embedded WireMockServer. Depends on `mock`, never the reverse.
lazy val wiremock = (project in file("wiremock"))
  .dependsOn(mock)
  .settings(
    name := "zio-bdd-wiremock",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.wiremock" % "wiremock" % "3.9.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    javaFloorSettings,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Conformance harness + matrix runner (#124): the executable definition of
// "implements MockControl". Main sources are the portable scenario sets + ConformanceHarness,
// programmed against the neutral SPI alone — published as `zio-bdd-mock-conformance` (#329) so
// third-party adapters (rift-scala's, and any future ones) can run the official suite in their
// own CI. Compile scope is deliberately `mock` only: the bundled adapters are Test-scope, needed
// by the in-repo matrix runner (the only code that depends on BOTH of them) and must never leak
// into the published POM.
lazy val conformance = Project("conformance", file("conformance"))
  .dependsOn(mock, rift % Test, wiremock % Test)
  .settings(
    name := "zio-bdd-mock-conformance",
    libraryDependencies ++= commonDependencies,
    // `EmbeddedConformanceSpec` runs the portable suite against the embedded provider. `rift % Test` is
    // `test->compile` (sbt classpath-dependency default), which pulls `rift`'s COMPILE classpath only —
    // it does NOT export `rift`'s own Test-scoped `rift-java-embedded`/`rift-java-natives` (a regression
    // once silently turned every embedded scenario into a false-green SKIP, since no engine resolved and
    // `EmbeddedRift.available` reads as legitimately-unsupported rather than misconfigured). Depend on
    // them directly at Test scope here too — do not rely on transitivity across a `% Test` project dep.
    libraryDependencies ++= Seq(
      "io.github.achird-labs" % "rift-java-embedded" % riftJavaVersion % Test
    ) ++ RiftNatives.currentClassifier
      .map(c => ("io.github.achird-labs" % "rift-java-natives" % riftJavaVersion % Test).classifier(c))
      .toSeq,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    javaFloorSettings,
    Test / fork := true,
    Test / javaOptions += "--enable-native-access=ALL-UNNAMED",
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

lazy val example = (project in file("example"))
  .dependsOn(core)
  .settings(
    name := "zio-bdd-example",
    libraryDependencies ++= commonDependencies,
    Test / testFrameworks    := Seq(new TestFramework("zio.bdd.ZIOBDDFramework")),
    Test / resourceDirectory := baseDirectory.value / "src" / "test" / "resources" / "features",
    publish / skip           := true,
    mimaPreviousArtifacts    := Set.empty // not published — MiMa is a no-op here
  )
