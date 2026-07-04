import xerial.sbt.Sonatype.sonatypeCentralHost
import StepGeneratorPlugin.autoImport.*
import com.typesafe.tools.mima.core.*

import java.net.URI
import java.nio.file.{Files => JFiles, StandardCopyOption}
import java.security.MessageDigest

// ---- librift_ffi native library packaging (#134) -------------------------------------------
// The embedded adapter loads librift_ffi from the classpath: the zio-bdd-rift-embedded-natives jar
// bundles the per-platform cdylibs as resources (native/<os>-<arch>/librift_ffi.<ext>), downloaded
// + checksum-verified from the Rift release at build time. The runtime loader extracts the host's
// lib to a temp file and loads it via Panama — no manual install, no system property. (FFM is a
// preview API on JDK 21, so the embedded code stays JDK-21-gated; see embeddedNativeSettings.)
// NOTE: build.sbt is compiled with the Scala 2.12 dialect — keep these blocks 2.12-compatible.

// Single source of truth for the pinned Rift release (#195). Both the FFI natives (riftNativesVersion,
// downloaded into the embedded-natives jar) and the container image tag (Rift.DefaultImage, via the
// generated RiftBuildInfo below) derive from it, so a version bump touches exactly one line.
val riftVersion        = "0.9.0"
val riftNativesVersion = riftVersion

// The (os, arch, ext) cdylibs bundled into the natives jar — linux/macOS × x86_64/aarch64 (#134).
val riftNativeTriples: Seq[(String, String, String)] = Seq(
  ("linux", "x86_64", "so"),
  ("linux", "aarch64", "so"),
  ("darwin", "x86_64", "dylib"),
  ("darwin", "aarch64", "dylib")
)

def sha256Hex(f: File): String =
  MessageDigest.getInstance("SHA-256").digest(JFiles.readAllBytes(f.toPath)).map(b => f"$b%02x").mkString

// Download `asset` from the Rift release to `out`, verifying it against the sibling .sha256.
// Idempotent: skips the download when `out` already matches the expected checksum. Returns `out`.
def downloadRiftAsset(version: String, asset: String, out: File, log: sbt.util.Logger): File = {
  val base = s"https://github.com/EtaCassiopeia/rift/releases/download/v$version"
  val expected = {
    val in = URI.create(s"$base/$asset.sha256").toURL.openStream()
    try new String(in.readAllBytes(), "UTF-8").trim.split("\\s+").head
    finally in.close()
  }
  if (out.exists() && sha256Hex(out) == expected) {
    log.info(s"[rift-ffi] cached $asset")
  } else {
    JFiles.createDirectories(out.toPath.getParent)
    log.info(s"[rift-ffi] downloading $asset (v$version) ...")
    // Download to a sibling temp file and atomically move into place only after the checksum passes,
    // so `out` never exists in a corrupt/partial state (the idempotency guard above trusts it).
    val tmp = new File(out.getParentFile, s".${out.getName}.part")
    try {
      val in = URI.create(s"$base/$asset").toURL.openStream()
      try JFiles.copy(in, tmp.toPath, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      val got = sha256Hex(tmp)
      if (got != expected) sys.error(s"[rift-ffi] checksum mismatch for $asset: got $got, expected $expected")
      JFiles.move(tmp.toPath, out.toPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      log.info(s"[rift-ffi] downloaded + verified $asset")
    } finally if (tmp.exists()) tmp.delete()
  }
  out
}

// The major version of the JDK building this project. FFM (java.lang.foreign) is finalized in JDK 22
// (JEP 454) and preview/absent before it, so the embedded Rift provider (the `riftEmbedded` module)
// compiles against the stable API and builds only on JDK 22+. The project baseline stays JDK 11 for
// everything else (core + the container adapter): on a < 22 JDK the embedded module and its natives
// are simply not aggregated, so the JDK-11 build and the published JDK-11 artifacts are unaffected.
// A dedicated JDK-22+ CI job and the release build/publish the embedded artifacts.
def jdkMajor: Int = {
  val raw = sys.props.getOrElse("java.specification.version", "11")
  val s   = if (raw.startsWith("1.")) raw.substring(2) else raw
  s.takeWhile(_.isDigit) match { case "" => 11; case d => d.toInt }
}
lazy val ffmEnabled: Boolean = jdkMajor >= 22

// Runtime flags the embedded suites' forked test JVM needs: stable FFM is NOT preview, so only
// --enable-native-access (which downgrades the restricted-method warning) — no --enable-preview.
// The native library is on the test classpath via the embeddedNatives test dependency (the loader
// extracts + loads it — no -Drift.ffi.lib needed).
lazy val embeddedTestJvmSettings: Seq[Def.Setting[_]] = Seq(
  Test / fork := true,
  Test / javaOptions += "--enable-native-access=ALL-UNNAMED"
)

// The bundled per-platform natives (#134): a pure-resources jar that packages the librift_ffi
// cdylibs (downloaded + checksum-verified at build time) so the embedded provider loads them from
// the classpath out-of-the-box. No Scala/Java code (JDK-agnostic), published as
// `zio-bdd-rift-embedded-natives`. The download is gated on a 22+ JDK (where embedded is built/used),
// so the JDK-11 build never pulls the ~60MB native artifacts; the JDK-22+ release publishes them.
lazy val embeddedNatives = (project in file("embedded-natives"))
  .settings(
    name             := "zio-bdd-rift-embedded-natives",
    crossPaths       := false,
    autoScalaLibrary := false,
    Compile / resourceGenerators ++= (
      if (!ffmEnabled) Nil
      else
        Seq(Def.task {
          val log = streams.value.log
          val dir = (Compile / resourceManaged).value
          riftNativeTriples.map { case (os, arch, ext) =>
            downloadRiftAsset(riftNativesVersion, s"librift_ffi-$os-$arch.$ext", dir / "native" / s"$os-$arch" / s"librift_ffi.$ext", log)
          }
        }.taskValue)
    ),
    mimaPreviousArtifacts := Set.empty
  )

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
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeature"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeatures"),
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

// The embedded provider + its natives are aggregated (built, tested, published) only on a 22+ JDK;
// on the JDK-11 baseline they're absent, so the default build and release publish container-only.
lazy val aggregatedProjects: Seq[Project] =
  Seq(core, gherkin, mock, rift, wiremock, conformance) ++
    (if (ffmEnabled) Seq(riftEmbedded, embeddedNatives) else Seq.empty)

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
    mimaSettings
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "zio-bdd-gherkin",
    libraryDependencies ++= commonDependencies,
    mimaSettings
  )

// Portable MockControl SPI (#110). Standalone, backend-neutral: no dependency on
// any adapter (Rift, WireMock) — adapters depend on this module, never the reverse.
lazy val mock = (project in file("mock"))
  .settings(
    name := "zio-bdd-mock",
    libraryDependencies ++= commonDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Rift adapter (#113): implements the portable MockControl SPI over the Rift
// (Mountebank-compatible) backend. Drives the admin API via zio-http and can
// stand up the published image via testcontainers. Depends on `mock`, never the
// reverse.
lazy val rift = Project("rift", file("rift"))
  .dependsOn(mock)
  .settings(
    name := "zio-bdd-rift",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "dev.zio"      %% "zio-http"                   % "3.2.0",
      "dev.zio"      %% "zio-json"                   % "0.7.3",
      "com.dimafeng" %% "testcontainers-scala-core"  % "0.41.4"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Generate RiftBuildInfo from `riftVersion` (the single source of truth, #195) so Rift.DefaultImage
    // derives its image tag from the same val the FFI natives version does — no hand-maintained literal.
    Compile / sourceGenerators += Def.task {
      val out = (Compile / sourceManaged).value / "zio" / "bdd" / "mock" / "rift" / "RiftBuildInfo.scala"
      IO.write(
        out,
        s"""package zio.bdd.mock.rift
           |
           |// GENERATED from `riftVersion` in build.sbt (#195) — do not edit. The single source of truth
           |// for the pinned Rift release; Rift.DefaultImage and the FFI natives version both derive from it.
           |private[rift] object RiftBuildInfo:
           |  val riftVersion: String = "$riftVersion"
           |""".stripMargin
      )
      Seq(out)
    }.taskValue,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Embedded Rift provider (#133/#193): drives the Rift engine in-process over librift_ffi via the
// stable Panama FFM API (JEP 454), so no Docker. A SEPARATE published artifact
// (`zio-bdd-rift-embedded`) from the container adapter because it requires JDK 22+ (FFM) + the native
// library — keeping `zio-bdd-rift` and core on the JDK-11 baseline. Built only on a 22+ JDK; on a
// < 22 JDK it is absent from the aggregate (so it is neither compiled nor published there).
lazy val riftEmbedded = Project("riftEmbedded", file("rift-embedded"))
  // test->test: the embedded capabilities spec reuses rift's FakeRift admin-API double (a test source)
  // to stand in for the in-process admin plane, exactly as it did when embedded lived inside `rift`.
  .dependsOn(rift % "compile->compile;test->test", mock, embeddedNatives % Test)
  .settings(
    name := "zio-bdd-rift-embedded",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % "3.2.0",
      "dev.zio" %% "zio-json" % "0.7.3"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // The Java FFM bridge compiles against the stable Foreign Function & Memory API (JEP 454, JDK 22).
    // No --enable-preview: the resulting class is a normal JDK-22 class, not version-locked to one JDK.
    javacOptions ++= Seq("--release", "22"),
    mimaPreviousArtifacts := Set.empty
  )
  .settings(embeddedTestJvmSettings)

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
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Conformance harness + matrix runner (#124): the executable definition of
// "implements MockControl". The only module that depends on BOTH adapters, so it
// can run one feature set across Rift + WireMock and emit the pass/skip/fail matrix.
lazy val conformance = {
  val base = Project("conformance", file("conformance"))
    .dependsOn(core, rift, wiremock)
    .settings(
      name := "zio-bdd-conformance",
      libraryDependencies ++= commonDependencies,
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
      publish / skip        := true, // a test harness, not a published artifact
      mimaPreviousArtifacts := Set.empty
    )
  // On a 22+ JDK the portable conformance suite also runs against the embedded provider: add the
  // gated test source (EmbeddedConformanceSpec, src/test/jdk22), the native-access test JVM, and the
  // rift-embedded + natives dependencies. On a < 22 JDK none of this is added (embedded is absent).
  if (ffmEnabled)
    base
      .dependsOn(riftEmbedded, embeddedNatives % Test)
      .settings(
        Test / unmanagedSourceDirectories += (Test / sourceDirectory).value / "jdk22",
        embeddedTestJvmSettings
      )
  else base
}

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
