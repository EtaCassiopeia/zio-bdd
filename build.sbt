import xerial.sbt.Sonatype.sonatypeCentralHost
import StepGeneratorPlugin.autoImport.*
import com.typesafe.tools.mima.core.*

import java.net.URI
import java.nio.file.{Files => JFiles, StandardCopyOption}
import java.security.MessageDigest

// ---- librift_ffi native library provisioning (embedded adapter, #133) ----------------------
// The embedded MockControl drives Rift in-process over the librift_ffi C-ABI via Panama FFM. The
// prebuilt shared library ships per host triple on the Rift GitHub release; the conformance gate
// downloads + checksum-verifies it and passes its path to the forked test JVM. FFM is a preview
// API on JDK 21, so those JVMs run with --enable-preview + --enable-native-access.

lazy val riftFfiVersion  = settingKey[String]("Rift release tag providing librift_ffi")
lazy val riftFfiLibFile  = settingKey[File]("Host-resolved path to the downloaded librift_ffi library")
lazy val downloadRiftFfi = taskKey[File]("Download + checksum-verify librift_ffi for the host triple")

// The release asset name for the host OS/arch, or None on an unsupported platform (tests then skip).
// NOTE: build.sbt is compiled with the Scala 2.12 dialect — keep this block 2.12-compatible.
def riftFfiAssetName: Option[String] = {
  val os = sys.props.getOrElse("os.name", "").toLowerCase
  val arch = sys.props.getOrElse("os.arch", "").toLowerCase match {
    case "aarch64" | "arm64" => "aarch64"
    case "x86_64" | "amd64"  => "x86_64"
    case other               => other
  }
  if (os.contains("mac") || os.contains("darwin")) Some(s"librift_ffi-darwin-$arch.dylib")
  else if (os.contains("linux")) Some(s"librift_ffi-linux-$arch.so")
  else if (os.contains("win")) Some(s"librift_ffi-windows-$arch.dll")
  else None
}

def riftFfiLibTarget(targetDir: File): File =
  targetDir / "native" / riftFfiAssetName.getOrElse("librift_ffi.unavailable")

def sha256Hex(f: File): String =
  MessageDigest.getInstance("SHA-256").digest(JFiles.readAllBytes(f.toPath)).map(b => f"$b%02x").mkString

// The major version of the JDK building this project. FFM (java.lang.foreign) is a preview API on
// 21 and absent before it, so the embedded adapter's sources only compile on 21+. The project's
// baseline is JDK 11 (CI + release), so embedded is gated: on a < 21 JDK its source dirs are not
// added (the rift/conformance modules build clean and the published artifacts stay JDK-11), and a
// dedicated JDK-21 CI job exercises it.
def jdkMajor: Int = {
  val raw = sys.props.getOrElse("java.specification.version", "11")
  val s   = if (raw.startsWith("1.")) raw.substring(2) else raw
  s.takeWhile(_.isDigit) match { case "" => 11; case d => d.toInt }
}
lazy val ffmEnabled: Boolean = jdkMajor >= 21

// JDK-21-only: provisions the native library + the embedded source dirs + the preview/native-access
// test JVM. Empty on a < 21 JDK, so the embedded adapter is simply absent from that build.
lazy val embeddedNativeSettings: Seq[Def.Setting[_]] =
  if (!ffmEnabled) Nil
  else
    Seq(
      Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value / "jdk21",
      Test / unmanagedSourceDirectories += (Test / sourceDirectory).value / "jdk21",
      // The Java FFM bridge uses the preview Foreign Function & Memory API.
      javacOptions ++= Seq("--release", "21", "--enable-preview"),
      riftFfiVersion := "0.7.0",
      riftFfiLibFile := riftFfiLibTarget(target.value),
  downloadRiftFfi := {
    val log     = streams.value.log
    val version = riftFfiVersion.value
    val out     = riftFfiLibFile.value
    riftFfiAssetName match {
      case None =>
        log.warn(
          s"[rift-ffi] no prebuilt librift_ffi for ${sys.props.getOrElse("os.name", "?")}/" +
            s"${sys.props.getOrElse("os.arch", "?")}; embedded tests will be skipped"
        )
        out
      case Some(asset) =>
        val base = s"https://github.com/EtaCassiopeia/rift/releases/download/v$version"
        val expected = {
          val in = URI.create(s"$base/$asset.sha256").toURL.openStream()
          try new String(in.readAllBytes(), "UTF-8").trim.split("\\s+").head
          finally in.close()
        }
        if (out.exists() && sha256Hex(out) == expected) {
          log.info(s"[rift-ffi] using cached $asset")
          out
        } else {
          JFiles.createDirectories(out.toPath.getParent)
          log.info(s"[rift-ffi] downloading $asset (v$version) ...")
          val in = URI.create(s"$base/$asset").toURL.openStream()
          try JFiles.copy(in, out.toPath, StandardCopyOption.REPLACE_EXISTING)
          finally in.close()
          val got = sha256Hex(out)
          if (got != expected) sys.error(s"[rift-ffi] checksum mismatch for $asset: got $got, expected $expected")
          log.info(s"[rift-ffi] downloaded + verified $asset")
          out
        }
    }
  },
  Test / fork := true,
  Test / javaOptions ++= Seq(
    "--enable-preview",
    "--enable-native-access=ALL-UNNAMED",
    s"-Drift.ffi.lib=${riftFfiLibFile.value.getAbsolutePath}"
  ),
  // Ensure the native library is present (and verified) before the forked test JVM starts.
  (Test / test) := (Test / test).dependsOn(downloadRiftFfi).value
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

lazy val root = (project in file("."))
  .aggregate(core, gherkin, mock, rift, wiremock, conformance)
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
lazy val rift = (project in file("rift"))
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
    // The embedded adapter (#133) lives in JDK-21-only source dirs (src/{main,test}/jdk21) and is
    // compiled with --release 21 --enable-preview — added by embeddedNativeSettings only on a 21+ JDK,
    // so on the JDK-11 baseline the rift module builds (and publishes) without the FFM bridge.
    embeddedNativeSettings,
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
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Conformance harness + matrix runner (#124): the executable definition of
// "implements MockControl". The only module that depends on BOTH adapters, so it
// can run one feature set across Rift + WireMock and emit the pass/skip/fail matrix.
lazy val conformance = (project in file("conformance"))
  .dependsOn(core, rift, wiremock)
  .settings(
    name := "zio-bdd-conformance",
    libraryDependencies ++= commonDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // The conformance matrix runs the portable scenarios against MockControl.embedded (#133), which
    // drives Rift in-process via FFM — so the forked test JVM needs the native library + preview.
    embeddedNativeSettings,
    publish / skip        := true, // a test harness, not a published artifact
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
