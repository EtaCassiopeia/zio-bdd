import xerial.sbt.Sonatype.sonatypeCentralHost
import StepGeneratorPlugin.autoImport.*
import com.typesafe.tools.mima.core.*

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
  // #96 added a `Tag[A]` member to `TypedExtractor[A]` (a prerequisite for type-based
  // HasGen discovery in property testing, #99) so `table`/`tableWithMapping` gained a
  // `Tag[T]` context bound and `TableExtractor` gained a `Tag[List[T]]` constructor param.
  // Source-compatible, not binary-compatible against 1.0.0.
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.table"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.tableWithMapping"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.TableExtractor.this"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.TypedExtractor.tag"),
    // #97 added `resolveTemplateColumns` to `StepRegistry` — structurally matches a
    // `@property` step template (placeholders still present) against the registry to find
    // which extractor governs each `<col>`. Source-compatible, not binary-compatible against 1.0.0.
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.StepRegistry.resolveTemplateColumns")
  )
)

lazy val root = (project in file("."))
  .aggregate(core, gherkin)
  .settings(
    name                  := "zio-bdd-root",
    description           := "A ZIO-based BDD testing framework for Scala 3",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty // not published — MiMa is a no-op here
  )
  .dependsOn(core, gherkin)

lazy val core = (project in file("core"))
  .dependsOn(gherkin)
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
