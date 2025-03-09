import xerial.sbt.Sonatype.GitHubHosting
import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / scalaVersion  := "3.6.4"
ThisBuild / organization  := "io.github.etacassiopeia"
ThisBuild / version       := "0.0.1"
ThisBuild / versionScheme := Some("early-semver")

lazy val commonDependencies = Seq(
  "dev.zio" %% "zio"          % "2.1.16",
  "dev.zio" %% "zio-logging"  % "2.5.0",
  "dev.zio" %% "zio-test"     % "2.1.16" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.16" % Test
)

lazy val root = (project in file("."))
  .aggregate(core, gherkin)
  .settings(
    name        := "zio-bdd",
    description := "A ZIO-based BDD testing framework for Scala 3",
    licenses    := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage    := Some(url("https://github.com/EtaCassiopeia/zio-bdd")),
    developers := List(
      Developer(
        id = "etacassiopeia",
        name = "Mohsen Zainalpour",
        email = "zainalpour@gmail.com",
        url = url("https://github.com/EtaCassiopeia")
      )
    ),
    sonatypeCredentialHost := sonatypeCentralHost,
    sonatypeProjectHosting := Some(GitHubHosting("EtaCassiopeia", "zio-bdd", "zainalpour@gmail.com")),
    publishMavenStyle      := true,
    publishTo              := sonatypePublishToBundle.value,
    Test / publishArtifact := false,
    pomIncludeRepository   := { _ => false }
  )
  .dependsOn(core, gherkin)

lazy val core = (project in file("core"))
  .dependsOn(gherkin)
  .settings(
    name := "zio-bdd-core",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.scala-sbt"           % "test-interface" % "1.0"    % "provided",
      "org.scala-lang.modules" %% "scala-xml"      % "2.3.0",
      "dev.zio"                %% "zio-stream"     % "2.1.16" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    publish / skip := true
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "zio-bdd-gherkin",
    libraryDependencies ++= commonDependencies ++ Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1"
    ),
    publish / skip := true
  )

lazy val example = (project in file("example"))
  .dependsOn(core)
  .settings(
    name := "zio-bdd-example",
    libraryDependencies ++= commonDependencies,
    Test / testFrameworks    := Seq(new TestFramework("zio.bdd.core.ZIOBDDFramework")),
    Test / resourceDirectory := baseDirectory.value / "src" / "test" / "resources" / "features",
    publish / skip           := true
  )
