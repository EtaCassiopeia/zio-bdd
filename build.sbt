ThisBuild / scalaVersion := "3.3.5"
ThisBuild / organization := "zio.bdd"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val commonDependencies = Seq(
  "dev.zio" %% "zio" % "2.1.16",
  "dev.zio" %% "zio-test" % "2.1.16" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.16" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

lazy val root = (project in file("."))
  .aggregate(core, gherkin, example)
  .settings(
    name := "zio-bdd"
  )

lazy val core = (project in file("core"))
  .dependsOn(gherkin)
  .settings(
    name := "core",
    libraryDependencies ++= commonDependencies,
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0" % "provided",
    Test / testFrameworks := Seq(new TestFramework("zio.bdd.core.ZIOBDDFramework"))
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "gherkin",
    libraryDependencies ++= commonDependencies ++ Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1"
    )
  )

lazy val example = (project in file("example"))
  .dependsOn(core)
  .settings(
    name := "example",
    libraryDependencies ++= commonDependencies,
    Test / testFrameworks := Seq(new TestFramework("zio.bdd.core.ZIOBDDFramework")),
    Test / resourceDirectory := baseDirectory.value / "example" / "src" / "test" / "resources" / "features"
  )