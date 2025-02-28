ThisBuild / scalaVersion     := "3.3.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "dev.zio"

lazy val root = (project in file("."))
  .settings(
    name := "zio-bdd",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.16",
      "dev.zio" %% "zio-test" % "2.1.16" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.16" % Test,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
