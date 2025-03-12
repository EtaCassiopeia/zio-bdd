inThisBuild(
  List(
    organization := "io.github.etacassiopeia",
    homepage     := Some(url("https://github.com/EtaCassiopeia/zio-bdd")),
    scalaVersion := "3.6.4",
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        id = "etacassiopeia",
        name = "Mohsen Zainalpour",
        email = "zainalpour@gmail.com",
        url = url("https://github.com/EtaCassiopeia")
      )
    ),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
  )
)

lazy val commonDependencies = Seq(
  "dev.zio" %% "zio"          % "2.1.16",
  "dev.zio" %% "zio-logging"  % "2.5.0",
  "dev.zio" %% "zio-test"     % "2.1.16" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.16" % Test
)

lazy val root = (project in file("."))
  .aggregate(core, gherkin)
  .settings(
    name        := "zio-bdd-root",
    description := "A ZIO-based BDD testing framework for Scala 3",
    publish / skip := true
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
      "dev.zio"                %% "zio-streams"    % "2.1.16"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "zio-bdd-gherkin",
    libraryDependencies ++= commonDependencies ++ Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1"
    )
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
