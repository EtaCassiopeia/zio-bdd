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
  .aggregate(core, gherkin, reporters, example)
  .settings(
    name := "zio-bdd"
  )

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= commonDependencies
  )

lazy val gherkin = (project in file("gherkin"))
  .dependsOn(core)
  .settings(
    name := "gherkin",
    libraryDependencies ++= commonDependencies ++ Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1"
    )
  )

lazy val reporters = (project in file("reporters"))
  .dependsOn(core)
  .settings(
    name := "reporters",
    libraryDependencies ++= commonDependencies
  )

lazy val example = (project in file("example"))
  .dependsOn(core, gherkin, reporters)
  .settings(
    name := "example",
    libraryDependencies ++= commonDependencies,
//    Test / test := Def.task {
//      Unsafe.unsafe { implicit unsafe =>
//        val runtime = Runtime.default
//        val env = ZLayer.succeed(new UserSteps.UserRepo {
//          def createUser(name: String) = ZIO.succeed(UserSteps.User(name, s"$name@example.com".toLowerCase))
//        }) ++ ZLayer.succeed(new UserSteps.EmailService {
//          private var emails: List[String] = Nil
//          def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
//          def getSentEmails = ZIO.succeed(emails)
//        }) ++ ZLayer.succeed(ZIO.logLevel(LogLevel.Info)) ++ LogCollector.live ++ ZLayer.succeed(ConsoleReporter) ++ ZLayer.succeed(FileReporter)
//
//        val exitCode = runtime.unsafe.run(
//          TestRunner.runTests(UserSteps, new File("example/src/test/resources/features"), (parallelism in Test).value)
//            .provideLayer(env)
//        ).getOrThrowFiberFailure()
//
//        if (exitCode.isFailure) throw new RuntimeException("Tests failed")
//      }
//    }.value
  )