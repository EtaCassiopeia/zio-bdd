//package zio.bdd.example
//
//import zio.*
//import zio.bdd.core.*
//import zio.bdd.gherkin.*
//import java.io.File
//
//object TestRunner {
//  def runTests[R](
//                   steps: ZIOSteps[R],
//                   directory: File,
//                   parallelism: Int
//                 ): ZIO[R with LogCollector with Reporter, Nothing, ExitCode] =
//    for {
//      _        <- ZIO.logInfo(s"Starting test run for directory: ${directory.getAbsolutePath}")
//      features <- GherkinParser.loadFeatures(directory).orDie
//      _        <- ZIO.logInfo(s"Loaded ${features.length} features")
//      results <- ZIO.foreachPar(features) { feature =>
//        ZIO.service[Reporter].flatMap(_.startFeature(feature.name)) *>
//          ScenarioRunner.runScenarios(steps, feature, parallelism) // Explicit parallelism
//            .flatMap { scenarioResults =>
//              ZIO.service[Reporter].flatMap(_.endFeature(feature.name, scenarioResults)) *>
//                ZIO.succeed(scenarioResults)
//            }
//      }.withParallelism(parallelism)
//      exitCode = if (results.flatten.exists(_.exists(_.succeeded == false))) ExitCode.failure else ExitCode.success
//      _       <- ZIO.logInfo(s"Test run completed with exit code: $exitCode")
//    } yield exitCode
//
//  @Retry(3)
//  @Flaky
//  def dslScenario: String = """
//    Given a user exists with name "Alice"
//    When the user requests a password reset
//    And the reset email is logged
//    Then an email should be sent to "alice@example.com"
//  """
//
//  def runDslTests[R](steps: ZIOSteps[R]): ZIO[R with LogCollector with Reporter, Nothing, ExitCode] = {
//    val metadata = ScenarioMetadata(retryCount = 3, isFlaky = true)
//    for {
//      _           <- ZIO.logInfo(s"Starting DSL test run for scenario")
//      _           <- ZIO.service[Reporter].flatMap(_.startFeature("DSL Test Feature"))
//      stepResults <- ZIO.logAnnotate("scenarioId", dslScenario.hashCode.toString) {
//        ScenarioRunner.run(steps, dslScenario, metadata).orDie
//      }
//      _ <- ZIO.service[Reporter].flatMap(_.startScenario("DSL Scenario")) *>
//        ZIO.foreach(stepResults)(result =>
//          ZIO.service[Reporter].flatMap(_.startStep(result.step)) *>
//            ZIO.service[Reporter].flatMap(_.endStep(result.step, result))
//        ) *>
//        ZIO.logInfo(s"DSL Scenario produced ${stepResults.length} step results") *>
//        ZIO.service[Reporter].flatMap(_.endScenario("DSL Scenario", stepResults))
//      _        <- ZIO.logInfo(s"Feature DSL Test Feature has 1 scenario with ${stepResults.length} step results") *>
//        ZIO.service[Reporter].flatMap(_.endFeature("DSL Test Feature", List(stepResults)))
//      exitCode <- ZIO.succeed(if (stepResults.exists(_.succeeded == false)) ExitCode.failure else ExitCode.success)
//      _        <- ZIO.logInfo(s"DSL test run completed with exit code: $exitCode")
//    } yield exitCode
//  }
//
//  def main(args: Array[String]): Unit =
//    Unsafe.unsafe { implicit unsafe =>
//      val runtime    = Runtime.default
//      val featureDir = new File("zio-bdd-example/src/test/resources/features")
//      val env = ZLayer.succeed(new UserRepo {
//        def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
//      }) ++ ZLayer.succeed(new EmailService {
//        private var emails: List[String] = Nil
//        def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
//        def getSentEmails                 = ZIO.succeed(emails)
//      }) ++ ZLayer.succeed(ZIO.logLevel(LogLevel.Info)) ++ LogCollector.live ++ ZLayer.succeed(
//        ConsoleReporter
//      ) ++ ZLayer.succeed(FileReporter)
//
//      runtime.unsafe
//        .run(
//          (runTests(UserSteps, featureDir, 4) *> runDslTests(UserSteps))
//            .provideLayer(env)
//        )
//        .getOrThrowFiberFailure() match {
//        case exitCode => println(s"Tests completed with exit code: $exitCode")
//      }
//    }
//}
//
////package zio.bdd.example
////
////import zio.*
////import zio.bdd.core.*
////import zio.bdd.gherkin.*
////import zio.bdd.reporters.*
////import java.io.File
////
////object TestRunner {
////  def runTests[R](
////    steps: ZIOSteps[R],
////    directory: File,
////    parallelism: Int
////  ): ZIO[R with LogCollector with Reporter, Nothing, ExitCode] =
////    for {
////      _        <- ZIO.logInfo(s"Starting test run for directory: ${directory.getAbsolutePath}")
////      features <- GherkinParser.loadFeatures(directory).orDie
////      _        <- ZIO.logInfo(s"Loaded ${features.length} features")
////      results <- ZIO
////                   .foreachPar(features) { feature =>
////                     ZIO.service[Reporter].flatMap(_.startFeature(feature.name)) *>
////                       ZIO
////                         .foreachPar(feature.scenarios) { scenario =>
////                           val fullScenarioText = (feature.background ++ scenario.steps).mkString("\n")
////                           ZIO.logAnnotate("scenarioId", scenario.name.hashCode.toString) {
////                             if (scenario.examples.isEmpty) {
////                               ScenarioRunner.run(steps, fullScenarioText, scenario.metadata).orDie
////                             } else {
////                               ZIO
////                                 .foreachPar(scenario.examples) { row =>
////                                   val parameterizedText = (feature.background ++ scenario.steps)
////                                     .map(s => row.data.foldLeft(s)((acc, kv) => acc.replace(s"<${kv._1}>", kv._2)))
////                                     .mkString("\n")
////                                   ScenarioRunner.run(steps, parameterizedText, scenario.metadata).orDie
////                                 }
////                                 .map(_.flatten)
////                             }
////                           }
////                         }.tap(scenarioResults => ZIO.serviceWithZIO[Reporter](_.endFeature(feature.name, scenarioResults)))
////                   }
////                   .withParallelism(parallelism)
////      exitCode = if (results.flatten.exists(_.exists(_.succeeded == false))) ExitCode.failure else ExitCode.success
////      _       <- ZIO.logInfo(s"Test run completed with exit code: $exitCode")
////    } yield exitCode
////
////  @Retry(3)
////  @Flaky
////  def dslScenario: String = """
////    Given a user exists with name "Alice"
////    When the user requests a password reset
////    And the reset email is logged
////    Then an email should be sent to "alice@example.com"
////  """
////
//////  def runDslTests[R](steps: ZIOSteps[R]): ZIO[R with LogCollector with Reporter, Nothing, ExitCode] = {
//////    val metadata = ScenarioMetadata(retryCount = 3, isFlaky = true)
//////    ScenarioRunner.run(steps, dslScenario, metadata).map { results =>
//////      if (results.exists(_.succeeded == false)) ExitCode.failure else ExitCode.success
//////    }.orDie
//////  }
////
////  def runDslTests[R](steps: ZIOSteps[R]): ZIO[R with LogCollector with Reporter, Nothing, ExitCode] = {
////    val metadata = ScenarioMetadata(retryCount = 3, isFlaky = true)
////    for {
////      _ <- ZIO.logInfo(s"Starting DSL test run for scenario")
////      _ <- ZIO.serviceWithZIO[Reporter](_.startFeature("DSL Test Feature"))
////      stepResults <- ZIO
////                       .logAnnotate("scenarioId", dslScenario.hashCode.toString) {
////                         ScenarioRunner.run(steps, dslScenario, metadata)
////                       }
////                       .orDie // Converts Throwable to Nothing
////      _ <- ZIO.serviceWithZIO[Reporter](_.startScenario("DSL Scenario")) *>
////        ZIO.foreachDiscard(stepResults) {
////          result =>
////            ZIO.serviceWithZIO[Reporter](_.startStep(result.step)) *>
////              ZIO.serviceWithZIO[Reporter](_.endStep(result.step, result))
////        } *>
////             ZIO.logInfo(s"DSL Scenario produced ${stepResults.length} step results") *>
////        ZIO.serviceWithZIO[Reporter](_.endScenario("DSL Scenario", stepResults))
////      _ <- ZIO.logInfo(s"Feature DSL Test Feature has 1 scenario with ${stepResults.length} step results") *>
////        ZIO.serviceWithZIO[Reporter](_.endFeature("DSL Test Feature", List(stepResults)))
////      exitCode <- ZIO.succeed(if (stepResults.exists(_.succeeded == false)) ExitCode.failure else ExitCode.success)
////      _        <- ZIO.logInfo(s"DSL test run completed with exit code: $exitCode")
////    } yield exitCode
////  }
////
////  def main(args: Array[String]): Unit =
////    Unsafe.unsafe { implicit unsafe =>
////      val runtime    = Runtime.default
////      val featureDir = new File("example/src/test/resources/features")
////      val env = ZLayer.succeed(new UserRepo {
////        def createUser(name: String) = ZIO.succeed(User(name, s"$name@example.com".toLowerCase))
////      }) ++ ZLayer.succeed(new EmailService {
////        private var emails: List[String]  = Nil
////        def sendResetEmail(email: String) = ZIO.succeed { emails = email :: emails }
////        def getSentEmails                 = ZIO.succeed(emails)
////      }) ++ ZLayer.succeed(ZIO.logLevel(LogLevel.Info)) ++ LogCollector.live ++ ZLayer.succeed(
////        ConsoleReporter
////      ) ++ ZLayer.succeed(FileReporter)
////
////      runtime.unsafe
////        .run(
////          (runTests(UserSteps, featureDir, 4) *> runDslTests(UserSteps))
////            .provideLayer(env)
////        )
////        .getOrThrowFiberFailure() match {
////        case exitCode => println(s"Tests completed with exit code: $exitCode")
////      }
////    }
////}
