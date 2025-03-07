package zio.bdd.core

import sbt.testing.*
import zio.bdd.gherkin.{Feature, GherkinParser}
import zio.{Clock, Runtime, Unsafe, ZLayer}

import java.io.File
import java.lang.annotation.Annotation

class ZIOBDDFingerprint extends AnnotatedFingerprint {
  override def annotationName(): String = "zio.bdd.core.ZIOBDDTest"
  override def isModule(): Boolean      = true
}

class ZIOBDDFramework extends Framework {
  override def name(): String = "ZIOBDD"

  override def fingerprints(): Array[Fingerprint] =
    Array(new ZIOBDDFingerprint)

  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
    new ZIOBDDRunner(args, remoteArgs, testClassLoader)
}

class ZIOBDDRunner(runnerArgs: Array[String], runnerRemoteArgs: Array[String], testClassLoader: ClassLoader)
    extends Runner {
  private val runtime = Runtime.default

  override def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.map { taskDef =>
      new ZIOBDDTask(taskDef, testClassLoader, runtime, runnerArgs)
    }

  override def done(): String = "ZIOBDD execution completed"

  override def args(): Array[String]       = runnerArgs
  override def remoteArgs(): Array[String] = runnerRemoteArgs
}

class ZIOBDDTask(
  taskDefinition: TaskDef,
  testClassLoader: ClassLoader,
  runtime: Runtime[Any],
  args: Array[String]
) extends Task {
  override def taskDef(): TaskDef = taskDefinition

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val className = taskDef.fullyQualifiedName()
    loggers.foreach(_.info(s"ZIOBDDTask: Executing test for $className"))
    val stepInstance = instantiateStepClass(className)

    val reporter             = parseReporter(args).getOrElse(ConsoleReporter)
    val featureFilesFromArgs = parseFeatureFiles(args)
    val featureFiles = if (featureFilesFromArgs.isEmpty) {
      // Fallback to annotation-provided directory
      val clazz = testClassLoader.loadClass(className + "$")
      // Explicitly type as Class[_ <: Annotation] and cast to ZIOBDDTest
      // TODO: Fails to find annotation - Remove default
      val annotation =
        clazz.getAnnotation(classOf[ZIOBDDTest].asInstanceOf[Class[? <: Annotation]]).asInstanceOf[ZIOBDDTest]
      val featureDir = if (annotation != null) annotation.featureDir else "example/src/test/resources/features"
      val dir        = new File(featureDir)
      if (dir.exists() && dir.isDirectory) {
        dir
          .listFiles()
          .filter(_.getName.endsWith(".feature"))
          .map(_.getAbsolutePath)
          .toList
      } else {
        loggers.foreach(_.warn(s"ZIOBDDTask: Feature directory '$featureDir' not found or not a directory"))
        List()
      }
    } else {
      featureFilesFromArgs
    }
    loggers.foreach(_.info(s"ZIOBDDTask: Feature files: ${featureFiles.mkString(", ")}"))
    val env = ZLayer.succeed(stepInstance) ++ LogCollector.live ++ ZLayer.succeed(reporter) ++ stepInstance.environment

    val features =
      try {
        discoverFeatures(stepInstance, featureFiles)
      } catch {
        case e: Throwable =>
          loggers.foreach(_.error(s"ZIOBDDTask: Failed to parse features: ${e.getMessage}"))
          Feature("Failed Feature", scenarios = Nil)
      }
    loggers.foreach(_.info(s"ZIOBDDTask: Parsed features: ${features.toString}"))

    val program = ScenarioRunner
      .runScenarios(stepInstance, features, parallelism = 1)
      .map(_.flatten)

    val results =
      try {
        Unsafe.unsafe { implicit unsafe =>
          val result = runtime.unsafe.run(program.provideLayer(env))
          loggers.foreach(_.info(s"ZIOBDDTask: Run result: $result"))
          result.getOrThrowFiberFailure()
        }
      } catch {
        case e: Throwable =>
          loggers.foreach(_.error(s"ZIOBDDTask: Execution failed: ${e.getMessage}"))
          loggers.foreach(_.debug(s"ZIOBDDTask: Exception stack trace: ${e.getStackTrace.mkString("\n")}"))
          Nil
      }

    reportResults(results, eventHandler, loggers)
    Array()
  }

  override def tags(): Array[String] = Array("zio-bdd")

  private def instantiateStepClass(className: String): ZIOSteps[Any] =
    try {
      val moduleClassName = if (className.endsWith("$")) className else className + "$"
      val clazz           = testClassLoader.loadClass(moduleClassName)
      val instanceField   = clazz.getField("MODULE$")
      val instance        = instanceField.get(null).asInstanceOf[ZIOSteps[Any]]
      instance
    } catch {
      case e: Exception =>
        throw e
    }

  private def parseReporter(args: Array[String]): Option[Reporter] =
    args.sliding(2).collectFirst {
      case Array("--reporter", "console") => ConsoleReporter
      case Array("--reporter", "file")    => FileReporter
    }

  private def parseFeatureFiles(args: Array[String]): List[String] =
    args
      .sliding(2)
      .collect { case Array("--feature-file", path) =>
        path
      }
      .toList

  private def discoverFeatures(steps: ZIOSteps[Any], featureFiles: List[String]): Feature =
    if (featureFiles.nonEmpty) {
      val featureContents = featureFiles.map { path =>
        scala.io.Source.fromFile(path).mkString
      }.mkString("\n")
      Unsafe.unsafe { implicit unsafe =>
        runtime.unsafe.run(GherkinParser.parseFeature(featureContents)).getOrThrowFiberFailure()
      }
    } else {
      Feature("Default Feature", scenarios = Nil)
    }

  private def reportResults(
    results: List[StepResult],
    eventHandler: EventHandler,
    loggers: Array[Logger]
  ): Unit = {
    loggers.foreach(_.info(s"ZIOBDDTask: Reporting ${results.length} results"))
    if (results.isEmpty) {
      loggers.foreach(_.warn("ZIOBDDTask: No results to report - test may have failed or produced no steps"))
      val event = new Event {
        override def fullyQualifiedName(): String = taskDef.fullyQualifiedName()

        override def fingerprint(): Fingerprint = taskDef.fingerprint()

        override def selector(): Selector = new SuiteSelector()

        override def status(): Status = Status.Failure

        override def throwable(): OptionalThrowable = new OptionalThrowable(new Exception("No test steps executed"))

        override def duration(): Long = 0L
      }
      eventHandler.handle(event)
    } else {
      results.foreach { result =>
        val event = new Event {
          override def fullyQualifiedName(): String = taskDef.fullyQualifiedName()

          override def fingerprint(): Fingerprint = taskDef.fingerprint()

          override def selector(): Selector = new TestSelector(result.step)

          override def status(): Status = if (result.succeeded) Status.Success else Status.Failure

          override def throwable(): OptionalThrowable = result.error match {
            case Some(t) => new OptionalThrowable(t)
            case None    => new OptionalThrowable()
          }

          override def duration(): Long = result.duration.toMillis
        }
        eventHandler.handle(event)

        loggers.foreach { logger =>
          val logMsg =
            s"${result.step} - ${if (result.succeeded) "PASSED" else "FAILED"} (duration: ${result.duration.toMillis}ms)"
          logger.info(logMsg)
          result.logs.foreach { case (msg, time) => logger.debug(s"[$time] $msg") }
          result.error.foreach { t =>
            logger.error(s"Error: ${t.getMessage}")
            logger.debug(s"Stack trace: ${t.getStackTrace.mkString("\n")}")
          }
        }
      }
    }
  }
}
