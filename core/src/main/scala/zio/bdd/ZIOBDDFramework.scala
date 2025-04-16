package zio.bdd

import sbt.testing.*
import zio.bdd.core.report.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.core.{FeatureResult, InternalLogLevel, LogCollector, LogLevelConfig}
import zio.bdd.gherkin.{Feature, GherkinParser}
import zio.{Runtime, Unsafe, ZIO, ZLayer}

import java.io.File

class ZIOBDDFingerprint extends AnnotatedFingerprint {
  override def annotationName(): String = "zio.bdd.core.Suite"
  override def isModule: Boolean        = true
}

class ZIOBDDFramework extends Framework {
  override def name(): String = "zio-bdd"

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

  override def done(): String = "zio-bdd execution completed"

  override def args(): Array[String]       = runnerArgs
  override def remoteArgs(): Array[String] = runnerRemoteArgs
}

case class BDDTestConfig(
  featureFiles: List[String] = Nil,
  reporters: List[Reporter] = List(PrettyReporter()),
  parallelism: Int = 1,
  includeTags: Set[String] = Set.empty, // Inclusive filter (e.g., run only these tags)
  excludeTags: Set[String] = Set.empty, // Exclusive filter (e.g., skip these tags)
  logLevel: InternalLogLevel = InternalLogLevel.Info
)

case class CompositeReporter(reporters: List[Reporter]) extends Reporter {
  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    ZIO.foreachDiscard(reporters)(_.report(results))
}

class ZIOBDDTask(
  taskDefinition: TaskDef,
  testClassLoader: ClassLoader,
  runtime: Runtime[Any],
  args: Array[String]
) extends Task {

  private val defaultTestResultDir = "target/test-results"

  override def taskDef(): TaskDef = taskDefinition

  private def filterFeatures(features: List[Feature], config: BDDTestConfig): List[Feature] = {
    val includeTags = config.includeTags
    val excludeTags = config.excludeTags

    // Determine if a set of tags should result in ignoring the feature or scenario
    def shouldIgnore(tags: List[String]): Boolean = {
      val hasExcludeTag     = tags.exists(excludeTags.contains)
      val missingIncludeTag = includeTags.nonEmpty && !tags.exists(includeTags.contains)
      hasExcludeTag || missingIncludeTag
    }

    // Process each feature and its scenarios
    features.map { feature =>
      // Update feature tags if it should be ignored
      val featureTags = if (shouldIgnore(feature.tags)) feature.tags :+ "ignore" else feature.tags

      // Update each scenario's tags if it should be ignored
      val updatedScenarios = feature.scenarios.map { scenario =>
        val scenarioTags = if (shouldIgnore(scenario.tags)) scenario.tags :+ "ignore" else scenario.tags
        scenario.copy(tags = scenarioTags)
      }

      // Return a new Feature with updated tags and scenarios
      feature.copy(tags = featureTags, scenarios = updatedScenarios)
    }
  }

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val className = taskDef.fullyQualifiedName()
    loggers.foreach(_.info(s"Executing test for $className"))
    val suiteInstance = instantiateSuiteClass(className)
    val config        = parseConfig(args, className, loggers)

    val featureFiles = resolveFeatureFiles(config, className, loggers)
    loggers.foreach(_.info(s"Feature files: ${featureFiles.mkString(", ")}"))

    val reporter = if (config.reporters.length > 1) CompositeReporter(config.reporters) else config.reporters.head
    val env = ZLayer.succeed(suiteInstance) ++
      LogCollector.live(LogLevelConfig(config.logLevel)) ++
      ZLayer.succeed(reporter) ++
      suiteInstance.environment

    val features =
      try {
        discoverFeatures(suiteInstance, featureFiles)
      } catch {
        case e: Throwable =>
          loggers.foreach(_.error(s"Failed to parse features: ${e.getMessage}"))
          List(
            Feature(
              name = "Failed Feature",
              scenarios = Nil,
              file = Some("unknown.feature"),
              line = Some(1)
            )
          )
      }
    loggers.foreach(_.info(s"Parsed features: ${features.map(_.name).mkString(", ")}"))

    val updateFeatures = filterFeatures(features, config)

    val program = suiteInstance.run(updateFeatures)

    val results =
      try {
        Unsafe.unsafe { implicit unsafe =>
          val result = runtime.unsafe.run(program.provideLayer(env))
          result.getOrThrowFiberFailure()
        }
      } catch {
        case e: Throwable =>
          loggers.foreach(_.error(s"Execution failed: ${e.getMessage}"))
          loggers.foreach(_.debug(s"Exception stack trace: ${e.getStackTrace.mkString("\n")}"))
          Nil
      }

    reportResults(results, eventHandler, loggers)
    Array()
  }

  override def tags(): Array[String] = Array("zio-bdd")

  private def instantiateSuiteClass(className: String): ZIOSteps[Any, Any] =
    try {
      val moduleClassName = if (className.endsWith("$")) className else className + "$"
      val clazz           = testClassLoader.loadClass(moduleClassName)
      val instanceField   = clazz.getField("MODULE$")
      instanceField.get(null).asInstanceOf[ZIOSteps[Any, Any]]
    } catch {
      case e: Exception => throw e
    }

  private def parseConfig(args: Array[String], className: String, loggers: Array[Logger]): BDDTestConfig = {
    val clazz      = testClassLoader.loadClass(className + "$")
    val annotation = Option(clazz.getAnnotation(classOf[zio.bdd.core.Suite]))

    def instantiateReporter(name: String): Reporter = name match {
      case "pretty" =>
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe
            .run(
              ZIO.scoped(
                PrettyReporter.live.build.map(_.get[Reporter])
              )
            )
            .getOrThrowFiberFailure()
        }
      case "junitxml" =>
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe
            .run(
              ZIO.scoped(
                JUnitXMLReporter
                  .live(JUnitReporterConfig(outputDir = defaultTestResultDir, format = JUnitXMLFormatter.Format.JUnit5))
                  .build
                  .map(_.get[Reporter])
              )
            )
            .getOrThrowFiberFailure()
        }
      case _ =>
        loggers.foreach(_.warn(s"Unknown reporter '$name', defaulting to ConsoleReporter"))
        PrettyReporter()
    }

    val annoConfig = annotation
      .map(a =>
        BDDTestConfig(
          featureFiles = if (a.featureDir().isEmpty) Nil else List(a.featureDir()),
          reporters = a.reporters().map(instantiateReporter).toList,
          parallelism = a.parallelism(),
          includeTags = a.includeTags().toSet,
          excludeTags = a.excludeTags().toSet,
          logLevel = parseLogLevelFromAnnotation(a.logLevel())
        )
      )
      .getOrElse(BDDTestConfig())

    val cliConfig = BDDTestConfig(
      featureFiles = parseFeatureFiles(args),
      reporters = parseReporters(args, loggers),
      parallelism = parseParallelism(args),
      includeTags = parseIncludeTags(args),
      excludeTags = parseExcludeTags(args),
      logLevel = parseLogLevel(args, loggers)
    )

    BDDTestConfig(
      featureFiles = if (cliConfig.featureFiles.nonEmpty) cliConfig.featureFiles else annoConfig.featureFiles,
      reporters = if (cliConfig.reporters.nonEmpty) cliConfig.reporters else annoConfig.reporters,
      parallelism = if (cliConfig.parallelism != 1) cliConfig.parallelism else annoConfig.parallelism,
      includeTags = if (cliConfig.includeTags.nonEmpty) cliConfig.includeTags else annoConfig.includeTags,
      excludeTags = if (cliConfig.excludeTags.nonEmpty) cliConfig.excludeTags else annoConfig.excludeTags,
      logLevel =
        if (cliConfig.logLevel != InternalLogLevel.Info) cliConfig.logLevel
        else annoConfig.logLevel // CLI overrides annotation
    )
  }

  private def parseLogLevel(args: Array[String], loggers: Array[Logger]): InternalLogLevel =
    args
      .sliding(2)
      .collectFirst { case Array("--log-level", level) =>
        level.toLowerCase match {
          case "debug" => InternalLogLevel.Debug
          case "info"  => InternalLogLevel.Info
          case "error" => InternalLogLevel.Error
          case _ =>
            loggers.foreach(_.warn(s"Unknown log level '$level', defaulting to Info"))
            InternalLogLevel.Info
        }
      }
      .getOrElse(InternalLogLevel.Info)

  private def parseLogLevelFromAnnotation(level: String): InternalLogLevel =
    level.toLowerCase match {
      case "debug" => InternalLogLevel.Debug
      case "info"  => InternalLogLevel.Info
      case "error" => InternalLogLevel.Error
      case _       => InternalLogLevel.Info // Default if annotation value is invalid
    }

  private def parseIncludeTags(args: Array[String]): Set[String] =
    args
      .sliding(2)
      .collect { case Array("--include-tags", tags) => tags.split(",").map(_.trim).toSet }
      .fold(Set.empty)(_ ++ _)

  private def parseExcludeTags(args: Array[String]): Set[String] =
    args
      .sliding(2)
      .collect { case Array("--exclude-tags", tags) => tags.split(",").map(_.trim).toSet }
      .fold(Set.empty)(_ ++ _)

  private def parseFeatureFiles(args: Array[String]): List[String] =
    args.sliding(2).collect { case Array("--feature-file", path) => path }.toList

  private def parseReporters(args: Array[String], loggers: Array[Logger]): List[Reporter] =
    args
      .sliding(2)
      .collect {
        case Array("--reporter", "pretty") =>
          Unsafe.unsafe { implicit unsafe =>
            runtime.unsafe
              .run(
                ZIO.scoped(
                  PrettyReporter.live.build.map(_.get[Reporter])
                )
              )
              .getOrThrowFiberFailure()
          }
        case Array("--reporter", "junitxml") =>
          Unsafe.unsafe { implicit unsafe =>
            runtime.unsafe
              .run(
                ZIO.scoped(
                  JUnitXMLReporter
                    .live(
                      JUnitReporterConfig(outputDir = defaultTestResultDir, format = JUnitXMLFormatter.Format.JUnit5)
                    )
                    .build
                    .map(_.get[Reporter])
                )
              )
              .getOrThrowFiberFailure()
          }
        case Array("--reporter", unknown) =>
          loggers.foreach(_.warn(s"Unknown reporter '$unknown', defaulting to ConsoleReporter"))
          Unsafe.unsafe { implicit unsafe =>
            runtime.unsafe
              .run(
                ZIO.scoped(
                  PrettyReporter.live.build.map(_.get[Reporter])
                )
              )
              .getOrThrowFiberFailure()
          }
      }
      .toList

  private def parseParallelism(args: Array[String]): Int =
    args
      .sliding(2)
      .collectFirst { case Array("--parallelism", n) =>
        n.toIntOption.getOrElse(1)
      }
      .getOrElse(1)

  private def resolveFeatureFiles(config: BDDTestConfig, className: String, loggers: Array[Logger]): List[String] =
    if (config.featureFiles.nonEmpty) {
      config.featureFiles.flatMap { path =>
        val file = new File(path)
        if (file.isDirectory) {
          file.listFiles().filter(_.getName.endsWith(".feature")).map(_.getAbsolutePath).toList
        } else if (file.exists() && file.getName.endsWith(".feature")) {
          List(file.getAbsolutePath)
        } else {
          loggers.foreach(_.warn(s"Invalid feature file or directory: $path"))
          Nil
        }
      }
    } else {
      val clazz      = testClassLoader.loadClass(className + "$")
      val annotation = Option(clazz.getAnnotation(classOf[zio.bdd.core.Suite]))
      val featureDir = annotation.map(_.featureDir()).getOrElse("src/test/resources/features")
      val dir        = new File(featureDir)
      if (dir.exists() && dir.isDirectory) {
        dir.listFiles().filter(_.getName.endsWith(".feature")).map(_.getAbsolutePath).toList
      } else {
        loggers.foreach(_.warn(s"Feature directory '$featureDir' not found or not a directory"))
        List()
      }
    }

  private def discoverFeatures(steps: ZIOSteps[Any, Any], featureFiles: List[String]): List[Feature] =
    if (featureFiles.nonEmpty) {
      featureFiles.map { path =>
        val featureContent = scala.io.Source.fromFile(path).mkString
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe.run(GherkinParser.parseFeature(featureContent, path)).getOrThrowFiberFailure()
        }
      }
    } else {
      List(
        Feature(
          name = "Default Feature",
          scenarios = Nil,
          file = Some("unknown.feature"),
          line = Some(1)
        )
      )
    }

  private def reportResults(results: List[FeatureResult], eventHandler: EventHandler, loggers: Array[Logger]): Unit = {
    loggers.foreach(_.info(s"Reporting ${results.length} results"))
    if (results.isEmpty) {
      loggers.foreach(_.warn("No results to report - test may have failed or produced no steps"))
      val event = new Event {
        override def fullyQualifiedName(): String   = taskDef.fullyQualifiedName()
        override def fingerprint(): Fingerprint     = taskDef.fingerprint()
        override def selector(): Selector           = new SuiteSelector()
        override def status(): Status               = Status.Failure
        override def throwable(): OptionalThrowable = new OptionalThrowable(new Exception("No test steps executed"))
        override def duration(): Long               = 0L
      }
      eventHandler.handle(event)
    } else {
      results.foreach { result =>
        val event = new Event {
          override def fullyQualifiedName(): String = taskDef.fullyQualifiedName()
          override def fingerprint(): Fingerprint   = taskDef.fingerprint()
          override def selector(): Selector         = new TestSelector(result.feature.name)
          override def status(): Status             = if (result.isPassed) Status.Success else Status.Failure
          override def throwable(): OptionalThrowable = result.error match {
            case Some(t) => new OptionalThrowable(new Exception(t.getMessage, t.getCause))
            case None    => new OptionalThrowable()
          }
          override def duration(): Long = result.duration
        }
        eventHandler.handle(event)
      }
    }
  }
}
