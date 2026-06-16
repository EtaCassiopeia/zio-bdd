package zio.bdd

import sbt.testing.*
import zio.bdd.core.report.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.core.{FeatureResult, InternalLogLevel, LogCollector, LogLevelConfig}
import zio.bdd.gherkin.{Feature, GherkinParser}
import zio.{Runtime, Unsafe, ZIO, ZLayer}

import scala.jdk.CollectionConverters.*
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
  reporters: List[Reporter] = List(new PrettyReporter(AnsiRenderer())),
  /** Max concurrent features. 1 = sequential (default). */
  featureParallelism: Int = 1,
  /**
   * Max concurrent scenarios per feature. 0 = auto (use available processors).
   */
  scenarioParallelism: Int = 0,
  includeTags: Set[String] = Set.empty,
  excludeTags: Set[String] = Set.empty,
  logLevel: InternalLogLevel = InternalLogLevel.Info,
  /** Optional glob/regex to filter scenarios by name (e.g. "Happy path*"). */
  scenarioNameFilter: Option[String] = None,
  /**
   * When true: step bodies are NOT executed. Only step matching is validated.
   * All steps appear as PASSED in the report. Used to validate that all Gherkin
   * steps in `.feature` files have a corresponding step definition.
   */
  dryRun: Boolean = false,
  /**
   * Step execution timeout in seconds from @Suite annotation. None = unlimited.
   */
  stepTimeoutSeconds: Option[Int] = None
)

case class CompositeReporter(reporters: List[Reporter]) extends Reporter {
  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    ZIO.foreachDiscard(reporters)(_.report(results))
}

/**
 * Resolves a single path entry (from `featureDirs` or `featureDir`) to a list
 * of `.feature` file paths. Supports both filesystem paths and classpath
 * resources via the `classpath:` prefix:
 *
 *   - `"src/test/resources/features"` — directory on the filesystem
 *   - `"classpath:features"` — directory on the classpath (e.g. from a shared
 *     jar)
 *   - `"classpath:features/my.feature"` — single file on the classpath
 */
case class FeatureFiles(path: String, testClassLoader: ClassLoader):
  private val ClasspathPrefix = "classpath:"

  def retrieve(): List[String] =
    if (path.isEmpty) Nil
    else if (path.startsWith(ClasspathPrefix)) resolveClasspath()
    else resolveFilesystem(new File(path))

  private def resolveClasspath(): List[String] =
    testClassLoader
      .getResources(path.stripPrefix(ClasspathPrefix))
      .asScala
      .toList
      .flatMap(url => collectFeatures(new File(url.toURI())))

  private def resolveFilesystem(file: File): List[String] =
    if (file.exists()) collectFeatures(file) else Nil

  private def collectFeatures(f: File): List[String] =
    if (f.isDirectory)
      Option(f.listFiles()).map(_.filter(_.getName.endsWith(".feature")).map(_.getAbsolutePath).toList).getOrElse(Nil)
    else if (f.getName.endsWith(".feature"))
      List(f.getAbsolutePath)
    else
      Nil

class ZIOBDDTask(
  taskDefinition: TaskDef,
  testClassLoader: ClassLoader,
  runtime: Runtime[Any],
  args: Array[String]
) extends Task {

  private val defaultTestReportDirName = "target/test-reports"

  /**
   * Resolve the test-reports directory relative to the suite's module root.
   *
   * sbt executes all tests with CWD = root project, but each module writes its
   * reports under <module>/target/test-reports/. We derive the module root by
   * walking up from the suite class's classfile location until we find the
   * "target" directory containing the class, giving us the same base that sbt's
   * JUnit listener uses for TEST-*.xml files.
   */
  private def resolveTestReportDir(className: String): String = {
    val resource = testClassLoader.getResource(className.replace('.', '/') + "$.class")
    if (resource == null) return defaultTestReportDirName
    try {
      val classPath = java.net.URLDecoder.decode(resource.getPath, "UTF-8")
      // classPath: …/tmm-tests-zio-bdd/target/scala-3.3.5/test-classes/…
      val targetIdx = classPath.lastIndexOf("/target/")
      if (targetIdx < 0) defaultTestReportDirName
      else classPath.substring(0, targetIdx) + "/target/test-reports"
    } catch {
      case _: Exception => defaultTestReportDirName
    }
  }

  override def taskDef(): TaskDef = taskDefinition

  private def filterFeatures(features: List[Feature], config: BDDTestConfig): List[Feature] = {
    val includeTags = config.includeTags
    val excludeTags = config.excludeTags

    def shouldIgnoreFeature(tags: List[String]): Boolean =
      tags.exists(excludeTags.contains)

    def shouldIgnoreScenario(tags: List[String], name: String): Boolean = {
      val hasExcludeTag     = tags.exists(excludeTags.contains)
      val missingIncludeTag = includeTags.nonEmpty && !tags.exists(includeTags.contains)
      // Scenario-name filter: treat as glob where * = any sequence of chars
      val nameFiltered = config.scenarioNameFilter.exists { pattern =>
        val regex = ("(?i)" + java.util.regex.Pattern.quote(pattern).replace("\\*", ".*")).r
        !regex.matches(name)
      }
      hasExcludeTag || missingIncludeTag || nameFiltered
    }

    features.map { feature =>
      val featureTags = if (shouldIgnoreFeature(feature.tags)) feature.tags :+ "ignore" else feature.tags
      val updatedScenarios = feature.scenarios.map { scenario =>
        val scenarioTags =
          if (shouldIgnoreScenario(scenario.tags, scenario.name)) scenario.tags :+ "ignore"
          else scenario.tags
        scenario.copy(tags = scenarioTags)
      }
      feature.copy(tags = featureTags, scenarios = updatedScenarios)
    }
  }

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val className = taskDef.fullyQualifiedName()
    loggers.foreach(_.info(s"Executing test for $className"))
    val suiteInstance = instantiateSuiteClass(className)
    val config        = parseConfig(args, className, loggers)

    // Apply annotation-configured step timeout when present and the suite has not overridden stepTimeout itself
    config.stepTimeoutSeconds.foreach { secs =>
      suiteInstance.overrideStepTimeout(zio.Duration.fromSeconds(secs))
    }

    val featureFiles = resolveFeatureFiles(config, className, loggers)
    loggers.foreach(_.info(s"Feature files: ${featureFiles.mkString(", ")}"))

    val reporter = if (config.reporters.length > 1) CompositeReporter(config.reporters) else config.reporters.head
    val env = ZLayer.succeed(suiteInstance) ++
      LogCollector.live(LogLevelConfig(config.logLevel)) ++
      ZLayer.succeed(reporter) ++
      suiteInstance.globalLayer

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

    val program = suiteInstance
      .run(
        updateFeatures,
        featureParallelism = config.featureParallelism,
        scenarioParallelism = resolveParallelism(config.scenarioParallelism),
        dryRun = config.dryRun
      )
      .tap(reporter.report)

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
      case "pretty"   => prettyReporter
      case "junitxml" => junitReporter(resolveTestReportDir(className), Some(className))
      case _ =>
        loggers.foreach(_.warn(s"Unknown reporter '$name', defaulting to ConsoleReporter"))
        new PrettyReporter(AnsiRenderer())
    }

    val annoConfig = annotation.map { a =>
      // featureDirs (new) takes precedence over deprecated featureDir
      val dirs =
        if (
          a.featureDirs().nonEmpty && !(a.featureDirs().length == 1 && a.featureDirs()(
            0
          ) == "src/test/resources/features")
        )
          a.featureDirs().toList
        else if (a.featureDir().nonEmpty)
          List(a.featureDir())
        else
          a.featureDirs().toList
      BDDTestConfig(
        featureFiles = dirs,
        reporters = a.reporters().map(instantiateReporter).toList,
        featureParallelism = a.parallelism(),
        scenarioParallelism = a.scenarioParallelism(),
        includeTags = a.includeTags().toSet,
        excludeTags = a.excludeTags().toSet,
        logLevel = parseLogLevelFromAnnotation(a.logLevel()),
        stepTimeoutSeconds = if (a.stepTimeout() > 0) Some(a.stepTimeout()) else None
      )
    }
      .getOrElse(BDDTestConfig())

    val cliConfig = BDDTestConfig(
      featureFiles = parseFeatureFiles(args),
      reporters = parseReporters(args, loggers),
      featureParallelism = parseParallelism(args),
      scenarioParallelism = parseScenarioParallelism(args),
      includeTags = parseIncludeTags(args),
      excludeTags = parseExcludeTags(args),
      logLevel = parseLogLevel(args, loggers),
      scenarioNameFilter = parseScenarioName(args),
      dryRun = args.contains("--dry-run")
    )

    BDDTestConfig(
      featureFiles = if (cliConfig.featureFiles.nonEmpty) cliConfig.featureFiles else annoConfig.featureFiles,
      reporters = if (cliConfig.reporters.nonEmpty) cliConfig.reporters else annoConfig.reporters,
      featureParallelism =
        if (cliConfig.featureParallelism != 1) cliConfig.featureParallelism else annoConfig.featureParallelism,
      scenarioParallelism =
        if (cliConfig.scenarioParallelism != 0) cliConfig.scenarioParallelism else annoConfig.scenarioParallelism,
      includeTags = if (cliConfig.includeTags.nonEmpty) cliConfig.includeTags else annoConfig.includeTags,
      excludeTags = if (cliConfig.excludeTags.nonEmpty) cliConfig.excludeTags else annoConfig.excludeTags,
      logLevel = if (cliConfig.logLevel != InternalLogLevel.Info) cliConfig.logLevel else annoConfig.logLevel,
      scenarioNameFilter = cliConfig.scenarioNameFilter.orElse(annoConfig.scenarioNameFilter),
      dryRun = cliConfig.dryRun || annoConfig.dryRun,
      stepTimeoutSeconds = annoConfig.stepTimeoutSeconds
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

  private def parseScenarioName(args: Array[String]): Option[String] =
    args.sliding(2).collectFirst { case Array("--scenario-name", name) => name }

  // Build a Reporter from its layer on the runner's runtime. Centralises the Unsafe.run
  // boilerplate so reporter construction is defined once.
  private def buildReporter(layer: ZLayer[Any, Throwable, Reporter]): Reporter =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.run(ZIO.scoped(layer.build.map(_.get[Reporter]))).getOrThrowFiberFailure()
    }

  private def prettyReporter: Reporter = buildReporter(PrettyReporter.live)

  private def junitReporter(outputDir: String, suiteClass: Option[String]): Reporter =
    buildReporter(
      JUnitXMLReporter.live(
        JUnitReporterConfig(
          outputDir = outputDir,
          suiteClass = suiteClass.getOrElse(""),
          format = JUnitXMLFormatter.Format.JUnit5
        )
      )
    )

  private def parseReporters(args: Array[String], loggers: Array[Logger]): List[Reporter] =
    args
      .sliding(2)
      .collect {
        case Array("--reporter", "pretty")   => prettyReporter
        case Array("--reporter", "junitxml") => junitReporter(defaultTestReportDirName, None)
        case Array("--reporter", unknown) =>
          loggers.foreach(_.warn(s"Unknown reporter '$unknown', defaulting to ConsoleReporter"))
          prettyReporter
      }
      .toList

  private def parseParallelism(args: Array[String]): Int =
    args.sliding(2).collectFirst { case Array("--parallelism", n) => n.toIntOption.getOrElse(1) }.getOrElse(1)

  private def parseScenarioParallelism(args: Array[String]): Int =
    args
      .sliding(2)
      .collectFirst {
        case Array("--scenario-parallelism", "auto") => 0
        case Array("--scenario-parallelism", n)      => n.toIntOption.getOrElse(0)
      }
      .getOrElse(0)

  /**
   * Resolves the auto sentinel (0) to the actual number of available
   * processors, falling back to 2 if the JVM reports an unreasonable value.
   */
  private def resolveParallelism(n: Int): Int =
    if (n > 0) n
    else
      (java.lang.Runtime.getRuntime.availableProcessors() match {
        case p if p > 0 => p
        case _          => 2
      })

  private def resolveFeatureFiles(config: BDDTestConfig, className: String, loggers: Array[Logger]): List[String] = {
    val paths =
      if (config.featureFiles.nonEmpty) config.featureFiles
      else {
        val clazz      = testClassLoader.loadClass(className + "$")
        val annotation = Option(clazz.getAnnotation(classOf[zio.bdd.core.Suite]))
        annotation.map { a =>
          if (a.featureDirs().nonEmpty) a.featureDirs().toList
          else if (a.featureDir().nonEmpty) List(a.featureDir())
          else List("src/test/resources/features")
        }.getOrElse(List("src/test/resources/features"))
      }

    paths.flatMap { path =>
      val files = FeatureFiles(path, testClassLoader).retrieve()
      if (files.isEmpty) loggers.foreach(_.warn(s"No feature files found for path: $path"))
      files
    }
  }

  private def discoverFeatures(steps: ZIOSteps[Any, Any], featureFiles: List[String]): List[Feature] =
    if (featureFiles.nonEmpty) {
      featureFiles.flatMap { path =>
        val featureContent = scala.io.Source.fromFile(path).mkString
        Unsafe.unsafe { implicit unsafe =>
          // Per-file resilience (issue #46): skip unparseable files rather than aborting the run.
          runtime.unsafe
            .run(
              GherkinParser
                .parseFeature(featureContent, path)
                .foldZIO(
                  failure = { err =>
                    ZIO.logError(s"Skipping $path: ${err.getMessage}").as(None)
                  },
                  success = f => ZIO.succeed(Some(f))
                )
            )
            .getOrThrowFiberFailure()
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
