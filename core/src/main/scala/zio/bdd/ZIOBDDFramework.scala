package zio.bdd

import sbt.testing.*
import zio.bdd.core.report.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.core.{FeatureResult, InternalLogLevel, LogCollector, LogLevelConfig}
import zio.bdd.gherkin.{Feature, GherkinParser}
import zio.{FiberFailure, Runtime, Unsafe, ZIO, ZLayer}

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
  stepTimeoutSeconds: Option[Int] = None,
  /**
   * When true: scenarios excluded by a name or tag filter are removed from the
   * report entirely rather than printed as IGNORED. Pass `--focused` from the
   * IDE test runner to get a clean single-scenario report with no noise.
   */
  focused: Boolean = false
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

  private[bdd] def filterFeatures(features: List[Feature], config: BDDTestConfig): List[Feature] =
    ZIOBDDTask.filterFeatures(features, config)

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val className = taskDef.fullyQualifiedName()
    loggers.foreach(_.info(s"Executing test for $className"))
    try {
      // instantiateSuiteClass is inside the try so a suite object's <clinit>
      // linkage error (ExceptionInInitializerError / NoClassDefFoundError — Errors,
      // not Exceptions) is caught and reported enriched below, not escaped raw (#308).
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
            loggers.foreach(_.error(s"Failed to parse features:\n${ZIOBDDTask.describeFailure(e)}"))
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
          featureParallelism =
            ZIOBDDTask.effectiveParallelism(suiteInstance.featureParallelism, config.featureParallelism),
          scenarioParallelism =
            ZIOBDDTask.effectiveParallelism(suiteInstance.scenarioParallelism, config.scenarioParallelism),
          dryRun = config.dryRun
        )
        .tap { results =>
          // In focused mode, strip @ignore scenario results before reporting so the
          // output only shows what actually ran. Execution is unaffected — the
          // scenarios still run (or are skipped) according to the normal filter logic;
          // we just suppress the IGNORED noise in the reporter output.
          val toReport =
            if (config.focused)
              results
                .map(fr => fr.copy(scenarioResults = fr.scenarioResults.filterNot(_.isIgnored)))
                .filter(_.scenarioResults.nonEmpty)
            else results
          reporter.report(toReport)
        }

      val results =
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe.run(program.provideLayer(env)).getOrThrowFiberFailure()
        }

      reportResults(results, eventHandler, loggers)
    } catch {
      // A suite-level failure — a layer build that dies, a run interrupt, or a
      // <clinit> linkage error above — is unwrapped to its full cause (#308), not
      // collapsed to getMessage, and threaded into the empty-results event so
      // JUnit/CI reports carry it even after the log scrolls away.
      case e: Throwable =>
        loggers.foreach(_.error(s"Execution failed:\n${ZIOBDDTask.describeFailure(e)}"))
        reportResults(Nil, eventHandler, loggers, Some(e))
    }
    Array()
  }

  override def tags(): Array[String] = Array("zio-bdd")

  // Any failure here — including a suite object's <clinit> linkage Error — propagates
  // to execute's outer try, which reports it enriched via describeFailure (#308).
  private def instantiateSuiteClass(className: String): ZIOSteps[Any, Any] =
    val moduleClassName = if (className.endsWith("$")) className else className + "$"
    val clazz           = testClassLoader.loadClass(moduleClassName)
    val instanceField   = clazz.getField("MODULE$")
    instanceField.get(null).asInstanceOf[ZIOSteps[Any, Any]]

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
      logLevel = parseLogLevel(args),
      scenarioNameFilter = parseScenarioName(args),
      dryRun = args.contains("--dry-run"),
      focused = args.contains("--focused")
    )

    // Option B: CI-wide env override, slotted between the CLI flag and the annotation.
    val envFeatureParallelism  = ZIOBDDTask.parseEnvParallelism(sys.env.get("ZIO_BDD_FEATURE_PARALLELISM"))
    val envScenarioParallelism = ZIOBDDTask.parseEnvParallelism(sys.env.get("ZIO_BDD_SCENARIO_PARALLELISM"))

    BDDTestConfig(
      featureFiles = if (cliConfig.featureFiles.nonEmpty) cliConfig.featureFiles else annoConfig.featureFiles,
      reporters = if (cliConfig.reporters.nonEmpty) cliConfig.reporters else annoConfig.reporters,
      featureParallelism = ZIOBDDTask.resolveFeatureParallelism(
        cliConfig.featureParallelism,
        envFeatureParallelism,
        annoConfig.featureParallelism
      ),
      scenarioParallelism = ZIOBDDTask.resolveScenarioParallelism(
        cliConfig.scenarioParallelism,
        envScenarioParallelism,
        annoConfig.scenarioParallelism
      ),
      includeTags = if (cliConfig.includeTags.nonEmpty) cliConfig.includeTags else annoConfig.includeTags,
      excludeTags = if (cliConfig.excludeTags.nonEmpty) cliConfig.excludeTags else annoConfig.excludeTags,
      logLevel = if (cliConfig.logLevel != InternalLogLevel.Info) cliConfig.logLevel else annoConfig.logLevel,
      scenarioNameFilter = cliConfig.scenarioNameFilter.orElse(annoConfig.scenarioNameFilter),
      dryRun = cliConfig.dryRun || annoConfig.dryRun,
      focused = cliConfig.focused || annoConfig.focused,
      stepTimeoutSeconds = annoConfig.stepTimeoutSeconds
    )
  }

  private def parseLogLevel(args: Array[String]): InternalLogLevel =
    args
      .sliding(2)
      .collectFirst { case Array("--log-level", level) => ZIOBDDTask.parseLogLevelOrThrow(level) }
      .getOrElse(InternalLogLevel.Info)

  private def parseLogLevelFromAnnotation(level: String): InternalLogLevel =
    ZIOBDDTask.parseLogLevelOrThrow(level)

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
    args.sliding(2).collect { case Array("--feature-file", path) => unquote(path) }.toList

  private def parseScenarioName(args: Array[String]): Option[String] =
    args.sliding(2).collectFirst { case Array("--scenario-name", name) => unquote(name) }

  // sbt's argument parser does not strip single quotes — it passes them literally,
  // so '--feature-file '/path/file.feature'' yields the path with embedded quotes.
  // Strip matching outer quotes as a defensive measure.  Double quotes may arrive
  // similarly on Windows or when commands are composed by hand.
  private def unquote(s: String): String =
    if (
      s.length >= 2 &&
      ((s.charAt(0) == '\'' && s.last == '\'') ||
        (s.charAt(0) == '"' && s.last == '"'))
    )
      s.drop(1).dropRight(1)
    else s

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

  private def reportResults(
    results: List[FeatureResult],
    eventHandler: EventHandler,
    loggers: Array[Logger],
    cause: Option[Throwable] = None
  ): Unit = {
    loggers.foreach(_.info(s"Reporting ${results.length} results"))
    if (results.isEmpty) {
      loggers.foreach(_.warn("No results to report - test may have failed or produced no steps"))
      // Carry the real suite-level failure (if any) into the event so JUnit XML / CI
      // annotations show why nothing ran, instead of a synthetic placeholder (#308).
      val reported = cause.getOrElse(new Exception("No test steps executed"))
      val event = new Event {
        override def fullyQualifiedName(): String   = taskDef.fullyQualifiedName()
        override def fingerprint(): Fingerprint     = taskDef.fingerprint()
        override def selector(): Selector           = new SuiteSelector()
        override def status(): Status               = Status.Failure
        override def throwable(): OptionalThrowable = new OptionalThrowable(reported)
        override def duration(): Long               = 0L
      }
      eventHandler.handle(event)
    } else {
      results.foreach { result =>
        val event = new Event {
          override def fullyQualifiedName(): String = taskDef.fullyQualifiedName()
          override def fingerprint(): Fingerprint   = taskDef.fingerprint()
          override def selector(): Selector         = new TestSelector(result.feature.name)
          override def status(): Status             = ZIOBDDTask.statusOf(result)
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

object ZIOBDDTask {

  /**
   * A full, CI-legible diagnostic for a suite-level failure (#308). The sbt
   * runner otherwise reports only `Throwable.getMessage`, which collapses the
   * two most common wrappers to an undebuggable one-liner: a
   * `zio.FiberFailure`'s message is a single typed-failure line (losing the
   * interrupter, defects and trace), and an
   * `ExceptionInInitializerError`/`NoClassDefFoundError` has no message of its
   * own (losing the original cause entirely). Unwrap both: `FiberFailure` → the
   * full `Cause.prettyPrint`; anything else → its whole `printStackTrace` chain
   * (which includes every `Caused by:`).
   */
  def describeFailure(t: Throwable): String =
    t match
      case ff: FiberFailure => ff.cause.prettyPrint
      case _ =>
        val sw = new java.io.StringWriter
        t.printStackTrace(new java.io.PrintWriter(sw))
        sw.toString.trim

  /**
   * The sbt build status for one feature. A `PENDING` scenario is a first-class
   * "not built yet" status that must NOT fail the build, so this gates on
   * `isComplete` (pending-tolerant), not `isPassed` — see `FeatureResult` in
   * Result.scala. Issue #304.
   */
  def statusOf(result: FeatureResult): Status =
    if (result.isComplete) Status.Success else Status.Failure

  /**
   * Parse a log-level name (case-insensitive) into an `InternalLogLevel`,
   * accepting all five levels. `Left` names the offending value and the valid
   * set. Shared by the `--log-level` CLI flag and the `@Suite(logLevel)`
   * annotation so both accept the same strings — issue #278 item 4.
   */
  def parseLogLevelString(level: String): Either[String, InternalLogLevel] =
    level.toLowerCase match {
      case "debug"   => Right(InternalLogLevel.Debug)
      case "info"    => Right(InternalLogLevel.Info)
      case "warning" => Right(InternalLogLevel.Warning)
      case "error"   => Right(InternalLogLevel.Error)
      case "fatal"   => Right(InternalLogLevel.Fatal)
      case other =>
        Left(s"Unknown log level '$other'. Valid values: debug, info, warning, error, fatal.")
    }

  /**
   * Parse a log-level name, failing loud (throwing) on an unknown value rather
   * than silently falling back to Info. Used by both config paths.
   */
  def parseLogLevelOrThrow(level: String): InternalLogLevel =
    parseLogLevelString(level).fold(msg => throw new IllegalArgumentException(msg), identity)

  /**
   * Parse a `ZIO_BDD_*_PARALLELISM` env value: `"auto"` → `Some(0)`, a valid
   * integer → `Some(n)`, and empty/absent/garbage → `None` (defer to the next
   * source in the precedence chain).
   */
  def parseEnvParallelism(raw: Option[String]): Option[Int] =
    raw.filter(_.nonEmpty).flatMap(s => if (s == "auto") Some(0) else s.toIntOption)

  /**
   * Resolve scenario parallelism across CLI > env > annotation. The CLI "unset"
   * sentinel is `0` (auto), so a non-zero CLI value wins; otherwise the env
   * value applies, else the annotation value.
   */
  def resolveScenarioParallelism(cli: Int, env: Option[Int], anno: Int): Int =
    if (cli != 0) cli else env.getOrElse(anno)

  /**
   * Resolve feature parallelism across CLI > env > annotation. The CLI "unset"
   * sentinel is `1` (sequential), so a CLI value other than `1` wins; otherwise
   * the env value applies, else the annotation value.
   */
  def resolveFeatureParallelism(cli: Int, env: Option[Int], anno: Int): Int =
    if (cli != 1) cli else env.getOrElse(anno)

  /**
   * Apply the suite-level override (Option A), which has the highest
   * precedence: `Some(n)` wins over the already-resolved config value; `None`
   * defers to it.
   */
  def effectiveParallelism(suiteOverride: Option[Int], configValue: Int): Int =
    suiteOverride.getOrElse(configValue)

  /**
   * Apply include/exclude tag filters and the scenario-name filter to a list of
   * features.
   *
   * Per the Gherkin spec, feature-level tags are inherited by all of a
   * feature's scenarios for the purpose of filtering: a feature tagged `@smoke`
   * makes every scenario match `--include-tags smoke`, even when the scenario
   * carries no tags of its own. Scenarios that do not survive the filter are
   * marked `@ignore`; a feature whose scenarios are all ignored is itself
   * marked `@ignore`.
   */
  def filterFeatures(features: List[Feature], config: BDDTestConfig): List[Feature] = {
    val includeTags = config.includeTags
    val excludeTags = config.excludeTags

    def shouldIgnoreScenario(featureTags: List[String], scenarioTags: List[String], name: String): Boolean = {
      // Feature tags are inherited by the scenario for filtering.
      val effectiveTags     = (featureTags ++ scenarioTags).distinct
      val hasExcludeTag     = effectiveTags.exists(excludeTags.contains)
      val missingIncludeTag = includeTags.nonEmpty && !effectiveTags.exists(includeTags.contains)
      // Scenario-name filter: glob where '*' = any sequence of chars, rest matched
      // literally and case-insensitively. Split on '*' and quote each literal segment so
      // regex metacharacters in the name (and the pattern) are not interpreted.
      val nameFiltered = config.scenarioNameFilter.exists { pattern =>
        val regex = ("(?i)" + pattern.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*")).r
        !regex.matches(name)
      }
      hasExcludeTag || missingIncludeTag || nameFiltered
    }

    features.map { feature =>
      val updatedScenarios = feature.scenarios.map { scenario =>
        val scenarioTags =
          if (shouldIgnoreScenario(feature.tags, scenario.tags, scenario.name)) scenario.tags :+ "ignore"
          else scenario.tags
        scenario.copy(tags = scenarioTags)
      }
      // Mark the feature itself as ignored when all of its scenarios are ignored.
      val allScenariosIgnored = updatedScenarios.nonEmpty && updatedScenarios.forall(_.isIgnored)
      val featureTags         = if (allScenariosIgnored) feature.tags :+ "ignore" else feature.tags
      feature.copy(tags = featureTags, scenarios = updatedScenarios)
    }
  }
}
