package zio.bdd.core.property

import zio.*
import zio.bdd.gherkin.Scenario

import java.io.{File, IOException, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.time.Instant

/**
 * Persists and replays failing property seeds across test runs.
 *
 * On any property failure the executor writes a JSON file to
 * `.zio-bdd/failures/<scenario-slug>.json`. On the next run the stored failing
 * seed is replayed FIRST so developers get immediate feedback on whether a fix
 * worked — without burning new samples.
 *
 * Body-hash invalidation: if the scenario's step list changes, the stale
 * failure is discarded with a warning.
 */
object PropertyFailureStore:

  case class FailureRecord(
    scenarioId: String,
    bodyHash: String,
    seed: Long,
    sampleIndex: Int,
    shrunkValues: Map[String, String],
    generatorLabels: Map[String, String],
    timestamp: String
  )

  /**
   * The production on-disk location, used whenever no `withBaseDir` override is
   * in effect.
   */
  private[property] val defaultBaseDir: Path = Paths.get(".zio-bdd", "failures")

  // The active base directory for per-scenario failure records. A FiberRef (not a constant)
  // so a caller can redirect it for the duration of an effect via `withBaseDir`, scoped to
  // its own fiber and children — concurrent scenarios never observe one another's override.
  // Nothing overrides it in normal runs, so the on-disk layout is unchanged; tests use it to
  // isolate the store and stop racing on a single shared directory — see #138.
  private val baseDir: FiberRef[Path] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(defaultBaseDir))

  /**
   * Run `zio` with the failure store redirected to `dir` (for this fiber and
   * its children).
   */
  def withBaseDir[R, E, A](dir: Path)(zio: => ZIO[R, E, A]): ZIO[R, E, A] =
    baseDir.locally(dir)(zio)

  private[property] def slug(scenario: Scenario): String =
    val feature = scenario.file.getOrElse("unknown").replaceAll(""".*[/\\]""", "").stripSuffix(".feature")
    val name    = scenario.name
    (feature + "--" + name).toLowerCase.replaceAll("[^a-z0-9]+", "-").take(120)

  private[property] def bodyHash(scenario: Scenario): String =
    val content = scenario.steps.map(s => s"${s.stepType}:${s.pattern}").mkString("|")
    Integer.toHexString(content.hashCode)

  private def fileIn(base: Path, scenario: Scenario): File =
    base.resolve(slug(scenario) + ".json").toFile

  // ── Minimal JSON encode/decode (avoids adding a library dependency) ───────
  // We control the output format: keys are known identifiers, values are
  // strings or longs — no nesting beyond one level of object. Sampled column values are
  // arbitrary user-generated strings, though, so the nested-object scan below is
  // quote-aware (tracks whether it's inside a quoted string) rather than naively
  // stopping at the first '}' byte — a generated value containing a literal brace
  // must not be mistaken for the end of the object.

  private def jsonUnescape(s: String): String =
    s.replace("\\\"", "\"").replace("\\\\", "\\")

  /**
   * Find the index of the `}` that closes the object opened at `openIdx` (which
   * must point at that object's `{`), skipping over any `}` that appears inside
   * a quoted string value. Returns -1 if unterminated.
   */
  private def matchingBrace(s: String, openIdx: Int): Int = {
    var i        = openIdx + 1
    var inString = false
    var result   = -1
    while (result == -1 && i < s.length) {
      val c = s.charAt(i)
      if (inString) {
        if (c == '\\') i += 1 // skip the escaped character, whatever it is
        else if (c == '"') inString = false
      } else if (c == '"') inString = true
      else if (c == '}') result = i
      i += 1
    }
    result
  }

  private def encodeJson(rec: FailureRecord): String =
    def esc(s: String)              = s.replace("\\", "\\\\").replace("\"", "\\\"")
    def str(s: String)              = "\"" + esc(s) + "\""
    def kvStr(k: String, v: String) = str(k) + ":" + str(v)
    def kvLong(k: String, v: Long)  = str(k) + ":" + v.toString
    def kvInt(k: String, v: Int)    = str(k) + ":" + v.toString
    def obj(m: Map[String, String]) =
      "{" + m.map { case (k, v) => kvStr(k, v) }.mkString(",") + "}"
    List(
      kvStr("scenarioId", rec.scenarioId),
      kvStr("bodyHash", rec.bodyHash),
      kvLong("seed", rec.seed),
      kvInt("sampleIndex", rec.sampleIndex),
      "\"shrunkValues\":" + obj(rec.shrunkValues),
      "\"generatorLabels\":" + obj(rec.generatorLabels),
      kvStr("timestamp", rec.timestamp)
    ).mkString("{", ",", "}")

  private def decodeJson(json: String): Option[FailureRecord] = {
    // Extract a "key":"value" pair. key is a plain identifier, value may contain \" escapes.
    def strVal(key: String): Option[String] = {
      val keyPart   = "\"" + key + "\""            // literal key with quotes
      val valuePart = "\"((?:[^\"\\\\]|\\\\.)*)\"" // captured value
      val sep       = "\\s*:\\s*"
      (keyPart + sep + valuePart).r
        .findFirstMatchIn(json)
        .map(m => jsonUnescape(m.group(1)))
    }
    def longVal(key: String): Option[Long] = {
      val keyPart = "\"" + key + "\""
      (keyPart + "\\s*:\\s*(-?\\d+)").r
        .findFirstMatchIn(json)
        .flatMap(m => m.group(1).toLongOption)
    }
    def intVal(key: String): Option[Int] = {
      val keyPart = "\"" + key + "\""
      (keyPart + "\\s*:\\s*(-?\\d+)").r
        .findFirstMatchIn(json)
        .flatMap(m => m.group(1).toIntOption)
    }
    def objVal(key: String): Map[String, String] = {
      val keyPart = "\"" + key + "\""
      val openPat = (keyPart + "\\s*:\\s*\\{").r
      val pairPat = "\"([^\"]+)\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\""
      openPat
        .findFirstMatchIn(json)
        .flatMap { m =>
          val openIdx  = json.indexOf('{', m.start)
          val closeIdx = matchingBrace(json, openIdx)
          if (closeIdx < 0) None
          else {
            val body = json.substring(openIdx + 1, closeIdx)
            Some(
              pairPat.r
                .findAllMatchIn(body)
                .map(pm => pm.group(1) -> jsonUnescape(pm.group(2)))
                .toMap
            )
          }
        }
        .getOrElse(Map.empty)
    }

    try {
      for {
        scenarioId  <- strVal("scenarioId")
        bodyHash    <- strVal("bodyHash")
        seed        <- longVal("seed")
        sampleIndex <- intVal("sampleIndex")
        timestamp   <- strVal("timestamp")
      } yield FailureRecord(
        scenarioId = scenarioId,
        bodyHash = bodyHash,
        seed = seed,
        sampleIndex = sampleIndex,
        shrunkValues = objVal("shrunkValues"),
        generatorLabels = objVal("generatorLabels"),
        timestamp = timestamp
      )
    } catch {
      case _: Exception => None
    }
  }

  // The three outcomes of reading a record's file, so a present-but-broken record is
  // distinguished from a genuinely absent one rather than both collapsing to None.
  private enum ReadOutcome:
    case Absent
    case Parsed(rec: FailureRecord)
    case Corrupt(reason: String)

  def read(scenario: Scenario): UIO[Option[FailureRecord]] =
    import ReadOutcome.*
    baseDir.get.flatMap { base =>
      ZIO.attemptBlocking {
        val f = fileIn(base, scenario)
        if (!f.exists()) Absent
        else {
          val json = scala.util.Using.resource(scala.io.Source.fromFile(f))(_.mkString)
          decodeJson(json) match {
            case Some(rec) => Parsed(rec)
            case None      => Corrupt(s"could not parse ${f.getName}")
          }
        }
      }
        // Only I/O failures are "corrupt record" — a defect anywhere else (parser bug, etc.)
        // must die, not be relabelled and swallowed as a warning.
        .refineToOrDie[IOException]
        .catchAll(e => ZIO.succeed(Corrupt(Option(e.getMessage).getOrElse(e.toString))))
        .flatMap {
          case Absent          => ZIO.none
          case Corrupt(reason) =>
            // A record exists but can't be read/parsed (interrupted write, hand-edit, disk
            // issue). Replay is best-effort so we still fall back to a fresh run — but surface
            // it, so a broken store isn't silently mistaken for "no failure recorded". See #140.
            ZIO.logWarning(s"Unreadable property-failure record for '${scenario.name}': $reason").as(None)
          case Parsed(rec) if rec.bodyHash == bodyHash(scenario) => ZIO.some(rec)
          case Parsed(_)                                         =>
            // The scenario's step list changed since this failure was recorded — the stored
            // counterexample no longer corresponds to the current scenario body. Discard it
            // rather than replaying a test that no longer exists in this form.
            ZIO.logWarning(
              s"Discarding stale property-failure record for '${scenario.name}' — scenario body changed since it was recorded."
            ) *> clear(scenario).as(None)
        }
    }

  def write(
    scenario: Scenario,
    seed: Long,
    sampleIndex: Int,
    shrunkValues: Map[String, String],
    generatorLabels: Map[String, String]
  ): UIO[Unit] =
    baseDir.get.flatMap { base =>
      ZIO.attemptBlocking {
        Files.createDirectories(base)
        val rec = FailureRecord(
          scenarioId = slug(scenario),
          bodyHash = bodyHash(scenario),
          seed = seed,
          sampleIndex = sampleIndex,
          shrunkValues = shrunkValues,
          generatorLabels = generatorLabels,
          timestamp = Instant.now().toString
        )
        scala.util.Using.resource(new PrintWriter(fileIn(base, scenario)))(_.write(encodeJson(rec)))
      }.orDie
    }

  def clear(scenario: Scenario): UIO[Unit] =
    baseDir.get.flatMap(base => ZIO.attemptBlocking { val _ = fileIn(base, scenario).delete() }.orDie)
