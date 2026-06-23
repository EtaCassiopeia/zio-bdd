package zio.bdd.core.property

import zio.*
import zio.bdd.gherkin.Scenario

import java.io.{File, PrintWriter}
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

  private val dir: Path = Paths.get(".zio-bdd", "failures")

  private[property] def slug(scenario: Scenario): String =
    val feature = scenario.file.getOrElse("unknown").replaceAll(""".*[/\\]""", "").stripSuffix(".feature")
    val name    = scenario.name
    (feature + "--" + name).toLowerCase.replaceAll("[^a-z0-9]+", "-").take(120)

  private[property] def bodyHash(scenario: Scenario): String =
    val content = scenario.steps.map(s => s"${s.stepType}:${s.pattern}").mkString("|")
    Integer.toHexString(content.hashCode)

  private def file(scenario: Scenario): File =
    dir.resolve(slug(scenario) + ".json").toFile

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

  def read(scenario: Scenario): UIO[Option[FailureRecord]] =
    ZIO.attemptBlocking {
      val f = file(scenario)
      if (!f.exists()) None
      else {
        val json = scala.util.Using.resource(scala.io.Source.fromFile(f))(_.mkString)
        decodeJson(json)
      }
    }
      .orElse(ZIO.none)
      .flatMap {
        case Some(rec) if rec.bodyHash == bodyHash(scenario) => ZIO.some(rec)
        case Some(_)                                         =>
          // The scenario's step list changed since this failure was recorded — the stored
          // counterexample no longer corresponds to the current scenario body. Discard it
          // rather than replaying a test that no longer exists in this form.
          ZIO.logWarning(
            s"Discarding stale property-failure record for '${scenario.name}' — scenario body changed since it was recorded."
          ) *> clear(scenario).as(None)
        case None => ZIO.none
      }

  def write(
    scenario: Scenario,
    seed: Long,
    sampleIndex: Int,
    shrunkValues: Map[String, String],
    generatorLabels: Map[String, String]
  ): UIO[Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(dir)
      val rec = FailureRecord(
        scenarioId = slug(scenario),
        bodyHash = bodyHash(scenario),
        seed = seed,
        sampleIndex = sampleIndex,
        shrunkValues = shrunkValues,
        generatorLabels = generatorLabels,
        timestamp = Instant.now().toString
      )
      scala.util.Using.resource(new PrintWriter(file(scenario)))(_.write(encodeJson(rec)))
    }.orDie

  def clear(scenario: Scenario): UIO[Unit] =
    ZIO.attemptBlocking { val _ = file(scenario).delete() }.orDie
