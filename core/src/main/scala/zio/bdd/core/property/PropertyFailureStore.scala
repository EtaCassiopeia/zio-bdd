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
  // strings or longs — no nesting beyond one level of object.

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
        .map(m => m.group(1).replace("\\\"", "\"").replace("\\\\", "\\"))
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
      val objBody = (keyPart + "\\s*:\\s*\\{([^}]*)\\}").r
      objBody
        .findFirstMatchIn(json)
        .map { m =>
          val body    = m.group(1)
          val pairPat = "\"([^\"]+)\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\""
          pairPat.r
            .findAllMatchIn(body)
            .map { pm =>
              pm.group(1) -> pm.group(2).replace("\\\"", "\"").replace("\\\\", "\\")
            }
            .toMap
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
    ZIO.attempt {
      val f = file(scenario)
      if (!f.exists()) None
      else {
        val json = scala.io.Source.fromFile(f).mkString
        decodeJson(json).flatMap { rec =>
          if (rec.bodyHash == bodyHash(scenario)) Some(rec) else None
        }
      }
    }.orElse(ZIO.none)

  def write(
    scenario: Scenario,
    seed: Long,
    sampleIndex: Int,
    shrunkValues: Map[String, String],
    generatorLabels: Map[String, String]
  ): UIO[Unit] =
    ZIO.attempt {
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
      val w = new PrintWriter(file(scenario))
      try w.write(encodeJson(rec))
      finally w.close()
    }.orDie

  def clear(scenario: Scenario): UIO[Unit] =
    ZIO.attempt { val _ = file(scenario).delete() }.orDie
