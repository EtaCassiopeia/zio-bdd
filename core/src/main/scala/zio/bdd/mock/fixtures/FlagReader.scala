package zio.bdd.mock.fixtures

import zio.*
import zio.bdd.gherkin.ScenarioMetadata

/**
 * Reads the feature flag that selects which mock a scenario deploys (see
 * [[MockFixtures.byFlag]]). A narrow seam, deliberately free of any
 * feature-flag SDK: zio-openfeature already depends on zio-bdd (its conformance
 * suite), so zio-bdd must not depend back on it. An OpenFeature-backed reader
 * lives in the zio-openfeature project and plugs in here; zio-bdd ships
 * [[fromMetadata]].
 */
trait FlagReader:
  /** The flag's string value, or `None` if it is not set. */
  def getString(flag: String): UIO[Option[String]]

object FlagReader:
  /**
   * Reads the value carried by the scenario's `@flags(k=v)` matrix tag
   * (`meta.flagValues`) — the in-framework flag source, no SDK required.
   */
  def fromMetadata(meta: ScenarioMetadata): FlagReader = new FlagReader:
    def getString(flag: String): UIO[Option[String]] = ZIO.succeed(meta.flagValues.get(flag))
