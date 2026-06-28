package zio.bdd.mock.fixtures

import zio.*
import zio.bdd.gherkin.ScenarioMetadata
import zio.bdd.mock.{MockControl, MockError, MockSource, MockSpace}

/**
 * The mock spaces a fixture scope provisioned, exposed to the suite's steps.
 */
final case class MockFixture(spaces: List[MockSpace])

/**
 * Parser for the `@mock(name, ...)` Gherkin tag. Tags arrive without the `@`.
 */
object MockTag:
  private val pattern = """^mock\(([^)]*)\)$""".r

  /**
   * The catalog entry names in a single `@mock(...)` tag, or `None` if the tag
   * is not a `@mock(...)` tag at all. An empty `@mock()` yields `Some(Nil)`.
   */
  def parse(tag: String): Option[List[String]] =
    pattern.findFirstMatchIn(tag.trim.stripPrefix("@")).map { m =>
      m.group(1).split(",").map(_.trim).filter(_.nonEmpty).toList
    }

  /**
   * All `@mock(...)` names across the tags, in order,
   * first-occurrence-deduplicated.
   */
  def extract(tags: List[String]): List[String] =
    tags.flatMap(parse(_).getOrElse(Nil)).distinct

object MockFixtures:

  /**
   * Feature-tier fixtures: provision `sources` once (the three-tier model
   * memoizes this layer per feature). Heavy, read-only fixtures shared by all
   * of a feature's scenarios.
   */
  def feature(sources: MockSource*): ZLayer[MockControl, Throwable, MockFixture] =
    ZLayer.scoped(provisionScoped(sources.toList).mapError(asThrowable))

  /**
   * Scenario-tier fixtures: deploy the catalog entries named by the scenario's
   * `@mock(...)` tags. Fresh per scenario (share-nothing, auto-port via the
   * adapter); a name with no catalog entry fails the scenario at setup.
   */
  def scenario(meta: ScenarioMetadata, catalog: Map[String, MockSource]): ZLayer[MockControl, Throwable, MockFixture] =
    ZLayer.scoped {
      resolve(MockTag.extract(meta.tags), catalog).flatMap(provisionScoped).mapError(asThrowable)
    }

  // Provision each source as its own scoped acquisition, so a failure partway
  // through still tears down the spaces already created (each registers its own
  // finalizer in the scope). The scope destroys ONLY these spaces — never a
  // global teardown (the §5.9 invariant). A teardown that fails is logged and
  // skipped so one bad destroy can't strand its siblings.
  private def provisionScoped(sources: List[MockSource]): ZIO[MockControl & Scope, MockError, MockFixture] =
    ZIO.serviceWithZIO[MockControl] { control =>
      ZIO
        .foreach(sources) { source =>
          ZIO.acquireRelease(control.provision(source))(spaces =>
            ZIO.foreachDiscard(spaces)(space =>
              control
                .destroy(space)
                .catchAllCause(cause =>
                  ZIO.logWarningCause(s"mock fixture teardown failed for ${space.id.value}", cause)
                )
            )
          )
        }
        .map(perSource => MockFixture(perSource.flatten))
    }

  private def resolve(names: List[String], catalog: Map[String, MockSource]): IO[MockError, List[MockSource]] =
    ZIO.foreach(names) { name =>
      ZIO
        .fromOption(catalog.get(name))
        .orElseFail(MockError.InvalidDefinition(s"@mock($name): no catalog entry named '$name'"))
    }

  private def asThrowable(e: MockError): Throwable = new RuntimeException(s"MockFixtures: $e")
