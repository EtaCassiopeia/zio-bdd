package zio.bdd.core

/**
 * A scenario-level aspect, applied either via a Gherkin tag or the code-side
 * `ZIOSteps.scenarioAspects` override.
 *
 *   - [[Retry]] / [[Flaky]]: run up to `n` attempts, passing on the first
 *     success.
 *   - [[NonFlaky]]: run up to `n` attempts, failing fast on the first failure
 *     (passes only if every attempt passes).
 *   - [[ExpectedFailure]]: the scenario is known to fail — its outcome is
 *     inverted (a failure passes; an unexpected pass fails). Takes precedence
 *     over the retry aspects, so the body runs once.
 */
enum ScenarioAspect:
  case Retry(n: Int)
  case Flaky(n: Int)
  case NonFlaky(n: Int)
  case ExpectedFailure

object ScenarioAspect:
  // Matches `retry(3)` / `flaky(5)` / `nonFlaky(2)`, case-insensitive on the name.
  private val pattern = """(?i)^(retry|flaky|nonflaky)\((\d+)\)$""".r

  /**
   * Parse a single tag (with or without a leading `@`). `n` is clamped to `>=
   * 1`. `@expectedFailure` / `@failing` map to [[ExpectedFailure]].
   */
  def fromTag(tag: String): Option[ScenarioAspect] =
    val t = tag.trim.stripPrefix("@")
    if (t.equalsIgnoreCase("expectedFailure") || t.equalsIgnoreCase("failing")) Some(ExpectedFailure)
    else
      pattern.findFirstMatchIn(t).flatMap { m =>
        m.group(2).toIntOption.map(_.max(1)).flatMap { n =>
          m.group(1).toLowerCase match
            case "retry"    => Some(Retry(n))
            case "flaky"    => Some(Flaky(n))
            case "nonflaky" => Some(NonFlaky(n))
            case _          => None
        }
      }

  /** The first retry aspect found among the scenario's tags, if any. */
  def fromTags(tags: List[String]): Option[ScenarioAspect] =
    tags.iterator.flatMap(fromTag).nextOption()
