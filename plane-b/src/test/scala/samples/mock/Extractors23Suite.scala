package samples.mock

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.Suite
import zio.bdd.core.step.ZIOSteps
import zio.schema.{DeriveSchema, Schema}

import java.util.UUID

/**
 * Captured extractor outputs (UUID/BigDecimal stored as String for a simple
 * Schema).
 */
final case class ExtractState(
  word: String = "",
  rest: String = "",
  amount: String = "",
  uuid: String = "",
  seq: Long = 0L,
  status: String = "",
  condition: String = "",
  optPresent: Boolean = false,
  clause: String = ""
)
object ExtractState:
  given Schema[ExtractState] = DeriveSchema.gen[ExtractState]

/**
 * 23 — advanced step extractors (D2). A pure step-DSL sample (no backend): each
 * `When` drives a distinct extractor and stores what it captured; each `Then`
 * asserts the capture. Covers `word` (\S+) vs `rest` (.+), `bigDecimal` exact
 * precision, `uuid`, a max-bound `long`, `regex` alternation groups, `oneOf`
 * longest-first matching, and `optional` → `Option[String]` (present/absent).
 */
@Suite(
  featureDirs = Array("src/test/resources/features/mock/23-extractors-advanced.feature"),
  reporters = Array("pretty"),
  scenarioParallelism = 1
)
object Extractors23Suite extends ZIOSteps[Any, ExtractState]:

  When("I split " / word / " and the remaining tokens are " / rest) { (w: String, r: String) =>
    ScenarioContext.update(_.copy(word = w, rest = r))
  }

  When("I read the amount " / bigDecimal) { (bd: BigDecimal) =>
    ScenarioContext.update(_.copy(amount = bd.toString))
  }

  When("I read id " / uuid / " and seq " / long) { (id: UUID, seq: Long) =>
    ScenarioContext.update(_.copy(uuid = id.toString, seq = seq))
  }

  When("the account status is " / regex("(active|inactive)")) { (s: String) =>
    ScenarioContext.update(_.copy(status = s))
  }

  When("the account " / oneOf("is", "is not") / " provisioned") { (c: String) =>
    ScenarioContext.update(_.copy(condition = c))
  }

  When("a deploy runs" / optional(" with rollback")) { (o: Option[String]) =>
    ScenarioContext.update(_.copy(optPresent = o.isDefined, clause = o.getOrElse("")))
  }

  Then("the captured word is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.word, e, "word (\\S+)"))
  }

  Then("the captured rest is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.rest, e, "rest (.+ to EOL)"))
  }

  Then("the captured amount is exactly " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.amount, e, "bigDecimal exact precision"))
  }

  Then("the captured uuid is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.uuid, e, "uuid"))
  }

  Then("the captured seq is " / long) { (e: Long) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.seq, e, "long bound"))
  }

  Then("the captured status is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.status, e, "regex group"))
  }

  Then("the captured condition is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.condition, e, "oneOf longest-first"))
  }

  Then("the optional clause was present") {
    ScenarioContext.get.flatMap(s => Assertions.assertTrue(s.optPresent, "optional literal should be present"))
  }

  Then("the optional clause was absent") {
    ScenarioContext.get.flatMap(s => Assertions.assertTrue(!s.optPresent, "optional literal should be absent"))
  }

  Then("the captured clause is " / string) { (e: String) =>
    ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.clause, e, "optional Some value"))
  }

  override def environment: ZLayer[Any, Throwable, Any] = ZLayer.empty
