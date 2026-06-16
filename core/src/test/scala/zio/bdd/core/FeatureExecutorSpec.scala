package zio.bdd.core

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.bdd.gherkin.*
import zio.bdd.core.step.{ZIOSteps, StepRegistry}
import zio.schema.{DeriveSchema, Schema}

/**
 * Tests for FeatureExecutor and ScenarioExecutor:
 *   - Happy-path feature execution
 *   - Background step injection
 *   - \@ignore / @skip semantics
 *   - Scenario Outline expansion
 *   - Data table steps
 *   - Step failure → SKIPPED propagation
 *   - PENDING status
 *   - Hooks (beforeAll/afterAll, beforeScenario with metadata, tagged hooks)
 *   - Duration tracking
 *   - Multiple features in one run
 */
object FeatureExecutorSpec extends ZIOSpecDefault {

  case class User(name: String, age: Int, role: String)
  case class SystemState(
    users: Map[String, User] = Map.empty,
    loggedInUser: Option[String] = None
  )

  given Schema[User]        = DeriveSchema.gen[User]
  given Schema[SystemState] = DeriveSchema.gen[SystemState]

  import zio.bdd.core.step.DefaultTypedExtractor

  // Steps are defined as a fresh class (never shared between tests) to avoid
  // step-registration side-effects bleeding across tests.
  class UserSteps extends ZIOSteps[Any, SystemState] with DefaultTypedExtractor {
    Given("a system is running") {
      ScenarioContext.update(_ => SystemState())
    }
    Given("a user named " / string / " with age " / int / " and role " / string) {
      (name: String, age: Int, role: String) =>
        ScenarioContext.update(s => s.copy(users = s.users + (name -> User(name, age, role))))
    }
    Given("the following users are created" / table[User]) { (users: List[User]) =>
      ScenarioContext.update(s => s.copy(users = s.users ++ users.map(u => u.name -> u).toMap))
    }
    When("the user " / string / " logs in") { (name: String) =>
      ScenarioContext.get.flatMap { s =>
        if (s.users.contains(name)) ScenarioContext.update(_.copy(loggedInUser = Some(name)))
        else ZIO.fail(new RuntimeException(s"User $name does not exist"))
      }
    }
    Then("the user " / string / " should exist") { (name: String) =>
      ScenarioContext.get.flatMap(s => Assertions.assertTrue(s.users.contains(name), s"$name not found"))
    }
    Then("the logged-in user should be " / string) { (expected: String) =>
      ScenarioContext.get.flatMap(s => Assertions.assertEquals(s.loggedInUser, Some(expected)))
    }
    Then("the user " / string / " should not have role " / string) { (name: String, role: String) =>
      ScenarioContext.get.flatMap { s =>
        val user = s.users.getOrElse(name, throw new RuntimeException(s"$name not found"))
        Assertions.assertTrue(user.role != role)
      }
    }
    Then("an error occurs") {
      ZIO.fail(new RuntimeException("Simulated error")).unit
    }
  }

  private val F = "test.feature"

  private def run(content: String)(using steps: ZIOSteps[Any, SystemState]) =
    GherkinParser.parseFeature(content, F).flatMap { feature =>
      FeatureExecutor.executeFeatures[Any, SystemState](List(feature), steps.getSteps, steps)
    }

  // ── Happy-path execution ───────────────────────────────────────────────────

  private val happyPath = suite("Happy-path execution")(
    test("single scenario with Given/Then passes") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: User Management
          |  Scenario: Create user
          |    Given a system is running
          |    Given a user named "Alice" with age 30 and role "admin"
          |    Then the user "Alice" should exist
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    },
    test("scenario with When/Then passes after login") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Auth
          |  Scenario: Login
          |    Given a system is running
          |    Given a user named "Alice" with age 30 and role "admin"
          |    When the user "Alice" logs in
          |    Then the logged-in user should be "Alice"
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    },
    test("multiple scenarios all pass") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Ops
          |  Scenario: Create
          |    Given a system is running
          |    Given a user named "Alice" with age 30 and role "admin"
          |    Then the user "Alice" should exist
          |  Scenario: Login
          |    Given a system is running
          |    Given a user named "Bob" with age 25 and role "user"
          |    When the user "Bob" logs in
          |    Then the logged-in user should be "Bob"
          |""".stripMargin
      ).map(r => assertTrue(r.head.scenarioResults.length == 2, r.head.scenarioResults.forall(_.isPassed)))
    }
  )

  private val backgroundExec = suite("Background execution")(
    test("background steps are prepended to each scenario's steps") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Auth
          |  Background:
          |    Given a system is running
          |    Given a user named "Alice" with age 30 and role "admin"
          |  Scenario: Login
          |    When the user "Alice" logs in
          |    Then the logged-in user should be "Alice"
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    },
    test("background with data table is processed correctly") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Setup
          |  Background:
          |    Given a system is running
          |  Scenario: Create from table
          |    Given the following users are created
          |      | name  | age | role  |
          |      | Alice | 30  | admin |
          |    Then the user "Alice" should exist
          |""".stripMargin
      ).map(r => assertTrue(r.head.isPassed))
    }
  )

  private val ignoreTag = suite("@ignore tag")(
    test("@ignore scenario is not executed and isIgnored returns true") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: F
          |  @ignore
          |  Scenario: Skipped
          |    Given a system is running
          |    Then an error occurs
          |""".stripMargin
      ).map(r => assertTrue(r.head.scenarioResults.head.isIgnored))
    }
  )

  private val outlineExec = suite("Scenario Outline execution")(
    test("outline with two example rows runs two scenarios") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Login Validation
          |  Scenario Outline: Validate login
          |    Given a system is running
          |    Given a user named <name> with age 25 and role "user"
          |    When the user <name> logs in
          |    Then the logged-in user should be <name>
          |  Examples:
          |    | name  |
          |    | Alice |
          |    | Bob   |
          |""".stripMargin
      ).map(r => assertTrue(r.head.scenarioResults.length == 2, r.head.scenarioResults.forall(_.isPassed)))
    }
  )

  private val failureAndSkipped = suite("Step failure and SKIPPED")(
    test("a failing step causes the feature result to be failed") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        """Feature: Error
          |  Scenario: Trigger error
          |    Given a system is running
          |    Then an error occurs
          |""".stripMargin
      ).map { r =>
        val fr = r.head
        assertTrue(!fr.isPassed, fr.error.isDefined, fr.error.exists(_.getMessage == "Simulated error"))
      }
    },
    test("remaining steps after a failure are marked SKIPPED") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      // The first Then fails; the subsequent steps should be SKIPPED
      run(
        """Feature: F
          |  Scenario: s
          |    Given a system is running
          |    Then an error occurs
          |    Then the user "nobody" should exist
          |    Then the logged-in user should be "nobody"
          |""".stripMargin
      ).map { r =>
        val results = r.head.scenarioResults.head.stepResults
        assertTrue(
          // step 0 (Given) passes, step 1 (Then error) fails,
          // steps 2 and 3 should be skipped
          results.length == 4,
          !results(1).isPassed,
          results(2).isSkipped,
          results(3).isSkipped
        )
      }
    }
  )

  private val pendingStatus = suite("PENDING status")(
    test("PendingException from a step body produces StepStatus.Pending") {
      val result = StepResult(
        Step(StepType.GivenStep, "not implemented"),
        Left(Cause.fail(new PendingException("todo")))
      )
      assertTrue(result.isPending, result.status.isInstanceOf[StepStatus.Pending])
    },
    test("a step with pending() helper marks that step PENDING") {
      case class S(v: Int = 0)
      given Schema[S] = DeriveSchema.gen[S]
      val pendingSteps = new ZIOSteps[Any, S] {
        Given("not implemented yet")(pending("implement me"))
      }
      val scenario = Scenario(
        "Pending test",
        Nil,
        List(Step(StepType.GivenStep, "not implemented yet")),
        Some(F),
        Some(1)
      )
      ScenarioExecutor
        .executeScenario[Any, S](scenario, pendingSteps)
        .provideSomeLayer[Any](StepRegistry.layer[Any, S](pendingSteps.getSteps))
        .map(r => assertTrue(r.stepResults.head.isPending))
    },
    test("ScenarioResult.hasPending is true when any step is pending") {
      val pendingStep = StepResult(
        Step(StepType.GivenStep, "pending step"),
        Left(Cause.fail(new PendingException("todo")))
      )
      val passedStep = StepResult(Step(StepType.ThenStep, "passing"), Right(()))
      val sc         = Scenario("s", Nil, Nil, Some(F), Some(1))
      val scResult   = ScenarioResult(sc, List(passedStep, pendingStep))
      assertTrue(scResult.hasPending)
    }
  )

  private val hooks = suite("Hooks")(
    test("beforeAll fires once before all features") {
      val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      val suiteSteps = new UserSteps {
        beforeAll(ZIO.succeed(counter.incrementAndGet()).unit)
      }
      for {
        f1 <- GherkinParser.parseFeature("Feature: F1\n  Scenario: s\n    Given a system is running\n", F)
        f2 <- GherkinParser.parseFeature("Feature: F2\n  Scenario: s\n    Given a system is running\n", F)
        _  <- FeatureExecutor.executeFeatures[Any, SystemState](List(f1, f2), suiteSteps.getSteps, suiteSteps)
      } yield assertTrue(counter.get() == 1)
    },
    test("afterAll fires once after all features") {
      val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      val suiteSteps = new UserSteps {
        afterAll(ZIO.succeed(counter.incrementAndGet()).unit)
      }
      for {
        f1 <- GherkinParser.parseFeature("Feature: F1\n  Scenario: s\n    Given a system is running\n", F)
        f2 <- GherkinParser.parseFeature("Feature: F2\n  Scenario: s\n    Given a system is running\n", F)
        _  <- FeatureExecutor.executeFeatures[Any, SystemState](List(f1, f2), suiteSteps.getSteps, suiteSteps)
      } yield assertTrue(counter.get() == 1)
    },
    test("beforeScenario receives ScenarioMetadata with the scenario name") {
      var capturedName = ""
      val suiteSteps = new UserSteps {
        beforeScenario(meta => ZIO.attempt { capturedName = meta.name }.orDie)
      }
      for {
        f <- GherkinParser.parseFeature(
               "Feature: F\n  Scenario: My Scenario\n    Given a system is running\n",
               F
             )
        _ <- FeatureExecutor.executeFeatures[Any, SystemState](List(f), suiteSteps.getSteps, suiteSteps)
      } yield assertTrue(capturedName == "My Scenario")
    },
    test("beforeScenarioTagged fires only for tagged scenarios") {
      val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      val suiteSteps = new UserSteps {
        beforeScenarioTagged("run-me")(_ => ZIO.succeed(counter.incrementAndGet()).unit)
      }
      for {
        f <- GherkinParser.parseFeature(
               """Feature: F
                 |  @run-me
                 |  Scenario: tagged
                 |    Given a system is running
                 |  Scenario: not tagged
                 |    Given a system is running
                 |""".stripMargin,
               F
             )
        _ <- FeatureExecutor.executeFeatures[Any, SystemState](List(f), suiteSteps.getSteps, suiteSteps)
      } yield assertTrue(counter.get() == 1)
    }
  )

  private val duration = suite("Duration tracking")(
    test("ScenarioResult.duration is non-negative after execution") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        "Feature: F\n  Scenario: s\n    Given a system is running\n"
      ).map(r => assertTrue(r.head.scenarioResults.head.duration >= 0L))
    },
    test("FeatureResult.duration is non-negative after execution") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      run(
        "Feature: F\n  Scenario: s\n    Given a system is running\n"
      ).map(r => assertTrue(r.head.duration >= 0L))
    }
  )

  private val resultStatus = suite("Result status correctness")(
    test("SkippedStepResult.isSkipped is true and isPassed is false") {
      val step    = Step(StepType.ThenStep, "skipped")
      val skipped = StepResult.skipped(step)
      assertTrue(skipped.isSkipped, !skipped.isPassed, skipped.error.isEmpty)
    },
    test("StepStatus.Skipped is distinct from Failed") {
      val skipped = StepStatus.Skipped
      val failed  = StepStatus.Failed(Cause.fail(new RuntimeException()))
      assertTrue(
        skipped != failed,
        skipped.isInstanceOf[StepStatus.Skipped.type]
      )
    },
    test("FeatureResult.isComplete is true when all scenarios are passed or pending") {
      val sc     = Scenario("s", Nil, Nil, Some(F), Some(1))
      val passed = ScenarioResult(sc, List(StepResult(Step(StepType.GivenStep, "g"), Right(()))))
      val pending = ScenarioResult(
        sc,
        List(
          StepResult(
            Step(StepType.GivenStep, "p"),
            Left(Cause.fail(new PendingException("todo")))
          )
        )
      )
      val fr = FeatureResult(Feature("F", Nil, List(sc, sc)), List(passed, pending))
      assertTrue(fr.isComplete)
    }
  )

  // ── Per-scenario layer override ────────────────────────────────────────────

  private val scenarioLayerOverride = suite("Per-scenario layer override")(
    test("scenarioLayer is called per-scenario and receives ScenarioMetadata") {
      val observed = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      case class TagEnv(tag: String)

      class TagSuite extends ZIOSteps[TagEnv, SystemState]:
        override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, TagEnv] = {
          observed.add(meta.name)
          ZLayer.succeed(TagEnv(meta.tags.headOption.getOrElse("none")))
        }
        Given("a system is running")(ScenarioContext.update(_ => SystemState()))
        Then("the environment tag is " / string) { (expected: String) =>
          ZIO.serviceWith[TagEnv](_.tag).flatMap(actual => Assertions.assertEquals(actual, expected))
        }

      val suite = new TagSuite {}
      for {
        f1 <- GherkinParser.parseFeature(
                """Feature: F
                  |  @smoke
                  |  Scenario: tagged
                  |    Given a system is running
                  |    Then the environment tag is smoke
                  |  Scenario: untagged
                  |    Given a system is running
                  |    Then the environment tag is none
                  |""".stripMargin,
                F
              )
        results <- suite.run(List(f1)).provide(suite.environment)
      } yield assertTrue(
        results.head.isPassed,
        observed.size() == 2,
        observed.contains("tagged"),
        observed.contains("untagged")
      )
    }
  )

  private val snapshotHelper = suite("withSnapshot helper")(
    test("withSnapshot captures state value before the inner block runs") {
      given steps: ZIOSteps[Any, SystemState] = new UserSteps {}
      // Add snapshot-using steps
      val snapshotSuite = new ZIOSteps[Any, SystemState] with step.DefaultTypedExtractor:
        Given("a system is running")(ScenarioContext.update(_ => SystemState()))
        Given("a user named " / string / " with age " / int / " and role " / string) {
          (name: String, age: Int, role: String) =>
            ScenarioContext.update(s => s.copy(users = s.users + (name -> User(name, age, role))))
        }
        Then("adding a user increases the user count") {
          withSnapshot(_.users.size) { countBefore =>
            for {
              _ <- ScenarioContext.update(s => s.copy(users = s.users + ("NewUser" -> User("NewUser", 0, "viewer"))))
              s <- ScenarioContext.get
              _ <- Assertions.assertTrue(s.users.size == countBefore + 1)
            } yield ()
          }
        }

      GherkinParser
        .parseFeature(
          """Feature: F
            |  Scenario: snapshot
            |    Given a system is running
            |    Given a user named "Alice" with age 30 and role "admin"
            |    Then adding a user increases the user count
            |""".stripMargin,
          F
        )
        .flatMap { f =>
          snapshotSuite.run(List(f))
        }
        .map(r => assertTrue(r.head.isPassed))
    }
  )

  def spec: Spec[TestEnvironment & Scope, Any] = suite("FeatureExecutor")(
    happyPath,
    backgroundExec,
    ignoreTag,
    outlineExec,
    failureAndSkipped,
    pendingStatus,
    hooks,
    duration,
    resultStatus,
    scenarioLayerOverride,
    snapshotHelper
  )
}
