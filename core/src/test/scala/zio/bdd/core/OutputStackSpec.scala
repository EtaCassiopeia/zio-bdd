package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*

object OutputStackSpec extends ZIOSpecDefault {

  val testScenarioId1 = "test-scenario-1"
  val testScenarioId2 = "test-scenario-2"

  // Helper to create StepRecord with default outputTag
  private def makeStepRecord(stepType: StepType, stepText: String, output: Any, scenarioId: String): StepRecord =
    StepRecord(stepType, stepText, output, scenarioId, Tag[Any].tag)

  override def spec: Spec[Any, Nothing] =
    suite("OutputStack")(
      test("make creates an empty stack") {
        for {
          stackRef <- OutputStack.make
          isEmpty  <- OutputStack.isEmpty(stackRef, testScenarioId1)
        } yield assertTrue(isEmpty)
      },
      test("push adds a record to the top of the stack") {
        val record = makeStepRecord(StepType.GivenStep, "Given I have data", "data", testScenarioId1)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record)
          top      <- OutputStack.peek(stackRef, testScenarioId1)
          isEmpty  <- OutputStack.isEmpty(stackRef, testScenarioId1)
        } yield assertTrue(top.contains(record), !isEmpty)
      },
      test("peek returns the most recent record for the scenario without removing it") {
        val record1 = makeStepRecord(StepType.GivenStep, "Given I start", "first", testScenarioId1)
        val record2 = makeStepRecord(StepType.WhenStep, "When I act", "second", testScenarioId1)
        val record3 = makeStepRecord(StepType.ThenStep, "Then I check", "third", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          _        <- OutputStack.push(stackRef, record3)
          peek1    <- OutputStack.peek(stackRef, testScenarioId1)
          peek2    <- OutputStack.peek(stackRef, testScenarioId1)
          peek3    <- OutputStack.peek(stackRef, testScenarioId2)
        } yield assertTrue(peek1.contains(record2), peek2.contains(record2), peek3.contains(record3))
      },
      test("pop removes and returns the most recent record for the scenario") {
        val record1 = makeStepRecord(StepType.GivenStep, "Given I start", "first", testScenarioId1)
        val record2 = makeStepRecord(StepType.WhenStep, "When I act", "second", testScenarioId1)
        val record3 = makeStepRecord(StepType.ThenStep, "Then I check", "third", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          _        <- OutputStack.push(stackRef, record3)
          popped   <- OutputStack.pop(stackRef, testScenarioId1)
          topAfter <- OutputStack.peek(stackRef, testScenarioId1)
          topOther <- OutputStack.peek(stackRef, testScenarioId2)
        } yield assertTrue(popped.contains(record2), topAfter.contains(record1), topOther.contains(record3))
      },
      test("pop returns None when no records exist for the scenario") {
        val record = makeStepRecord(StepType.GivenStep, "Given I start", "first", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record)
          popped   <- OutputStack.pop(stackRef, testScenarioId1)
        } yield assertTrue(popped.isEmpty)
      },
      test("clear empties the entire stack") {
        val record1 = makeStepRecord(StepType.GivenStep, "Given I have data", "data", testScenarioId1)
        val record2 = makeStepRecord(StepType.WhenStep, "When I act", "second", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          _        <- OutputStack.clear(stackRef)
          isEmpty1 <- OutputStack.isEmpty(stackRef, testScenarioId1)
          isEmpty2 <- OutputStack.isEmpty(stackRef, testScenarioId2)
        } yield assertTrue(isEmpty1, isEmpty2)
      },
      test("isEmpty returns true for empty stack or no records for scenario") {
        val record = makeStepRecord(StepType.GivenStep, "Given I have data", "data", testScenarioId1)
        for {
          stackRef <- OutputStack.make
          empty1   <- OutputStack.isEmpty(stackRef, testScenarioId1)
          _        <- OutputStack.push(stackRef, record)
          empty2   <- OutputStack.isEmpty(stackRef, testScenarioId1)
          empty3   <- OutputStack.isEmpty(stackRef, testScenarioId2)
        } yield assertTrue(empty1, !empty2, empty3)
      },
      test("getPriorOutput combines prior output with params for the scenario") {
        val record1 = makeStepRecord(StepType.GivenStep, "Given I start", "first", testScenarioId1)
        val record2 = makeStepRecord(StepType.WhenStep, "When I act", "second", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          result   <- OutputStack.getPriorOutput[(String, String)](stackRef, testScenarioId1, List("extra")).orDie
        } yield assertTrue(result == ("first", "extra"))
      },
      test("getPriorOutput returns params when no prior output exists for scenario") {
        val record = makeStepRecord(StepType.GivenStep, "Given I start", "first", testScenarioId2)
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record)
          result   <- OutputStack.getPriorOutput[Int](stackRef, testScenarioId1, List(42)).orDie
        } yield assertTrue(result == 42)
      },
      test("combineTyped flattens previous output with new params") {
        val prev      = (1, "prev")
        val params    = List("new", 42)
        val resultZIO = OutputStack.combineTyped[(Int, String, String, Int)](prev, params).orDie
        assertZIO(resultZIO)(equalTo((1, "prev", "new", 42)))
      },
      test("combineTyped handles various input cases correctly") {
        checkAll(
          Gen.fromIterable(
            List(
              ((), Nil, ()),
              ("data", Nil, "data"),
              ((), List(42), 42),
              (("data", 42), Nil, ("data", 42)),
              (("data", 42), List("extra"), ("data", 42, "extra"))
            )
          )
        ) { case (prev, params, expected) =>
          val resultZIO = OutputStack.combineTyped[Any](prev, params).orDie
          assertZIO(resultZIO)(equalTo(expected))
        }
      }
    )
}
