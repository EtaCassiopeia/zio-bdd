package zio.bdd.core

import zio.*
import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*

object OutputStackSpec extends ZIOSpecDefault {

  override def spec: Spec[Any, Nothing] =
    suite("OutputStack")(
      test("make creates an empty stack") {
        for {
          stackRef <- OutputStack.make
          isEmpty  <- OutputStack.isEmpty(stackRef)
        } yield assertTrue(isEmpty)
      },
      test("push adds a record to the top of the stack") {
        val record = StepRecord(StepType.GivenStep, "Given I have data", "data")
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record)
          top      <- OutputStack.peek(stackRef)
          isEmpty  <- OutputStack.isEmpty(stackRef)
        } yield assertTrue(top.contains(record), !isEmpty)
      },
      test("peek returns the top record without removing it") {
        val record1 = StepRecord(StepType.GivenStep, "Given I start", "first")
        val record2 = StepRecord(StepType.WhenStep, "When I act", "second")
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          peek1    <- OutputStack.peek(stackRef)
          peek2    <- OutputStack.peek(stackRef) // Peek again to ensure no change
        } yield assertTrue(peek1.contains(record2), peek2.contains(record2))
      },
      test("pop removes and returns the top record") {
        val record1 = StepRecord(StepType.GivenStep, "Given I start", "first")
        val record2 = StepRecord(StepType.WhenStep, "When I act", "second")
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record1)
          _        <- OutputStack.push(stackRef, record2)
          popped   <- OutputStack.pop(stackRef)
          topAfter <- OutputStack.peek(stackRef)
        } yield assertTrue(popped.contains(record2), topAfter.contains(record1))
      },
      test("pop returns None from an empty stack") {
        for {
          stackRef <- OutputStack.make
          popped   <- OutputStack.pop(stackRef)
        } yield assertTrue(popped.isEmpty)
      },
      test("clear empties the stack") {
        val record = StepRecord(StepType.GivenStep, "Given I have data", "data")
        for {
          stackRef <- OutputStack.make
          _        <- OutputStack.push(stackRef, record)
          _        <- OutputStack.clear(stackRef)
          isEmpty  <- OutputStack.isEmpty(stackRef)
        } yield assertTrue(isEmpty)
      },
      test("isEmpty returns true for empty stack and false for non-empty") {
        val record = StepRecord(StepType.GivenStep, "Given I have data", "data")
        for {
          stackRef <- OutputStack.make
          empty1   <- OutputStack.isEmpty(stackRef)
          _        <- OutputStack.push(stackRef, record)
          empty2   <- OutputStack.isEmpty(stackRef)
        } yield assertTrue(empty1, !empty2)
      },
      test("findNonUnitOutput returns the first non-unit output") {
        val records = Chunk(
          StepRecord(StepType.GivenStep, "Given I start", ()), // Unit output
          StepRecord(StepType.WhenStep, "When I act", "data"), // Non-unit output
          StepRecord(StepType.ThenStep, "Then I see", 42)      // Non-unit output
        )
        for {
          stackRef <- OutputStack.make
          _        <- stackRef.set(records)
          output   <- OutputStack.findNonUnitOutput(stackRef)
        } yield assertTrue(output.contains("data"))
      },
      test("findNonUnitOutput returns None when all outputs are unit") {
        val records = Chunk(
          StepRecord(StepType.GivenStep, "Given I start", ()),
          StepRecord(StepType.WhenStep, "When I act", ())
        )
        for {
          stackRef <- OutputStack.make
          _        <- stackRef.set(records)
          output   <- OutputStack.findNonUnitOutput(stackRef)
        } yield assertTrue(output.isEmpty)
      },
      test("findLastNonAndStepType returns the first non-And step type") {
        val records = Chunk(
          StepRecord(StepType.AndStep, "And I continue", "and1"),
          StepRecord(StepType.WhenStep, "When I act", "when"),
          StepRecord(StepType.AndStep, "And I finish", "and2"),
          StepRecord(StepType.GivenStep, "Given I start", "given")
        )
        for {
          stackRef <- OutputStack.make
          _        <- stackRef.set(records)
          stepType <- OutputStack.findLastNonAndStepType(stackRef)
        } yield assertTrue(stepType == StepType.WhenStep)
      },
      test("findLastNonAndStepType returns GivenStep when all are And") {
        val records = Chunk(
          StepRecord(StepType.AndStep, "And I continue", "and1"),
          StepRecord(StepType.AndStep, "And I finish", "and2")
        )
        for {
          stackRef <- OutputStack.make
          _        <- stackRef.set(records)
          stepType <- OutputStack.findLastNonAndStepType(stackRef)
        } yield assertTrue(stepType == StepType.GivenStep)
      },
      test("flattenOutput handles nested outputs correctly") {
        checkAll(
          Gen.fromIterable(
            List(
              ((), ()),     // Both unit
              ("data", ()), // One unit
              ((), 42),     // One unit
              ("data", 42), // No unit
              ()            // Single unit
            )
          )
        ) { input =>
          val result = OutputStack.flattenOutput(input)
          val expected = input match {
            case ((), ())     => ()
            case ("data", ()) => "data"
            case ((), 42)     => 42
            case ("data", 42) => ("data", 42)
            case ()           => ()
            case _            => input
          }
          assertTrue(result == expected)
        }
      }
    )
}
