package zio.bdd.core

import izumi.reflect.Tag
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
      test("combineTyped flattens previous output with new outputs") {
        val prev    = (1, "prev")
        val outputs = List("new", 42)
        val resultZIO =
          OutputStack.combineTyped(prev, outputs).orDie // Convert Throwable to Nothing
        assertZIO(resultZIO)(equalTo((1, "prev", "new", 42)))
      },
      test("combineTyped handles nested outputs correctly") {
        checkAll(
          Gen.fromIterable(
            List(
              ((), Nil, ()),
              ("data", Nil, "data"),
              ((), List(42), 42),
              (("data", 42), Nil, ("data", 42))
            )
          )
        ) { case (prev, params, expected) =>
          val resultZIO = OutputStack.combineTyped[Any](prev, params).orDie
          assertZIO(resultZIO)(equalTo(expected))
        }
      }
    )
}
