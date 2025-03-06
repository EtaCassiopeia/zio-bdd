package zio.bdd.core

import zio.*
import zio.bdd.gherkin.StepType

// Manages a stack of step records, tracking outputs across step executions
object OutputStack {
  // Creates an empty stack
  def make: UIO[Ref[Chunk[StepRecord]]] = Ref.make(Chunk.empty[StepRecord])

  // Adds a new record to the top of the stack
  def push(stackRef: Ref[Chunk[StepRecord]], record: StepRecord): UIO[Unit] =
    stackRef.update(record +: _)

  // Returns the top record without removing it
  def peek(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[StepRecord]] =
    stackRef.get.map(_.headOption)

  // Removes and returns the top record
  def pop(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[StepRecord]] =
    stackRef.modify { chunk =>
      if (chunk.isEmpty) (None, chunk)
      else (Some(chunk.head), chunk.tail)
    }

  // Clears the stack
  def clear(stackRef: Ref[Chunk[StepRecord]]): UIO[Unit] =
    stackRef.set(Chunk.empty)

  // Checks if the stack is empty
  def isEmpty(stackRef: Ref[Chunk[StepRecord]]): UIO[Boolean] =
    stackRef.get.map(_.isEmpty)

  // Finds the first non-unit output in the stack
  def findNonUnitOutput(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[Any]] =
    stackRef.get.map { chunk =>
      chunk.find(record => flattenOutput(record.output) != ()).map(_.output)
    }

  // Finds the last non-"And" step type (used to determine expected type for "And" steps)
  def findLastNonAndStepType(stackRef: Ref[Chunk[StepRecord]]): UIO[StepType] =
    stackRef.get.map { chunk =>
      chunk.find(_.stepType != StepType.AndStep).map(_.stepType).getOrElse(StepType.GivenStep)
    }

  // Flattens nested outputs, collapsing unit values
  private[core] def flattenOutput(value: Any): Any = value match {
    case () => ()
    case (a, b) =>
      (flattenOutput(a), flattenOutput(b)) match {
        case ((), ()) => ()
        case ((), b)  => b
        case (a, ())  => a
        case (a, b)   => (a, b)
      }
    case other => other
  }
}
