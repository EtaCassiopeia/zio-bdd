package zio.bdd.core

import izumi.reflect.Tag
import zio.*
import zio.bdd.gherkin.StepType

// A stack of step records, tracking outputs across step executions
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

  // Finds the last non-"And" step type (used to determine expected type for "And" steps)
  def findLastNonAndStepType(stackRef: Ref[Chunk[StepRecord]]): UIO[StepType] =
    stackRef.get.map { chunk =>
      chunk.find(_.stepType != StepType.AndStep).map(_.stepType).getOrElse(StepType.GivenStep)
    }

  // Type-safe combine previous output with step parameters into a single input value with runtime validation
  private[core] def combineTyped[I](prev: Any, params: List[Any])(implicit iTag: Tag[I]): ZIO[Any, Throwable, I] = {
    val combined = (prev, params) match {
      case ((), Nil)           => ()
      case ((), single :: Nil) => single
      case ((), multiple)      => Tuple.fromArray(multiple.toArray)
      case (prior, Nil)        => prior // Return prior output as-is when no params
      case (prior: Tuple, nonEmpty) =>
        Tuple.fromArray((prior.productIterator.toList.filter(_ != ()) ++ nonEmpty.filter(_ != ())).toArray)
      case (prior, nonEmpty) => Tuple.fromArray((prior :: nonEmpty.filter(_ != ())).toArray)
    }
    ZIO.attempt {
      val expectedTag = iTag.tag
      combined match {
        case i: I => i
        case _ =>
          val combinedStr = combined match {
            case tuple: Product => s"Tuple${tuple.productArity}(${tuple.productIterator.mkString(", ")})"
            case other          => other.toString
          }
          throw new Exception(s"Type mismatch: expected $expectedTag, got $combinedStr")
      }
    }
  }
}
