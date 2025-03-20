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

  def peek(stackRef: Ref[Chunk[StepRecord]], scenarioId: String): UIO[Option[StepRecord]] =
    stackRef.get.map(_.find(_.scenarioId == scenarioId))

  // Removes and returns the top record
  def pop(stackRef: Ref[Chunk[StepRecord]]): UIO[Option[StepRecord]] =
    stackRef.modify { chunk =>
      if (chunk.isEmpty) (None, chunk)
      else (Some(chunk.head), chunk.tail)
    }

  def pop(stackRef: Ref[Chunk[StepRecord]], scenarioId: String): UIO[Option[StepRecord]] =
    stackRef.modify { chunk =>
      val (matching, remaining) = chunk.partition(_.scenarioId == scenarioId)
      (matching.headOption, remaining ++ matching.drop(1))
    }

  // Clears the stack
  def clear(stackRef: Ref[Chunk[StepRecord]]): UIO[Unit] =
    stackRef.set(Chunk.empty)

  // Checks if the stack is empty
  def isEmpty(stackRef: Ref[Chunk[StepRecord]]): UIO[Boolean] =
    stackRef.get.map(_.isEmpty)

  def isEmpty(stackRef: Ref[Chunk[StepRecord]], scenarioId: String): UIO[Boolean] =
    stackRef.get.map(_.forall(_.scenarioId != scenarioId))

  // Method to get and combine prior output for a scenario
  def getPriorOutput[I](
    stackRef: Ref[Chunk[StepRecord]],
    scenarioId: String,
    params: List[Any]
  )(implicit iTag: Tag[I]): ZIO[Any, Throwable, I] =
    peek(stackRef, scenarioId).flatMap { maybeRecord =>
      val priorOutput = maybeRecord.map(_.output).getOrElse(())
      combineTyped(priorOutput, params)
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
