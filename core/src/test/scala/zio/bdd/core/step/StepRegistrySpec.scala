package zio.bdd.core.step

import zio.bdd.gherkin.StepType
import zio.test.*
import zio.test.Assertion.*
import zio.{RIO, UIO, ZIO, ZLayer}

object StepRegistrySpec extends ZIOSpecDefault with DefaultTypedExtractor {

  private val givenStep = StepDefImpl[Any, Unit, Tuple1[String]](
    StepType.GivenStep,
    StepExpression(List(Literal("user "), Extractor(string))),
    { case Tuple1(name) => ZIO.succeed(println(s"Given user: $name")) }
  )

  private val whenStep = StepDefImpl[Any, Unit, Tuple1[Int]](
    StepType.WhenStep,
    StepExpression(List(Literal("age is "), Extractor(int))),
    { case Tuple1(age) => ZIO.succeed(println(s"When age is: $age")) }
  )

  private val andStep = StepDefImpl[Any, Unit, Tuple1[String]](
    StepType.AndStep,
    StepExpression(List(Literal("role is "), Extractor(string))),
    { case Tuple1(role) => ZIO.succeed(println(s"And role is: $role")) }
  )

  private val steps = List(givenStep, whenStep, andStep)

  def createInput(text: String): StepInput = StepInput(text, None)

  def spec = suite("StepRegistry")(
    test("find and execute step with exact type match") {
      val registry = StepRegistryLive[Any, Unit](steps)
      val input    = createInput("user Alice")
      for {
        effect <- registry.findStep(StepType.GivenStep, input)
        _      <- effect
      } yield assertCompletes
    },
    test("fallback to And step when exact type not found") {
      val registry = StepRegistryLive[Any, Unit](steps)
      val input    = createInput("role is admin")
      for {
        effect <- registry.findStep(StepType.ThenStep, input) // No ThenStep, should fallback to AndStep
        _      <- effect
      } yield assertCompletes
    },
    test("fail when no matching step found") {
      val registry = StepRegistryLive[Any, Unit](steps)
      val input    = createInput("unknown step")
      val result   = registry.findStep(StepType.GivenStep, input)
      assertZIO(result.either)(isLeft(anything))
    },
    test("execute step with parameters") {
      val registry = StepRegistryLive[Any, Unit](steps)
      val input    = createInput("age is 30")
      for {
        effect <- registry.findStep(StepType.WhenStep, input)
        _      <- effect
      } yield assertCompletes
    }
  ).provide(ZLayer.succeed(new State[Unit] {
    override def get: UIO[Unit] = ZIO.unit

    override def update(f: Unit => Unit): UIO[Unit] = ZIO.unit
  }))
}
