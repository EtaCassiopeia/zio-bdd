import sbt.*
import sbt.Keys.*

object StepGeneratorPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  lazy val generateStepMethods = taskKey[Seq[File]]("Generate step definition methods for ZIOSteps")

  object autoImport {
    lazy val stepMethodGeneratorSettings: Seq[Def.Setting[?]] = Seq(
      generateStepMethods := {
        val outputDir = (Compile / sourceManaged).value / "zio" / "bdd" / "core" / "step"
        val file      = outputDir / "GeneratedStepMethods.scala"

        IO.createDirectory(outputDir)

        val content = StepMethodGenerator.generateStepMethods()
        val fullContent =
          s"""package zio.bdd.core.step
             |
             |import zio.*
             |import zio.bdd.gherkin.StepType
             |
             |trait GeneratedStepMethods[R, S] { self: ZIOSteps[R, S] =>
             |
             |$content
             |}
             |""".stripMargin

        IO.write(file, fullContent)
        Seq(file)
      },
      Compile / sourceGenerators += generateStepMethods.taskValue
    )
  }

  override def projectSettings: Seq[Def.Setting[?]] = Seq.empty
}

object StepMethodGenerator {
  def generateStepMethods(maxArity: Int = 5): String = {
    val stepTypes = List(
      ("Given", "StepType.GivenStep"),
      ("When", "StepType.WhenStep"),
      ("Then", "StepType.ThenStep"),
      ("But", "StepType.ButStep"),
      ("And", "StepType.AndStep")
    )

    val typeParamLetters = ('A' to 'Z').map(_.toString)

    stepTypes.flatMap { case (methodName, stepType) =>
      (0 to maxArity).map { i =>
        if (i == 0) {
          s"""  def $methodName(stepExpr: StepExpression[EmptyTuple])(f: => RIO[R with State[S], Unit]): Unit = {
             |    val adaptedF: EmptyTuple => RIO[R with State[S], Unit] = _ => f
             |    self.register(StepDefImpl[R, S, EmptyTuple]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else if (i == 1) {
          s"""  def $methodName[A](stepExpr: StepExpression[Tuple1[A]])(f: A => RIO[R with State[S], Unit]): Unit = {
             |    val adaptedF: Tuple1[A] => RIO[R with State[S], Unit] = tuple => f(tuple._1)
             |    self.register(StepDefImpl[R, S, Tuple1[A]]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else {
          val typeParams = typeParamLetters.take(i).mkString(", ")
          val inputType = s"((${typeParamLetters.take(i).mkString(", ")}))"
          val fnParams = s"(${typeParamLetters.take(i).mkString(", ")})"
          val caseParams = typeParamLetters.take(i).map(letter => s"${letter.toLowerCase}").mkString(", ")
          val adaptedFn = s"{ case ($caseParams) => f($caseParams) }"

          s"""  def $methodName[$typeParams](stepExpr: StepExpression[$inputType])(f: $fnParams => RIO[R with State[S], Unit]): Unit = {
             |    val adaptedF: $inputType => RIO[R with State[S], Unit] = $adaptedFn
             |    self.register(StepDefImpl[R, S, $inputType]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        }
      }
    }.mkString("\n")
  }
}
