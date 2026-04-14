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
  def generateStepMethods(maxArity: Int = 8): String = {
    val stepTypes = List(
      ("Given", "StepType.GivenStep"),
      ("When", "StepType.WhenStep"),
      ("Then", "StepType.ThenStep"),
      ("But", "StepType.ButStep"),
      ("And", "StepType.AndStep")
    )

    val typeParamLetters = ('A' to 'Z').map(_.toString)
    val stepEffect = "RIO[R & State[S] & Scope, Unit]"

    val standard = stepTypes.flatMap { case (methodName, stepType) =>
      (0 to maxArity).map { i =>
        if (i == 0) {
          s"""  def $methodName(stepExpr: StepExpression[EmptyTuple])(f: => $stepEffect): Unit = {
             |    val adaptedF: EmptyTuple => $stepEffect = _ => f
             |    self.register(StepDefImpl[R, S, EmptyTuple]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else if (i == 1) {
          s"""  def $methodName[A](stepExpr: StepExpression[Tuple1[A]])(f: A => $stepEffect): Unit = {
             |    val adaptedF: Tuple1[A] => $stepEffect = tuple => f(tuple._1)
             |    self.register(StepDefImpl[R, S, Tuple1[A]]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else {
          val typeParams  = typeParamLetters.take(i).mkString(", ")
          val inputType   = s"((${typeParamLetters.take(i).mkString(", ")}))"
          val fnParams    = s"(${typeParamLetters.take(i).mkString(", ")})"
          val caseVars    = (1 to i).map(n => s"v$n").mkString(", ")
          val caseArgs    = (1 to i).map(n => s"v$n").mkString(", ")
          val adaptedFn   = s"{ case ($caseVars) => f($caseArgs) }"

          s"""  def $methodName[$typeParams](stepExpr: StepExpression[$inputType])(f: $fnParams => $stepEffect): Unit = {
             |    val adaptedF: $inputType => $stepEffect = $adaptedFn
             |    self.register(StepDefImpl[R, S, $inputType]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        }
      }
    }

    // State-injecting variants: GivenS/WhenS/ThenS/AndS/ButS
    // f receives S as first curried parameter, then the Gherkin-extracted params.
    val withState = stepTypes.flatMap { case (methodName, stepType) =>
      val sName = methodName + "S"
      (0 to maxArity).map { i =>
        if (i == 0) {
          s"""  def $sName(stepExpr: StepExpression[EmptyTuple])(f: S => $stepEffect): Unit = {
             |    val adaptedF: EmptyTuple => $stepEffect = _ => State.get[S].flatMap(s => f(s))
             |    self.register(StepDefImpl[R, S, EmptyTuple]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else if (i == 1) {
          s"""  def $sName[A](stepExpr: StepExpression[Tuple1[A]])(f: S => A => $stepEffect): Unit = {
             |    val adaptedF: Tuple1[A] => $stepEffect = tuple => State.get[S].flatMap(s => f(s)(tuple._1))
             |    self.register(StepDefImpl[R, S, Tuple1[A]]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        } else {
          val typeParams = typeParamLetters.take(i).mkString(", ")
          val inputType  = s"((${typeParamLetters.take(i).mkString(", ")}))"
          val fnParams   = s"(${typeParamLetters.take(i).mkString(", ")})"
          val caseVars   = (1 to i).map(n => s"v$n").mkString(", ")
          val caseArgs   = (1 to i).map(n => s"v$n").mkString(", ")

          s"""  def $sName[$typeParams](stepExpr: StepExpression[$inputType])(f: S => $fnParams => $stepEffect): Unit = {
             |    val adaptedF: $inputType => $stepEffect = { case ($caseVars) => State.get[S].flatMap(s => f(s)($caseArgs)) }
             |    self.register(StepDefImpl[R, S, $inputType]($stepType, stepExpr, adaptedF))
             |  }
             |""".stripMargin
        }
      }
    }

    (standard).mkString("\n")
  }
}
