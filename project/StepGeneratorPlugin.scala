//import sbt._
//import sbt.Keys._
//
//object StepGeneratorPlugin extends AutoPlugin {
//  override def trigger: PluginTrigger = allRequirements
//
//  lazy val generateStepMethods = taskKey[Seq[File]]("Generate step definition methods for ZIOSteps")
//
//  object autoImport {
//    lazy val stepMethodGeneratorSettings: Seq[Def.Setting[?]] = Seq(
//      generateStepMethods := {
//        val outputDir = (Compile / sourceManaged).value / "zio" / "bdd" / "core"
//        val file = outputDir / "GeneratedStepMethods.scala"
//
//        IO.createDirectory(outputDir)
//
//        val content = StepMethodGenerator.generateStepMethods()
//        val fullContent =
//          s"""package zio.bdd.core
//             |
//             |import izumi.reflect.Tag
//             |import zio.ZIO
//             |import zio.bdd.gherkin.StepType
//             |
//             |trait GeneratedStepMethods[R] { self: ZIOSteps[R] =>
//             |
//             |$content
//             |}
//             |""".stripMargin
//
//        IO.write(file, fullContent)
//        Seq(file)
//      },
//      Compile / sourceGenerators += generateStepMethods.taskValue
//    )
//  }
//
//  override def projectSettings: Seq[Def.Setting[?]] = Seq.empty
//}
//
//object StepMethodGenerator {
//  def generateStepMethods(maxArity: Int = 22): String = {
//    val stepTypes = List(
//      ("Given", "StepType.GivenStep"),
//      ("When", "StepType.WhenStep"),
//      ("Then", "StepType.ThenStep"),
//      ("And", "StepType.AndStep")
//    )
//
//    val methods = (0 to maxArity).flatMap { i =>
//      stepTypes.map { case (methodName, stepType) =>
//        val typeParams = if (i == 0) "" else (1 to i).map(j => s"A$j: Tag").mkString(", ")
//        val fnParams = if (i == 0) "()" else if (i == 1) "A1" else s"(${ (1 to i).map(j => s"A$j").mkString(", ") })"
//        val inputType = if (i == 0) "Unit" else if (i == 1) "A1" else s"((${ (1 to i).map(j => s"A$j").mkString(", ") }))"
//        val adaptedFn = if (i == 0) "(_: Unit) => fn()" else if (i == 1) "fn" else s"{ case (${ (1 to i).map(j => s"a$j").mkString(", ") }) => fn(${ (1 to i).map(j => s"a$j").mkString(", ") }) }"
//
//        s"""  inline def $methodName[${ if (i > 0) typeParams + ", " else "" }O: Tag](pattern: String)(
//           |      fn: $fnParams => ZIO[R, Throwable, O]
//           |  ): Unit = {
//           |    val adaptedFn: $inputType => ZIO[R, Throwable, O] = $adaptedFn
//           |    this.register[$inputType, O]($stepType, pattern, adaptedFn)
//           |  }
//           |""".stripMargin
//      }
//    }
//    methods.mkString("\n")
//  }
//}
