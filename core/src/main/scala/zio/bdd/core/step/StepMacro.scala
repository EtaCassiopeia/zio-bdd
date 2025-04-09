package zio.bdd.core.step

import scala.quoted.*
import zio.*
import izumi.reflect.Tag

object StepMacro {
  // Public inline method to define steps
  inline def stepDef[R, S, T](inline builder: StepPatternBuilder)(inline f: T => RIO[R with State[S], Unit]): Unit =
    ${ stepDefImpl[R, S, T]('builder, 'f) }

  // Private macro implementation
  private def stepDefImpl[R: Type, S: Type, T: Type](
                                                      builderExpr: Expr[StepPatternBuilder],
                                                      fExpr: Expr[T => RIO[R with State[S], Unit]]
                                                    )(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    // Extract builder components
    val regexExpr: Expr[String] = '{ $builderExpr.regexParts.mkString }
    val segmentsExpr: Expr[List[Either[String, TypedExtractor[_]]]] = '{ $builderExpr.segments }
    val extractorsExpr: Expr[List[TypedExtractor[_]]] = '{ $builderExpr.extractors }

    // Determine expected arity from T
    val expectedArity = Type.of[T] match {
      case '[Unit]           => 0
      case '[ (t1, t2) ]     => 2
      case '[ (t1, t2, t3) ] => 3
      case _                 => 1 // Single parameter if not a tuple or Unit
    }

    // Validate arity
    val actualArityExpr: Expr[Int] = '{ $extractorsExpr.length }
    val arityCheck: Expr[Unit] = '{
      val expected = ${Expr(expectedArity)}
      val actual = $actualArityExpr
      if (expected != actual) {
        throw new Exception(s"Expected $expected parameters but got $actual extractors")
      }
    }

    // Construct StepPattern and StepDefImpl based on T
    Type.of[T] match {
      case '[Unit] =>
        val patternExpr: Expr[StepPattern[Unit]] = '{ StepPattern0($regexExpr, $segmentsExpr) }
        '{
          $arityCheck
          ZIO.serviceWithZIO[StepRegistry[R, S]](_.register(
            StepDefImpl[R, S, Unit]($patternExpr, $fExpr.asInstanceOf[Unit => RIO[R with State[S], Unit]])
          )).unit
        }
      case '[a] =>
        val e1Expr: Expr[TypedExtractor[a]] = '{ $extractorsExpr.apply(0).asInstanceOf[TypedExtractor[a]] }
        val patternExpr: Expr[StepPattern[a]] = '{ StepPattern1[a]($regexExpr, $segmentsExpr, $e1Expr) }
        '{
          $arityCheck
          ZIO.serviceWithZIO[StepRegistry[R, S]](_.register(
            StepDefImpl[R, S, a]($patternExpr, $fExpr.asInstanceOf[a => RIO[R with State[S], Unit]])
          )).unit
        }
      case '[ (a, b) ] =>
        val e1Expr: Expr[TypedExtractor[a]] = '{ $extractorsExpr.apply(0).asInstanceOf[TypedExtractor[a]] }
        val e2Expr: Expr[TypedExtractor[b]] = '{ $extractorsExpr.apply(1).asInstanceOf[TypedExtractor[b]] }
        val patternExpr: Expr[StepPattern[(a, b)]] = '{ StepPattern2[a, b]($regexExpr, $segmentsExpr, $e1Expr, $e2Expr) }
        '{
          $arityCheck
          ZIO.serviceWithZIO[StepRegistry[R, S]](_.register(
            StepDefImpl[R, S, (a, b)]($patternExpr, $fExpr.asInstanceOf[((a, b)) => RIO[R with State[S], Unit]])
          )).unit
        }
      case '[ (a, b, c) ] =>
        val e1Expr: Expr[TypedExtractor[a]] = '{ $extractorsExpr.apply(0).asInstanceOf[TypedExtractor[a]] }
        val e2Expr: Expr[TypedExtractor[b]] = '{ $extractorsExpr.apply(1).asInstanceOf[TypedExtractor[b]] }
        val e3Expr: Expr[TypedExtractor[c]] = '{ $extractorsExpr.apply(2).asInstanceOf[TypedExtractor[c]] }
        val patternExpr: Expr[StepPattern[(a, b, c)]] = '{ StepPattern3[a, b, c]($regexExpr, $segmentsExpr, $e1Expr, $e2Expr, $e3Expr) }
        '{
          $arityCheck
          ZIO.serviceWithZIO[StepRegistry[R, S]](_.register(
            StepDefImpl[R, S, (a, b, c)]($patternExpr, $fExpr.asInstanceOf[((a, b, c)) => RIO[R with State[S], Unit]])
          )).unit
        }
      case _ =>
        report.errorAndAbort(s"Unsupported type for step function: ${Type.show[T]}")
    }
  }
}
