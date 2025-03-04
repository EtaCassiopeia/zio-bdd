package zio.bdd.core

import scala.annotation.StaticAnnotation

case class Retry(count: Int)  extends StaticAnnotation
case class Flaky()            extends StaticAnnotation
case class Repeat(count: Int) extends StaticAnnotation

@scala.annotation.meta.getter
case class ZIOBDDTest(featureDir: String) extends StaticAnnotation
