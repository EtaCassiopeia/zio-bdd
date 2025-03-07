package zio.bdd.core

import scala.annotation.StaticAnnotation

@scala.annotation.meta.getter
case class ZIOBDDTest(featureDir: String) extends StaticAnnotation
