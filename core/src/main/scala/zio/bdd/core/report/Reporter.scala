package zio.bdd.core.report

import zio.ZIO
import zio.bdd.core.{FeatureResult, LogCollector}

trait Reporter {
  def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit]
}
