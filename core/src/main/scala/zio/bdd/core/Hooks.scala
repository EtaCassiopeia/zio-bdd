package zio.bdd.core

import zio.*

trait Hooks[R] {
  def beforeStep(scenarioId: String): ZIO[R, Throwable, Unit] = ZIO.unit

  def afterStep(scenarioId: String): ZIO[R, Throwable, Unit] = ZIO.unit

  def beforeScenario(scenarioId: String): ZIO[R, Throwable, Unit] = ZIO.unit

  def afterScenario(scenarioId: String): ZIO[R, Throwable, Unit] = ZIO.unit

  def beforeFeature: ZIO[R, Throwable, Unit] = ZIO.unit

  def afterFeature: ZIO[R, Throwable, Unit] = ZIO.unit
}
