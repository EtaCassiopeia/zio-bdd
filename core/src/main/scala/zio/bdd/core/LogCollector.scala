package zio.bdd.core

import zio.*
import java.time.Instant

trait LogCollector {
  def log(message: String): ZIO[Any, Nothing, Unit]
  def getLogs: ZIO[Any, Nothing, List[(String, Instant)]]
  def clearLogs: ZIO[Any, Nothing, Unit]
}

object LogCollector {
  val live: ZLayer[Any, Nothing, LogCollector] = ZLayer {
    for {
      ref <- Ref.make[List[(String, Instant)]](Nil)
    } yield new LogCollector {
      def log(message: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(Instant.now()).flatMap(now => ref.update(list => (message, now) :: list))
      def getLogs: ZIO[Any, Nothing, List[(String, Instant)]] = ref.get
      def clearLogs: ZIO[Any, Nothing, Unit]                  = ref.set(Nil)
    }
  }
}
