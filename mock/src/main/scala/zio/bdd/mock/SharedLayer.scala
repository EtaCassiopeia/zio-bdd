package zio.bdd.mock

import zio.*

/**
 * Turn a per-use scoped layer into a process-wide **memoized** one (#309).
 *
 * The underlying resource is built **at most once** — even under concurrent
 * access from independent runtimes / sbt `@Suite` tasks — every caller receives
 * the same instance, and the resource is **not** released when a caller's scope
 * closes: it is built on a long-lived scope that survives every consumer and is
 * finalized on JVM shutdown. This is the supported way to share one expensive
 * `MockControl` (e.g. an embedded engine) across suite classes, instead of
 * hand-rolling a `Runtime.default.unsafe.run(layer.build)` in a `static val`,
 * which deadlocks / interrupts under sbt's default `Test / parallelExecution`
 * because a suite's `<clinit>` runs on a shared-runtime worker thread while a
 * sibling suite blocks on the class-init lock.
 *
 * Safety: the only `unsafe.run` here creates the guard cell
 * (`Ref.Synchronized`) — a trivial, non-blocking allocation done once when the
 * memoized layer value is defined, so it cannot reproduce that class-init
 * deadlock. The resource build itself runs as an ordinary ZIO effect inside the
 * caller's runtime, serialized by the cell's semaphore.
 */
object SharedLayer:

  /**
   * A process-wide memoized view of `base`. Each returned `ZLayer` value owns
   * one memoization cell, so define it once (`val shared =
   * SharedLayer.memoize(layer)`) and reuse that value everywhere.
   */
  def memoize[R: EnvironmentTag, E, A: Tag](base: => ZLayer[R, E, A]): ZLayer[R, E, A] =
    val cell: Ref.Synchronized[Option[A]] =
      Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(Ref.Synchronized.make(Option.empty[A])).getOrThrow())

    ZLayer.scoped[R] {
      ZIO.environment[R].flatMap { env =>
        cell.modifyZIO {
          case current @ Some(a) => Exit.succeed((a, current))
          case None              =>
            // Build the resource on a long-lived scope (not the caller's), so it survives every
            // suite; that scope is closed on JVM shutdown — the only well-defined "all suites done"
            // point (a per-suite / runner `done()` release would couple the runner to the backend
            // and break sharing across concurrent suites).
            Scope.make.flatMap { longLived =>
              longLived
                .extend[R](base.build)
                .provideEnvironment(env)
                .map(_.get[A])
                .map { a =>
                  registerShutdown(longLived)
                  (a, Some(a))
                }
                // On a failed/interrupted build, close the long-lived scope so any partially
                // acquired resource is released; the cell stays `None`, so a later build retries.
                .onError(_ => longLived.close(Exit.unit))
            }
        }
      }
    }

  private def registerShutdown(scope: Scope.Closeable): Unit =
    java.lang.Runtime.getRuntime.addShutdownHook(
      new Thread(() => Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(scope.close(Exit.unit)).getOrThrow()))
    )
