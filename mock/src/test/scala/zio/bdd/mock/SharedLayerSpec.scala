package zio.bdd.mock

import zio.*
import zio.test.*

/**
 * Gate for issue #309: `SharedLayer.memoize` must build its resource exactly
 * once under concurrent access, hand every caller the same instance, and NOT
 * release it when a caller's scope closes (the shared engine outlives suites).
 */
object SharedLayerSpec extends ZIOSpecDefault:

  def spec = suite("SharedLayer.memoize (#309)")(
    test("builds once under concurrency, shares one instance, and survives caller-scope close") {
      for
        acquired <- Ref.make(0)
        released <- Ref.make(0)
        base      = ZLayer.scoped[Any](ZIO.acquireRelease(acquired.updateAndGet(_ + 1))(_ => released.update(_ + 1)))
        shared    = SharedLayer.memoize(base)
        // 20 concurrent, independent scoped builds — each closes its own caller scope.
        seen <- ZIO.foreachPar(1 to 20)(_ => ZIO.scoped(shared.build.map(_.get[Int])))
        acq  <- acquired.get
        rel  <- released.get
      yield assertTrue(
        acq == 1,                 // built exactly once despite 20 concurrent builds
        seen.distinct == List(1), // every caller received the SAME instance
        rel == 0                  // caller scopes closed, but the shared resource was NOT released
      )
    },
    test("a failed first build propagates and does not poison the cell — a later build succeeds") {
      for
        attempts <- Ref.make(0)
        base = ZLayer.scoped[Any](
                 ZIO.acquireRelease(
                   attempts
                     .updateAndGet(_ + 1)
                     .flatMap(n => if n == 1 then ZIO.fail(new RuntimeException("boom")) else ZIO.succeed(n))
                 )(_ => ZIO.unit)
               )
        shared  = SharedLayer.memoize(base)
        first  <- ZIO.scoped(shared.build.map(_.get[Int])).either // first build fails
        second <- ZIO.scoped(shared.build.map(_.get[Int])).either // cell still None → rebuilds & succeeds
        n      <- attempts.get
      yield assertTrue(first.isLeft, second == Right(2), n == 2)
    },
    test("a second, later build still returns the memoized instance without rebuilding") {
      for
        acquired <- Ref.make(0)
        base      = ZLayer.scoped[Any](ZIO.acquireRelease(acquired.updateAndGet(_ + 1))(_ => ZIO.unit))
        shared    = SharedLayer.memoize(base)
        a        <- ZIO.scoped(shared.build.map(_.get[Int]))
        b        <- ZIO.scoped(shared.build.map(_.get[Int])) // separate, sequential scope
        n        <- acquired.get
      yield assertTrue(a == 1, b == 1, n == 1)
    }
  )
