package samples.mock.support

import zio.bdd.mock.*
import zio.bdd.mock.dsl.*

/**
 * Named mock sources deployed by scenario-tier `@mock(name)` fixtures
 * ([[zio.bdd.mock.fixtures.MockFixtures]]). A scenario tagged `@mock(greeting)`
 * gets the `greeting` space provisioned fresh and torn down in its fixture
 * scope.
 *
 * The catalog is deliberately **mixed-kind**: `greeting`/`farewell` are
 * portable `Dsl` sources, while `orders` is a raw-file `Resource` (a Rift
 * imposter doc) — so `@mock(orders)` exercises the file-backed catalog
 * resolution path, not just the DSL one (#64). A `Resource` entry is Rift-only,
 * so scenarios naming it are tagged `@rift`.
 */
object Catalog:
  val entries: Map[String, MockSource] = Map(
    "greeting" -> mock(get("/hello").respondWith(ok.text("hi"))).source,
    "farewell" -> mock(get("/bye").respondWith(ok.text("bye"))).source,
    "orders"   -> MockSource.Resource("mocksources/rift/orders.json")
  )
