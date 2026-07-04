package zio.bdd.mock.steps

import zio.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.mock.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*

final case class AllMocksState()
object AllMocksState:
  given Schema[AllMocksState] = DeriveSchema.gen[AllMocksState]

// Real zio-bdd suites are top-level objects: `object MySuite extends ZIOSteps...`.
// Modelled the same here so the reflection test walks the same path tooling does
// (module `MODULE$`), and so no live backend is ever provisioned.
object CatalogSuite extends ZIOSteps[MockControl, AllMocksState] with MockSteps[MockControl, AllMocksState]:
  // No live backend: static discovery must not touch it.
  override def environment: ZLayer[Any, Throwable, MockControl] =
    ZLayer.fail(new RuntimeException("no backend during static discovery"))

  override def mockCatalog: Map[String, MockSource] = Map(
    "payments"  -> MockSource.Resource("wire/payments.json"),
    "inventory" -> MockSource.Dsl(MockSpec(Nil)),
    "ledger"    -> MockSource.Json("{}"),
    "orders"    -> MockSource.File("orders.json"),
    "catalog"   -> MockSource.Dir("mocks/")
  )

// A suite that mixes MockSteps but declares no catalog (backward-compat).
object EmptyCatalogSuite extends ZIOSteps[MockControl, AllMocksState] with MockSteps[MockControl, AllMocksState]:
  override def environment: ZLayer[Any, Throwable, MockControl] =
    ZLayer.fail(new RuntimeException("no backend"))

/**
 * Gate for #204: the static-discovery contract of `allMocks`.
 *
 * Mirrors the `allDefinitions` tooling contract — the zio-bdd StepLoader
 * reflects a suite instance and reads the catalog WITHOUT provisioning any mock
 * backend. These suites never provide a `MockControl`; `environment`
 * deliberately fails so a regression that touches the backend during discovery
 * would surface.
 */
object AllMocksSpec extends ZIOSpecDefault:
  def spec = suite("allMocks — static catalog discovery")(
    test("AC1: exposes name + sourceKind for each catalog entry, no backend needed") {
      val mocks = CatalogSuite.allMocks
      assertTrue(
        // stable, name-sorted order so tooling completion is deterministic
        mocks.map(_.name) == List("catalog", "inventory", "ledger", "orders", "payments"),
        mocks.forall(_.sourceKind.nonEmpty)
      )
    },
    test("AC2: sourceKind reflects the MockSource variant") {
      val byName = CatalogSuite.allMocks.map(m => m.name -> m.sourceKind).toMap
      assertTrue(
        byName("payments") == "Resource",
        byName("inventory") == "Dsl",
        byName("ledger") == "Json",
        byName("orders") == "File",
        byName("catalog") == "Dir"
      )
    },
    test("AC3: reflectively callable on a backend-free instance; name/sourceKind readable via reflection") {
      // The StepLoader path: resolve the suite module, invoke `allMocks`, read fields.
      val module = Class.forName("zio.bdd.mock.steps.CatalogSuite$").getField("MODULE$").get(null)
      val result = module.getClass.getMethod("allMocks").invoke(module).asInstanceOf[List[?]]
      val entries = result.map { m =>
        val name       = m.getClass.getMethod("name").invoke(m).asInstanceOf[String]
        val sourceKind = m.getClass.getMethod("sourceKind").invoke(m).asInstanceOf[String]
        name -> sourceKind
      }.toMap
      assertTrue(
        result.size == 5,
        entries("payments") == "Resource",
        entries("inventory") == "Dsl"
      )
    },
    test("AC4: default (no override) exposes an empty catalog") {
      assertTrue(EmptyCatalogSuite.allMocks.isEmpty)
    }
  )
