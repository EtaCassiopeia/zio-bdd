package zio.bdd.gherkin

import zio.test.*

/**
 * Gate for issue #290: the deferred property-tag settings (`shrink`,
 * `maxShrinks`, `verbose`) are parsed but not yet functional. Setting one must
 * be surfaced via `PropertyConfig.deferredWarning` (and warned at run time)
 * rather than silently ignored. Setting only functional keys must stay silent.
 */
object PropertyInertTagSpec extends ZIOSpecDefault {

  def spec = suite("PropertyInertTagSpec")(
    test("explicitly set deferred keys are recorded as inert") {
      val cfg = PropertyTag.parse("property(shrink=false, maxShrinks=5, verbose=true)").get
      assertTrue(cfg.inertKeys == Set("shrink", "maxShrinks", "verbose"))
    },
    test("a single deferred key is recorded (case-insensitive)") {
      val cfg = PropertyTag.parse("property(MAXSHRINKS=10)").get
      assertTrue(cfg.inertKeys == Set("maxShrinks"))
    },
    test("only-functional keys produce no inert warning") {
      val cfg = PropertyTag.parse("property(samples=50, seed=7, replay=false)").get
      assertTrue(cfg.inertKeys.isEmpty, cfg.deferredWarning.isEmpty)
    },
    test("bare @property produces no inert warning") {
      val cfg = PropertyTag.parse("property").get
      assertTrue(cfg.inertKeys.isEmpty, cfg.deferredWarning.isEmpty)
    },
    test("deferredWarning names the set keys and explains they have no effect") {
      val cfg = PropertyTag.parse("property(verbose=true)").get
      assertTrue(
        cfg.deferredWarning.exists(_.contains("verbose")),
        cfg.deferredWarning.exists(_.contains("no effect"))
      )
    }
  )
}
