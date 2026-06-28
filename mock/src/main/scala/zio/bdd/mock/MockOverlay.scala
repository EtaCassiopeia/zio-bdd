package zio.bdd.mock

import zio.*

/**
 * Overlay and mutation helpers over the [[MockControl]] core port (#116).
 *
 *   - [[scoped]] — a revertible overlay: each rule is added at
 *     [[Priority.Overlay]] (highest priority, shadowing the base rules) on
 *     acquire and removed on release, so an overlay applied in a hook or `When`
 *     step reverts exactly when the scope closes.
 *   - [[remove]] / [[replaceAll]] — targeted and wholesale mutation for steps
 *     that change a live space ("now everything times out").
 *
 * Rule identity is the adapter's responsibility: an adapter that addresses
 * stubs positionally (Rift, until EtaCassiopeia/rift#202) maps the [[RuleId]]
 * to a live index; these helpers only thread the ids that `addRule` returned
 * back to `removeRule`.
 */
object MockOverlay:

  /**
   * Apply `rules` as a scoped overlay. Each rule registers its own finalizer,
   * so a failure partway through still removes the rules already added; the
   * scope removes ONLY the rules it added. A teardown failure is logged and
   * skipped so one bad removal can't strand its siblings.
   */
  def scoped(space: MockSpace)(rules: MockRule*): ZIO[MockControl & Scope, MockError, List[RuleId]] =
    ZIO.serviceWithZIO[MockControl] { control =>
      ZIO.foreach(rules.toList) { rule =>
        ZIO.acquireRelease(control.addRule(space, rule, Priority.Overlay))(id =>
          control
            .removeRule(space, id)
            .catchAllCause(cause =>
              ZIO.logWarningCause(s"overlay teardown failed for rule ${id.value} on space ${space.id.value}", cause)
            )
        )
      }
    }

  /** Targeted mutation: remove a single rule by its [[RuleId]]. */
  def remove(space: MockSpace, id: RuleId): ZIO[MockControl, MockError, Unit] =
    ZIO.serviceWithZIO[MockControl](_.removeRule(space, id))

  /**
   * Wholesale mutation: replace every rule in `space` (e.g. a `When` step that
   * makes everything time out).
   */
  def replaceAll(space: MockSpace, rules: MockRule*): ZIO[MockControl, MockError, Unit] =
    ZIO.serviceWithZIO[MockControl](_.replaceRules(space, rules.toList))
