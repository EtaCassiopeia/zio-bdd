package zio.bdd.mock

import zio.*
import zio.test.*

object MockOverlaySpec extends ZIOSpecDefault:

  // A MockControl that models an ordered, per-space rule list. addRule(Overlay)
  // prepends (highest priority → shadows base); addRule(Base) appends; removeRule
  // deletes by id; replaceRules swaps the whole list. No global teardown exists,
  // so a scope can only ever touch the rules it added.
  private final case class RuleEntry(id: String, rule: MockRule)
  private final case class TState(nextId: Int, rules: Map[String, List[RuleEntry]])
  private object TState:
    val init: TState = TState(0, Map.empty)

  private final class TestControl(
    ref: Ref[TState],
    failAddRule: MockRule => Boolean = _ => false,
    failRemoveRule: String => Boolean = _ => false
  ) extends MockControl:
    def backendName: String           = "test"
    def capabilities: Set[Capability] = Set.empty

    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      if failAddRule(rule) then ZIO.fail(MockError.InvalidDefinition("add boom"))
      else
        ref.modify { s =>
          val id      = s"r${s.nextId}"
          val current = s.rules.getOrElse(space.id.value, Nil)
          val updated = priority match
            case Priority.Overlay => RuleEntry(id, rule) :: current
            case Priority.Base    => current :+ RuleEntry(id, rule)
          (RuleId(id), s.copy(nextId = s.nextId + 1, rules = s.rules.updated(space.id.value, updated)))
        }

    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
      if failRemoveRule(id.value) then ZIO.fail(MockError.CommunicationError("remove boom"))
      else
        ref.modify { s =>
          val current = s.rules.getOrElse(space.id.value, Nil)
          if current.exists(_.id == id.value) then
            (ZIO.unit, s.copy(rules = s.rules.updated(space.id.value, current.filterNot(_.id == id.value))))
          else (ZIO.fail(MockError.RuleNotFound(space.id, id)), s)
        }.flatten

    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] =
      ref.update { s =>
        val entries = rules.zipWithIndex.map { case (r, i) => RuleEntry(s"x${s.nextId + i}", r) }
        s.copy(nextId = s.nextId + rules.size, rules = s.rules.updated(space.id.value, entries))
      }

    def provision(source: MockSource): IO[MockError, List[MockSpace]] = ZIO.succeed(Nil)
    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.fail(MockError.InvalidDefinition("n/a"))
    def destroy(space: MockSpace): IO[MockError, Unit]                   = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]] = ZIO.succeed(Nil)
    def faults: IO[Unsupported, Faults]                                  = ZIO.fail(Unsupported(Capability.Faults, backendName))
    def scenarios: IO[Unsupported, StatefulScenarios]                    = ZIO.fail(Unsupported(Capability.StatefulScenarios, backendName))
    def stateInspection: IO[Unsupported, StateInspection] =
      ZIO.fail(Unsupported(Capability.StateInspection, backendName))
    def scripting: IO[Unsupported, Scripting]     = ZIO.fail(Unsupported(Capability.Scripting, backendName))
    def proxyRecord: IO[Unsupported, ProxyRecord] = ZIO.fail(Unsupported(Capability.ProxyRecord, backendName))
    def templating: IO[Unsupported, Templating]   = ZIO.fail(Unsupported(Capability.Templating, backendName))

  private def rulesOf(ref: Ref[TState], space: MockSpace): UIO[List[MockRule]] =
    ref.get.map(_.rules.getOrElse(space.id.value, Nil).map(_.rule))

  private def rule(path: String): MockRule =
    MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status = 200, body = Body.Text(path)))

  private val base    = rule("/base")
  private val overlay = rule("/overlay")
  private val spaceA  = MockSpace("http://a", identity, SpaceId("A"))
  private val spaceB  = MockSpace("http://b", identity, SpaceId("B"))

  def spec = suite("MockOverlay")(
    test("a scoped overlay shadows the base rule and reverts exactly on release (AC1)") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        _               <- ctl.addRule(spaceA, base, Priority.Base)
        during <- ZIO.scoped {
                    MockOverlay.scoped(spaceA)(overlay) *> rulesOf(ref, spaceA)
                  }.provideLayer(ZLayer.succeed(ctl))
        after <- rulesOf(ref, spaceA)
      yield assertTrue(
        during == List(overlay, base), // overlay prepended → shadows base, base still present
        after == List(base)            // reverted exactly to the base rule
      )
    },
    test("concurrent overlays on different spaces don't interfere (AC2)") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        _               <- ctl.addRule(spaceA, base, Priority.Base)
        _               <- ctl.addRule(spaceB, base, Priority.Base)
        res <- ZIO.scoped {
                 MockOverlay.scoped(spaceA)(overlay) *> ZIO.scoped {
                   MockOverlay.scoped(spaceB)(overlay) *> (rulesOf(ref, spaceA) <*> rulesOf(ref, spaceB))
                 }.flatMap(bothOpen => (rulesOf(ref, spaceA) <*> rulesOf(ref, spaceB)).map((bothOpen, _)))
               }.provideLayer(ZLayer.succeed(ctl))
      yield
        val (bothOpen, afterBClosed) = res
        val (aBoth, bBoth)           = bothOpen
        val (aAfter, bAfter)         = afterBClosed
        assertTrue(
          aBoth == List(overlay, base),  // A overlaid
          bBoth == List(overlay, base),  // B overlaid — neither leaked into the other
          aAfter == List(overlay, base), // closing B left A's overlay intact
          bAfter == List(base)           // only B reverted
        )
    },
    test("a scoped overlay adds all rules and removes all of them on release") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        _               <- ctl.addRule(spaceA, base, Priority.Base)
        during <- ZIO.scoped {
                    MockOverlay.scoped(spaceA)(rule("/o1"), rule("/o2")) *> rulesOf(ref, spaceA)
                  }.provideLayer(ZLayer.succeed(ctl))
        after <- rulesOf(ref, spaceA)
      yield assertTrue(
        during.size == 3,
        during.last == base,                                   // overlays sit above the base
        during.take(2).toSet == Set(rule("/o1"), rule("/o2")), // both overlays applied
        after == List(base)                                    // all overlays removed on release
      )
    },
    test("remove deletes one rule by id; replaceAll replaces every rule") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        id1             <- ctl.addRule(spaceA, rule("/x"), Priority.Base)
        _               <- ctl.addRule(spaceA, rule("/y"), Priority.Base)
        _               <- MockOverlay.remove(spaceA, id1).provideLayer(ZLayer.succeed(ctl))
        afterRemove     <- rulesOf(ref, spaceA)
        _               <- MockOverlay.replaceAll(spaceA, rule("/z")).provideLayer(ZLayer.succeed(ctl))
        afterReplace    <- rulesOf(ref, spaceA)
      yield assertTrue(
        afterRemove == List(rule("/y")), // only /x removed
        afterReplace == List(rule("/z")) // everything replaced
      )
    },
    test("a failed addRule mid-overlay reverts the rules already added") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref, failAddRule = _.`match`.path == PathMatch.Exact("/bad"))
        _               <- ctl.addRule(spaceA, base, Priority.Base)
        res <- ZIO
                 .scoped(MockOverlay.scoped(spaceA)(rule("/o1"), rule("/bad")))
                 .provideLayer(ZLayer.succeed(ctl))
                 .either
        after <- rulesOf(ref, spaceA)
      yield assertTrue(
        res.isLeft,         // the overlay failed
        after == List(base) // /o1 was rolled back — only the base rule remains
      )
    },
    test("a teardown failure for one overlay rule does not strand the others") {
      for
        ref <- Ref.make(TState.init)
        // The first overlay rule added gets id "r0"; make its removal fail on release.
        ctl: MockControl = TestControl(ref, failRemoveRule = _ == "r0")
        res <- ZIO
                 .scoped(MockOverlay.scoped(spaceA)(rule("/o1"), rule("/o2")).unit)
                 .provideLayer(ZLayer.succeed(ctl))
                 .either
        after <- rulesOf(ref, spaceA)
      yield assertTrue(
        res.isRight,               // the logged teardown failure doesn't fail the scope
        after == List(rule("/o1")) // /o2 was still removed; only the stuck /o1 remains
      )
    },
    test("an overlay scope closes cleanly even if its rules were replaced wholesale while open") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        res <- ZIO.scoped {
                 MockOverlay.scoped(spaceA)(overlay) *> MockOverlay.replaceAll(spaceA, rule("/timeout"))
               }.provideLayer(ZLayer.succeed(ctl)).either
        after <- rulesOf(ref, spaceA)
      yield assertTrue(
        res.isRight,                    // overlay teardown hits RuleNotFound, is logged, scope still closes
        after == List(rule("/timeout")) // the wholesale replacement stands
      )
    },
    test("remove of an unknown rule id surfaces RuleNotFound") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        res             <- MockOverlay.remove(spaceA, RuleId("ghost")).provideLayer(ZLayer.succeed(ctl)).either
      yield assertTrue(res == Left(MockError.RuleNotFound(spaceA.id, RuleId("ghost"))))
    },
    test("overlays applied in parallel on different spaces each see only their own") {
      for
        ref             <- Ref.make(TState.init)
        ctl: MockControl = TestControl(ref)
        _               <- ctl.addRule(spaceA, base, Priority.Base)
        _               <- ctl.addRule(spaceB, base, Priority.Base)
        seen <- ZIO
                  .scoped(MockOverlay.scoped(spaceA)(rule("/oa")) *> rulesOf(ref, spaceA))
                  .provideLayer(ZLayer.succeed(ctl))
                  .zipPar(
                    ZIO
                      .scoped(MockOverlay.scoped(spaceB)(rule("/ob")) *> rulesOf(ref, spaceB))
                      .provideLayer(ZLayer.succeed(ctl))
                  )
      yield assertTrue(
        seen._1 == List(rule("/oa"), base), // A's scope saw only A's overlay
        seen._2 == List(rule("/ob"), base)  // B's scope saw only B's overlay
      )
    }
  )
