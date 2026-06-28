package zio.bdd.mock

import zio.*
import zio.test.*

/**
 * AC1 gate: a suite authored entirely from raw `.json` sources (no DSL) must
 * still drive every mutation and assertion on [[MockControl]]. This file
 * deliberately does NOT `import zio.bdd.mock.dsl.*` — that it compiles and runs
 * proves the DSL builders are pure convenience, never a requirement. Overlay
 * rules are constructed from the bare canonical model, not the DSL.
 */
object RawSourceNoDslSpec extends ZIOSpecDefault:

  // An in-memory adapter: rules and the loaded source payload live in Refs.
  // `provision` runs through the real [[Provisioning]] path so a raw `.json`
  // source is genuinely read (a missing resource fails the test), then records
  // the normalized payload per space. Enough to exercise
  // addRule/removeRule/replaceRules/received/destroy and the provisionNative
  // escape hatch without any backend.
  private final case class InMemoryControl(
    prov: Provisioning,
    rules: Ref[Map[SpaceId, List[(RuleId, MockRule)]]],
    loaded: Ref[Map[SpaceId, SourcePayload]],
    seq: Ref[Int]
  ) extends MockControl:
    def backendName: String           = "in-memory"
    def capabilities: Set[Capability] = Set.empty

    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      prov.provision(source) { (norm, space) =>
        rules.update(_.updated(space.id, Nil)) *> loaded.update(_.updated(space.id, norm.payload))
      }

    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      val space = MockSpace("http://localhost:0", identity, SpaceId("native"))
      rules.update(_.updated(space.id, Nil)).as(List(space))

    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      seq.updateAndGet(_ + 1).map(n => RuleId(s"r$n")).flatMap { id =>
        rules.update(m => m.updated(space.id, m.getOrElse(space.id, Nil) :+ (id, rule))).as(id)
      }

    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit] =
      rules.update(m => m.updated(space.id, m.getOrElse(space.id, Nil).filterNot(_._1 == id)))

    def replaceRules(space: MockSpace, rs: List[MockRule]): IO[MockError, Unit] =
      rules.update(m => m.updated(space.id, rs.map(r => (RuleId("replaced"), r))))

    def destroy(space: MockSpace): IO[MockError, Unit] =
      rules.update(_.removed(space.id))

    def received(space: MockSpace): IO[MockError, List[RecordedRequest]] =
      ZIO.succeed(Nil)

    private def unsupported[A](c: Capability): IO[Unsupported, A] = ZIO.fail(Unsupported(c, "in-memory"))
    def faults: IO[Unsupported, Faults]                           = unsupported(Capability.Faults)
    def scenarios: IO[Unsupported, StatefulScenarios]             = unsupported(Capability.StatefulScenarios)
    def stateInspection: IO[Unsupported, StateInspection]         = unsupported(Capability.StateInspection)
    def scripting: IO[Unsupported, Scripting]                     = unsupported(Capability.Scripting)
    def proxyRecord: IO[Unsupported, ProxyRecord]                 = unsupported(Capability.ProxyRecord)
    def templating: IO[Unsupported, Templating]                   = unsupported(Capability.Templating)

  private def control: UIO[InMemoryControl] =
    for
      prov   <- Provisioning.make
      rules  <- Ref.make(Map.empty[SpaceId, List[(RuleId, MockRule)]])
      loaded <- Ref.make(Map.empty[SpaceId, SourcePayload])
      seq    <- Ref.make(0)
    yield InMemoryControl(prov, rules, loaded, seq)

  // A canonical overlay rule built WITHOUT the DSL — bare model constructors only.
  private val overlay: MockRule =
    MockRule(
      `match` = RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/echo")),
      respond = ResponseDef(status = 201, body = Body.Json("""{"ok":true}"""))
    )

  def spec = suite("raw .json source drives every mutation/assertion without the DSL")(
    test("provision from a .json resource, then add/replace/remove/received/destroy") {
      for
        mc       <- control
        spaces   <- mc.provision(MockSource.Resource("mocks/ping.json"))
        space    <- ZIO.fromOption(spaces.headOption).orElseFail(MockError.ProvisionFailed("no space"))
        payload  <- mc.loaded.get.map(_(space.id))
        id       <- mc.addRule(space, overlay)
        afterAdd <- mc.rules.get
        _        <- mc.replaceRules(space, List(overlay))
        afterRep <- mc.rules.get
        _        <- mc.removeRule(space, RuleId("replaced"))
        afterRem <- mc.rules.get
        recs     <- mc.received(space)
        _        <- mc.destroy(space)
        afterDel <- mc.rules.get
      yield assertTrue(
        spaces.size == 1,
        // the raw .json was genuinely read by the provisioning path
        payload match
          case SourcePayload.Raw(t) => t.contains("\"/ping\"")
          case _                    => false,
        id == RuleId("r1"),
        afterAdd(space.id).map(_._2) == List(overlay),
        afterRep(space.id).map(_._2) == List(overlay),
        afterRem(space.id).isEmpty,
        recs.isEmpty,
        !afterDel.contains(space.id)
      )
    },
    test("provisioning a missing .json resource fails — no DSL fallback hides it") {
      for
        mc  <- control
        res <- mc.provision(MockSource.Resource("mocks/does-not-exist.json")).either
      yield assertTrue(res == Left(MockError.InvalidDefinition("resource not found: mocks/does-not-exist.json")))
    },
    test("provisionNative escape hatch stands up a space without a portable source") {
      val nativeSpec = NativeSpec.Rift("""{"stubs":[]}""")
      for
        mc     <- control
        spaces <- mc.provisionNative(nativeSpec)
      yield assertTrue(spaces.map(_.id) == List(SpaceId("native")))
    },
    test("an unadvertised capability fails fast with typed Unsupported") {
      for
        mc  <- control
        res <- mc.faults.either
      yield assertTrue(res == Left(Unsupported(Capability.Faults, "in-memory")))
    }
  )
