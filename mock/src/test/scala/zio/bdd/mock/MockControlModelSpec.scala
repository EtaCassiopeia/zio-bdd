package zio.bdd.mock

import zio.*
import zio.test.*

object MockControlModelSpec extends ZIOSpecDefault:

  // A minimal in-spec adapter that implements the *total* core port. Its mere
  // existence proves `MockControl` is implementable end-to-end with no adapter
  // dependency (AC1); it also exercises the capability fail-fast contract (AC4).
  private final case class StubMockControl(backend: String, caps: Set[Capability]) extends MockControl:
    def backendName: String           = backend
    def capabilities: Set[Capability] = caps

    def provision(source: MockSource): IO[MockError, List[MockSpace]] =
      ZIO.succeed(List(MockSpace("http://localhost:0", identity, SpaceId("s1"))))

    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] =
      ZIO.succeed(Nil)

    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      ZIO.succeed(RuleId("r1"))

    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]              = ZIO.unit
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] = ZIO.unit
    def destroy(space: MockSpace): IO[MockError, Unit]                             = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]]           = ZIO.succeed(Nil)

    private def cap[A](c: Capability)(a: => A): IO[Unsupported, A] =
      if caps(c) then ZIO.succeed(a) else ZIO.fail(Unsupported(c, backend))

    def faults: IO[Unsupported, Faults]                   = cap(Capability.Faults)(new Faults {})
    def scenarios: IO[Unsupported, StatefulScenarios]     = cap(Capability.StatefulScenarios)(new StatefulScenarios {})
    def stateInspection: IO[Unsupported, StateInspection] = cap(Capability.StateInspection)(new StateInspection {})
    def scripting: IO[Unsupported, Scripting]             = cap(Capability.Scripting)(new Scripting {})
    def proxyRecord: IO[Unsupported, ProxyRecord]         = cap(Capability.ProxyRecord)(new ProxyRecord {})
    def templating: IO[Unsupported, Templating]           = cap(Capability.Templating)(new Templating {})

  def spec = suite("MockControl port + canonical model")(
    test("the total port is implementable by a stub adapter") {
      val control: MockControl = StubMockControl("stub", Set.empty)
      val rule = MockRule(
        `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
        respond = ResponseDef(status = 200, body = Body.Text("pong"))
      )
      for
        spaces <- control.provision(MockSource.Dsl(MockSpec(Nil)))
        space  <- ZIO.fromOption(spaces.headOption).orElseFail(MockError.ProvisionFailed("stub returned no space"))
        id     <- control.addRule(space, rule)
        _      <- control.replaceRules(space, List(rule))
        _      <- control.removeRule(space, id)
        recs   <- control.received(space)
        _      <- control.destroy(space)
      yield
        val req = HttpRequest(Method.Get, "http://x/")
        assertTrue(
          spaces.nonEmpty,
          id == RuleId("r1"),
          recs.isEmpty,
          space.inject(req) == req // stub isolation is identity
        )
    },
    test("Body covers Text | Json | Base64 (plus Empty)") {
      val bodies: List[Body] = List(Body.Text("a"), Body.Json("{}"), Body.Base64("YQ=="), Body.Empty)
      val shapes = bodies.map {
        case Body.Text(_)   => "text"
        case Body.Json(_)   => "json"
        case Body.Base64(_) => "base64"
        case Body.Empty     => "empty"
      }
      assertTrue(shapes == List("text", "json", "base64", "empty"))
    },
    test("PathMatch covers Exact | Regex | Template (plus Any)") {
      val paths: List[PathMatch] =
        List(PathMatch.Exact("/u"), PathMatch.Regex("/u/.*"), PathMatch.Template("/u/{id}"), PathMatch.Any)
      val shapes = paths.map {
        case PathMatch.Exact(_)    => "exact"
        case PathMatch.Regex(_)    => "regex"
        case PathMatch.Template(_) => "template"
        case PathMatch.Any         => "any"
      }
      assertTrue(shapes == List("exact", "regex", "template", "any"))
    },
    test("MockError and Unsupported are typed, not Throwable") {
      val err: MockError   = MockError.SpaceNotFound(SpaceId("s1"))
      val uns: Unsupported = Unsupported(Capability.Faults, "stub")
      assertTrue(
        !err.isInstanceOf[Throwable],
        !uns.isInstanceOf[Throwable],
        uns.message.contains("Faults"),
        uns.message.contains("stub")
      )
    },
    test("every capability accessor fails fast with its own Unsupported when not advertised") {
      // Each accessor must name *its own* capability — the likeliest copy-paste bug in adapters.
      val accessors: List[(Capability, MockControl => IO[Unsupported, Any])] = List(
        Capability.Faults            -> (_.faults),
        Capability.StatefulScenarios -> (_.scenarios),
        Capability.StateInspection   -> (_.stateInspection),
        Capability.Scripting         -> (_.scripting),
        Capability.ProxyRecord       -> (_.proxyRecord),
        Capability.Templating        -> (_.templating)
      )
      val none: MockControl = StubMockControl("stub", Set.empty)
      val all: MockControl  = StubMockControl("stub", Capability.values.toSet)
      ZIO
        .foreach(accessors) { (cap, access) =>
          for
            unsupported <- access(none).either
            supported   <- access(all).either
          yield assertTrue(
            unsupported == Left(Unsupported(cap, "stub")),
            supported.isRight
          )
        }
        .map(_.reduce(_ && _))
    },
    test("Isolation enum has both isolation modes") {
      assertTrue(Isolation.values.toSet == Set(Isolation.PerInstance, Isolation.Correlated))
    },
    test("MockControl.isolation defaults to PerInstance for an adapter that does not override it") {
      assertTrue(StubMockControl("stub", Set.empty).isolation == Isolation.PerInstance)
    },
    test("NativeSpec is backend-tagged and carries its raw payload") {
      // The type ascriptions are the real assertion: they only compile because
      // each case is pinned to its backend tag (Rift / WireMock).
      val rift: NativeSpec[Backend.Rift]     = NativeSpec.Rift("""{"stubs":[]}""")
      val wire: NativeSpec[Backend.WireMock] = NativeSpec.WireMock("""{"mappings":[]}""")
      assertTrue(
        rift.isInstanceOf[NativeSpec.Rift],
        wire.isInstanceOf[NativeSpec.WireMock],
        (rift match { case NativeSpec.Rift(j) => j; case _ => "" }) == """{"stubs":[]}"""
      )
    },
    test("model types are immutable value types (case-class semantics)") {
      val r1 = ResponseDef(status = 200, body = Body.Json("{}"))
      val r2 = r1.copy(status = 404)
      assertTrue(
        r1 == ResponseDef(status = 200, body = Body.Json("{}")),
        r1.status == 200, // original unchanged by copy
        r2.status == 404,
        r2.body == Body.Json("{}")
      )
    },
    test("RuleId and SpaceId are zero-cost newtypes carrying their value") {
      assertTrue(
        RuleId("r1") == RuleId("r1"),
        RuleId("r1").value == "r1",
        SpaceId("s1").value == "s1"
      )
    }
  )
