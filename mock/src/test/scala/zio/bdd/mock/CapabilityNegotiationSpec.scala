package zio.bdd.mock

import zio.*
import zio.test.*

object CapabilityNegotiationSpec extends ZIOSpecDefault:

  // A minimal adapter that advertises exactly `caps` and is honest about them:
  // each accessor returns an instance iff its capability is advertised. Used to
  // exercise `require` negotiation without standing up a real backend.
  private final case class StubMockControl(backendName: String, caps: Set[Capability]) extends MockControl:
    def capabilities: Set[Capability] = caps

    def provision(source: MockSource): IO[MockError, List[MockSpace]]                      = ZIO.succeed(Nil)
    def provisionNative[B <: Backend](spec: NativeSpec[B]): IO[MockError, List[MockSpace]] = ZIO.succeed(Nil)
    def addRule(space: MockSpace, rule: MockRule, priority: Priority): IO[MockError, RuleId] =
      ZIO.succeed(RuleId("r1"))
    def removeRule(space: MockSpace, id: RuleId): IO[MockError, Unit]              = ZIO.unit
    def replaceRules(space: MockSpace, rules: List[MockRule]): IO[MockError, Unit] = ZIO.unit
    def destroy(space: MockSpace): IO[MockError, Unit]                             = ZIO.unit
    def received(space: MockSpace): IO[MockError, List[RecordedRequest]]           = ZIO.succeed(Nil)

    private def cap[A](c: Capability)(a: => A): IO[Unsupported, A] =
      if caps(c) then ZIO.succeed(a) else ZIO.fail(Unsupported(c, backendName))

    private val anyFault: Faults = (_, _, _) => ZIO.succeed(RuleId("fault"))

    def faults: IO[Unsupported, Faults]                   = cap(Capability.Faults)(anyFault)
    def scenarios: IO[Unsupported, StatefulScenarios]     = cap(Capability.StatefulScenarios)(StubCaps.scenarios)
    def stateInspection: IO[Unsupported, StateInspection] = cap(Capability.StateInspection)(StubCaps.stateInspection)
    def scripting: IO[Unsupported, Scripting]             = cap(Capability.Scripting)(StubCaps.scripting)
    def proxyRecord: IO[Unsupported, ProxyRecord]         = cap(Capability.ProxyRecord)(StubCaps.proxyRecord)
    def templating: IO[Unsupported, Templating]           = cap(Capability.Templating)(StubCaps.templating)

  def spec = suite("capability fail-fast negotiation")(
    test("require fails at layer construction, before first use") {
      // AC1: declaring a needed capability against a backend lacking it must be
      // rejected when the layer is built — the program that *uses* the mock must
      // never run. Two independent proofs: (a) building the layer in isolation
      // already fails, and (b) the `used` Ref stays false, so construction
      // failed before first use (not at the accessor).
      val backend = StubMockControl("rift", Set.empty)
      val layer   = ZLayer.fromZIO(backend.require(Capability.Faults).as[MockControl](backend))
      for
        used      <- Ref.make(false)
        buildExit <- ZIO.scoped(layer.build).exit
        program: ZIO[MockControl, Unsupported, Unit] =
          used.set(true) *> ZIO.serviceWithZIO[MockControl](_.faults).unit
        exit    <- program.provideLayer(layer).exit
        wasUsed <- used.get
      yield assertTrue(
        buildExit.causeOption.flatMap(_.failureOption).contains(Unsupported(Capability.Faults, "rift")),
        exit.isFailure,
        exit.causeOption.flatMap(_.failureOption).contains(Unsupported(Capability.Faults, "rift")),
        !wasUsed
      )
    },
    test("require succeeds at layer construction so the wired capability is usable") {
      // AC1 positive path: when the capability IS advertised, the same wiring
      // pattern builds the layer, runs the program, and yields a real instance.
      val backend = StubMockControl("rift", Set(Capability.Faults))
      val layer   = ZLayer.fromZIO(backend.require(Capability.Faults).as[MockControl](backend))
      for
        used <- Ref.make(false)
        program: ZIO[MockControl, Unsupported, Faults] =
          used.set(true) *> ZIO.serviceWithZIO[MockControl](_.faults)
        exit    <- program.provideLayer(layer).exit
        wasUsed <- used.get
      yield assertTrue(exit.isSuccess, wasUsed)
    },
    test("require names the backend and the missing capability") {
      // Scope: the failure message must name both the backend and the gap.
      val backend = StubMockControl("wiremock", Set(Capability.Templating))
      for either <- backend.require(Capability.Faults).either
      yield
        val msg = either.swap.toOption.map(_.message).getOrElse("")
        assertTrue(
          either == Left(Unsupported(Capability.Faults, "wiremock")),
          msg.contains("wiremock"),
          msg.contains("Faults")
        )
    },
    test("require is fail-fast on the first missing capability and passes when satisfied") {
      // Deterministic: fails with the first missing capability in argument order;
      // succeeds when every required capability is advertised (including empty).
      val partial = StubMockControl("stub", Set(Capability.Faults))
      val full    = StubMockControl("stub", Capability.values.toSet)
      for
        firstMissing <- partial.require(Capability.Faults, Capability.Scripting, Capability.Templating).either
        allPresent   <- full.require(Capability.values*).either
        noneRequired <- partial.require().either
      yield assertTrue(
        firstMissing == Left(Unsupported(Capability.Scripting, "stub")),
        allPresent == Right(()),
        noneRequired == Right(())
      )
    },
    test("advertised accessors and require both succeed for every capability") {
      // AC2: for an advertised capability the accessor never returns Unsupported,
      // and require for that capability succeeds — the two sides of the honesty
      // contract agree. This stub is honest by construction; the real check for a
      // *dishonest* adapter (advertises a capability but its accessor still fails)
      // belongs to the cross-adapter conformance suite (M2, #122).
      val all: MockControl = StubMockControl("stub", Capability.values.toSet)
      val accessors: List[(Capability, MockControl => IO[Unsupported, Any])] = List(
        Capability.Faults            -> (_.faults),
        Capability.StatefulScenarios -> (_.scenarios),
        Capability.StateInspection   -> (_.stateInspection),
        Capability.Scripting         -> (_.scripting),
        Capability.ProxyRecord       -> (_.proxyRecord),
        Capability.Templating        -> (_.templating)
      )
      ZIO
        .foreach(accessors) { (cap, access) =>
          for
            accessed <- access(all).either
            required <- all.require(cap).either
          yield assertTrue(accessed.isRight, required == Right(()))
        }
        .map(_.reduce(_ && _))
    }
  )
