package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * The capability-negotiation and error-semantics conformance features (#127):
 * portable scenarios programmed against the neutral [[MockControl]], so they
 * run identically on every adapter via the matrix. (The native-escape-hatch
 * feature is per-adapter — each adapter needs its own native snippet — so it
 * lives in `NativeEscapeHatchSpec`, not here.)
 */
object NegotiationErrorScenarios:

  lazy val all: List[ConformanceScenario] = negotiation ++ errorSemantics

  // ---- helpers ------------------------------------------------------------------

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private def srcOf(rules: MockRule*): MockSource = MockSource.Dsl(MockSpec(rules.toList))

  private def rule(path: String, body: String): MockRule =
    MockRule(RequestMatch(path = PathMatch.Exact(path)), ResponseDef(status = 200, body = Body.Text(body)))

  private def scen(name: String)(check: MockControl => ZIO[Scope, Throwable, Unit]): ConformanceScenario =
    ConformanceScenario(name, Set.empty, check)

  private def space(control: MockControl, rules: MockRule*): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(srcOf(rules*)).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  // ---- capability-negotiation ---------------------------------------------------

  private val negotiation = List(
    scen("negotiation: capabilities are honest — each accessor matches what's advertised") { control =>
      val accessors: List[(Capability, IO[Unsupported, Any])] = List(
        Capability.Faults            -> control.faults,
        Capability.StatefulScenarios -> control.scenarios,
        Capability.StateInspection   -> control.stateInspection,
        Capability.Scripting         -> control.scripting,
        Capability.ProxyRecord       -> control.proxyRecord,
        Capability.Templating        -> control.templating,
        Capability.Intercept         -> control.intercept
      )
      for
        outcomes <- ZIO.foreach(accessors)((cap, acc) => acc.either.map(cap -> _))
        tlsOk    <- tlsHonest(control)
        _ <- ensure(
               // the check must cover EVERY capability, so a future one can't escape it (Tls has no
               // accessor — it is a provisioning option, probed by `tlsHonest`),
               outcomes.map(_._1).toSet + Capability.Tls == Capability.values.toSet && tlsOk &&
                 // advertised => the accessor succeeds; un-advertised => fails fast with Unsupported(cap, backend).
                 outcomes.forall { (cap, e) =>
                   if control.capabilities.contains(cap) then e.isRight
                   else e == Left(Unsupported(cap, control.backendName))
                 },
               s"capabilities not honest: caps=${control.capabilities} outcomes=$outcomes"
             )
      yield ()
    },
    scen("negotiation: require fails at wiring naming the gap (and the backend)") { control =>
      for
        emptyOk <- control.require().either // requiring nothing always succeeds
        _ <- Capability.values.find(c => !control.capabilities.contains(c)) match
               // an un-advertised capability: require must fail naming THAT gap + the backend.
               case Some(gap) =>
                 control
                   .require(gap)
                   .either
                   .flatMap(gapE =>
                     ensure(
                       emptyOk.isRight && gapE == Left(Unsupported(gap, control.backendName)),
                       s"require gap: empty=$emptyOk gap=$gap gapE=$gapE"
                     )
                   )
               // a fully-capable adapter has no gap: requiring everything it advertises must succeed.
               case None =>
                 control
                   .require(Capability.values*)
                   .either
                   .flatMap(allE => ensure(emptyOk.isRight && allE.isRight, s"require all: empty=$emptyOk all=$allE"))
      yield ()
    }
  )

  // Tls is honest when an advertising backend serves a TLS spec on an https:// base URI, and a
  // non-advertising one refuses it at provision with InvalidDefinition rather than silently serving
  // plain HTTP (a downgrade the SUT's test would never notice).
  private def tlsHonest(control: MockControl): UIO[Boolean] =
    val spec = MockSpec(
      List(rule("/tls", "tls")),
      tls = Some(Tls(TlsMaterial(TlsFixtures.serverCert, TlsFixtures.serverKey)))
    )
    control.provision(MockSource.Dsl(spec)).either.flatMap {
      case Right(spaces) =>
        ZIO.foreachDiscard(spaces)(control.destroy(_).ignore) *>
          ZIO.succeed(
            control.capabilities.contains(Capability.Tls) && spaces.nonEmpty && spaces
              .forall(_.baseUri.startsWith("https://"))
          )
      case Left(MockError.InvalidDefinition(_)) => ZIO.succeed(!control.capabilities.contains(Capability.Tls))
      case Left(_)                              => ZIO.succeed(false)
    }

  // ---- error-semantics ----------------------------------------------------------

  private val errorSemantics = List(
    scen("error: a malformed source fails fast with a typed MockError (no hang)") { control =>
      for
        // .timeout proves it doesn't hang: a completed effect is Some(...), a hang is None.
        result <- control.provision(MockSource.Json("this is not json")).either.timeout(5.seconds)
        _ <- ensure(
               result.exists(_.swap.toOption.exists {
                 case MockError.InvalidDefinition(_) => true
                 case _                              => false
               }),
               s"malformed source: $result"
             )
      yield ()
    },
    scen("error: removeRule of an unknown id fails with RuleNotFound") { control =>
      for
        s <- space(control, rule("/p", "x"))
        e <- control.removeRule(s, RuleId("ghost")).either
        _ <- ensure(e == Left(MockError.RuleNotFound(s.id, RuleId("ghost"))), s"removeRule unknown: $e")
      yield ()
    },
    // (ops on a destroyed space -> defined MockError is covered by CoreConformanceScenarios #125,
    //  "operations on a destroyed space fail with SpaceNotFound" — all four ops, every adapter.)
    scen(
      "error: adding the same rule twice yields distinct ids with precise, non-aliased removal (duplicate-id behaviour)"
    ) { control =>
      val dup = rule("/dup", "dup")
      for
        s       <- space(control, rule("/keep", "keep"))
        id1     <- control.addRule(s, dup, Priority.Base).mapError(asT) // identical rules...
        id2     <- control.addRule(s, dup, Priority.Base).mapError(asT) // ...get distinct managed ids
        _       <- control.removeRule(s, id1).mapError(asT)
        rm2     <- control.removeRule(s, id2).either                    // the twin is still removable (not aliased to id1)
        rmGhost <- control.removeRule(s, id1).either                    // re-removing the gone id -> RuleNotFound
        _ <- ensure(
               id1 != id2 && rm2.isRight && rmGhost == Left(MockError.RuleNotFound(s.id, id1)),
               s"rule ids: id1=$id1 id2=$id2 rm2=$rm2 rmGhost=$rmGhost"
             )
      yield ()
    }
  )
