package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

import javax.net.ssl.SSLContext

import TlsFixtures.*

/**
 * The portable HTTPS / mTLS mock-space conformance scenarios (#343) — the
 * `cap-tls` feature. Gated on [[Capability.Tls]], so a backend that does not
 * advertise it SKIPs them. Every check is a real TLS round-trip through a
 * [[SutClient]] built from the [[Tls]] PEM helpers, using the checked-in
 * [[TlsFixtures]] (no certificate generation), so each adapter must prove the
 * same client-observable handshake behaviour:
 *
 *   - an HTTPS space reports an `https://` base URI and serves a client that
 *     trusts its CA;
 *   - a client that does not trust that CA is refused;
 *   - an mTLS space serves a client presenting a certificate from a trusted CA,
 *     and refuses one presenting none or one from another CA;
 *   - a TLS space is its own listener, so it works under Correlated isolation
 *     too (its `inject` is still applied, and harmless);
 *   - malformed TLS material fails provisioning with a typed
 *     [[MockError.InvalidDefinition]].
 */
object TlsScenarios:

  lazy val all: List[ConformanceScenario] =
    List(
      httpsServes,
      untrustedRefused,
      mtlsServes,
      mtlsNoCertRefused,
      mtlsWrongCaRefused,
      mtlsAnyListedCa,
      rulesMutate,
      malformed
    )

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private val needsTls = Set(Capability.Tls)

  private val ping = MockRule(RequestMatch(path = PathMatch.Exact("/ping")), ResponseDef(body = Body.Text("pong")))

  private val httpsSpec = MockSpec(List(ping), tls = Some(Tls(TlsMaterial(serverCert, serverKey))))
  private val mtlsSpec =
    httpsSpec.copy(tls = Some(Tls(TlsMaterial(serverCert, serverKey), ClientAuth.Required(NonEmptyChunk(caCert)))))

  private def space(control: MockControl, spec: MockSpec): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(MockSource.Dsl(spec)).mapError(asT).map(_.head))(s =>
      control.destroy(s).ignoreLogged
    )

  // `rejected` must be refused by `s` while `accepted` is served by the very same space — the positive
  // control proves the failure is the TLS policy, not an unreachable or broken space. The refusal must
  // be a prompt transport (IOException, e.g. SSLHandshakeException) failure: never a hang or a response.
  private def refused(name: String, s: MockSpace, accepted: SSLContext, rejected: SSLContext): IO[Throwable, Unit] =
    for
      ok <- SutClient.make(s, accepted).send(Method.Get, "/ping")
      _  <- ensure(ok.status == 200 && ok.body == "pong", s"$name: control client not served: $ok")
      r  <- SutClient.make(s, rejected).send(Method.Get, "/ping").exit.timeout(20.seconds)
      _ <- ensure(
             r.exists(_.causeOption.flatMap(_.failureOption).exists(_.isInstanceOf[java.io.IOException])),
             s"$name: expected a refused handshake (IOException), got $r"
           )
    yield ()

  private def trusting: IO[Throwable, SSLContext] = Tls.trust(caCert).mapError(asT)

  private val httpsServes = ConformanceScenario(
    "tls: an https space reports an https base URI and serves a trusting client",
    needsTls,
    control =>
      for
        s    <- space(control, httpsSpec)
        ssl  <- trusting
        resp <- SutClient.make(s, ssl).send(Method.Get, "/ping")
        _ <- ensure(
               s.baseUri.startsWith("https://") && resp.status == 200 && resp.body == "pong",
               s"https: baseUri=${s.baseUri} status=${resp.status} body=${resp.body}"
             )
      yield ()
  )

  private val untrustedRefused = ConformanceScenario(
    "tls: a client that does not trust the space's CA is refused",
    needsTls,
    control =>
      for
        s   <- space(control, httpsSpec)
        ok  <- trusting
        ssl <- Tls.trust(otherCaCert).mapError(asT)
        _   <- refused("untrusted", s, ok, ssl)
      yield ()
  )

  private val mtlsServes = ConformanceScenario(
    "tls: an mTLS space serves a client presenting a certificate from a trusted CA",
    needsTls,
    control =>
      for
        s    <- space(control, mtlsSpec)
        ssl  <- Tls.clientIdentity(clientCert, clientKey, caCert).mapError(asT)
        resp <- SutClient.make(s, ssl).send(Method.Get, "/ping")
        _    <- ensure(resp.status == 200 && resp.body == "pong", s"mtls: status=${resp.status} body=${resp.body}")
      yield ()
  )

  private val mtlsNoCertRefused = ConformanceScenario(
    "tls: an mTLS space refuses a client that presents no certificate",
    needsTls,
    control =>
      for
        s   <- space(control, mtlsSpec)
        ok  <- Tls.clientIdentity(clientCert, clientKey, caCert).mapError(asT)
        ssl <- trusting
        _   <- refused("mtls without a client certificate", s, ok, ssl)
      yield ()
  )

  private val mtlsWrongCaRefused = ConformanceScenario(
    "tls: an mTLS space refuses a client certificate from an untrusted CA",
    needsTls,
    control =>
      for
        s   <- space(control, mtlsSpec)
        ok  <- Tls.clientIdentity(clientCert, clientKey, caCert).mapError(asT)
        ssl <- Tls.clientIdentity(otherClientCert, otherClientKey, caCert).mapError(asT)
        _   <- refused("mtls with a foreign client certificate", s, ok, ssl)
      yield ()
  )

  // Every listed CA is a trust anchor, not just the first: clients from either CA are served.
  private val mtlsAnyListedCa = ConformanceScenario(
    "tls: an mTLS space trusts every listed client CA",
    needsTls,
    control =>
      val twoCas =
        httpsSpec.copy(tls =
          Some(Tls(TlsMaterial(serverCert, serverKey), ClientAuth.Required(NonEmptyChunk(otherCaCert, caCert))))
        )
      for
        s     <- space(control, twoCas)
        first <- Tls.clientIdentity(otherClientCert, otherClientKey, caCert).mapError(asT)
        last  <- Tls.clientIdentity(clientCert, clientKey, caCert).mapError(asT)
        r1    <- SutClient.make(s, first).send(Method.Get, "/ping")
        r2    <- SutClient.make(s, last).send(Method.Get, "/ping")
        _     <- ensure(r1.status == 200 && r2.status == 200, s"two CAs: first=${r1.status} last=${r2.status}")
      yield ()
  )

  // A TLS space is still an ordinary space: rules added after provisioning are served, and it
  // records what it received — over TLS.
  private val rulesMutate = ConformanceScenario(
    "tls: an https space accepts added rules and records requests",
    needsTls,
    control =>
      val extra = MockRule(RequestMatch(path = PathMatch.Exact("/extra")), ResponseDef(status = 201))
      for
        s    <- space(control, httpsSpec)
        _    <- control.addRule(s, extra).mapError(asT)
        ssl  <- trusting
        resp <- SutClient.make(s, ssl).send(Method.Get, "/extra")
        recv <- control.received(s).mapError(asT)
        _ <- ensure(
               resp.status == 201 && recv.exists(_.uri.endsWith("/extra")),
               s"rules: status=${resp.status} received=${recv.map(_.uri)}"
             )
      yield ()
  )

  private val malformed = ConformanceScenario(
    "tls: malformed TLS material fails provisioning with InvalidDefinition",
    needsTls,
    control =>
      val bad = MockSpec(List(ping), tls = Some(Tls(TlsMaterial("not a certificate", serverKey))))
      control
        .provision(MockSource.Dsl(bad))
        .either
        .flatMap {
          case Left(MockError.InvalidDefinition(_)) => ZIO.unit
          case Right(spaces) =>
            ZIO.foreachDiscard(spaces)(control.destroy(_).ignore) *>
              ZIO.fail(new AssertionError("malformed TLS material was provisioned"))
          case Left(other) => ZIO.fail(new AssertionError(s"expected InvalidDefinition, got $other"))
        }
  )
