package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json

/**
 * The [[Intercept]] capability over the embedded engine's intercept FFI
 * (rift#410, #219): starts the TLS-MITM forward-proxy lazily on first use
 * (memoized in `started`), installs rules, and exports the CA truststore —
 * entirely over the C-ABI, no loopback HTTP. The engine's scope owns the
 * listener (`rift_stop` shuts it down), so this holds no finalizer of its own.
 *
 * Isolation semantics (#123): `redirectTo` forwards an intercepted host to the
 * target space's imposter PORT. Under PerInstance (the embedded default) each
 * space has its own port, so a redirect lands on exactly that space.
 * `respondWith` is isolation-independent (an inline stub). A Correlated space
 * shares one imposter port and is separated by a correlation header the
 * intercepted request does not carry, so redirecting a host to a Correlated
 * space would hit the shared imposter's unscoped stubs — use PerInstance (the
 * default) for host redirects.
 */
private[embedded] final class EmbeddedIntercept(
  engine: EmbeddedEngine,
  started: Ref.Synchronized[Option[Int]],
  config: EmbeddedRift.InterceptConfig
) extends Intercept:

  def proxyPort: IO[MockError, Int] = ensureStarted

  // The listener binds `config.bindHost` (loopback by default), so report that as the reachable host —
  // a wider bind (e.g. 0.0.0.0 for a containerized SUT) is where a caller points its proxy, not 127.0.0.1.
  override def proxyEndpoint: IO[MockError, (String, Int)] = ensureStarted.map((config.bindHost, _))

  def add(rule: InterceptRule): IO[MockError, Unit] =
    ZIO.fromEither(EmbeddedIntercept.ruleJson(rule)).flatMap(json => ensureStarted *> engine.interceptAddRules(json))

  def trustStore(format: TrustStoreFormat, to: Option[java.nio.file.Path]): IO[MockError, TrustStore] =
    for
      _ <- ensureStarted
      out <- ZIO
               // Local filesystem work (temp file / caller path), not a backend round-trip → ProvisionFailed,
               // per the package's error taxonomy (CommunicationError is reserved for backend comms).
               .attemptBlocking(TrustStore.exportPath(to, format))
               .mapError(t => MockError.ProvisionFailed(s"resolving intercept truststore path: ${message(t)}"))
      _ <- engine.interceptExportTruststore(format.wire, EmbeddedIntercept.DefaultPassword, out.toString)
    yield TrustStore(out, EmbeddedIntercept.DefaultPassword, format)

  // Start the listener at most once (race-safe), memoizing its bound proxy port. Validate the bind host
  // first so a hostname fails with a clear message here, not opaquely inside the engine (#262).
  private def ensureStarted: IO[MockError, Int] =
    started.modifyZIO {
      case s @ Some(p) => ZIO.succeed((p, s))
      case None =>
        ZIO.fromEither(EmbeddedIntercept.validateBindHost(config.bindHost)) *>
          engine
            .startIntercept(EmbeddedIntercept.startOptions(config))
            .map(i => (i.interceptPort, Some(i.interceptPort)))
    }

  private def message(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)

private[embedded] object EmbeddedIntercept:
  private val DefaultPassword: String = "changeit"

  // Start options for the TLS-MITM listener. `bindHost` defaults to loopback (127.0.0.1); a wider bind
  // (0.0.0.0 / a NIC address) makes the proxy reachable from another host or container. `port` defaults
  // to 0 (OS-assigned); a pinned value binds a known port for a SUT whose proxy target is fixed up front.
  private[embedded] def startOptions(config: EmbeddedRift.InterceptConfig): String =
    Json.Obj("host" -> Json.Str(config.bindHost), "port" -> Json.Num(config.port.getOrElse(0))).toJson

  // rift binds the intercept listener via SocketAddr::parse, which accepts only IP literals — a hostname
  // (`localhost` / a DNS name) fails there with an opaque error. Reject it early, DNS-free (no resolution),
  // with a clear message. IPv4 is validated strictly (rejecting leading zeros, matching Rust's parser);
  // IPv6 leniently (colon + hex/dot chars) so a valid literal passes through.
  private[embedded] def validateBindHost(host: String): Either[MockError, Unit] =
    if isIpLiteral(host) then Right(())
    else
      Left(
        MockError.InvalidDefinition(
          s"intercept bindHost must be an IP literal (e.g. 0.0.0.0 or a NIC address), not a hostname: $host"
        )
      )

  private def isIpLiteral(host: String): Boolean =
    // ASCII digits only — Rust's socket-address parser rejects non-ASCII, and `Char.isDigit`/`toInt`
    // would otherwise accept Unicode decimal digits (e.g. Arabic-Indic) that the engine won't.
    def asciiDigit(c: Char): Boolean = c >= '0' && c <= '9'
    def isIpv4: Boolean =
      val parts = host.split("\\.", -1)
      parts.length == 4 && parts.forall { p =>
        p.nonEmpty && p.length <= 3 && p.forall(asciiDigit) && p.toInt <= 255 && (p == "0" || !p.startsWith("0"))
      }
    def isIpv6: Boolean =
      host.contains(":") && host.forall { c =>
        asciiDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c == ':' || c == '.'
      }
    isIpv4 || isIpv6

  /**
   * Build the rift intercept-rule JSON from a portable [[InterceptRule]]. Left
   * on a Redirect whose target space has no port in its `baseUri`.
   */
  private[embedded] def ruleJson(rule: InterceptRule): Either[MockError, String] =
    rule match
      case InterceptRule.Redirect(host, space) =>
        portOf(space.baseUri).map { port =>
          Json
            .Obj("host" -> Json.Str(host), "action" -> Json.Obj("forward" -> Json.Obj("port" -> Json.Num(port))))
            .toJson
        }
      case InterceptRule.Serve(host, stub) =>
        val headers = Json.Obj(stub.headers.map((k, v) => k -> Json.Str(v)).toSeq*)
        val serve = Json.Obj(
          "statusCode" -> Json.Num(stub.status),
          "headers"    -> headers,
          "body"       -> stub.body.fold[Json](Json.Null)(Json.Str(_))
        )
        Right(Json.Obj("host" -> Json.Str(host), "action" -> Json.Obj("serve" -> serve)).toJson)

  private def portOf(baseUri: String): Either[MockError, Int] =
    scala.util
      .Try(java.net.URI.create(baseUri).getPort)
      .toOption
      .filter(_ > 0)
      .toRight(MockError.InvalidDefinition(s"intercept redirect target has no port in its baseUri: $baseUri"))
