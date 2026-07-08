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
  started: Ref.Synchronized[Option[(String, Int)]],
  config: EmbeddedRift.InterceptConfig
) extends Intercept:

  def proxyPort: IO[MockError, Int] = ensureStarted.map(_._2)

  // Report the engine's actual bound endpoint: the host parsed from `rift_start_intercept`'s interceptUrl
  // (ground truth since rift#425), paired with interceptPort. For the common loopback / 0.0.0.0 binds this
  // equals the requested `config.bindHost`; for a specific-NIC bind it surfaces the interface the engine bound.
  override def proxyEndpoint: IO[MockError, (String, Int)] = ensureStarted

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

  // Start the listener at most once (race-safe), memoizing its bound endpoint (host, port). Validate the
  // bind host first so a hostname fails with a clear message here, not opaquely inside the engine (#262).
  private def ensureStarted: IO[MockError, (String, Int)] =
    started.modifyZIO {
      case s @ Some(ep) => ZIO.succeed((ep, s))
      case None =>
        ZIO.fromEither(EmbeddedIntercept.validateBindHost(config.bindHost)) *>
          ZIO.fromEither(EmbeddedIntercept.validateCaPair(config)) *>
          engine
            .startIntercept(EmbeddedIntercept.startOptions(config))
            .flatMap { i =>
              EmbeddedIntercept
                .hostOf(i.interceptUrl)
                // A host-less / unparseable interceptUrl (the class of bug rift#425 fixed) is not fatal: fall
                // back to the requested bindHost so the endpoint never regresses. Log it — the memoized result
                // locks the fallback in for the listener's life, so a silent degrade would be hard to diagnose.
                .fold(
                  ZIO
                    .logWarning(
                      s"rift_start_intercept returned interceptUrl '${i.interceptUrl}' with no usable host; " +
                        s"reporting the requested bindHost '${config.bindHost}'"
                    )
                    .as(config.bindHost)
                )(ZIO.succeed(_))
                .map { host =>
                  val ep = (host, i.interceptPort)
                  (ep, Some(ep))
                }
            }
    }

  private def message(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)

private[embedded] object EmbeddedIntercept:
  private val DefaultPassword: String = "changeit"

  // Start options for the TLS-MITM listener. `bindHost` defaults to loopback (127.0.0.1); a wider bind
  // (0.0.0.0 / a NIC address) makes the proxy reachable from another host or container. `port` defaults
  // to 0 (OS-assigned); a pinned value binds a known port for a SUT whose proxy target is fixed up front.
  // `caCertPath`/`caKeyPath` (both-or-neither) load a persistent caller CA (#273) — emitted only when both
  // are set (rift's InterceptOptions uses deny_unknown_fields, so only known keys may appear).
  private[embedded] def startOptions(config: EmbeddedRift.InterceptConfig): String =
    val base = List("host" -> Json.Str(config.bindHost), "port" -> Json.Num(config.port.getOrElse(0)))
    val ca = (config.caCert, config.caKey) match
      case (Some(cert), Some(key)) =>
        List("caCertPath" -> Json.Str(cert.toString), "caKeyPath" -> Json.Str(key.toString))
      case _ => Nil
    Json.Obj((base ++ ca)*).toJson

  // Both-or-neither: a lone caCert/caKey is a misconfiguration (rift needs the pair to load a CA). Reject
  // it early with a clear message rather than silently dropping to an ephemeral CA / an opaque engine error.
  private[embedded] def validateCaPair(config: EmbeddedRift.InterceptConfig): Either[MockError, Unit] =
    (config.caCert, config.caKey) match
      case (Some(_), None) | (None, Some(_)) =>
        Left(MockError.InvalidDefinition("intercept caCert and caKey must be set together (both or neither)"))
      case _ => Right(())

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

  // Parse the reachable host from the engine's reported interceptUrl (ground truth since rift#425); None
  // for a malformed URL or one without a host (the caller then falls back to the requested bindHost).
  private[embedded] def hostOf(interceptUrl: String): Option[String] =
    scala.util
      .Try(java.net.URI.create(interceptUrl).getHost)
      .toOption
      .flatMap(Option(_))
      .filter(_.nonEmpty)

  /**
   * Build the rift intercept-rule JSON from a portable [[InterceptRule]]. Left
   * on a Redirect whose target space has no port in its `baseUri`. Delegates to
   * the backend-neutral [[zio.bdd.mock.rift.InterceptRuleJson]] (#253), shared
   * with the container adapter — both drive the same wire shape.
   */
  private[embedded] def ruleJson(rule: InterceptRule): Either[MockError, String] =
    zio.bdd.mock.rift.InterceptRuleJson.ruleJson(rule)
