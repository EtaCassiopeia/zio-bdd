package zio.bdd.mock.rift.embedded

import zio.*
import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.Files

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

  def trustStore(format: TrustStoreFormat): IO[MockError, TrustStore] =
    for
      _ <- ensureStarted
      tmp <- ZIO.attemptBlocking {
               val p = Files.createTempFile("rift-intercept-", s".${format.wire}")
               p.toFile.deleteOnExit()
               p
             }
               .mapError(t => MockError.CommunicationError(s"creating intercept truststore temp file: ${message(t)}"))
      _ <- engine.interceptExportTruststore(format.wire, EmbeddedIntercept.DefaultPassword, tmp.toString)
    yield TrustStore(tmp, EmbeddedIntercept.DefaultPassword, format)

  // Start the listener at most once (race-safe), memoizing its bound proxy port.
  private def ensureStarted: IO[MockError, Int] =
    started.modifyZIO {
      case s @ Some(p) => ZIO.succeed((p, s))
      case None =>
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

  /**
   * Build the rift intercept-rule JSON from a portable [[InterceptRule]]. Left
   * on a Redirect whose target space has no port in its `baseUri`. Delegates to
   * the backend-neutral [[zio.bdd.mock.rift.InterceptRuleJson]] (#253), shared
   * with the container adapter — both drive the same wire shape.
   */
  private[embedded] def ruleJson(rule: InterceptRule): Either[MockError, String] =
    zio.bdd.mock.rift.InterceptRuleJson.ruleJson(rule)
