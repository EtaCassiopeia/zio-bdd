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
  started: Ref.Synchronized[Option[Int]]
) extends Intercept:

  def proxyPort: IO[MockError, Int] = ensureStarted

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
        engine.startIntercept(EmbeddedIntercept.StartOptions).map(i => (i.interceptPort, Some(i.interceptPort)))
    }

  private def message(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)

private[embedded] object EmbeddedIntercept:
  // Loopback, OS-assigned port — the listener binds where the SUT proxies through.
  private val StartOptions: String    = Json.Obj("host" -> Json.Str("127.0.0.1"), "port" -> Json.Num(0)).toJson
  private val DefaultPassword: String = "changeit"

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
