package zio.bdd.mock

import java.nio.file.Path

/**
 * Portable value types for the [[Capability.Intercept]] capability (#219): a
 * built-in HTTPS intercept / TLS-MITM forward-proxy that redirects a hard-coded
 * external host to a mock, so a suite can drop an external mitmproxy container.
 * Backend-neutral — no rift (or any adapter) types leak in, so a different
 * backend could implement `Intercept` over its own proxy features.
 */

/**
 * The truststore format the SUT's JVM consumes
 * (`-Djavax.net.ssl.trustStoreType`).
 */
enum TrustStoreFormat:
  case Pkcs12, Jks

  /** The lowercase wire token an adapter passes to its backend. */
  def wire: String = this match
    case Pkcs12 => "pkcs12"
    case Jks    => "jks"

/**
 * A ready-to-use truststore file containing the intercept CA, plus the password
 * that opens it. Hand both to the SUT's JVM so it trusts the intercept's
 * per-host leaf certificates: `-Djavax.net.ssl.trustStore=<path>
 * -Djavax.net.ssl.trustStorePassword=<password>`.
 */
final case class TrustStore(path: Path, password: String, format: TrustStoreFormat)

/**
 * An inline response served for an intercepted host (the `respondWith` action).
 */
final case class InterceptStub(
  status: Int = 200,
  headers: Map[String, String] = Map.empty,
  body: Option[String] = None
)

/**
 * A portable intercept rule: match an intercepted HTTPS `host` and either
 * forward it to a mock [[MockSpace]] or answer it inline with a
 * [[InterceptStub]]. Build one with the DSL —
 * `dsl.intercept(host).redirectTo(space)` / `.respondWith(stub)` — and apply it
 * via [[Intercept.add]] (or the `redirectTo`/`respondWith` convenience
 * methods).
 */
enum InterceptRule:
  /** Intercept `host` and forward its requests to `to`'s imposter. */
  case Redirect(host: String, to: MockSpace)

  /** Intercept `host` and answer its requests inline with `stub`. */
  case Serve(host: String, stub: InterceptStub)
