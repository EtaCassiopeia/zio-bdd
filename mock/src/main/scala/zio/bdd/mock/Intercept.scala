package zio.bdd.mock

import zio.{IO, ZIO}

import java.nio.file.{Files, Path}
import java.security.KeyStore
import java.security.cert.X509Certificate
import javax.net.ssl.{TrustManagerFactory, X509TrustManager}
import scala.util.Using

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

  /** The JCE keystore type for `KeyStore.getInstance` (uppercase). */
  def keystoreName: String = this match
    case Pkcs12 => "PKCS12"
    case Jks    => "JKS"

/**
 * A ready-to-use truststore file containing the intercept CA, plus the password
 * that opens it. Hand both to the SUT's JVM so it trusts the intercept's
 * per-host leaf certificates: `-Djavax.net.ssl.trustStore=<path>
 * -Djavax.net.ssl.trustStorePassword=<password>`.
 */
final case class TrustStore(path: Path, password: String, format: TrustStoreFormat)

object TrustStore:
  /**
   * The file a truststore export writes to. `None` → a fresh temp file (removed
   * on JVM exit); `Some(p)` → `p` itself, with its parent directories created —
   * a caller-chosen path, e.g. a host dir bind-mounted into a containerized SUT
   * (#263). Does file-system work, so call it inside a blocking effect.
   */
  def exportPath(to: Option[Path], format: TrustStoreFormat): Path =
    to match
      case Some(p) =>
        Option(p.getParent).foreach(Files.createDirectories(_))
        p
      case None =>
        val tmp = Files.createTempFile("rift-intercept-", s".${format.wire}")
        tmp.toFile.deleteOnExit()
        tmp

  /**
   * Return a copy of `caOnly` whose store ALSO contains the running JVM's
   * default trust anchors (its `cacerts`). Pointing a SUT at the CA-only export
   * via `-Djavax.net.ssl.trustStore` *replaces* the JVM default store, so the
   * SUT would trust the intercept CA but lose trust for every real HTTPS host;
   * the merged store keeps both. Preserves `caOnly`'s format + password. Writes
   * a fresh temp file, or `to` when given (see [[exportPath]]). Backend-neutral
   * — pure `java.security`, no adapter types.
   */
  def plusSystemCAs(caOnly: TrustStore, to: Option[Path] = None): IO[MockError, TrustStore] =
    ZIO.attemptBlocking {
      // The whole point is to ADD the JVM defaults; an empty set means a misconfigured/empty cacerts
      // and a merged store that is silently CA-only — fail loudly rather than degrade to that.
      val anchors = systemTrustAnchors
      if anchors.isEmpty then
        throw new IllegalStateException(
          "no default JVM trust anchors resolved (empty cacerts?); the merged truststore would trust only the intercept CA"
        )

      val ksType  = caOnly.format.keystoreName
      val caStore = KeyStore.getInstance(ksType)
      Using.resource(Files.newInputStream(caOnly.path))(in => caStore.load(in, caOnly.password.toCharArray))

      val merged = KeyStore.getInstance(ksType)
      merged.load(null, null)
      // Index-keyed aliases: system anchors can share a subject, so a subject-derived alias would collide.
      anchors.zipWithIndex.foreach((cert, i) => merged.setCertificateEntry(s"system-ca-$i", cert))
      val aliases = caStore.aliases()
      while aliases.hasMoreElements do
        val alias = aliases.nextElement()
        Option(caStore.getCertificate(alias)).foreach(cert => merged.setCertificateEntry(s"intercept-$alias", cert))

      val out = exportPath(to, caOnly.format)
      Using.resource(Files.newOutputStream(out))(os => merged.store(os, caOnly.password.toCharArray))
      TrustStore(out, caOnly.password, caOnly.format)
    }
      // A local java.security failure, not a backend round-trip — ProvisionFailed, per the package's
      // error taxonomy (CommunicationError is reserved for backend comms).
      .mapError(t =>
        MockError.ProvisionFailed(
          s"merging the intercept CA with the JVM default trust anchors: ${Option(t.getMessage).getOrElse(t.getClass.getSimpleName)}"
        )
      )

  // The running JVM's default trust anchors (its cacerts), read via the platform TrustManager.
  private def systemTrustAnchors: Vector[X509Certificate] =
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(null: KeyStore)
    tmf.getTrustManagers.collect { case x: X509TrustManager => x.getAcceptedIssuers }.flatten.toVector

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
