package zio.bdd.mock

import zio.*

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, KeyStore, PrivateKey, SecureRandom, Signature}
import java.util.Base64
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * A server identity for an HTTPS mock space (#343): the PEM certificate the
 * space presents (the leaf first, optionally followed by its chain) and the
 * matching private key as an unencrypted PKCS#8 PEM (`-----BEGIN PRIVATE
 * KEY-----`). Supplied by the caller: zio-bdd never generates certificates.
 */
final case class TlsMaterial(certPem: String, keyPem: String)

/** Whether an HTTPS mock space demands a client certificate (mTLS). */
enum ClientAuth:
  /** Any client may connect; no certificate is requested. */
  case Off

  /**
   * Every client must present a certificate chaining to one of these PEM CA
   * certificates; any other client — or one presenting none — is refused at the
   * handshake.
   */
  case Required(trustedCaPems: NonEmptyChunk[String])

/**
 * The TLS settings of an HTTPS mock space (#343), set on a [[MockSpec]]
 * (`dsl.https` / `dsl.mutualTls`). Requires [[Capability.Tls]]. A TLS space is
 * always its own listener (TLS is per-server), so even under Correlated
 * isolation it gets a dedicated server/imposter with `inject = identity`.
 *
 * The SUT must trust the space's certificate: build its client with
 * [[Tls.trust]] (or [[Tls.clientIdentity]] for mTLS) and hand it to
 * [[SutClient.make]]. Configuring the real SUT's trust store is the caller's
 * job.
 */
final case class Tls(server: TlsMaterial, clientAuth: ClientAuth = ClientAuth.Off)

object Tls:

  /**
   * A client `SSLContext` trusting exactly the given PEM CA certificates (not
   * the JVM defaults). Fails with [[MockError.InvalidDefinition]] if any PEM
   * holds no parsable certificate.
   */
  def trust(caPem: String, moreCaPems: String*): IO[MockError, SSLContext] =
    context(None, caPem +: moreCaPems)

  /**
   * A client `SSLContext` that presents `certPem`/`keyPem` to an mTLS space and
   * trusts exactly the given CA certificates. The key must be unencrypted
   * PKCS#8 and match the certificate; anything else fails with
   * [[MockError.InvalidDefinition]].
   */
  def clientIdentity(certPem: String, keyPem: String, caPem: String, moreCaPems: String*): IO[MockError, SSLContext] =
    context(Some(TlsMaterial(certPem, keyPem)), caPem +: moreCaPems)

  /**
   * Check every PEM in `tls` parses (and the key matches its certificate), so a
   * malformed spec fails at provisioning with the same typed error on every
   * backend.
   */
  private[mock] def validate(tls: Tls): Either[String, Unit] =
    for
      _ <- keyStore(tls.server, contextPassword).left.map(e => s"TLS server identity: $e")
      _ <- tls.clientAuth match
             case ClientAuth.Off           => Right(())
             case ClientAuth.Required(cas) => trustStore(cas.toList).left.map(e => s"TLS client CA: $e")
    yield ()

  /**
   * An in-memory PKCS#12 key store holding `material` under the alias `mock`.
   */
  private[mock] def keyStore(material: TlsMaterial, password: Array[Char]): Either[String, KeyStore] =
    for
      chain <- certificates(material.certPem)
      key   <- privateKey(material.keyPem, chain.head)
      store <- guard("building the key store") {
                 val ks = KeyStore.getInstance("PKCS12")
                 ks.load(null, null)
                 ks.setKeyEntry("mock", key, password, chain.toArray)
                 ks
               }
    yield store

  /** An in-memory PKCS#12 trust store holding every certificate in `caPems`. */
  private[mock] def trustStore(caPems: Seq[String]): Either[String, KeyStore] =
    for
      anchors <- caPems.foldLeft[Either[String, List[X509Certificate]]](Right(Nil)) { (acc, pem) =>
                   acc.flatMap(sofar => certificates(pem).map(sofar ++ _))
                 }
      store <- guard("building the trust store") {
                 val ts = KeyStore.getInstance("PKCS12")
                 ts.load(null, null)
                 anchors.zipWithIndex.foreach((c, i) => ts.setCertificateEntry(s"ca-$i", c))
                 ts
               }
    yield store

  private def context(identity: Option[TlsMaterial], caPems: Seq[String]): IO[MockError, SSLContext] =
    ZIO
      .fromEither(
        for
          ts <- trustStore(caPems).left.map(e => s"trusted CA: $e")
          ks <- identity match
                  case None    => Right(None)
                  case Some(m) => keyStore(m, contextPassword).map(Some(_)).left.map(e => s"client identity: $e")
          ctx <- guard("initialising the SSLContext") {
                   val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
                   tmf.init(ts)
                   val kms = ks.map { store =>
                     val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
                     kmf.init(store, contextPassword)
                     kmf.getKeyManagers
                   }
                   val c = SSLContext.getInstance("TLS")
                   c.init(kms.orNull, tmf.getTrustManagers, null)
                   c
                 }
        yield ctx
      )
      .mapError(MockError.InvalidDefinition(_))

  // The key store never leaves this process; the password only satisfies the JDK API.
  private val contextPassword: Array[Char] = "zio-bdd".toCharArray

  private val certMarker = "-----BEGIN CERTIFICATE-----"
  private val keyBegin   = "-----BEGIN PRIVATE KEY-----"
  private val keyEnd     = "-----END PRIVATE KEY-----"
  // Recognisable non-PKCS#8 key encodings, refused with a conversion hint rather than a KeyFactory error.
  private val otherKeyHeaders =
    List("RSA PRIVATE KEY", "EC PRIVATE KEY", "DSA PRIVATE KEY", "ENCRYPTED PRIVATE KEY", "OPENSSH PRIVATE KEY")

  private def certificates(pem: String): Either[String, List[X509Certificate]] =
    if !pem.contains(certMarker) then Left(s"no PEM certificate found (expected a $certMarker block)")
    else
      guard("parsing the PEM certificate") {
        CertificateFactory
          .getInstance("X.509")
          .generateCertificates(ByteArrayInputStream(pem.getBytes(StandardCharsets.US_ASCII)))
          .asScala
          .toList
          .collect { case c: X509Certificate => c }
      }.flatMap(cs => if cs.isEmpty then Left("the PEM holds no X.509 certificate") else Right(cs))

  private def privateKey(pem: String, cert: X509Certificate): Either[String, PrivateKey] =
    val begin = pem.indexOf(keyBegin)
    val end   = pem.indexOf(keyEnd)
    if begin < 0 || end < begin then
      otherKeyHeaders.find(h => pem.contains(s"-----BEGIN $h-----")) match
        case Some(h) =>
          Left(
            s"the private key is a '$h' block; it must be an unencrypted PKCS#8 key ($keyBegin) — " +
              "convert it with `openssl pkcs8 -topk8 -nocrypt -in key.pem`"
          )
        case None => Left(s"no PEM private key found (expected an unencrypted PKCS#8 $keyBegin block)")
    else
      val algorithm = cert.getPublicKey.getAlgorithm
      for
        der <- guard("decoding the private key") {
                 Base64.getMimeDecoder.decode(pem.substring(begin + keyBegin.length, end).trim)
               }
        key <- guard(s"parsing the PKCS#8 $algorithm private key") {
                 KeyFactory.getInstance(algorithm).generatePrivate(PKCS8EncodedKeySpec(der))
               }
        _ <- matches(key, cert)
      yield key

  // Prove the key belongs to the certificate by signing with one and verifying with the other; a
  // mismatch would otherwise start fine and only fail every handshake.
  private def matches(key: PrivateKey, cert: X509Certificate): Either[String, Unit] =
    signatureAlgorithm(key.getAlgorithm) match
      // Fail closed: a key whose match can't be proven would only surface as failed handshakes.
      case None => Left(s"unsupported private key algorithm '${key.getAlgorithm}' (use an RSA, EC or Ed25519 key)")
      case Some(alg) =>
        guard("checking the private key against the certificate") {
          val probe = new Array[Byte](32)
          SecureRandom().nextBytes(probe)
          val signer = Signature.getInstance(alg)
          signer.initSign(key)
          signer.update(probe)
          val sig      = signer.sign()
          val verifier = Signature.getInstance(alg)
          verifier.initVerify(cert.getPublicKey)
          verifier.update(probe)
          verifier.verify(sig)
        }.flatMap(ok => if ok then Right(()) else Left("the private key does not match the certificate"))

  private def signatureAlgorithm(keyAlgorithm: String): Option[String] = keyAlgorithm match
    case "RSA"               => Some("SHA256withRSA")
    case "EC"                => Some("SHA256withECDSA")
    case "Ed25519" | "EdDSA" => Some("Ed25519")
    case _                   => None

  private def guard[A](action: String)(thunk: => A): Either[String, A] =
    try Right(thunk)
    catch
      case NonFatal(e) =>
        Left(s"$action failed: ${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("<no message>")}")
