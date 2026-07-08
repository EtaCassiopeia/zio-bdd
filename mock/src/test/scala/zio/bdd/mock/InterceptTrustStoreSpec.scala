package zio.bdd.mock

import zio.*
import zio.test.*

import java.io.ByteArrayInputStream
import java.nio.file.Files
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import javax.net.ssl.{TrustManagerFactory, X509TrustManager}

/**
 * Always-on unit coverage for [[TrustStore.plusSystemCAs]] (#259): the pure
 * java.security merge that adds the JVM default trust anchors to the intercept
 * CA-only store. Runs on any host — no native library — using a synthetic
 * (non-system) self-signed CA as the stand-in intercept CA.
 */
object InterceptTrustStoreSpec extends ZIOSpecDefault:

  // A throwaway self-signed cert (CN=zio-bdd-intercept-test-ca) — a plausible intercept CA that is
  // guaranteed NOT to be one of the JVM's real trust anchors, so the merge is genuinely additive.
  private val testCaPem: String =
    """|-----BEGIN CERTIFICATE-----
       |MIIDKTCCAhGgAwIBAgIULNSwCU4uf6H4c3MUW69w2kgrElowDQYJKoZIhvcNAQEL
       |BQAwJDEiMCAGA1UEAwwZemlvLWJkZC1pbnRlcmNlcHQtdGVzdC1jYTAeFw0yNjA3
       |MDgwMzQzMTJaFw0zNjA3MDUwMzQzMTJaMCQxIjAgBgNVBAMMGXppby1iZGQtaW50
       |ZXJjZXB0LXRlc3QtY2EwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDw
       |QAAYQdIW1kcSdSv7WbyeO6J+2IO5ou2nRIM1CxigvS5NjrhWShCWhaGOkj3SCAOm
       |Lqa4KIcErUvEJrSE6Fuom/2ZXS8pjSexk4ziL4a8V+wTwUIdS3r4E9prxXb+PkTx
       |IU/nkCjtHBfTgL0aub1jAHvJoUsnEaAbEvYg2A2t0NYkFqF16ZTpJF96Edrc8OhU
       |k1VoTxq/DwptqLRcvIOE/gzrU3H4T17fqY6faahNueCePvOsbFYMTTlHlAwrp/GZ
       |JZ7+4LjmwkHhzbMr7YYJWaO7xoJTCxgGXRlQShW84tulk2yARWsUk4AydJQ40dSI
       |v7g+b+q/mcaGho4Pp7KnAgMBAAGjUzBRMB0GA1UdDgQWBBTyVD4dwsvzUcPvv2RD
       |5sNoe86xyDAfBgNVHSMEGDAWgBTyVD4dwsvzUcPvv2RD5sNoe86xyDAPBgNVHRMB
       |Af8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQCAQaNXkxIVTIeA20eqpaxTjv9u
       |BnoRyYmjiBKjHXrXfl0LmugOwBgwVLBbb0FWeBlbUAryEo7bVqq1iXpZAbAoLdbM
       |NSArEWT9cLCpRM7kKF1f+nyAbfzrPkmk20RMYpTFvXghO+W37aVEvXWABnI5XNmP
       |Glg6P7jvUNj0wl/RUYUYZwofeYlG9Nihb6hcdGUnyLE+zjLsTAEn0H6TdFNRTTPJ
       |cdAC9HqlUzW0Iolzedilal8/HpdKsl3BVGcMUkV9mqa15gnbbXIv8aJ8O2FQd21+
       |0kqVJayZKMdjLHBDBnOYyqm+QIouPNWhP0P6ewXvFE/iHNcT+3xWjsPL7U2F
       |-----END CERTIFICATE-----""".stripMargin

  private val password = "changeit"

  private def parseCert(pem: String): X509Certificate =
    CertificateFactory
      .getInstance("X.509")
      .generateCertificate(new ByteArrayInputStream(pem.getBytes("UTF-8")))
      .asInstanceOf[X509Certificate]

  // A synthetic CA-only truststore file in `format`, containing exactly the test CA — the shape
  // `Intercept.trustStore(format)` produces, without needing a running intercept engine.
  private def caOnlyStore(format: TrustStoreFormat): Task[TrustStore] =
    ZIO.attemptBlocking {
      val ks = KeyStore.getInstance(format.keystoreName)
      ks.load(null, null)
      ks.setCertificateEntry("intercept-ca", parseCert(testCaPem))
      val p = Files.createTempFile("test-ca-only-", s".${format.wire}")
      p.toFile.deleteOnExit()
      val os = Files.newOutputStream(p)
      try ks.store(os, password.toCharArray)
      finally os.close()
      TrustStore(p, password, format)
    }

  private def certsOf(ts: TrustStore): Task[Set[X509Certificate]] =
    ZIO.attemptBlocking {
      import scala.jdk.CollectionConverters.*
      val ks = KeyStore.getInstance(ts.format.keystoreName)
      val in = Files.newInputStream(ts.path)
      try ks.load(in, ts.password.toCharArray)
      finally in.close()
      ks.aliases().asScala.flatMap(a => Option(ks.getCertificate(a))).collect { case x: X509Certificate => x }.toSet
    }

  private val systemAnchors: Task[Set[X509Certificate]] =
    ZIO.attemptBlocking {
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(null: KeyStore)
      tmf.getTrustManagers.collect { case x: X509TrustManager => x.getAcceptedIssuers.toSet }.flatten.toSet
    }

  // The trust anchors a real JSSE TrustManagerFactory derives from the store — i.e. what the store
  // would actually validate against — not just its raw keystore entries.
  private def acceptedIssuersOf(ts: TrustStore): Task[Set[X509Certificate]] =
    ZIO.attemptBlocking {
      val ks = KeyStore.getInstance(ts.format.keystoreName)
      val in = Files.newInputStream(ts.path)
      try ks.load(in, ts.password.toCharArray)
      finally in.close()
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(ks)
      tmf.getTrustManagers.collect { case x: X509TrustManager => x.getAcceptedIssuers.toSet }.flatten.toSet
    }

  private def mergeTest(format: TrustStoreFormat) =
    test(s"plusSystemCAs(${format.wire}) keeps the intercept CA and adds every JVM default trust anchor") {
      for
        caOnly  <- caOnlyStore(format)
        merged  <- TrustStore.plusSystemCAs(caOnly)
        certs   <- certsOf(merged)
        issuers <- acceptedIssuersOf(merged)
        system  <- systemAnchors
        testCa   = parseCert(testCaPem)
      yield assertTrue(
        merged.format == format,            // format preserved
        merged.password == caOnly.password, // password preserved
        merged.path != caOnly.path,         // fresh file, original untouched
        certs.contains(testCa),             // the intercept CA survived the merge
        system.nonEmpty,                    // sanity: the JVM actually has default anchors
        !system.contains(testCa),           // the test CA is genuinely non-system → the merge is additive
        system.subsetOf(certs),             // every JVM default anchor is present in the merged store
        // …and the store functions as a real JSSE trust source over BOTH the intercept CA and the defaults:
        issuers.contains(testCa),
        system.subsetOf(issuers)
      )
    }

  // plusSystemCAs(to = Some(path)) writes to that exact path, creating parent dirs, JVM-loadable (#263).
  private def toPathTest(format: TrustStoreFormat) =
    test(s"plusSystemCAs(${format.wire}, to = Some(path)) writes to the given path (parent dirs created)") {
      for
        caOnly <- caOnlyStore(format)
        base   <- ZIO.attemptBlocking(Files.createTempDirectory("rift-mnt"))
        // a parent dir that does NOT exist yet — plusSystemCAs must create it
        target  = base.resolve("sub").resolve(s"intercept.${format.wire}")
        merged <- TrustStore.plusSystemCAs(caOnly, to = Some(target))
        certs  <- certsOf(merged)
        system <- systemAnchors
        exists <- ZIO.attemptBlocking(Files.exists(target))
      yield assertTrue(
        merged.path == target, // returned at the exact requested path
        exists,                // parent dir was created and the file written
        certs.contains(parseCert(testCaPem)),
        system.subsetOf(certs) // JVM-loadable merged store at the mounted path
      )
    }

  // A stub Intercept whose only real method is `trustStore` (recording the `to` it receives) — to unit-test
  // the trait DEFAULT `trustStoreWithSystemCAs` threading, with no native library (#263).
  private def stubIntercept(seen: Ref[Option[Option[java.nio.file.Path]]]): Intercept =
    new Intercept:
      def proxyPort: IO[MockError, Int]                 = ZIO.succeed(0)
      def add(rule: InterceptRule): IO[MockError, Unit] = ZIO.unit
      def trustStore(format: TrustStoreFormat, to: Option[java.nio.file.Path]): IO[MockError, TrustStore] =
        seen.set(Some(to)) *> caOnlyStore(format).mapError(t =>
          MockError.ProvisionFailed(Option(t.getMessage).getOrElse(t.getClass.getSimpleName))
        )

  def spec = suite("TrustStore.plusSystemCAs (#259)")(
    mergeTest(TrustStoreFormat.Pkcs12),
    mergeTest(TrustStoreFormat.Jks),
    toPathTest(TrustStoreFormat.Pkcs12),
    toPathTest(TrustStoreFormat.Jks),
    test(
      "trustStoreWithSystemCAs(to = Some(p)) applies `to` at the merge step; the inner CA export stays default (#263)"
    ) {
      for
        seen  <- Ref.make(Option.empty[Option[java.nio.file.Path]])
        base  <- ZIO.attemptBlocking(Files.createTempDirectory("rift-mnt"))
        target = base.resolve("out").resolve("merged.p12") // parent 'out' does not exist yet
        merged <-
          stubIntercept(seen).trustStoreWithSystemCAs(to = Some(target)).mapError(e => new RuntimeException(e.toString))
        inner  <- seen.get
        certs  <- certsOf(merged)
        system <- systemAnchors
      yield assertTrue(
        inner == Some(None),   // the trait default calls the inner CA export with to=None, NOT the merged target
        merged.path == target, // `to` is applied only at the merge write → merged store at the requested path
        certs.contains(parseCert(testCaPem)),
        system.subsetOf(certs) // JVM-loadable merged store
      )
    }
  )
