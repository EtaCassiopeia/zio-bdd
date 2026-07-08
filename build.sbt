import xerial.sbt.Sonatype.sonatypeCentralHost
import StepGeneratorPlugin.autoImport.*
import com.typesafe.tools.mima.core.*

import java.net.{Authenticator, PasswordAuthentication, URI}
import java.nio.file.{Files => JFiles, StandardCopyOption}
import java.security.MessageDigest

// ---- librift_ffi native library packaging (#134) -------------------------------------------
// The embedded adapter loads librift_ffi from the classpath: the zio-bdd-rift-embedded-natives jar
// bundles the per-platform cdylibs as resources (native/<os>-<arch>/librift_ffi.<ext>), downloaded
// + checksum-verified from the Rift release at build time. The runtime loader extracts the host's
// lib to a temp file and loads it via Panama — no manual install, no system property. (FFM is a
// preview API on JDK 21, so the embedded code stays JDK-21-gated; see embeddedNativeSettings.)
// NOTE: build.sbt is compiled with the Scala 2.12 dialect — keep these blocks 2.12-compatible.

// Single source of truth for the pinned Rift release (#195). Both the FFI natives (riftNativesVersion,
// downloaded into the embedded-natives jar) and the container image tag (Rift.DefaultImage, via the
// generated RiftBuildInfo below) derive from it, so a version bump touches exactly one line.
val riftVersion        = "0.11.1"
val riftNativesVersion = riftVersion

// The (os, arch, ext) cdylibs bundled into the natives jar — linux/macOS × x86_64/aarch64 (#134).
val riftNativeTriples: Seq[(String, String, String)] = Seq(
  ("linux", "x86_64", "so"),
  ("linux", "aarch64", "so"),
  ("darwin", "x86_64", "dylib"),
  ("darwin", "aarch64", "dylib")
)

def sha256Hex(f: File): String =
  MessageDigest.getInstance("SHA-256").digest(JFiles.readAllBytes(f.toPath)).map(b => f"$b%02x").mkString

// Base URL the natives are fetched from. Defaults to the Rift GitHub releases, overridable to an
// internal mirror (Artifactory, …) for networks that reach an internal host but not the GitHub CDN.
def riftNativesBaseUrl(version: String): String =
  sys.env
    .get("RIFT_NATIVES_BASE_URL")
    .filter(_.nonEmpty)
    .orElse(sys.props.get("rift.natives.baseUrl").filter(_.nonEmpty))
    .map(_.stripSuffix("/"))
    .getOrElse(s"https://github.com/EtaCassiopeia/rift/releases/download/v$version")

// Install a proxy Authenticator so `URL.openStream` can authenticate to an HTTPS proxy when
// `https.proxyUser`/`https.proxyPassword` are set — otherwise a proxied build fails the fetch with a
// 407. Clearing `jdk.http.auth.tunneling.disabledSchemes` is required: since 8u111 the JDK disables
// Basic auth on the CONNECT tunnel used for HTTPS-through-proxy, which is exactly this case. A plain
// def (re-installing an equivalent default Authenticator is harmless) keeps this .sbt-friendly.
def installRiftProxyAuthenticator(): Unit =
  for {
    user <- sys.props.get("https.proxyUser").filter(_.nonEmpty)
    pass <- sys.props.get("https.proxyPassword")
  } {
    System.setProperty("jdk.http.auth.tunneling.disabledSchemes", "")
    Authenticator.setDefault(new Authenticator {
      override protected def getPasswordAuthentication(): PasswordAuthentication =
        if (getRequestorType == Authenticator.RequestorType.PROXY) new PasswordAuthentication(user, pass.toCharArray)
        else null
    })
  }

// Server (mirror) credentials for an authenticated RIFT_NATIVES_BASE_URL host — the mirror itself
// answers 401 (distinct from a proxy 407, which installRiftProxyAuthenticator handles). Opt-in via
// RIFT_NATIVES_USER/RIFT_NATIVES_PASSWORD, and ONLY applied when a mirror base is configured, so
// credentials are never sent to the default GitHub CDN. Sent preemptively as Basic auth, so a
// challenge-first server and a preemptive-only one both work.
// The configured mirror base, if any (env RIFT_NATIVES_BASE_URL or sysprop rift.natives.baseUrl). The
// same selection riftNativesBaseUrl uses, minus the GitHub default — so `None` means "no mirror".
def riftMirrorBase(): Option[String] =
  sys.env
    .get("RIFT_NATIVES_BASE_URL")
    .filter(_.nonEmpty)
    .orElse(sys.props.get("rift.natives.baseUrl").filter(_.nonEmpty))
    .map(_.stripSuffix("/"))

def riftMirrorAuthHeader(): Option[String] =
  // Only when a mirror is configured — credentials are never produced for the default GitHub CDN. Both
  // user and pass must be non-empty, so a blank env var (e.g. an unresolved CI secret) disables auth and
  // surfaces as a loud 401 rather than a silent `user:` credential.
  if (riftMirrorBase().isEmpty) None
  else
    for {
      user <- sys.env.get("RIFT_NATIVES_USER").filter(_.nonEmpty)
      pass <- sys.env.get("RIFT_NATIVES_PASSWORD").filter(_.nonEmpty)
    } yield "Basic " + java.util.Base64.getEncoder.encodeToString(s"$user:$pass".getBytes("UTF-8"))

// Open `url` for reading. With no mirror auth this is a plain openStream (auto-following redirects) — the
// default GitHub fetch is byte-for-byte unchanged. With mirror auth we follow redirects MANUALLY and
// re-attach the Basic header ONLY while the host matches the mirror host: the JDK would otherwise forward
// `Authorization` across a cross-host redirect (a mirror 302 to a blob store), leaking the credential.
// (For an authenticated mirror, prefer an https:// base so the header isn't sent in cleartext.)
def openRiftStream(url: String): java.io.InputStream =
  riftMirrorAuthHeader() match {
    case None => URI.create(url).toURL.openStream()
    case Some(header) =>
      val mirrorHost = riftMirrorBase().map(b => URI.create(b).toURL.getHost)
      def open(u: java.net.URL, hops: Int): java.io.InputStream = {
        if (hops > 5) sys.error(s"[rift-ffi] too many redirects fetching $url")
        val conn = u.openConnection().asInstanceOf[java.net.HttpURLConnection]
        conn.setInstanceFollowRedirects(false)
        if (mirrorHost.contains(u.getHost)) conn.setRequestProperty("Authorization", header)
        val code = conn.getResponseCode
        if (code >= 300 && code < 400) {
          val loc = conn.getHeaderField("Location")
          conn.disconnect()
          if (loc == null) sys.error(s"[rift-ffi] redirect ($code) with no Location fetching $u")
          open(u.toURI.resolve(loc).toURL, hops + 1)
        } else conn.getInputStream // 2xx → stream; 4xx/5xx → getInputStream throws (loud, as before)
      }
      open(URI.create(url).toURL, 0)
  }

def readRiftUrl(url: String): Array[Byte] = {
  val in = openRiftStream(url)
  try in.readAllBytes()
  finally in.close()
}

// A .sha256 sibling is "<hex>  <filename>"; take the leading digest token.
def parseRiftSha256(bytes: Array[Byte]): String = new String(bytes, "UTF-8").trim.split("\\s+").head

// Obtain `asset` (a librift_ffi cdylib) into `out`, verifying it against the expected sha256. The
// source is chosen, in order, from three opt-in escape hatches for restricted networks — the default
// (none set) is byte-for-byte the original GitHub-releases fetch:
//
//   1. RIFT_NATIVES_DIR  — an offline directory of pre-provisioned `$asset` + `$asset.sha256`; copied
//                          from disk with NO network access (air-gapped builds).
//   2. RIFT_NATIVES_BASE_URL / -Drift.natives.baseUrl — an alternate base URL (internal mirror). If the
//                          mirror itself needs credentials, set RIFT_NATIVES_USER/RIFT_NATIVES_PASSWORD
//                          (opt-in Basic auth, applied only to a configured mirror — see openRiftStream).
//   3. default — https://github.com/EtaCassiopeia/rift/releases/download/v$version.
//
// An authenticated HTTPS proxy is honored via the standard https.proxyHost/Port + https.proxyUser/
// Password sysprops (see installRiftProxyAuthenticator); an authenticated mirror host via
// RIFT_NATIVES_USER/PASSWORD (see riftMirrorAuthHeader). The `.sha256` checksum is verified on EVERY
// path — a mirror or offline dir must still match the expected digest.
// Idempotent: skips the copy/download when `out` already matches the expected checksum. Returns `out`.
def downloadRiftAsset(version: String, asset: String, out: File, log: sbt.util.Logger): File = {
  installRiftProxyAuthenticator()
  // Blank env vars are treated as unset (fall back to the network default) rather than firing the
  // offline branch — an accidentally-empty RIFT_NATIVES_DIR in CI shouldn't hard-fail the build.
  sys.env.get("RIFT_NATIVES_DIR").filter(_.nonEmpty).map(d => new File(d)) match {
    case Some(dir) =>
      // Offline: copy from disk and verify against the pre-provisioned sibling .sha256. No network.
      val src     = new File(dir, asset)
      val shaFile = new File(dir, s"$asset.sha256")
      if (!src.exists()) sys.error(s"[rift-ffi] RIFT_NATIVES_DIR=$dir is set but $asset is missing")
      if (!shaFile.exists()) sys.error(s"[rift-ffi] RIFT_NATIVES_DIR=$dir is set but $asset.sha256 is missing")
      val expected = parseRiftSha256(JFiles.readAllBytes(shaFile.toPath))
      val got      = sha256Hex(src)
      if (got != expected) sys.error(s"[rift-ffi] checksum mismatch for $asset: got $got, expected $expected")
      if (out.exists() && sha256Hex(out) == expected) {
        log.info(s"[rift-ffi] cached $asset (RIFT_NATIVES_DIR)")
      } else {
        JFiles.createDirectories(out.toPath.getParent)
        JFiles.copy(src.toPath, out.toPath, StandardCopyOption.REPLACE_EXISTING)
        log.info(s"[rift-ffi] copied $asset from RIFT_NATIVES_DIR")
      }
    case None =>
      val base     = riftNativesBaseUrl(version)
      val expected = parseRiftSha256(readRiftUrl(s"$base/$asset.sha256"))
      if (out.exists() && sha256Hex(out) == expected) {
        log.info(s"[rift-ffi] cached $asset")
      } else {
        JFiles.createDirectories(out.toPath.getParent)
        log.info(s"[rift-ffi] downloading $asset (v$version) from $base ...")
        // Download to a sibling temp file and atomically move into place only after the checksum
        // passes, so `out` never exists in a corrupt/partial state (the idempotency guard trusts it).
        val tmp = new File(out.getParentFile, s".${out.getName}.part")
        try {
          val in = openRiftStream(s"$base/$asset")
          try JFiles.copy(in, tmp.toPath, StandardCopyOption.REPLACE_EXISTING)
          finally in.close()
          val got = sha256Hex(tmp)
          if (got != expected) sys.error(s"[rift-ffi] checksum mismatch for $asset: got $got, expected $expected")
          JFiles.move(tmp.toPath, out.toPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
          log.info(s"[rift-ffi] downloaded + verified $asset")
        } finally if (tmp.exists()) tmp.delete()
      }
  }
  out
}

// The major version of the JDK building this project. FFM (java.lang.foreign) drives the embedded
// Rift provider; it is a PREVIEW API on JDK 21 (JEP 442) and FINALIZED on JDK 22 (JEP 454). Because a
// class that touches FFM is version-locked at the class-file level (a preview-21 class loads only on
// 21; a stable-22 class only on 22+), the embedded provider ships as TWO artifacts from one shared
// source (rift-embedded/src, see the bridge's reflective shim): `zio-bdd-rift-embedded-jdk21` built on
// JDK 21 and `zio-bdd-rift-embedded` built on JDK 22+. The container adapter + core stay JDK-11 and
// are unaffected — on a JDK that can't build a given variant it is simply not aggregated.
def jdkMajor: Int = {
  val raw = sys.props.getOrElse("java.specification.version", "11")
  val s   = if (raw.startsWith("1.")) raw.substring(2) else raw
  s.takeWhile(_.isDigit) match { case "" => 11; case d => d.toInt }
}
lazy val previewFfm: Boolean = jdkMajor == 21     // the JDK-21 preview variant builds/publishes here
lazy val stableFfm: Boolean  = jdkMajor >= 22     // the JDK-22+ stable variant + full stack build here
lazy val ffmAny: Boolean     = jdkMajor >= 21     // either variant → the natives jar is needed

// Forked-test-JVM flags for the embedded suites. Both variants need --enable-native-access (downgrades
// the restricted-method warning); the JDK-21 preview variant additionally needs --enable-preview to
// load its preview-compiled bridge. The native library is on the test classpath via the embeddedNatives
// test dependency (the loader extracts + loads it — no -Drift.ffi.lib needed).
lazy val embeddedTestJvmSettings: Seq[Def.Setting[_]] = Seq(
  Test / fork := true,
  Test / javaOptions += "--enable-native-access=ALL-UNNAMED"
)
lazy val embeddedPreviewTestJvmSettings: Seq[Def.Setting[_]] = Seq(
  Test / fork := true,
  Test / javaOptions ++= Seq("--enable-preview", "--enable-native-access=ALL-UNNAMED")
)

// The bundled per-platform natives (#134): a pure-resources jar that packages the librift_ffi
// cdylibs (downloaded + checksum-verified at build time) so the embedded provider loads them from
// the classpath out-of-the-box. No Scala/Java code (JDK-agnostic), published as
// `zio-bdd-rift-embedded-natives`. JDK-agnostic (same cdylibs serve both embedded variants), so the
// download is gated on any FFM-capable JDK (21+); the JDK-11 build never pulls the ~60MB natives.
// Published once, from the JDK-22 stable release job (skipped on the JDK-21 preview job).
lazy val embeddedNatives = (project in file("embedded-natives"))
  .settings(
    name             := "zio-bdd-rift-embedded-natives",
    crossPaths       := false,
    autoScalaLibrary := false,
    // The natives jar is published only by the JDK-22 job; the JDK-21 preview job skips it (avoids a
    // duplicate publish of the same coordinates).
    publish / skip := previewFfm,
    Compile / resourceGenerators ++= (
      if (!ffmAny) Nil
      else
        Seq(Def.task {
          val log = streams.value.log
          val dir = (Compile / resourceManaged).value
          riftNativeTriples.map { case (os, arch, ext) =>
            downloadRiftAsset(riftNativesVersion, s"librift_ffi-$os-$arch.$ext", dir / "native" / s"$os-$arch" / s"librift_ffi.$ext", log)
          }
        }.taskValue)
    ),
    mimaPreviousArtifacts := Set.empty
  )

inThisBuild(
  List(
    organization := "io.github.etacassiopeia",
    homepage     := Some(url("https://github.com/EtaCassiopeia/zio-bdd")),
    scalaVersion := "3.3.4",
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        id = "etacassiopeia",
        name = "Mohsen Zainalpour",
        email = "zainalpour@gmail.com",
        url = url("https://github.com/EtaCassiopeia")
      )
    ),
    sonatypeCredentialHost := sonatypeCentralHost,
    sonatypeRepository     := "https://s01.oss.sonatype.org/service/local",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:imports"
    )
  )
)

// Java-version floor for the published NON-FFM modules (#205). Without a pinned `-release`, Scala 3
// targets the build JDK's class-file version, so a release cut on JDK 22 produced class-66 bytecode
// that even JDK 21 cannot load. These modules use no Java-22 API (the container adapter + core are
// JDK-11 by design, see the FFM note above), so pin class 55 — loadable on Java 11+ — regardless of
// the JDK that cuts the release. The FFM/embedded modules are deliberately NOT given this floor: they
// are version-locked to JDK 21/22 by their own `--release` and must stay so.
lazy val javaFloorSettings: Seq[Def.Setting[_]] = Seq(
  Compile / scalacOptions += "-release:11",
  Compile / javacOptions ++= Seq("--release", "11")
)

lazy val commonDependencies = Seq(
  "dev.zio" %% "zio"                   % "2.1.17",
  "dev.zio" %% "zio-schema"            % "1.6.6",
  "dev.zio" %% "zio-schema-derivation" % "1.6.6",
  "dev.zio" %% "zio-logging"           % "2.5.0",
  "dev.zio" %% "zio-test"              % "2.1.17",
  "dev.zio" %% "zio-test-sbt"          % "2.1.17" % Test
)

// Binary-compatibility checking. The baseline is established at 1.0.0: until a 1.x is published,
// `previousStableVersion` is a 0.x tag and is filtered out (MiMa is a no-op), so the 1.0.0 release
// itself is not checked against 0.1.0. From 1.0.0 onward, 1.0.x / 1.1 are verified against the
// latest stable 1.x release.
lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := previousStableVersion.value
    // Only check against a final 1.x release — skip 0.x and pre-releases (RC/M/SNAPSHOT, which
    // contain a '-'), so the 1.0.0 release itself is not checked against 0.1.0 or 1.0.0-RCx.
    .filter(v => v.startsWith("1.") && !v.contains("-"))
    .map(organization.value %% moduleName.value % _)
    .toSet,
  // Property-based testing (#91) added new defaulted fields to `Scenario` / the internal
  // `RawExamplesBlock`, a new defaulted `genLookup` parameter on `FeatureExecutor`, and
  // replaced several `PrettyReporter.Color` case objects with `val` aliases of the same type.
  // All of these are source-compatible but not binary-compatible against 1.0.0 — expected for
  // a minor feature release; filtered rather than worked around.
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.GherkinParser#RawExamplesBlock.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.gherkin.Scenario.copy"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeature"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.FeatureExecutor.executeFeatures"),
    // #225: ScenarioResult gained a trailing `attempts: Int = 1` field (retry-tag support).
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.ScenarioResult.copy"),
    // #238: TestCaseRecord gains an `attempts` field (retry aspect in JUnit XML) — additive, defaulted.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.report.JUnitXMLFormatter#TestCaseRecord.copy"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.Yellow"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleGreen"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleCyan"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleYellow"),
    ProblemFilters.exclude[MissingFieldProblem]("zio.bdd.core.report.Color.PaleRed"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleCyan$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleGreen$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleRed$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$PaleYellow$"),
    ProblemFilters.exclude[MissingClassProblem]("zio.bdd.core.report.Color$Yellow$"),
    // #96 added a `Tag[A]` member to `TypedExtractor[A]` (a prerequisite for type-based
    // HasGen discovery in property testing, #99) so `table`/`tableWithMapping` gained a
    // `Tag[T]` context bound and `TableExtractor` gained a `Tag[List[T]]` constructor param.
    // Source-compatible, not binary-compatible against 1.0.0.
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.table"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.DefaultTypedExtractor.tableWithMapping"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.core.step.TableExtractor.this"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.TypedExtractor.tag"),
    // #97 added `resolveTemplateColumns` to `StepRegistry` — structurally matches a
    // `@property` step template (placeholders still present) against the registry to find
    // which extractor governs each `<col>`. Source-compatible, not binary-compatible against 1.0.0.
    ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.bdd.core.step.StepRegistry.resolveTemplateColumns"),
    // #108 added `focused: Boolean = false` to `BDDTestConfig`, enabling the --focused CLI flag
    // which suppresses @ignore noise from the report when running a single scenario from the IDE.
    // Binary-incompatible with 1.0.0 (case class apply/copy/constructor changed).
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("zio.bdd.BDDTestConfig.copy")
  )
)

// What the root aggregates — i.e. what `test`/`publishSigned`/`ci-release` operate on — depends on
// the build JDK, so each release job publishes exactly its slice with a standard `ci-release` (no
// per-module publish/skip juggling):
//   - JDK 22+ (stableFfm): the full stack + the stable embedded variant + the natives jar.
//   - JDK 21   (previewFfm): ONLY the JDK-21 preview embedded variant (its rift/mock/natives deps are
//                            compiled but not published here — they ship from the JDK-22 job).
//   - JDK < 21: container/core only (the JDK-11 baseline, embedded absent).
// (Consequence: a bare `sbt test` on JDK 21 builds only the preview variant; run the container stack
// on JDK 11/17 or the stable embedded stack on JDK 22+. CI drives each variant with an explicit job.)
lazy val aggregatedProjects: Seq[Project] =
  if (previewFfm) Seq(riftEmbedded21)
  else Seq(core, gherkin, mock, rift, wiremock, conformance) ++ (if (stableFfm) Seq(riftEmbedded, embeddedNatives) else Seq.empty)

// Diagnostic: fetch the host-platform cdylib through the real downloadRiftAsset into a temp dir. Lets
// a restricted-network build validate its RIFT_NATIVES_DIR / RIFT_NATIVES_BASE_URL / https.proxy*
// config on any JDK, without a full JDK-21+ embedded build (where the natives fetch normally runs). #255
lazy val riftNativesSelfCheck = taskKey[Unit](
  "Fetch the host-platform librift_ffi via downloadRiftAsset (honors RIFT_NATIVES_DIR / RIFT_NATIVES_BASE_URL / proxy) into a temp dir."
)

lazy val root = (project in file("."))
  .aggregate(aggregatedProjects.map(p => LocalProject(p.id)): _*)
  .settings(
    name                  := "zio-bdd-root",
    description           := "A ZIO-based BDD testing framework for Scala 3",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty, // not published — MiMa is a no-op here
    riftNativesSelfCheck := {
      val log    = streams.value.log
      val osName = sys.props.getOrElse("os.name", "").toLowerCase
      val osArch = sys.props.getOrElse("os.arch", "").toLowerCase
      // librift_ffi is published for linux + macOS only (riftNativeTriples). Windows/other dev boxes
      // have no native — fail clearly rather than fetch a wrong-OS asset. CI runs on Linux.
      val (os, ext) =
        if (osName.contains("mac") || osName.contains("darwin")) ("darwin", "dylib")
        else if (osName.contains("linux")) ("linux", "so")
        else sys.error(s"[rift-ffi] no librift_ffi native is published for os.name='$osName' (linux and macOS only)")
      val arch =
        if (osArch.contains("aarch64") || osArch.contains("arm64")) "aarch64"
        else if (osArch.contains("amd64") || osArch.contains("x86_64") || osArch.contains("x86-64")) "x86_64"
        else sys.error(s"[rift-ffi] unsupported os.arch='$osArch' (x86_64 or aarch64 only)")
      val out = IO.createTemporaryDirectory / s"librift_ffi.$ext"
      downloadRiftAsset(riftNativesVersion, s"librift_ffi-$os-$arch.$ext", out, log)
      log.info(s"[rift-ffi] self-check OK: $os-$arch -> ${out.length()} bytes at $out")
    }
  )
  .dependsOn(core, gherkin)

lazy val core = (project in file("core"))
  .dependsOn(gherkin, mock)
  .settings(
    name := "zio-bdd",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.scala-sbt"           % "test-interface" % "1.0" % "provided",
      "org.scala-lang.modules" %% "scala-xml"      % "2.3.0",
      "dev.zio"                %% "zio-streams"    % "2.1.17",
      "dev.zio"                %% "izumi-reflect"  % "3.0.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    stepMethodGeneratorSettings,
    javaFloorSettings,
    mimaSettings
  )

lazy val gherkin = (project in file("gherkin"))
  .settings(
    name := "zio-bdd-gherkin",
    libraryDependencies ++= commonDependencies,
    javaFloorSettings,
    mimaSettings
  )

// Portable MockControl SPI (#110). Standalone, backend-neutral: no dependency on
// any adapter (Rift, WireMock) — adapters depend on this module, never the reverse.
lazy val mock = (project in file("mock"))
  .settings(
    name := "zio-bdd-mock",
    libraryDependencies ++= commonDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    javaFloorSettings,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Rift adapter (#113): implements the portable MockControl SPI over the Rift
// (Mountebank-compatible) backend. Drives the admin API via zio-http and can
// stand up the published image via testcontainers. Depends on `mock`, never the
// reverse.
lazy val rift = Project("rift", file("rift"))
  .dependsOn(mock)
  .settings(
    name := "zio-bdd-rift",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "dev.zio"      %% "zio-http"                   % "3.2.0",
      "dev.zio"      %% "zio-json"                   % "0.7.3",
      "com.dimafeng" %% "testcontainers-scala-core"  % "0.41.4"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Generate RiftBuildInfo from `riftVersion` (the single source of truth, #195) so Rift.DefaultImage
    // derives its image tag from the same val the FFI natives version does — no hand-maintained literal.
    Compile / sourceGenerators += Def.task {
      val out = (Compile / sourceManaged).value / "zio" / "bdd" / "mock" / "rift" / "RiftBuildInfo.scala"
      IO.write(
        out,
        s"""package zio.bdd.mock.rift
           |
           |// GENERATED from `riftVersion` in build.sbt (#195) — do not edit. The single source of truth
           |// for the pinned Rift release; Rift.DefaultImage and the FFI natives version both derive from it.
           |private[rift] object RiftBuildInfo:
           |  val riftVersion: String = "$riftVersion"
           |""".stripMargin
      )
      Seq(out)
    }.taskValue,
    javaFloorSettings,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Embedded Rift provider (#133/#193): drives the Rift engine in-process over librift_ffi via Panama
// FFM, so no Docker. A SEPARATE published artifact from the container adapter because it needs FFM
// (JDK 21/22+) + the native library — keeping `zio-bdd-rift` and core on the JDK-11 baseline. Ships as
// two variants from ONE shared source (rift-embedded/src): the STABLE variant below (JDK 22+, no
// preview) and the PREVIEW variant (`riftEmbedded21`, JDK 21) just after. Each is built/aggregated
// only on the JDK that can produce it.
lazy val embeddedDeps: Seq[ModuleID] =
  commonDependencies ++ Seq("dev.zio" %% "zio-http" % "3.2.0", "dev.zio" %% "zio-json" % "0.7.3")

// Both variants share these project deps. test->test on rift: the embedded capabilities spec reuses
// rift's FakeRift admin-API double (a test source) to stand in for the in-process admin plane, exactly
// as it did when embedded lived in `rift`.
lazy val riftEmbedded = Project("riftEmbedded", file("rift-embedded"))
  .dependsOn(rift % "compile->compile;test->test", mock, embeddedNatives % Test)
  .settings(
    name := "zio-bdd-rift-embedded",
    libraryDependencies ++= embeddedDeps,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // The Java FFM bridge compiles against the stable Foreign Function & Memory API (JEP 454, JDK 22).
    // No --enable-preview: the resulting class is a normal JDK-22 class, not version-locked to one JDK.
    javacOptions ++= Seq("--release", "22"),
    mimaPreviousArtifacts := Set.empty
  )
  .settings(embeddedTestJvmSettings)

// The JDK-21 PREVIEW variant of the embedded provider — the exact same sources as `riftEmbedded`
// (shared via source-dir overrides; only the FFM compile target differs), compiled
// `--release 21 --enable-preview`. Its bridge is a preview-21 class that loads only on JDK 21 (with
// --enable-preview); the reflective shim in RiftFfiBridge binds the preview-named FFM methods. Built
// and published as `zio-bdd-rift-embedded-jdk21` only on JDK 21.
lazy val riftEmbedded21 = Project("riftEmbedded21", file("rift-embedded-jdk21"))
  .dependsOn(rift % "compile->compile;test->test", mock, embeddedNatives % Test)
  .settings(
    name := "zio-bdd-rift-embedded-jdk21",
    libraryDependencies ++= embeddedDeps,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Share riftEmbedded's sources verbatim — do not duplicate the bridge/adapter.
    Compile / scalaSource := (ThisBuild / baseDirectory).value / "rift-embedded" / "src" / "main" / "scala",
    Compile / javaSource  := (ThisBuild / baseDirectory).value / "rift-embedded" / "src" / "main" / "java",
    Test / scalaSource    := (ThisBuild / baseDirectory).value / "rift-embedded" / "src" / "test" / "scala",
    // Preview FFM: the bridge is compiled --release 21 --enable-preview (preview-locked to JDK 21).
    javacOptions ++= Seq("--release", "21", "--enable-preview"),
    mimaPreviousArtifacts := Set.empty
  )
  .settings(embeddedPreviewTestJvmSettings)

// WireMock adapter (#122): the in-process, pure-JVM, zero-Docker provider with
// Correlated isolation by default. Implements the portable MockControl SPI over
// an embedded WireMockServer. Depends on `mock`, never the reverse.
lazy val wiremock = (project in file("wiremock"))
  .dependsOn(mock)
  .settings(
    name := "zio-bdd-wiremock",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.wiremock" % "wiremock" % "3.9.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    javaFloorSettings,
    // Not yet published as a 1.x artifact — no binary-compat baseline to check against.
    mimaPreviousArtifacts := Set.empty
  )

// Conformance harness + matrix runner (#124): the executable definition of
// "implements MockControl". The only module that depends on BOTH adapters, so it
// can run one feature set across Rift + WireMock and emit the pass/skip/fail matrix.
lazy val conformance = {
  val base = Project("conformance", file("conformance"))
    .dependsOn(core, rift, wiremock)
    .settings(
      name := "zio-bdd-conformance",
      libraryDependencies ++= commonDependencies,
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
      publish / skip        := true, // a test harness, not a published artifact
      mimaPreviousArtifacts := Set.empty
    )
  // On a 22+ JDK the portable conformance suite also runs against the (stable) embedded provider: add
  // the gated test source (EmbeddedConformanceSpec, src/test/jdk22), the native-access test JVM, and
  // the rift-embedded + natives dependencies. On JDK < 22 none of this is added — the preview variant
  // is verified by riftEmbedded21's own suite, so conformance stays wired to the stable variant only.
  if (stableFfm)
    base
      .dependsOn(riftEmbedded, embeddedNatives % Test)
      .settings(
        Test / unmanagedSourceDirectories += (Test / sourceDirectory).value / "jdk22",
        embeddedTestJvmSettings
      )
  else base
}

lazy val example = (project in file("example"))
  .dependsOn(core)
  .settings(
    name := "zio-bdd-example",
    libraryDependencies ++= commonDependencies,
    Test / testFrameworks    := Seq(new TestFramework("zio.bdd.ZIOBDDFramework")),
    Test / resourceDirectory := baseDirectory.value / "src" / "test" / "resources" / "features",
    publish / skip           := true,
    mimaPreviousArtifacts    := Set.empty // not published — MiMa is a no-op here
  )
