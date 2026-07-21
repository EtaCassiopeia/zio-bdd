/** Maps `os.name`/`os.arch` to one of the published `rift-java-natives` classifiers (#285) — modeled
  * on rift-scala's own `rift.bridge.RiftNatives` and on the (deleted) hand-rolled `riftNativeTriples`
  * os/arch tokenization this file used to carry for the FFI download. Fails closed: an unrecognised
  * platform throws rather than guessing a classifier that would silently resolve the wrong native.
  *
  * `build.sbt` is compiled with the Scala 2.12 dialect (sbt's own build definition language), so this
  * stays 2.12-compatible — no Scala 3 syntax.
  */
object RiftNatives {

  def classifierFor(osName: String, osArch: String): Option[String] = {
    val os = osName.toLowerCase
    val arch = normalizeArch(osArch)
    if (os.contains("mac") || os.contains("darwin")) darwinClassifier(arch)
    else if (os.contains("linux")) linuxClassifier(arch)
    else None
  }

  private def darwinClassifier(arch: String): Option[String] = arch match {
    case "aarch64" => Some("darwin-aarch64")
    case "x86_64" => Some("darwin-x86_64")
    case _ => None
  }

  /** Glibc only. `linux-musl-x86_64` also exists but musl isn't detectable from `os.arch`/`os.name`
    * alone.
    */
  private def linuxClassifier(arch: String): Option[String] = arch match {
    case "aarch64" => Some("linux-aarch64")
    case "x86_64" => Some("linux-x86_64")
    case _ => None
  }

  private def normalizeArch(arch: String): String = arch.toLowerCase match {
    case "arm64" => "aarch64"
    case "x64" | "amd64" => "x86_64"
    case other => other
  }

  /** The classifier for the JVM running the build, for wiring the `rift-java-natives` Test
    * dependency. `None` on a host with no published classifier (e.g. Windows, musl, ppc64le) — this
    * is evaluated eagerly while sbt loads `build.sbt`, so throwing here (the pre-#285 behavior) broke
    * `sbt compile` outright for such a contributor. The natives Test dependency is simply omitted for
    * `None`: `EmbeddedRift.available` already degrades gracefully to `false` when no provider resolves.
    */
  def currentClassifier: Option[String] = {
    val osName = System.getProperty("os.name")
    val osArch = System.getProperty("os.arch")
    classifierFor(osName, osArch)
  }
}
