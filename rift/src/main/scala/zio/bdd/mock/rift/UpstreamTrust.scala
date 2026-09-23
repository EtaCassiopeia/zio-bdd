package zio.bdd.mock.rift

import java.nio.file.Files

import zio.*
import zio.bdd.mock.MockError

/**
 * How the engine trusts an HTTPS origin that a proxy stub (the `proxyRecord`
 * capability) or the intercept listener's origin leg dials: `CaFile(path)` /
 * `CaPem(pem)` trust a private CA, `SkipVerify` disables verification. Unset,
 * the engine trusts only public roots. Accepted by both
 * [[embedded.EmbeddedRift.layer]] and [[Rift.managed]]. The SDK's type,
 * re-exported so callers need no `rift.bridge` import.
 */
export _root_.rift.bridge.UpstreamTrust

private[rift] object UpstreamTrustCheck:

  /**
   * `layer`, preceded by a check that fails a `CaFile` that is not a readable
   * file with a typed [[MockError.InvalidDefinition]]. The SDK reads the file
   * only when the engine or container starts, and an unreadable one surfaces
   * there as a defect (achird-labs/rift-scala#197). A malformed `CaPem` needs
   * no check here: the SDK already refuses it as a typed error.
   */
  def guarded[A: Tag](
    trust: Option[UpstreamTrust],
    layer: ZLayer[Any, MockError, A]
  ): ZLayer[Any, MockError, A] =
    trust match
      case Some(UpstreamTrust.CaFile(path)) =>
        ZLayer
          .fromZIO(
            ZIO
              .fail(MockError.InvalidDefinition(s"upstreamTrust CaFile is not a readable file: $path"))
              .unlessZIO(ZIO.succeedBlocking(Files.isRegularFile(path) && Files.isReadable(path)))
          )
          .flatMap(_ => layer)
      case _ => layer
