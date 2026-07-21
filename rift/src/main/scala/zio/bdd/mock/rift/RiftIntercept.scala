package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock as spi

import rift.dsl.{status, IsResponseBuilder}
import rift.bridge.TruststoreFormat
import rift.zio.{ImposterHandle, InterceptHandle}

/**
 * The [[spi.Intercept]] capability over the SDK's `rift.zio.InterceptHandle`
 * (#253/#219, re-based onto rift-scala for #285). Transport-agnostic: the same
 * wire logic serves the embedded, connect and container backends — only how
 * `RiftMockControl` obtains the (lazily-started, memoized) `handle` effect and
 * resolves the redirect target's imposter differs per transport, and neither is
 * this class's concern.
 *
 * `trustStoreWithSystemCAs` is NOT reimplemented here: `spi.Intercept` already
 * provides it as a default built on [[trustStore]] (pure `java.security`, no
 * adapter types) — see `zio.bdd.mock.Intercept.trustStoreWithSystemCAs`.
 */
private[rift] final class RiftIntercept(
  handle: IO[spi.MockError, InterceptHandle],
  imposterOf: spi.MockSpace => IO[spi.MockError, ImposterHandle]
) extends spi.Intercept:

  def proxyPort: IO[spi.MockError, Int] =
    handle.flatMap(_.address.mapBoth(RiftModelMapping.toMockError(None), _.getPort))

  // The default (`zio.bdd.mock.Intercept.proxyEndpoint`) always reports loopback — wrong for a proxy
  // bound to a wider interface (0.0.0.0, or a specific NIC). Report the engine's actual bound address
  // instead, so a caller that configured a wide bind sees it reflected here (#254).
  override def proxyEndpoint: IO[spi.MockError, (String, Int)] =
    handle.flatMap(_.address.mapBoth(RiftModelMapping.toMockError(None), a => (a.getHostString, a.getPort)))

  def add(rule: spi.InterceptRule): IO[spi.MockError, Unit] = rule match
    case spi.InterceptRule.Redirect(host, to) =>
      for
        h      <- handle
        target <- imposterOf(to)
        _      <- h.rule(host).redirectTo(target).mapError(RiftModelMapping.toMockError(Some(to.id)))
      yield ()
    case spi.InterceptRule.Serve(host, stub) =>
      for
        h <- handle
        _ <- h.rule(host).serve(interceptResponse(stub)).mapError(RiftModelMapping.toMockError(None))
      yield ()

  def trustStore(
    format: spi.TrustStoreFormat,
    to: Option[java.nio.file.Path]
  ): IO[spi.MockError, spi.TrustStore] =
    for
      h <- handle
      path <-
        ZIO
          .attempt(spi.TrustStore.exportPath(to, format))
          .mapError(t => spi.MockError.ProvisionFailed(s"resolving truststore path: ${RiftModelMapping.message(t)}"))
      password <- Random.nextUUID.map(_.toString)
      _ <- h
             .exportTruststore(truststoreFormatOf(format), password, path)
             .mapError(RiftModelMapping.toMockError(None))
    yield spi.TrustStore(path, password, format)

  private def interceptResponse(stub: spi.InterceptStub): IsResponseBuilder =
    val withHeaders = stub.headers.toVector.sortBy(_._1).foldLeft(status(stub.status)) { case (b, (k, v)) =>
      b.header(k, v)
    }
    stub.body.fold(withHeaders)(withHeaders.text)

  private def truststoreFormatOf(f: spi.TrustStoreFormat): TruststoreFormat = f match
    case spi.TrustStoreFormat.Pkcs12 => TruststoreFormat.Pkcs12
    case spi.TrustStoreFormat.Jks    => TruststoreFormat.Jks
