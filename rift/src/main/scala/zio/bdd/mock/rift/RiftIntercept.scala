package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.*
import zio.http.{Body, Client, Header, MediaType, Request, Response, URL, Method as HttpMethod}

import java.nio.file.Files

/**
 * The [[Intercept]] capability over the container Rift's admin HTTP API (#253)
 * — a mechanical adaptation of the embedded adapter's `EmbeddedIntercept`
 * (rift#410): same [[InterceptRuleJson]] wire shape, same ergonomics, but
 * reached over `adminBase` instead of the FFI, since a containerized Rift has
 * no in-process engine to downcall into.
 *
 * Unlike the embedded adapter, this holds no `started`/memoization state: the
 * container's intercept listener is started by the container command
 * (`--intercept-port`, wired in [[Rift.managed]]) at boot, not lazily on first
 * use — so `proxyPort`/`proxyEndpoint` just report the already-bound,
 * host-mapped port handed in at construction.
 */
private[rift] final class RiftIntercept(client: Client, adminBase: String, host: String, proxyMappedPort: Int)
    extends Intercept:

  def proxyPort: IO[MockError, Int] = ZIO.succeed(proxyMappedPort)

  override def proxyEndpoint: IO[MockError, (String, Int)] = ZIO.succeed((host, proxyMappedPort))

  def add(rule: InterceptRule): IO[MockError, Unit] =
    for
      json <- ZIO.fromEither(InterceptRuleJson.ruleJson(rule))
      u    <- url("/intercept/rules")
      res  <- send(jsonRequest(HttpMethod.POST, u, json))
      _    <- expectSuccess("add intercept rule", res)
    yield ()

  def trustStore(format: TrustStoreFormat): IO[MockError, TrustStore] =
    for
      u    <- url(s"/intercept/truststore.${format.wire}")
      resp <- sendResponse(Request(method = HttpMethod.GET, url = u))
      _ <- ZIO.unless(resp.status.code >= 200 && resp.status.code < 300)(
             failBody(s"read intercept truststore.${format.wire}", resp)
           )
      password <- ZIO
                    .fromOption(resp.headers.get(RiftIntercept.TruststorePasswordHeader))
                    .orElseFail(
                      MockError.CommunicationError(
                        s"intercept truststore response missing the ${RiftIntercept.TruststorePasswordHeader} header"
                      )
                    )
      bytes <- resp.body.asArray.mapError(t =>
                 MockError.CommunicationError(s"reading intercept truststore body: ${message(t)}")
               )
      tmp <- ZIO.attemptBlocking {
               val p = Files.createTempFile("rift-intercept-", s".${format.wire}")
               p.toFile.deleteOnExit()
               Files.write(p, bytes)
               p
             }
               .mapError(t => MockError.CommunicationError(s"creating intercept truststore temp file: ${message(t)}"))
    yield TrustStore(tmp, password, format)

  private def url(path: String): IO[MockError, URL] =
    ZIO
      .fromEither(URL.decode(adminBase + path))
      .mapError(e => MockError.CommunicationError(s"invalid admin URL $adminBase$path: ${e.getMessage}"))

  private def jsonRequest(method: HttpMethod, u: URL, body: String): Request =
    Request(method = method, url = u, body = Body.fromString(body))
      .addHeader(Header.ContentType(MediaType.application.json))

  private def send(req: Request): IO[MockError, (Int, String)] =
    Client
      .batched(req)
      .flatMap(resp => resp.body.asString.map(body => (resp.status.code, body)))
      .provideEnvironment(ZEnvironment(client))
      .mapError { t =>
        val msg = Option(t.getMessage).filter(_.nonEmpty).fold("")(m => s": $m")
        MockError.CommunicationError(s"${t.getClass.getSimpleName}$msg")
      }

  private def sendResponse(req: Request): IO[MockError, Response] =
    Client
      .batched(req)
      .provideEnvironment(ZEnvironment(client))
      .mapError { t =>
        val msg = Option(t.getMessage).filter(_.nonEmpty).fold("")(m => s": $m")
        MockError.CommunicationError(s"${t.getClass.getSimpleName}$msg")
      }

  private def expectSuccess(action: String, res: (Int, String)): IO[MockError, String] =
    val (code, body) = res
    if code >= 200 && code < 300 then ZIO.succeed(body)
    else ZIO.fail(MockError.CommunicationError(s"$action: Rift returned HTTP $code: ${body.take(1000)}"))

  private def failBody(action: String, resp: Response): IO[MockError, Nothing] =
    resp.body.asString
      .mapError(t => MockError.CommunicationError(s"reading response body: ${message(t)}"))
      .orElseSucceed("")
      .flatMap(body =>
        ZIO.fail(MockError.CommunicationError(s"$action: Rift returned HTTP ${resp.status.code}: ${body.take(1000)}"))
      )

  private def message(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)

private[rift] object RiftIntercept:
  private val TruststorePasswordHeader: String = "x-truststore-password"
