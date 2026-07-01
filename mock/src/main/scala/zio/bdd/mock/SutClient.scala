package zio.bdd.mock

import zio.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}
import scala.jdk.CollectionConverters.*

/** A response the SUT received from its (mocked) dependency. */
final case class SutResponse(status: Int, body: String, headers: Headers)

/**
 * The SUT's dependency client, bound to one [[MockSpace]]. It derives the
 * dependency base URL from `space.baseUri` and applies `space.inject` to every
 * request, so the share-nothing isolation decoration (e.g. a Correlated
 * backend's correlation header) rides along.
 *
 * Provide it per scenario via [[SutClient.layer]] — never a process-wide env
 * var — so parallel scenarios each target their own space with no cross-talk.
 *
 * This is the canonical *Caller* of the isolation model (#123): it always sends
 * `space.inject(req)` to `space.baseUri` and never branches on PerInstance vs
 * Correlated — the whole difference lives in `space.inject`, so steps stay
 * isolation-agnostic. See [[MockControl.isolation]] for an adapter's mode.
 */
trait SutClient:
  /**
   * The base URL the SUT should treat as its dependency endpoint
   * (`space.baseUri`).
   */
  def baseUrl: String

  /**
   * Issue a request to the bound space: `path` is resolved against [[baseUrl]]
   * and the space's `inject` is applied before sending.
   */
  def send(
    method: Method,
    path: String,
    headers: Headers = Headers.empty,
    body: Option[String] = None
  ): IO[Throwable, SutResponse]

object SutClient:
  def make(space: MockSpace): SutClient = Live(space)

  // Mock servers speak cleartext HTTP/1.1; the JDK client defaults to HTTP/2, whose h2c upgrade
  // path intermittently EOFs on a 201 response under the test runtime (#183). Pin HTTP/1.1 — there
  // is no reason to negotiate HTTP/2 against a mock. Shared with MockSteps.sendThrough.
  private[mock] def http1Client: HttpClient =
    HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  /** Per-scenario layer: bind the SUT's dependency client to `space`. */
  def layer(space: MockSpace): ULayer[SutClient] = ZLayer.succeed(make(space))

  private final case class Live(space: MockSpace) extends SutClient:
    def baseUrl: String = space.baseUri

    def send(
      method: Method,
      path: String,
      headers: Headers,
      body: Option[String]
    ): IO[Throwable, SutResponse] =
      val injected = space.inject(HttpRequest(method, space.baseUri + path, headers, body))
      ZIO.attemptBlocking {
        // One builder.header(k, v) per value so multi-value request headers go on the wire intact.
        val builder = injected.headers.entries.foldLeft(JHttpRequest.newBuilder(URI.create(injected.uri))) {
          case (b, (k, vs)) => vs.foldLeft(b)((bb, v) => bb.header(k, v))
        }
        val publisher = injected.body match
          case Some(content) => JHttpRequest.BodyPublishers.ofString(content)
          case None          => JHttpRequest.BodyPublishers.noBody()
        val request  = builder.method(injected.method.toString.toUpperCase, publisher).build()
        val response = http1Client.send(request, JHttpResponse.BodyHandlers.ofString())
        // Preserve every value per response header key (Headers canonicalises keys to lower-case).
        val respHeaders = response.headers().map().asScala.foldLeft(Headers.empty) { case (h, (k, vs)) =>
          vs.asScala.foldLeft(h)((acc, v) => acc.add(k, v))
        }
        SutResponse(response.statusCode, response.body, respHeaders)
      }
