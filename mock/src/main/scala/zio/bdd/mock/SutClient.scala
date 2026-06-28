package zio.bdd.mock

import zio.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}
import scala.jdk.CollectionConverters.*

/** A response the SUT received from its (mocked) dependency. */
final case class SutResponse(status: Int, body: String, headers: Map[String, String])

/**
 * The SUT's dependency client, bound to one [[MockSpace]]. It derives the
 * dependency base URL from `space.baseUri` and applies `space.inject` to every
 * request, so the share-nothing isolation decoration (e.g. a Correlated
 * backend's correlation header) rides along.
 *
 * Provide it per scenario via [[SutClient.layer]] — never a process-wide env
 * var — so parallel scenarios each target their own space with no cross-talk.
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
    headers: Map[String, String] = Map.empty,
    body: Option[String] = None
  ): IO[Throwable, SutResponse]

object SutClient:
  def make(space: MockSpace): SutClient = Live(space)

  /** Per-scenario layer: bind the SUT's dependency client to `space`. */
  def layer(space: MockSpace): ULayer[SutClient] = ZLayer.succeed(make(space))

  private final case class Live(space: MockSpace) extends SutClient:
    def baseUrl: String = space.baseUri

    def send(
      method: Method,
      path: String,
      headers: Map[String, String],
      body: Option[String]
    ): IO[Throwable, SutResponse] =
      val injected = space.inject(HttpRequest(method, space.baseUri + path, headers, body))
      ZIO.attemptBlocking {
        val builder = injected.headers.foldLeft(JHttpRequest.newBuilder(URI.create(injected.uri))) { case (b, (k, v)) =>
          b.header(k, v)
        }
        val publisher = injected.body match
          case Some(content) => JHttpRequest.BodyPublishers.ofString(content)
          case None          => JHttpRequest.BodyPublishers.noBody()
        val request  = builder.method(injected.method.toString.toUpperCase, publisher).build()
        val response = HttpClient.newHttpClient().send(request, JHttpResponse.BodyHandlers.ofString())
        val respHeaders = response
          .headers()
          .map()
          .asScala
          .collect { case (k, vs) if !vs.isEmpty => k.toLowerCase -> vs.asScala.mkString(", ") }
          .toMap
        SutResponse(response.statusCode, response.body, respHeaders)
      }
