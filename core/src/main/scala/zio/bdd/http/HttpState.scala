package zio.bdd.http

import zio.bdd.core.Default
import zio.schema.{DeriveSchema, Schema}

/**
 * Projection of an HTTP response onto scalar fields for Schema-compatible
 * storage.
 *
 * Store this inside your scenario state type and provide a `HasLens[S,
 * HttpState]` to gain the pre-built assertion steps from `HttpSteps`:
 *
 * {{{
 *   import zio.bdd.core.step.HasLens
 *   import zio.bdd.http.{HttpState, HttpSteps}
 *
 *   final case class ScenarioState(http: HttpState = HttpState(), ...)
 *   object ScenarioState:
 *     given HasLens[ScenarioState, HttpState] =
 *       HasLens(_.http, (s, h) => s.copy(http = h))
 * }}}
 */
final case class HttpState(
  statusCode: Int = 0,
  body: String = "",
  headers: Map[String, String] = Map.empty,
  contentType: String = ""
)

object HttpState:
  given Schema[HttpState]  = DeriveSchema.gen[HttpState]
  given Default[HttpState] = Default.from(HttpState())
