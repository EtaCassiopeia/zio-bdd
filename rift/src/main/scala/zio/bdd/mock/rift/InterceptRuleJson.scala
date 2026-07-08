package zio.bdd.mock.rift

import zio.bdd.mock.*
import zio.json.*
import zio.json.ast.Json

/**
 * Backend-neutral intercept-rule JSON builder shared by the container
 * ([[RiftMockControl]]/[[RiftIntercept]]) and embedded
 * (`zio.bdd.mock.rift.embedded.EmbeddedIntercept`) adapters — both drive the
 * same Rift intercept-rule wire shape, over HTTP or the FFI respectively; only
 * the transport differs. Pure — no I/O, so it is testable (and tested,
 * [[InterceptRuleJsonSpec]]) without a running engine.
 */
private[rift] object InterceptRuleJson:

  /** Build a forward-action rule JSON: `{host, action:{forward:{port}}}`. */
  def forwardJson(host: String, port: Int): String =
    Json
      .Obj("host" -> Json.Str(host), "action" -> Json.Obj("forward" -> Json.Obj("port" -> Json.Num(port))))
      .toJson

  /**
   * Build a serve-action rule JSON: `{host,
   * action:{serve:{statusCode,headers,body}}}`.
   */
  def serveJson(host: String, stub: InterceptStub): String =
    val headers = Json.Obj(stub.headers.map((k, v) => k -> Json.Str(v)).toSeq*)
    val serve = Json.Obj(
      "statusCode" -> Json.Num(stub.status),
      "headers"    -> headers,
      "body"       -> stub.body.fold[Json](Json.Null)(Json.Str(_))
    )
    Json.Obj("host" -> Json.Str(host), "action" -> Json.Obj("serve" -> serve)).toJson

  /**
   * Build the rift intercept-rule JSON from a portable [[InterceptRule]]. Left
   * on a Redirect whose target space has no port in its `baseUri`.
   *
   * Used by the embedded adapter (`EmbeddedIntercept`), where the intercept
   * listener and the imposters share the same process/port-space as
   * `space.baseUri` — so the baseUri port IS the right forward target there.
   * The container adapter ([[RiftMockControl]]/[[RiftIntercept]]) does NOT use
   * this for Redirect: it resolves the container-internal imposter port itself
   * and calls [[forwardJson]] directly (#253) — `space.baseUri`'s port is the
   * host-mapped port, unreachable from inside the container's intercept
   * listener.
   */
  def ruleJson(rule: InterceptRule): Either[MockError, String] =
    rule match
      case InterceptRule.Redirect(host, space) =>
        portOf(space.baseUri).map(forwardJson(host, _))
      case InterceptRule.Serve(host, stub) =>
        Right(serveJson(host, stub))

  private def portOf(baseUri: String): Either[MockError, Int] =
    scala.util
      .Try(java.net.URI.create(baseUri).getPort)
      .toOption
      .filter(_ > 0)
      .toRight(MockError.InvalidDefinition(s"intercept redirect target has no port in its baseUri: $baseUri"))
