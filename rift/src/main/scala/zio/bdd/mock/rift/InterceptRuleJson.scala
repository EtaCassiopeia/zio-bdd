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

  /**
   * Build the rift intercept-rule JSON from a portable [[InterceptRule]]. Left
   * on a Redirect whose target space has no port in its `baseUri`.
   */
  def ruleJson(rule: InterceptRule): Either[MockError, String] =
    rule match
      case InterceptRule.Redirect(host, space) =>
        portOf(space.baseUri).map { port =>
          Json
            .Obj("host" -> Json.Str(host), "action" -> Json.Obj("forward" -> Json.Obj("port" -> Json.Num(port))))
            .toJson
        }
      case InterceptRule.Serve(host, stub) =>
        val headers = Json.Obj(stub.headers.map((k, v) => k -> Json.Str(v)).toSeq*)
        val serve = Json.Obj(
          "statusCode" -> Json.Num(stub.status),
          "headers"    -> headers,
          "body"       -> stub.body.fold[Json](Json.Null)(Json.Str(_))
        )
        Right(Json.Obj("host" -> Json.Str(host), "action" -> Json.Obj("serve" -> serve)).toJson)

  private def portOf(baseUri: String): Either[MockError, Int] =
    scala.util
      .Try(java.net.URI.create(baseUri).getPort)
      .toOption
      .filter(_ > 0)
      .toRight(MockError.InvalidDefinition(s"intercept redirect target has no port in its baseUri: $baseUri"))
