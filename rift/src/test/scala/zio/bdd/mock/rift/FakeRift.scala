package zio.bdd.mock.rift

import zio.*
import zio.http.*
import zio.json.ast.Json

/**
 * An in-process stand-in for Rift's admin API: it records the admin calls the
 * adapter makes and serves canned imposter views, so the adapter's protocol and
 * isolation behaviour can be unit-tested with no Docker and no real backend.
 *
 * It deliberately does NOT serve imposter traffic, nor does it actually filter
 * recorded requests — "serving" and the real header/flow-id filter are the real
 * container's job, proven by the tagged [[RiftContainerSpec]]. Here we assert
 * the adapter *issues* the right space-scoped calls.
 */
final case class FakeRift(
  posted: Ref[Chunk[String]],
  deletedPorts: Ref[Chunk[Int]],
  globalDeletes: Ref[Int],
  stubCalls: Ref[Chunk[String]],
  spaceStubs: Ref[Chunk[String]],
  spaceDeletes: Ref[Chunk[String]],
  requestMatches: Ref[Chunk[String]],
  filtered: Ref[String],
  views: Ref[Map[Int, String]],
  failProvision: Ref[Boolean]
):
  /**
   * Preset the `GET /imposters/:port` view (e.g. to inject recorded requests).
   */
  def setView(port: Int, json: String): UIO[Unit] = views.update(_.updated(port, json))

  /**
   * Preset the body returned by `GET /imposters/:port/requests?match=…` (#201).
   */
  def setFiltered(json: String): UIO[Unit] = filtered.set(json)

  val routes: Routes[Any, Response] =
    Routes(
      Method.POST / "imposters" -> handler { (req: Request) =>
        req.body.asString.orDie.flatMap { body =>
          val port = FakeRift.portOf(body).getOrElse(0)
          failProvision.get.flatMap {
            case true =>
              ZIO.succeed(
                Response(status = Status.BadRequest, body = Body.fromString("""{"errors":[{"code":"400"}]}"""))
              )
            case false =>
              posted.update(_ :+ body) *>
                views.update(m => if m.contains(port) then m else m.updated(port, FakeRift.emptyView(port))) *>
                ZIO.succeed(Response(status = Status.Created, body = Body.fromString(s"""{"port":$port}""")))
          }
        }
      },
      // Space-scoped stubs (rift#223): POST /imposters/:port/spaces/:flowId/stubs
      Method.POST / "imposters" / int("port") / "spaces" / string("flowId") / "stubs" -> handler {
        (port: Int, flowId: String, req: Request) =>
          req.body.asString.orDie
            .flatMap(b => spaceStubs.update(_ :+ s"$port/$flowId $b"))
            .as(Response(status = Status.Created, body = Body.fromString(s"""{"space":"$flowId","stubs":[]}""")))
      },
      // Per-space teardown (rift#223): DELETE /imposters/:port/spaces/:flowId
      Method.DELETE / "imposters" / int("port") / "spaces" / string("flowId") -> handler {
        (port: Int, flowId: String, _: Request) =>
          spaceDeletes.update(_ :+ s"$port/$flowId").as(Response.ok)
      },
      // Header/flow-id-filtered recorded requests (rift#201): GET /imposters/:port/requests?match=…
      Method.GET / "imposters" / int("port") / "requests" -> handler { (_: Int, req: Request) =>
        val m = req.url.queryParams.queryParam("match").getOrElse("")
        (requestMatches.update(_ :+ m) *> filtered.get).map(f => Response(body = Body.fromString(f)))
      },
      Method.GET / "imposters" / int("port") -> handler { (port: Int, _: Request) =>
        views.get.map(m => Response(body = Body.fromString(m.getOrElse(port, FakeRift.emptyView(port)))))
      },
      Method.DELETE / "imposters" / int("port") -> handler { (port: Int, _: Request) =>
        (deletedPorts.update(_ :+ port) *> views.update(_ - port)).as(Response.ok)
      },
      Method.DELETE / "imposters" -> handler { (_: Request) =>
        globalDeletes.update(_ + 1).as(Response.ok)
      },
      Method.POST / "imposters" / int("port") / "stubs" -> handler { (port: Int, req: Request) =>
        req.body.asString.orDie.flatMap(b => stubCalls.update(_ :+ s"POST $port $b")).as(Response.ok)
      },
      Method.PUT / "imposters" / int("port") / "stubs" -> handler { (port: Int, req: Request) =>
        req.body.asString.orDie.flatMap(b => stubCalls.update(_ :+ s"PUT $port $b")).as(Response.ok)
      },
      Method.DELETE / "imposters" / int("port") / "stubs" / int("index") -> handler {
        (port: Int, index: Int, _: Request) =>
          stubCalls.update(_ :+ s"DELETE $port/$index").as(Response.ok)
      }
    )

object FakeRift:
  private def emptyView(port: Int): String = s"""{"port":$port,"requests":[]}"""

  def make: UIO[FakeRift] =
    for
      a  <- Ref.make(Chunk.empty[String])
      b  <- Ref.make(Chunk.empty[Int])
      c  <- Ref.make(0)
      d  <- Ref.make(Chunk.empty[String])
      ss <- Ref.make(Chunk.empty[String])
      sd <- Ref.make(Chunk.empty[String])
      rm <- Ref.make(Chunk.empty[String])
      fl <- Ref.make("[]")
      e  <- Ref.make(Map.empty[Int, String])
      f  <- Ref.make(false)
    yield FakeRift(a, b, c, d, ss, sd, rm, fl, e, f)

  def portOf(body: String): Option[Int] =
    import zio.json.*
    body.fromJson[Json].toOption.flatMap {
      case o: Json.Obj => o.fields.toMap.get("port").collect { case Json.Num(n) => n.intValue }
      case _           => None
    }

  /** Stand a fresh fake up on an ephemeral port inside the current scope. */
  def started: ZIO[Scope, Throwable, (String, FakeRift)] =
    for
      fake  <- make
      env   <- Server.defaultWithPort(0).build
      server = env.get[Server]
      _     <- server.install(fake.routes)
      port  <- server.port
    yield (s"http://localhost:$port", fake)
