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
  failProvision: Ref[Boolean],
  scenarioPuts: Ref[Chunk[String]],
  scenarioGets: Ref[Chunk[String]],
  scenariosView: Ref[String],
  interceptRulesRef: Ref[Vector[String]]
):
  /**
   * Preset the `GET /imposters/:port` view (e.g. to inject recorded requests).
   */
  def setView(port: Int, json: String): UIO[Unit] = views.update(_.updated(port, json))

  /**
   * Preset the body returned by `GET /imposters/:port/requests?match=…` (#201).
   */
  def setFiltered(json: String): UIO[Unit] = filtered.set(json)

  /**
   * Preset the body returned by `GET /imposters/:port/scenarios` (rift#190).
   */
  def setScenarios(json: String): UIO[Unit] = scenariosView.set(json)

  /**
   * The bodies posted to `POST /intercept/rules` (#253), in call order.
   */
  def interceptRules: UIO[Vector[String]] = interceptRulesRef.get

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
      },
      // Scenario state set (rift#190): PUT /imposters/:port/scenarios/:name/state
      Method.PUT / "imposters" / int("port") / "scenarios" / string("name") / "state" -> handler {
        (port: Int, name: String, req: Request) =>
          req.body.asString.orDie.flatMap(b => scenarioPuts.update(_ :+ s"$port/$name $b")).as(Response.ok)
      },
      // Scenario list + state (rift#190): GET /imposters/:port/scenarios[?flowId=]
      Method.GET / "imposters" / int("port") / "scenarios" -> handler { (port: Int, req: Request) =>
        val flow = req.url.queryParams.queryParam("flowId").getOrElse("")
        (scenarioGets.update(_ :+ s"$port?$flow") *> scenariosView.get).map(v => Response(body = Body.fromString(v)))
      },
      // Intercept rule admin (#253): POST /intercept/rules
      Method.POST / "intercept" / "rules" -> handler { (req: Request) =>
        req.body.asString.orDie.flatMap { body =>
          interceptRulesRef.update(_ :+ body).as(Response.ok)
        }
      },
      // Intercept truststore export (#253): GET /intercept/truststore.<p12|jks> — the real rift route
      // uses the .p12 FILE extension, not the "pkcs12" wire token (a .pkcs12 route 404s). Fixed canned
      // bytes/password — the adapter only parses the header + bytes, never a real keystore, here.
      Method.GET / "intercept" / "truststore.p12" -> handler { (_: Request) =>
        ZIO.succeed(
          Response(status = Status.Ok, body = Body.fromArray(FakeRift.truststoreBytes))
            .addHeader(FakeRift.TruststorePasswordHeader, FakeRift.truststorePassword)
        )
      },
      Method.GET / "intercept" / "truststore.jks" -> handler { (_: Request) =>
        ZIO.succeed(
          Response(status = Status.Ok, body = Body.fromArray(FakeRift.truststoreBytes))
            .addHeader(FakeRift.TruststorePasswordHeader, FakeRift.truststorePassword)
        )
      }
    )

object FakeRift:
  private def emptyView(port: Int): String = s"""{"port":$port,"requests":[]}"""

  // Canned intercept truststore export (#253): the adapter only parses the header + bytes, so a
  // real keystore isn't needed to exercise the HTTP path hermetically. Mirrors
  // RiftIntercept's (private) header name — a literal here, not a cross-object reference.
  val TruststorePasswordHeader: String = "x-truststore-password"
  val truststoreBytes: Array[Byte]     = Array[Byte](1, 2, 3, 4)
  val truststorePassword: String       = "changeit"

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
      sp <- Ref.make(Chunk.empty[String])
      sg <- Ref.make(Chunk.empty[String])
      sv <- Ref.make("""{"flowId":"","scenarios":[]}""")
      ir <- Ref.make(Vector.empty[String])
    yield FakeRift(a, b, c, d, ss, sd, rm, fl, e, f, sp, sg, sv, ir)

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
