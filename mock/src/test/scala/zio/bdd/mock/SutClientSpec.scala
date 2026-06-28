package zio.bdd.mock

import zio.*
import zio.test.*

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import scala.jdk.CollectionConverters.*

object SutClientSpec extends ZIOSpecDefault:

  // An in-process stand-in for the SUT's dependency. It records each request under
  // a space key = the `x-correlation-id` header if present (Correlated isolation),
  // else the first path segment (PerInstance isolation). A request that carries
  // neither (e.g. inject was bypassed) lands under the wrong key, so a Correlated
  // request without inject never reaches its space.
  private final class TestDependency(
    port: Int,
    recordings: ConcurrentHashMap[String, CopyOnWriteArrayList[(String, String)]]
  ):
    def perInstanceSpace(id: String): MockSpace =
      MockSpace(s"http://localhost:$port/$id", identity, SpaceId(id))

    def correlatedSpace(id: String): MockSpace =
      MockSpace(
        s"http://localhost:$port",
        req => req.copy(headers = req.headers + ("x-correlation-id" -> id)),
        SpaceId(id)
      )

    def received(id: String): UIO[List[(String, String)]] =
      ZIO.succeed(Option(recordings.get(id)).map(_.asScala.toList).getOrElse(Nil))

  private val dependency: ZIO[Scope, Throwable, TestDependency] =
    ZIO
      .acquireRelease(
        ZIO.attempt {
          val recordings = new ConcurrentHashMap[String, CopyOnWriteArrayList[(String, String)]]()
          val server     = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
          server.createContext(
            "/",
            new HttpHandler:
              def handle(exchange: HttpExchange): Unit =
                val full    = exchange.getRequestURI.getPath
                val method  = exchange.getRequestMethod
                val reqBody = new String(exchange.getRequestBody.readAllBytes())
                val header  = Option(exchange.getRequestHeaders.getFirst("x-correlation-id"))
                val (key, path) = header match
                  case Some(h) => (h, full)
                  case None =>
                    val seg = full.stripPrefix("/").split("/", 2)
                    (seg(0), "/" + (if seg.length > 1 then seg(1) else ""))
                recordings.computeIfAbsent(key, _ => new CopyOnWriteArrayList[(String, String)]()).add((method, path))
                // Echo the request body back, stamp a header, and fail /down — so a
                // test can pin SutResponse status/body/header decoding and the
                // non-2xx-is-a-value contract.
                val status    = if path.endsWith("/down") then 503 else 200
                val respBytes = reqBody.getBytes
                exchange.getResponseHeaders.set("X-Served-By", "dep")
                exchange.sendResponseHeaders(status, if respBytes.isEmpty then -1 else respBytes.length.toLong)
                val os = exchange.getResponseBody
                os.write(respBytes)
                os.close()
          )
          server.start()
          (server, new TestDependency(server.getAddress.getPort, recordings))
        }
      )(acquired => ZIO.attempt(acquired._1.stop(0)).orDie)
      .map(_._2)

  // Send a raw request with NO inject applied (bypasses SutClient).
  private def rawGet(url: String): ZIO[Any, Throwable, Int] =
    ZIO.attemptBlocking {
      val req = JHttpRequest.newBuilder(URI.create(url)).GET().build()
      HttpClient.newHttpClient().send(req, JHttpResponse.BodyHandlers.ofString()).statusCode
    }

  def spec = suite("SutClient")(
    test("baseUrl derives from the space; PerInstance spaces target distinct endpoints (no fixed port)") {
      ZIO.scoped {
        dependency.map { dep =>
          val a = SutClient.make(dep.perInstanceSpace("A"))
          val b = SutClient.make(dep.perInstanceSpace("B"))
          assertTrue(
            a.baseUrl == dep.perInstanceSpace("A").baseUri, // derived from the space, not hardcoded
            a.baseUrl.endsWith("/A"),
            a.baseUrl != b.baseUrl // distinct per space
          )
        }
      }
    },
    test("SutClient applies inject so Correlated requests reach their own space (no cross-talk)") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          for
            _    <- SutClient.make(dep.correlatedSpace("A")).send(Method.Get, "/x")
            _    <- SutClient.make(dep.correlatedSpace("B")).send(Method.Post, "/y")
            recA <- dep.received("A")
            recB <- dep.received("B")
          yield assertTrue(recA == List(("GET", "/x")), recB == List(("POST", "/y")))
        }
      }
    },
    test("a Correlated request without inject does NOT reach its space (inject is load-bearing)") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          val spaceA = dep.correlatedSpace("A")
          for
            _        <- rawGet(spaceA.baseUri + "/x")                 // shared baseUri, no correlation header
            withoutI <- dep.received("A")
            _        <- SutClient.make(spaceA).send(Method.Get, "/x") // same, but via inject
            withInj  <- dep.received("A")
          yield assertTrue(
            withoutI.isEmpty,              // not routed to A — inject was load-bearing
            withInj == List(("GET", "/x")) // with inject it reaches A
          )
        }
      }
    },
    test("concurrent SUT calls via per-space clients don't cross-talk (PerInstance)") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          val n = 25
          for
            _ <-
              ZIO.foreachParDiscard(1 to n)(i => SutClient.make(dep.perInstanceSpace(s"s$i")).send(Method.Get, s"/p$i"))
            recs <- ZIO.foreach((1 to n).toList)(i => dep.received(s"s$i").map((i, _)))
          yield assertTrue(recs.forall((i, r) => r == List(("GET", s"/p$i"))))
        }
      }
    },
    test("concurrent SUT calls via per-space clients don't cross-talk (Correlated, shared base URL)") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          val n = 25
          for
            _ <-
              ZIO.foreachParDiscard(1 to n)(i => SutClient.make(dep.correlatedSpace(s"c$i")).send(Method.Get, s"/q$i"))
            recs <- ZIO.foreach((1 to n).toList)(i => dep.received(s"c$i").map((i, _)))
          yield assertTrue(recs.forall((i, r) => r == List(("GET", s"/q$i"))))
        }
      }
    },
    test("send issues the inject-rewritten request URI, not the original (inject may rewrite the URI)") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          val rewriting = dep.perInstanceSpace("Z").copy(inject = req => req.copy(uri = req.uri + "-rewritten"))
          for
            _   <- SutClient.make(rewriting).send(Method.Get, "/orig")
            rec <- dep.received("Z")
          yield assertTrue(rec == List(("GET", "/orig-rewritten"))) // the inject's URI rewrite was honoured
        }
      }
    },
    test("send returns the decoded dependency response (status, body, headers); a non-2xx is a value, not an error") {
      ZIO.scoped {
        dependency.flatMap { dep =>
          val client = SutClient.make(dep.perInstanceSpace("R"))
          for
            ok   <- client.send(Method.Post, "/echo", body = Some("hello"))
            down <- client.send(Method.Get, "/down")
          yield assertTrue(
            ok.status == 200,
            ok.body == "hello",                            // request body sent + response body decoded
            ok.headers.get("x-served-by").contains("dep"), // header normalised to lower-case
            down.status == 503                             // non-2xx surfaced as a SutResponse, not a failure
          )
        }
      }
    }
  )
