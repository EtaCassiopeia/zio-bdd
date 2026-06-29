package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*

/**
 * The MUST-pass core conformance scenarios (#125): matching, response fidelity,
 * rule precedence, lifecycle isolation, and verification. Each is programmed
 * against the neutral [[MockControl]] (never a concrete backend) and asserts
 * via a real [[SutClient]] round-trip, so it must pass identically on every
 * adapter.
 *
 * Scope is deliberately the cross-adapter intersection both Rift and WireMock
 * translate faithfully. Two model limits keep multi-value-header scenarios out:
 * `ResponseDef.headers`/`RecordedRequest.headers` are `Map[String, String]`
 * (single value per key), and WireMock records only the first value +
 * lowercases recorded header keys — so scenarios use single-value headers and
 * never assert a recorded request-header by exact case. (Multi-value headers
 * need a model change — a follow-up, not #125.)
 */
object CoreConformanceScenarios:

  // lazy: the group vals below initialise after this declaration in source order.
  lazy val all: List[ConformanceScenario] =
    matching ++ responseFidelity ++ precedence ++ lifecycle ++ verification

  // ---- helpers ------------------------------------------------------------------

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private def ensure(cond: Boolean, msg: => String): IO[Throwable, Unit] =
    ZIO.unless(cond)(ZIO.fail(new AssertionError(msg))).unit

  private def srcOf(rules: MockRule*): MockSource = MockSource.Dsl(MockSpec(rules.toList))

  private def text(status: Int, body: String): ResponseDef = ResponseDef(status = status, body = Body.Text(body))

  /**
   * A GET-exact-path rule returning `body` with status 200 — the common shape.
   */
  private def rule(path: String, body: String): MockRule =
    MockRule(RequestMatch(path = PathMatch.Exact(path)), text(200, body))

  private def scen(name: String)(check: MockControl => ZIO[Scope, Throwable, Unit]): ConformanceScenario =
    ConformanceScenario(name, Set.empty, check)

  /**
   * Provision a space serving `rules`, torn down when the scenario's scope
   * closes.
   */
  // ignoreLogged (not bare ignore): teardown must not override the verdict, but a
  // leak into the shared backend used by later scenarios should be diagnosable.
  private def space(control: MockControl, rules: MockRule*): ZIO[Scope, Throwable, MockSpace] =
    ZIO.acquireRelease(control.provision(srcOf(rules*)).mapError(asT).map(_.head))(s => control.destroy(s).ignoreLogged)

  private def countMatching(reqs: List[RecordedRequest], method: Method, path: String): Int =
    reqs.count(r => r.method == method && r.uri == path)

  // ---- core-matching: matching fires only on a full match (partial does not) ----

  private val matching = List(
    scen("matching: method (POST-only rule; a GET does not fire)") { control =>
      for
        s <- space(
               control,
               MockRule(RequestMatch(method = Some(Method.Post), path = PathMatch.Exact("/m")), text(200, "ok"))
             )
        miss <- SutClient.make(s).send(Method.Get, "/m")
        hit  <- SutClient.make(s).send(Method.Post, "/m")
        _ <- ensure(
               miss.status == 404 && hit.status == 200 && hit.body == "ok",
               s"method: miss=${miss.status} hit=${hit.status}/${hit.body}"
             )
      yield ()
    },
    scen("matching: path Exact") { control =>
      for
        s    <- space(control, MockRule(RequestMatch(path = PathMatch.Exact("/exact")), text(200, "ok")))
        hit  <- SutClient.make(s).send(Method.Get, "/exact")
        miss <- SutClient.make(s).send(Method.Get, "/exact/extra")
        _    <- ensure(hit.status == 200 && miss.status == 404, s"exact: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: path Regex") { control =>
      for
        s    <- space(control, MockRule(RequestMatch(path = PathMatch.Regex("^/r/[0-9]+$")), text(200, "ok")))
        hit  <- SutClient.make(s).send(Method.Get, "/r/123")
        miss <- SutClient.make(s).send(Method.Get, "/r/abc")
        _    <- ensure(hit.status == 200 && miss.status == 404, s"regex: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: path Template (single segment)") { control =>
      for
        s    <- space(control, MockRule(RequestMatch(path = PathMatch.Template("/u/{id}")), text(200, "ok")))
        hit  <- SutClient.make(s).send(Method.Get, "/u/42")
        miss <- SutClient.make(s).send(Method.Get, "/u/42/9")
        _    <- ensure(hit.status == 200 && miss.status == 404, s"template: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: query Equals") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/q"), query = Map("k" -> ValueMatch.Equals("v"))),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Get, "/q?k=v")
        miss <- SutClient.make(s).send(Method.Get, "/q?k=x")
        _    <- ensure(hit.status == 200 && miss.status == 404, s"query: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: header Contains") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/h"), headers = Map("X-Tok" -> ValueMatch.Contains("bear"))),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Get, "/h", headers = Map("X-Tok" -> "bearer-123"))
        miss <- SutClient.make(s).send(Method.Get, "/h")
        _    <- ensure(hit.status == 200 && miss.status == 404, s"header: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: body Contains") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(
                   method = Some(Method.Post),
                   path = PathMatch.Exact("/b"),
                   body = Some(BodyMatch.Contains("hello"))
                 ),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Post, "/b", body = Some("say hello world"))
        miss <- SutClient.make(s).send(Method.Post, "/b", body = Some("goodbye"))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"body: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: body JsonPath (with expected value)") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(
                   method = Some(Method.Post),
                   path = PathMatch.Exact("/j"),
                   body = Some(BodyMatch.JsonPath("$.k", Some("v")))
                 ),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Post, "/j", body = Some("""{"k":"v"}"""))
        miss <- SutClient.make(s).send(Method.Post, "/j", body = Some("""{"k":"x"}"""))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"jsonpath: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: body XPath (node exists)") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(
                   method = Some(Method.Post),
                   path = PathMatch.Exact("/x"),
                   body = Some(BodyMatch.XPath("/r/k"))
                 ),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Post, "/x", body = Some("<r><k>v</k></r>"))
        miss <- SutClient.make(s).send(Method.Post, "/x", body = Some("<r></r>"))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"xpath: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: composite AND (a partial match does not fire)") { control =>
      val rm = RequestMatch(
        method = Some(Method.Post),
        path = PathMatch.Exact("/c"),
        headers = Map("X-K" -> ValueMatch.Equals("v")),
        body = Some(BodyMatch.Contains("z"))
      )
      for
        s         <- space(control, MockRule(rm, text(200, "ok")))
        hit       <- SutClient.make(s).send(Method.Post, "/c", headers = Map("X-K" -> "v"), body = Some("zzz"))
        noHeader  <- SutClient.make(s).send(Method.Post, "/c", body = Some("zzz"))
        wrongBody <- SutClient.make(s).send(Method.Post, "/c", headers = Map("X-K" -> "v"), body = Some("qqq"))
        _ <- ensure(
               hit.status == 200 && noHeader.status == 404 && wrongBody.status == 404,
               s"composite: hit=${hit.status} noHeader=${noHeader.status} wrongBody=${wrongBody.status}"
             )
      yield ()
    },
    scen("matching: header Matches (regex)") { control =>
      for
        s <-
          space(
            control,
            MockRule(
              RequestMatch(path = PathMatch.Exact("/hm"), headers = Map("X-Tok" -> ValueMatch.Matches("^Bearer .+$"))),
              text(200, "ok")
            )
          )
        hit  <- SutClient.make(s).send(Method.Get, "/hm", headers = Map("X-Tok" -> "Bearer abc"))
        miss <- SutClient.make(s).send(Method.Get, "/hm", headers = Map("X-Tok" -> "Basic abc"))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"header-matches: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: body Equals (exact)") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(
                   method = Some(Method.Post),
                   path = PathMatch.Exact("/be"),
                   body = Some(BodyMatch.Equals("exact-body"))
                 ),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Post, "/be", body = Some("exact-body"))
        miss <- SutClient.make(s).send(Method.Post, "/be", body = Some("exact-body-plus"))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"body-equals: hit=${hit.status} miss=${miss.status}")
      yield ()
    },
    scen("matching: body Matches (regex)") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(
                   method = Some(Method.Post),
                   path = PathMatch.Exact("/bm"),
                   body = Some(BodyMatch.Matches("^id=[0-9]+$"))
                 ),
                 text(200, "ok")
               )
             )
        hit  <- SutClient.make(s).send(Method.Post, "/bm", body = Some("id=42"))
        miss <- SutClient.make(s).send(Method.Post, "/bm", body = Some("id=abc"))
        _    <- ensure(hit.status == 200 && miss.status == 404, s"body-matches: hit=${hit.status} miss=${miss.status}")
      yield ()
    }
  )

  // ---- response-fidelity: the response is reproduced verbatim -------------------

  private val responseFidelity = List(
    scen("response: status + single-value header + Text body verbatim") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/resp")),
                 ResponseDef(status = 418, headers = Map("X-R" -> "rv"), body = Body.Text("teapot"))
               )
             )
        r <- SutClient.make(s).send(Method.Get, "/resp")
        _ <- ensure(
               r.status == 418 && r.body == "teapot" && r.headers.get("x-r").contains("rv"),
               s"resp: status=${r.status} body=${r.body} headers=${r.headers}"
             )
      yield ()
    },
    scen("response: Json body") { control =>
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/json")),
                 ResponseDef(status = 200, body = Body.Json("""{"a":1}"""))
               )
             )
        r <- SutClient.make(s).send(Method.Get, "/json")
        _ <- ensure(
               r.status == 200 && r.body.replaceAll("\\s", "") == """{"a":1}""",
               s"json: status=${r.status} body=${r.body}"
             )
      yield ()
    },
    scen("response: Base64 body decodes to raw bytes") { control =>
      val b64 = java.util.Base64.getEncoder.encodeToString("bin-payload".getBytes("UTF-8"))
      for
        s <-
          space(
            control,
            MockRule(RequestMatch(path = PathMatch.Exact("/b64")), ResponseDef(status = 200, body = Body.Base64(b64)))
          )
        r <- SutClient.make(s).send(Method.Get, "/b64")
        _ <- ensure(r.status == 200 && r.body == "bin-payload", s"base64: status=${r.status} body=${r.body}")
      yield ()
    },
    scen("response: delay is applied (delayed minus baseline within tolerance)") { control =>
      // Measure a no-delay sibling on the same space as the baseline, so the
      // delta cancels connection/JIT latency — a backend that ignored the delay
      // directive yields delta ~0 and fails, not a spurious pass on a slow box.
      def elapsedOf(s: MockSpace, path: String): ZIO[Any, Throwable, (SutResponse, Duration)] =
        for
          t0 <- Clock.nanoTime
          r  <- SutClient.make(s).send(Method.Get, path)
          t1 <- Clock.nanoTime
        yield (r, Duration.fromNanos(t1 - t0))
      for
        s <- space(
               control,
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/nodelay")),
                 ResponseDef(status = 200, body = Body.Text("ok"))
               ),
               MockRule(
                 RequestMatch(path = PathMatch.Exact("/delay")),
                 ResponseDef(status = 200, body = Body.Text("ok"), delay = Some(300.millis))
               )
             )
        baseline <- elapsedOf(s, "/nodelay")
        delayed  <- elapsedOf(s, "/delay")
        delta     = Duration.fromNanos(delayed._2.toNanos - baseline._2.toNanos)
        _ <- ensure(
               delayed._1.status == 200 && delta >= 200.millis,
               s"delay: status=${delayed._1.status} delayed=${delayed._2.toMillis}ms baseline=${baseline._2.toMillis}ms delta=${delta.toMillis}ms"
             )
      yield ()
    }
  )

  // ---- rule-precedence: overlay shadows base; mutations take effect ------------

  private val precedence = List(
    scen("precedence: an overlay shadows the base rule (deterministic on repeat)") { control =>
      for
        s  <- space(control, rule("/p", "base"))
        _  <- control.addRule(s, rule("/p", "overlay"), Priority.Overlay).mapError(asT)
        r1 <- SutClient.make(s).send(Method.Get, "/p")
        r2 <- SutClient.make(s).send(Method.Get, "/p")
        _  <- ensure(r1.body == "overlay" && r2.body == "overlay", s"overlay: r1=${r1.body} r2=${r2.body}")
      yield ()
    },
    scen("precedence: removeRule reverts to the base rule") { control =>
      for
        s      <- space(control, rule("/pr", "base"))
        id     <- control.addRule(s, rule("/pr", "overlay"), Priority.Overlay).mapError(asT)
        before <- SutClient.make(s).send(Method.Get, "/pr")
        _      <- control.removeRule(s, id).mapError(asT)
        after  <- SutClient.make(s).send(Method.Get, "/pr")
        _ <-
          ensure(before.body == "overlay" && after.body == "base", s"revert: before=${before.body} after=${after.body}")
      yield ()
    },
    scen("precedence: replaceRules swaps the whole ruleset") { control =>
      for
        s    <- space(control, rule("/old", "old"))
        _    <- control.replaceRules(s, List(rule("/new", "new"))).mapError(asT)
        oldR <- SutClient.make(s).send(Method.Get, "/old")
        newR <- SutClient.make(s).send(Method.Get, "/new")
        _    <- ensure(oldR.status == 404 && newR.body == "new", s"replace: old=${oldR.status} new=${newR.body}")
      yield ()
    }
  )

  // ---- lifecycle-isolation: locality of provision/destroy ----------------------

  private val lifecycle = List(
    scen("lifecycle: destroy(A) leaves B serving") { control =>
      for
        a     <- control.provision(srcOf(rule("/li", "A"))).mapError(asT).map(_.head)
        b     <- control.provision(srcOf(rule("/li", "B"))).mapError(asT).map(_.head)
        _     <- control.destroy(a).mapError(asT)
        bResp <- SutClient.make(b).send(Method.Get, "/li")
        _     <- control.destroy(b).ignoreLogged
        _     <- ensure(bResp.status == 200 && bResp.body == "B", s"isolation: bResp=${bResp.status}/${bResp.body}")
      yield ()
    },
    scen("lifecycle: operations on a destroyed space fail with SpaceNotFound") { control =>
      for
        s        <- control.provision(srcOf(rule("/d", "x"))).mapError(asT).map(_.head)
        _        <- control.destroy(s).mapError(asT)
        gone      = Left(MockError.SpaceNotFound(s.id))
        recvE    <- control.received(s).either
        addE     <- control.addRule(s, rule("/d", "y"), Priority.Base).either
        removeE  <- control.removeRule(s, RuleId("r1")).either
        replaceE <- control.replaceRules(s, List(rule("/d", "z"))).either
        _ <- ensure(
               recvE == gone && addE == gone && removeE == gone && replaceE == gone,
               s"destroyed ops: recv=$recvE add=$addE remove=$removeE replace=$replaceE"
             )
      yield ()
    }
  )

  // ---- verification: received is A-only; count + set semantics ------------------

  private val verification = List(
    scen("verification: received(A) returns only A's traffic") { control =>
      for
        a  <- space(control, rule("/v", "a"))
        b  <- space(control, rule("/v", "b"))
        _  <- SutClient.make(a).send(Method.Get, "/v")
        _  <- SutClient.make(b).send(Method.Get, "/v")
        _  <- SutClient.make(b).send(Method.Get, "/v")
        ra <- control.received(a).mapError(asT)
        rb <- control.received(b).mapError(asT)
        _  <- ensure(ra.size == 1 && rb.size == 2, s"received: ra=${ra.size} rb=${rb.size}")
      yield ()
    },
    scen("verification: countMatching equals the number of matching requests sent") { control =>
      for
        s    <- space(control, rule("/c3", "ok"))
        _    <- ZIO.foreachDiscard(1 to 3)(_ => SutClient.make(s).send(Method.Get, "/c3"))
        reqs <- control.received(s).mapError(asT)
        _ <- ensure(
               countMatching(reqs, Method.Get, "/c3") == 3 && countMatching(reqs, Method.Post, "/c3") == 0,
               s"count: get=${countMatching(reqs, Method.Get, "/c3")}"
             )
      yield ()
    },
    scen("verification: existence (set) semantics — a sent request is present, an unsent path is not") { control =>
      for
        s    <- space(control, rule("/e", "ok"))
        _    <- SutClient.make(s).send(Method.Get, "/e")
        reqs <- control.received(s).mapError(asT)
        _ <- ensure(
               reqs.exists(r => r.method == Method.Get && r.uri == "/e") && !reqs.exists(_.uri == "/never"),
               s"set: reqs=$reqs"
             )
      yield ()
    }
  )
