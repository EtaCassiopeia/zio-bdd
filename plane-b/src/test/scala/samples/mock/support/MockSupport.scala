package samples.mock.support

import zio.*
import zio.bdd.core.Assertions
import zio.bdd.core.step.{Stage, ZIOSteps}
import zio.bdd.mock.*

/**
 * Shared step backbone for the mock corpus. Mix into a `ZIOSteps[MockControl,
 * S]` suite. Rules built up in `Given` steps are provisioned lazily into one
 * fresh space per scenario on the first `send`, and driven through the real
 * [[SutClient]] (honouring `space.inject`, so isolation rides along).
 * Assertions read what the backend actually served and what `received()`
 * actually recorded — engine as truth. The active space, pending rules, and
 * last response live in [[Stage]] (they carry functions / aren't Schema state),
 * so scenarios are share-nothing and parallel-safe.
 *
 * Registered here (common to every mock sample): the send step, the response
 * status/body/header assertions, and the `received()` assertions. Each suite
 * adds its own `Given` rule-builders and capability steps.
 */
trait MockSupport[S] { self: ZIOSteps[MockControl, S] =>

  // Tear the scenario's space down so stubs don't accumulate on a shared
  // (Correlated) server across scenarios. URIO hook: swallow teardown errors.
  afterScenario {
    Stage
      .get[MockSpace]
      .foldZIO(_ => ZIO.unit, space => ZIO.serviceWithZIO[MockControl](_.destroy(space)).ignore)
  }

  // ── MockControl plumbing ──────────────────────────────────────────────────

  /**
   * Run a MockControl op, surfacing its typed error on the Throwable channel.
   */
  protected def ctl[A](f: MockControl => IO[MockError, A]): ZIO[MockControl, Throwable, A] =
    ZIO.serviceWithZIO[MockControl](f).mapError(e => new RuntimeException(s"MockControl: $e"))

  /** Append a rule to the not-yet-provisioned space. */
  protected def stageRule(rule: MockRule): IO[Throwable, Unit] =
    Stage
      .get[PendingRules]
      .map(_.rules)
      .orElseSucceed(Nil)
      .flatMap(rs => Stage.put(PendingRules(rs :+ rule)))

  /**
   * Provision the staged rules into a fresh space (idempotent per scenario).
   */
  protected def ensureProvisioned: ZIO[MockControl, Throwable, MockSpace] =
    Stage.get[MockSpace].orElse {
      for
        pending <- Stage.get[PendingRules].orElseSucceed(PendingRules(Nil))
        spaces  <- ctl(_.provision(MockSource.Dsl(MockSpec(pending.rules))))
        space <- ZIO
                   .fromOption(spaces.headOption)
                   .orElseFail(new IllegalStateException("provision returned no space"))
        _ <- Stage.put(space)
      yield space
    }

  protected val activeSpace: ZIO[MockControl, Throwable, MockSpace] = ensureProvisioned

  protected val lastResponse: IO[Throwable, SutResponse] =
    Stage
      .get[SutResponse]
      .mapError(e => new IllegalStateException(s"no response — send first (${e.message})"))

  protected def methodOf(name: String): IO[Throwable, Method] =
    ZIO
      .fromOption(Method.values.find(_.toString.equalsIgnoreCase(name)))
      .orElseFail(new IllegalArgumentException(s"unknown HTTP method '$name'"))

  /**
   * Send through the active space via the real [[SutClient]] (applies
   * `inject`).
   */
  protected def send(
    method: Method,
    path: String,
    headers: Headers = Headers.empty,
    body: Option[String] = None
  ): ZIO[MockControl, Throwable, SutResponse] =
    ensureProvisioned.flatMap(space => SutClient.make(space).send(method, path, headers, body))

  /** The request body/headers staged for the next send (empty by default). */
  protected val pendingRequest: IO[Nothing, PendingRequest] =
    Stage.get[PendingRequest].orElseSucceed(PendingRequest())

  /** Fail a scenario clearly if the backend lacks a capability it needs. */
  protected def requiring(caps: Capability*): ZIO[MockControl, Throwable, Unit] =
    ZIO.serviceWithZIO[MockControl](_.require(caps*)).mapError(u => new RuntimeException(u.message))

  // ── common steps ──────────────────────────────────────────────────────────

  // Body/headers are staged by their own steps (below) so the send step stays a
  // single unambiguous `sends {string} {string}` — no overlapping step prefix.
  When("the request body is " / string) { (b: String) =>
    pendingRequest.flatMap(pr => Stage.put(pr.copy(body = Some(b))))
  }

  When("the request header " / string / " is " / string) { (h: String, v: String) =>
    pendingRequest.flatMap(pr => Stage.put(pr.copy(headers = pr.headers.add(h, v))))
  }

  When("the client sends " / string / " " / string) { (m: String, path: String) =>
    for
      method <- methodOf(m)
      pr     <- pendingRequest
      t0     <- Clock.nanoTime
      resp   <- send(method, path, pr.headers, pr.body)
      t1     <- Clock.nanoTime
      _      <- Stage.put(resp)
      _      <- Stage.put(ElapsedMillis((t1 - t0) / 1000000L))
    yield ()
  }

  Then("the response took at least " / int / " millis") { (min: Int) =>
    Stage
      .get[ElapsedMillis]
      .mapError(e => new IllegalStateException(e.message))
      .flatMap(el => Assertions.assertTrue(el.millis >= min.toLong, s"elapsed ${el.millis}ms < ${min}ms"))
  }

  Then("the recorded " / string / " to " / string / " has body " / string) { (m: String, path: String, body: String) =>
    for
      space  <- activeSpace
      method <- methodOf(m)
      reqs   <- ctl(_.received(space))
      _ <- Assertions.assertTrue(
             reqs.exists(r => r.method == method && r.uri.endsWith(path) && r.body.contains(body)),
             s"no $m $path with body '$body'; have ${reqs.map(r => s"${r.method} ${r.uri} ${r.body}").mkString(" | ")}"
           )
    yield ()
  }

  Then("the response status is " / int) { (expected: Int) =>
    lastResponse.flatMap(r => Assertions.assertEquals(r.status, expected, "response status"))
  }

  Then("the response body is " / string) { (expected: String) =>
    lastResponse.flatMap(r => Assertions.assertEquals(r.body, expected, "response body"))
  }

  Then("the response body contains " / string) { (expected: String) =>
    lastResponse.flatMap(r => Assertions.assertTrue(r.body.contains(expected), s"body '${r.body}' lacks '$expected'"))
  }

  Then("the response body is empty") {
    lastResponse.flatMap(r => Assertions.assertTrue(r.body.isEmpty, s"expected empty body, got '${r.body}'"))
  }

  Then("the response header " / string / " is " / string) { (name: String, expected: String) =>
    lastResponse.flatMap { r =>
      r.headers.first(name) match
        case Some(actual) => Assertions.assertEquals(actual, expected, s"response header '$name'")
        case None =>
          ZIO.fail(
            new AssertionError(s"header '$name' absent; have ${r.headers.keys.mkString(",")}")
          )
    }
  }

  Then("the response header " / string / " has values " / string) { (name: String, csv: String) =>
    lastResponse.flatMap { r =>
      val expected = csv.split(",").map(_.trim).toList
      Assertions.assertEquals(r.headers.values(name), expected, s"response header '$name' values")
    }
  }

  Then("the mock recorded " / int / " requests") { (expected: Int) =>
    for
      space <- activeSpace
      reqs  <- ctl(_.received(space))
      _     <- Assertions.assertEquals(reqs.size, expected, "recorded count")
    yield ()
  }

  Then("a " / string / " to " / string / " was recorded") { (m: String, path: String) =>
    for
      space  <- activeSpace
      method <- methodOf(m)
      reqs   <- ctl(_.received(space))
      _ <- Assertions.assertTrue(
             reqs.exists(r => r.method == method && r.uri.endsWith(path)),
             s"no $m $path recorded; have ${reqs.map(r => s"${r.method} ${r.uri}").mkString(", ")}"
           )
    yield ()
  }
}

/** Rules accumulated by `Given` steps before the space is provisioned. */
final case class PendingRules(rules: List[MockRule])

/** Body/headers staged for the next `send` by their own steps. */
final case class PendingRequest(headers: Headers = Headers.empty, body: Option[String] = None)

/** Wall-clock duration of the last send, for latency assertions. */
final case class ElapsedMillis(millis: Long)
