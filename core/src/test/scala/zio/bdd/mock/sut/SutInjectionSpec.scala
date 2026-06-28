package zio.bdd.mock.sut

import zio.*
import zio.bdd.core.step.ZIOSteps
import zio.bdd.gherkin.*
import zio.bdd.mock.*
import zio.schema.{DeriveSchema, Schema}
import zio.test.*

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import scala.jdk.CollectionConverters.*

/**
 * Integration proof of #117 AC1: a suite whose SUT reads its dependency URL
 * from a per-scenario layer (`SutClient`, injected via `scenarioLayer`) runs
 * under feature×scenario parallelism with no cross-talk — each scenario's
 * dependency call lands in its own space.
 */
object SutInjectionSpec extends ZIOSpecDefault:

  final case class TestState()
  object TestState:
    given Schema[TestState] = DeriveSchema.gen[TestState]

  // A shared in-process dependency that records each request under its first path
  // segment (the PerInstance space id baked into `space.baseUri`).
  private final class TestDependency(port: Int, recordings: ConcurrentHashMap[String, CopyOnWriteArrayList[String]]):
    def perInstanceSpace(id: String): MockSpace = MockSpace(s"http://localhost:$port/$id", identity, SpaceId(id))
    def received(id: String): UIO[List[String]] =
      ZIO.succeed(Option(recordings.get(id)).map(_.asScala.toList).getOrElse(Nil))
    def recordedKeys: UIO[Set[String]] = ZIO.succeed(recordings.keySet.asScala.toSet)

  private val dependency: ZIO[Scope, Throwable, TestDependency] =
    ZIO
      .acquireRelease(
        ZIO.attempt {
          val recordings = new ConcurrentHashMap[String, CopyOnWriteArrayList[String]]()
          val server     = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
          server.createContext(
            "/",
            new HttpHandler:
              def handle(exchange: HttpExchange): Unit =
                val seg  = exchange.getRequestURI.getPath.stripPrefix("/").split("/", 2)
                val key  = seg(0)
                val path = "/" + (if seg.length > 1 then seg(1) else "")
                recordings.computeIfAbsent(key, _ => new CopyOnWriteArrayList[String]()).add(path)
                exchange.sendResponseHeaders(200, -1)
                exchange.close()
          )
          server.start()
          (server, new TestDependency(server.getAddress.getPort, recordings))
        }
      )(acquired => ZIO.attempt(acquired._1.stop(0)).orDie)
      .map(_._2)

  // The SUT's dependency client is provided per scenario, bound to a space named
  // after the scenario — never a shared/global value.
  private class SutSuite(dep: TestDependency) extends ZIOSteps[SutClient, TestState]:
    override def scenarioLayer(meta: ScenarioMetadata): ZLayer[Any, Throwable, SutClient] =
      SutClient.layer(dep.perInstanceSpace(meta.name))

    When("the SUT calls its dependency at " / string) { (path: String) =>
      ZIO.serviceWithZIO[SutClient](_.send(Method.Get, path)).unit
    }

  private def scenario(name: String, feature: String) =
    Scenario(
      name,
      Nil,
      List(Step(StepType.WhenStep, """the SUT calls its dependency at "/call"""", None, None, None)),
      Some(s"$feature.feature"),
      Some(1)
    )

  def spec = suite("SutInjection")(
    test(
      "a suite whose SUT reads its dep URL from the layer runs under feature×scenario parallelism with no cross-talk"
    ) {
      val featureSpecs = List(
        "F1" -> List("F1-s1", "F1-s2", "F1-s3"),
        "F2" -> List("F2-s1", "F2-s2", "F2-s3")
      )
      val allNames = featureSpecs.flatMap(_._2)
      ZIO.scoped {
        dependency.flatMap { dep =>
          val suite    = new SutSuite(dep)
          val features = featureSpecs.map((f, names) => Feature(f, Nil, names.map(scenario(_, f))))
          for
            results <- suite
                         .run(features, featureParallelism = 2, scenarioParallelism = 3)
                         .provideEnvironment(ZEnvironment[SutClient](SutClient.make(dep.perInstanceSpace("__outer__"))))
            recs <- ZIO.foreach(allNames)(n => dep.received(n).map((n, _)))
            keys <- dep.recordedKeys
          yield assertTrue(
            results.forall(_.isPassed),                // all scenarios passed under parallelism
            recs.forall((_, r) => r == List("/call")), // each scenario's call landed in its OWN space, exactly once
            keys == allNames.toSet                     // the server saw exactly these keys — no stray/foreign space
          )
        }
      }
    }
  )
