package zio.bdd.mock.conformance

import zio.*
import zio.bdd.mock.*
import zio.bdd.mock.rift.Rift
import zio.test.*

import org.testcontainers.containers.{GenericContainer, Network}
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.utility.DockerImageName

/**
 * The `cap-proxy` conformance (#132): the full ProxyRecord acceptance — proxy
 * to a real upstream, record, then replay with the upstream down. Both the Rift
 * container and a tiny `http-echo` upstream join one shared Docker network, so
 * Rift reaches the upstream by its container IP on that network — pure
 * container-to-container, no host networking. After the first call records the
 * upstream response, the echo container is stopped; a successful replay then
 * proves the recording is served without contacting the (now-down) upstream.
 *
 * Gated on `RIFT_IT` (Docker) like [[zio.bdd.mock.rift.RiftContainerSpec]];
 * WireMock does not advertise ProxyRecord, so the portable capability is
 * correctly Rift-only.
 */
object ProxyRecordConformanceSpec extends ZIOSpecDefault:

  private def asT(e: MockError): Throwable = new RuntimeException(s"MockError: $e")

  private val adminPort     = Rift.DefaultAdminPort
  private val imposterPorts = (0 until 4).map(Rift.DefaultImposterBase + _).toList
  private val echoText      = "echo-ok"

  private def acquire[A](mk: => A)(close: A => Unit): ZIO[Scope, Throwable, A] =
    ZIO.acquireRelease(ZIO.attemptBlocking(mk))(a => ZIO.attemptBlocking(close(a)).orDie)

  private def send(space: MockSpace, label: String): IO[Throwable, SutResponse] =
    SutClient
      .make(space)
      .send(Method.Get, "/echo")
      .timeoutFail(new RuntimeException(s"SUT $label timed out after 15s"))(15.seconds)

  private def startEcho(net: Network): GenericContainer[?] =
    val c = new GenericContainer(DockerImageName.parse("hashicorp/http-echo:1.0.0"))
    c.withNetwork(net)
    c.withCommand(s"-text=$echoText")
    c.waitingFor(Wait.forLogMessage(".*server is listening.*", 1))
    c.start()
    c

  // The echo container's IP on the shared network — Rift proxies to it directly, so the test
  // doesn't depend on Rift's Rust proxy resolving a Docker DNS alias.
  private def ipOf(c: GenericContainer[?]): String =
    c.getContainerInfo.getNetworkSettings.getNetworks.values.iterator.next.getIpAddress

  private def startRift(net: Network): GenericContainer[?] =
    val c = new GenericContainer(DockerImageName.parse(Rift.DefaultImage))
    c.withNetwork(net)
    (adminPort :: imposterPorts).foreach(p => c.addExposedPort(Integer.valueOf(p)))
    c.waitingFor(Wait.forHttp("/imposters").forPort(adminPort))
    c.start()
    c

  private val proxyTest =
    test("proxy records the upstream response and replays it after the upstream is down (#132)") {
      ZIO.scoped {
        for
          net    <- acquire(Network.newNetwork())(_.close())
          echo   <- acquire(startEcho(net))(_.stop()) // stop() is idempotent — also called mid-test below
          rift   <- acquire(startRift(net))(_.stop())
          admin   = s"http://${rift.getHost}:${rift.getMappedPort(Integer.valueOf(adminPort))}"
          hostFor = (p: Int) => s"http://${rift.getHost}:${rift.getMappedPort(Integer.valueOf(p))}"
          // Build Provisioning + control into the test scope (not a nested one) so the SDK's engine
          // handle stays alive for the whole test, not just the layer build.
          env     <- (Provisioning.live >>> Rift.connect(admin, imposterPorts)(hostFor)).build
          control  = env.get[MockControl]
          space   <- control.provision(MockSource.Dsl(MockSpec(Nil))).mapError(asT).map(_.head)
          proxy   <- control.proxyRecord.mapError(u => new RuntimeException(u.toString))
          upstream = s"http://${ipOf(echo)}:5678"
          _       <- proxy.proxy(space, RequestMatch(path = PathMatch.Exact("/echo")), upstream).mapError(asT)
          // First call: Rift proxies to the upstream and records the response. Time it out so a
          // misconfigured proxy fails fast instead of blocking on the unbounded Java HttpClient.
          first <- send(space, "first call (proxy + record)")
          // Upstream goes down. Bound the blocking stop so a stalled daemon fails fast, like the SUT calls.
          _ <- ZIO
                 .attemptBlocking(echo.stop())
                 .timeoutFail(new RuntimeException("echo.stop() timed out after 30s"))(30.seconds)
          // Second call: served from the recording — the upstream is never contacted.
          second <- send(space, "second call (replay, upstream down)")
        yield assertTrue(
          first.status == 200,
          first.body.trim == echoText,
          second.status == 200,
          second.body.trim == echoText // identical with the upstream down => replayed, not re-proxied
        )
      }
    } @@ TestAspect.withLiveClock

  def spec =
    if sys.env.contains("RIFT_IT") then suite("ProxyRecordConformance")(proxyTest)
    else suite("ProxyRecordConformance (skipped — set RIFT_IT=1 to run against a real Rift container)")()
