package samples.mock.support

import zio.*

import java.net.{ServerSocket, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, Paths}
import java.time.Duration as JDuration
import java.util.concurrent.TimeUnit

/**
 * A released `rift` binary spawned as a child process — the engine behind the
 * `rift-external` backend.
 *
 * In rift-conformance this backend borrowed the Plane A `RiftHarness`, which
 * also drives intercept, metrics, reload, container and embedded engines. Plane
 * B needs none of that: only an admin URL to hand to `Rift.connect`. The binary
 * comes from `$RIFT_BUILD_DIR` (see `scripts/fetch-rift.sh`, which pins the
 * release).
 */
final case class ExternalRift(adminUrl: String):

  /**
   * The imposter host for `Rift.connect`'s `hostFor`: the engine is a local
   * child, so ports map 1:1.
   */
  def hostFor(port: Int): String = s"http://localhost:$port"

object ExternalRift:

  private val ReadinessBudgetMs = 15000L
  private val SpawnAttempts     = 5
  private val client            = HttpClient.newHttpClient()

  /**
   * Spawns one engine for the run and reaps it when the scope closes. A spawn
   * that never becomes ready (most often a port taken between the free-port
   * probe and rift's own bind) is killed and retried on fresh ports.
   */
  val layer: ZLayer[Any, Throwable, ExternalRift] =
    ZLayer.scoped {
      for
        binary <- binaryFromEnv
        spawned <- ZIO
                     .acquireRelease(spawnReady(binary))((proc, _) => kill(proc).orDie)
                     .retry(Schedule.recurs(SpawnAttempts - 1))
      yield ExternalRift(s"http://localhost:${spawned._2}")
    }

  private val binaryFromEnv: Task[Path] =
    ZIO.attempt {
      val dir = Option(java.lang.System.getenv("RIFT_BUILD_DIR")).map(_.trim).filter(_.nonEmpty).getOrElse {
        throw new IllegalStateException(
          "RIFT_BUILD_DIR must be set for MOCK_BACKEND=rift-external (eval \"$(scripts/fetch-rift.sh)\")"
        )
      }
      val bin = Paths.get(dir).resolve("rift")
      if !Files.isExecutable(bin) then throw new IllegalStateException(s"no executable rift at $bin")
      bin
    }

  private def spawnReady(binary: Path): Task[(Process, Int)] =
    for
      adminPort   <- freePort
      metricsPort <- freePort
      proc        <- spawn(binary, adminPort, metricsPort)
      _           <- awaitReady(s"http://localhost:$adminPort/imposters", proc).onError(_ => kill(proc).ignore)
    yield (proc, adminPort)

  private def spawn(binary: Path, adminPort: Int, metricsPort: Int): Task[Process] =
    ZIO.attemptBlocking {
      val workDir = Files.createTempDirectory("rift-plane-b-")
      val log     = workDir.resolve("rift.log").toFile
      val pb = new ProcessBuilder(
        binary.toString,
        "--port",
        adminPort.toString,
        "--metrics-port",
        metricsPort.toString,
        "--nologfile",
        "--allow-injection"
      )
      pb.directory(workDir.toFile)
      pb.redirectErrorStream(true)
      pb.redirectOutput(log)
      val proc = pb.start()
      // An aborted sbt run skips ZIO finalizers; never leave the child running past the JVM.
      java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => if proc.isAlive then proc.destroyForcibly(): Unit))
      proc
    }

  private val freePort: Task[Int] =
    ZIO.attemptBlocking {
      val s = new ServerSocket(0)
      try s.getLocalPort
      finally s.close()
    }

  private def awaitReady(url: String, proc: Process): Task[Unit] =
    ZIO.attemptBlocking {
      val deadline           = java.lang.System.nanoTime + ReadinessBudgetMs * 1000000L
      val request            = HttpRequest.newBuilder(URI.create(url)).timeout(JDuration.ofSeconds(2)).GET().build()
      var ready              = false
      var lastErr: Throwable = null
      while !ready && java.lang.System.nanoTime < deadline do
        if !proc.isAlive then
          throw new RuntimeException(s"rift exited early (code ${proc.exitValue()}) before $url came up")
        try
          ready = client.send(request, HttpResponse.BodyHandlers.discarding()).statusCode() == 200
          if !ready then Thread.sleep(100)
        catch
          case e: java.io.IOException =>
            lastErr = e
            Thread.sleep(100)
      if !ready then throw new RuntimeException(s"rift not ready on $url within ${ReadinessBudgetMs}ms", lastErr)
    }

  /**
   * destroy, then SIGKILL; fails if the process survives both, so a leaked
   * engine is loud.
   */
  private def kill(proc: Process): Task[Unit] =
    ZIO.attemptBlocking {
      proc.destroy()
      if !proc.waitFor(5, TimeUnit.SECONDS) then proc.destroyForcibly()
      if !proc.waitFor(5, TimeUnit.SECONDS) then
        throw new RuntimeException(s"rift (pid ${proc.pid()}) survived teardown")
    }
