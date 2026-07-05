package zio.bdd.mock

import zio.*

import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * The neutral, normalized form of a single mock space — the "wire config" every
 * [[MockSource]] reduces to before an adapter consumes it.
 *
 *   - `name` : the space label (the file name for a [[MockSource.Dir]];
 *     "dsl"/"json"/the resource or file base name otherwise).
 *   - `payload` : canonical rules (from the DSL) or raw backend wire text (from
 *     json/resource/file) the adapter parses itself.
 *   - `authoredPort`: the port the author requested, if any. Honoured as an
 *     opt-in fixed port by the adapters' port selection
 *     ([[Provisioning.choosePort]], #211); when absent a fresh free port is
 *     auto-assigned (the default).
 */
final case class NormalizedSource(name: String, payload: SourcePayload, authoredPort: Option[Int])

/** The two shapes a normalized source can take. */
enum SourcePayload:
  case Rules(rules: List[MockRule])
  case Raw(text: String)

private object MockIO:
  /**
   * Turn a JVM exception into a typed [[MockError.ProvisionFailed]] without
   * losing the path/exception class, and without rendering a null message as
   * the literal "null". `CommunicationError` is reserved for backend comms, so
   * a source-read failure is *not* one.
   */
  def failed(action: String, e: Throwable): MockError =
    val msg = Option(e.getMessage).getOrElse("<no message>")
    MockError.ProvisionFailed(s"$action failed: ${e.getClass.getSimpleName}: $msg")

/**
 * Hands out localhost ports that are free at the moment of issue and distinct
 * across the allocator's lifetime — the default (no authored port) path, and
 * the antidote to the fixed-port trap: every auto-assigned space gets its own
 * port. (An explicitly authored port bypasses the allocator entirely — see
 * [[Provisioning.choosePort]], #211.)
 *
 * Allocation probes the OS (`ServerSocket(0)`) and closes the probe, so there
 * is an inherent TOCTOU window before the caller binds the port; distinctness
 * within the allocator removes the *self*-collision (auto-assigning the same
 * port to two spaces).
 */
trait PortAllocator:
  def freePort: IO[MockError, Int]

final case class PortAllocatorLive(issued: Ref[Set[Int]]) extends PortAllocator:
  // Bound the dedup retry so a constrained ephemeral range surfaces as a typed
  // failure instead of livelocking on probe→close→same-port forever.
  private val maxAttempts = 100

  private val probe: IO[MockError, Int] =
    ZIO
      .scoped(ZIO.fromAutoCloseable(ZIO.attempt(ServerSocket(0))).map(_.getLocalPort))
      .mapError(MockIO.failed("allocating a free port", _))

  def freePort: IO[MockError, Int] =
    def go(attempt: Int): IO[MockError, Int] =
      if attempt >= maxAttempts then
        ZIO.fail(MockError.ProvisionFailed(s"could not obtain a distinct free port after $maxAttempts attempts"))
      else
        probe.flatMap { p =>
          issued
            .modify(s => if s(p) then (false, s) else (true, s + p))
            .flatMap(added => if added then ZIO.succeed(p) else go(attempt + 1))
        }
    go(0)

object PortAllocator:
  val make: UIO[PortAllocator]     = Ref.make(Set.empty[Int]).map(PortAllocatorLive(_))
  val layer: ULayer[PortAllocator] = ZLayer(make)

/**
 * The single provisioning entry point (#111). Normalizes any [[MockSource]] to
 * one [[NormalizedSource]] per space — memoizing the parsed form so repeated
 * provisioning of the same source is cheap. This convenience always
 * auto-assigns a free port; adapters that support an opt-in fixed port select
 * via [[choosePort]] on their own serve path instead (#211).
 */
final case class Provisioning(allocator: PortAllocator, cache: Ref[Map[MockSource, List[NormalizedSource]]]):

  /**
   * Select the port to bind an imposter on: honour a caller-authored port
   * verbatim (the opt-in fixed-port path, #211 — e.g. `MockSpec.onPort(n)`),
   * otherwise auto-assign a fresh free one (the share-nothing default). An
   * authored port that is already bound is not worked around here — the adapter
   * surfaces the backend's bind failure rather than silently falling back to a
   * random port.
   */
  def choosePort(authoredPort: Option[Int]): IO[MockError, Int] =
    authoredPort match
      case Some(port) => ZIO.succeed(port)
      case None       => allocator.freePort

  /** Resolve a source to one normalized form per space; memoized by source. */
  def normalize(source: MockSource): IO[MockError, List[NormalizedSource]] =
    cache.get.flatMap(_.get(source) match
      case Some(cached) => ZIO.succeed(cached)
      case None         => load(source).tap(ns => cache.update(_.updated(source, ns)))
    )

  /**
   * Stand up one space per normalized source. `serve` is the adapter seam: it
   * receives the normalized payload and the resolved [[MockSpace]] (whose
   * `baseUri` already carries the auto-assigned port) and binds the backend.
   */
  def provision[E >: MockError](
    source: MockSource
  )(serve: (NormalizedSource, MockSpace) => IO[E, Unit]): IO[E, List[MockSpace]] =
    normalize(source).flatMap { sources =>
      ZIO.foreach(sources) { src =>
        for
          port <- allocator.freePort
          space = MockSpace(s"http://localhost:$port", identity, SpaceId(s"${src.name}-$port"))
          _    <- serve(src, space)
        yield space
      }
    }

  private def load(source: MockSource): IO[MockError, List[NormalizedSource]] =
    source match
      case MockSource.Dsl(spec) =>
        ZIO.succeed(List(NormalizedSource("dsl", SourcePayload.Rules(spec.rules), spec.port)))
      case MockSource.Json(raw) =>
        ZIO.succeed(List(NormalizedSource("json", SourcePayload.Raw(raw), None)))
      case MockSource.Resource(path) =>
        readResource(path).map(t => List(NormalizedSource(baseName(path), SourcePayload.Raw(t), None)))
      case MockSource.File(path) =>
        readFile(path).map(t => List(NormalizedSource(baseName(path), SourcePayload.Raw(t), None)))
      case MockSource.Dir(path) =>
        readDir(path)

  private def readResource(path: String): IO[MockError, String] =
    val normalized = path.stripPrefix("/")
    // Acquire the (possibly null) stream uninterruptibly and always close it, so
    // an interrupt or read failure can never leak the handle.
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking(Option(getClass.getClassLoader.getResourceAsStream(normalized)))
        .mapError(MockIO.failed(s"opening resource $path", _))
    )(opt => ZIO.foreachDiscard(opt)(s => ZIO.attempt(s.close()).ignore)) {
      case None => ZIO.fail(MockError.InvalidDefinition(s"resource not found: $path"))
      case Some(stream) =>
        ZIO
          .attemptBlocking(String(stream.readAllBytes(), StandardCharsets.UTF_8))
          .mapError(MockIO.failed(s"reading resource $path", _))
    }

  private def readFile(path: String): IO[MockError, String] =
    val p = Paths.get(path)
    ZIO
      .attemptBlocking(Files.isRegularFile(p))
      .mapError(MockIO.failed(s"stat-ing file $path", _))
      .flatMap { isFile =>
        if !isFile then ZIO.fail(MockError.InvalidDefinition(s"file not found: $path"))
        else
          ZIO
            .attemptBlocking(Files.readString(p, StandardCharsets.UTF_8))
            .mapError(MockIO.failed(s"reading file $path", _))
      }

  private def readDir(path: String): IO[MockError, List[NormalizedSource]] =
    val dir = Paths.get(path)
    for
      isDir <- ZIO
                 .attemptBlocking(Files.isDirectory(dir))
                 .mapError(MockIO.failed(s"stat-ing directory $path", _))
      _ <- ZIO.unless(isDir)(ZIO.fail(MockError.InvalidDefinition(s"not a directory: $path")))
      files <- ZIO.attemptBlocking {
                 import scala.jdk.CollectionConverters.*
                 val stream = Files.list(dir)
                 try stream.iterator().asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.getFileName.toString)
                 finally stream.close()
               }
                 .mapError(MockIO.failed(s"listing directory $path", _))
      out <- ZIO.foreach(files) { f =>
               readFile(f.toString).map(t => NormalizedSource(f.getFileName.toString, SourcePayload.Raw(t), None))
             }
    yield out

  private def baseName(path: String): String =
    Option(Paths.get(path).getFileName).map(_.toString).getOrElse(path)

object Provisioning:
  private def from(allocator: PortAllocator): UIO[Provisioning] =
    Ref.make(Map.empty[MockSource, List[NormalizedSource]]).map(Provisioning(allocator, _))

  val make: UIO[Provisioning] = PortAllocator.make.flatMap(from)

  val layer: URLayer[PortAllocator, Provisioning] = ZLayer(ZIO.serviceWithZIO[PortAllocator](from))

  val live: ULayer[Provisioning] = PortAllocator.layer >>> layer
