package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock as spi

/**
 * A fixed pool of imposter ports a CONTAINER transport pre-exposes at startup
 * (#113, #303, re-based onto the rift-scala SDK for #285).
 *
 * With the SDK, `ImposterHandle.uri` already resolves through Docker's port
 * mapping (the container connector's `hostResolver`), and an engine-assigned
 * port (`ImposterBuilder.port(0)`) is fine for an in-process or bare-metal
 * engine. It is NOT fine for a container: only pre-exposed ports are ever
 * mapped, so a dynamically-assigned one binds inside the container on a port
 * nothing published — the imposter is created but host-unreachable. So the
 * container (and `connect`-to-a-pooled-remote-engine) transports still need to
 * draw imposter ports from a known, pre-declared pool; the embedded transport
 * needs no pool at all (`RiftMockControl`'s `portPool` is `None` there).
 */
private[rift] trait RiftPortPool:
  /** Take a port from the pool, or fail if it is exhausted. */
  def acquirePort: IO[spi.MockError, Int]

  /**
   * Claim a specific (caller-authored) port from the pool's free-list. Returns
   * `true` if the port was in the pool (and is now removed, so `acquirePort`
   * can't hand it out concurrently), `false` if it was out of range (pool
   * untouched, an out-of-pool fixed port the pool never owns).
   */
  def claimPort(port: Int): UIO[Boolean]

  /** Return a port to the pool after an imposter bound to it is destroyed. */
  def releasePort(port: Int): UIO[Unit]

  /**
   * Fail fast, with a typed [[spi.MockError.InvalidDefinition]], if `port` is
   * outside this pool's range — a container-authored port testcontainers never
   * exposed. Checked BEFORE creating the imposter: an unmapped port would
   * otherwise surface deep inside the SDK's container hostResolver (a
   * `getMappedPort` throw), which is not a `RiftError` and would die as a
   * defect instead of failing as a typed value.
   */
  def ensureInRange(port: Int): IO[spi.MockError, Unit]

private[rift] object RiftPortPool:
  def fixed(ports: Vector[Int]): UIO[RiftPortPool] =
    Ref.make(ports).map(Fixed(_, ports.toSet))

  private final case class Fixed(free: Ref[Vector[Int]], all: Set[Int]) extends RiftPortPool:
    def acquirePort: IO[spi.MockError, Int] =
      free.modify {
        case head +: tail => (ZIO.succeed(head), tail)
        case empty        => (ZIO.fail(spi.MockError.ProvisionFailed("Rift imposter port pool exhausted")), empty)
      }.flatten

    def claimPort(port: Int): UIO[Boolean] =
      free.modify(ps => if ps.contains(port) then (true, ps.filterNot(_ == port)) else (false, ps))

    def releasePort(port: Int): UIO[Unit] = free.update(_ :+ port)

    def ensureInRange(port: Int): IO[spi.MockError, Unit] =
      if all.contains(port) then ZIO.unit
      else
        ZIO.fail(
          spi.MockError.InvalidDefinition(
            s"authored imposter port $port is not reachable — it is outside the backend's exposed port pool; " +
              "use a port in the pool range or omit the port to auto-assign"
          )
        )
