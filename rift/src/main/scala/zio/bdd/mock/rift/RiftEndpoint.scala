package zio.bdd.mock.rift

import zio.*
import zio.bdd.mock.MockError

/**
 * Where Rift's admin API lives and how an imposter's port maps to a URI the SUT
 * can actually reach — the seam that isolates the container port-mapping
 * wrinkle from the protocol logic.
 *
 * With a container, imposters bind ports *inside* the container that
 * testcontainers maps to arbitrary host ports, so the adapter cannot pick a
 * host port up front (the way [[zio.bdd.mock.Provisioning]] does for in-process
 * backends). Instead it draws an imposter port from a pre-exposed pool and asks
 * the endpoint for the reachable base URI. Under host networking the mapping is
 * the identity; the pooled implementation below covers both.
 */
private[rift] trait RiftEndpoint:
  /** Base URL of the admin API, e.g. `http://localhost:2525`. */
  def adminBase: String

  /** Take an imposter port from the pool, or fail if the pool is exhausted. */
  def acquirePort: IO[MockError, Int]

  /**
   * Claim a specific (caller-authored) port from the pool's free-list. Returns
   * `true` if the port was in the pool (and is now removed, so `acquirePort`
   * can't hand it out concurrently), `false` if it was out of range (pool
   * untouched). `true` means the port is pool-managed and should be released on
   * destroy; `false` means it's a fixed out-of-pool port the pool never owns.
   */
  def claimPort(port: Int): UIO[Boolean]

  /** Return a port to the pool after an imposter is destroyed. */
  def releasePort(port: Int): UIO[Unit]

  /** The SUT-reachable base URI for an imposter bound to `imposterPort`. */
  def baseUriFor(imposterPort: Int): String

private[rift] object RiftEndpoint:

  /**
   * A fixed pool of imposter ports plus a caller-supplied mapping from an
   * imposter port to its reachable URI (identity under host networking; the
   * container's host-mapped port otherwise).
   */
  def pooled(admin: String, ports: List[Int])(hostFor: Int => String): UIO[RiftEndpoint] =
    Ref.make(ports).map(free => Pooled(admin, free, hostFor))

  private final case class Pooled(adminBase: String, free: Ref[List[Int]], hostFor: Int => String) extends RiftEndpoint:
    def acquirePort: IO[MockError, Int] =
      free.modify {
        case head :: tail => (ZIO.succeed(head), tail)
        case Nil          => (ZIO.fail(MockError.ProvisionFailed("Rift imposter port pool exhausted")), Nil)
      }.flatten

    def claimPort(port: Int): UIO[Boolean] =
      free.modify(list => if list.contains(port) then (true, list.filterNot(_ == port)) else (false, list))

    def releasePort(port: Int): UIO[Unit] = free.update(port :: _)

    def baseUriFor(imposterPort: Int): String = hostFor(imposterPort)
