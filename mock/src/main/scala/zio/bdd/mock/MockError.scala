package zio.bdd.mock

/**
 * Typed failures of the MockControl core port. Deliberately *not* a `Throwable`
 * subtype: the port keeps `Throwable` out of its public signatures, surfacing
 * backend failures as values an adapter normalises a cause into (as text).
 */
enum MockError:
  case ProvisionFailed(reason: String)
  case SpaceNotFound(space: SpaceId)
  case RuleNotFound(space: SpaceId, rule: RuleId)
  case InvalidDefinition(reason: String)
  case CommunicationError(reason: String)

/**
 * Typed failure of a capability accessor when the backend does not advertise
 * the capability. Names both the gap and the backend. Not a `Throwable`: the
 * capability accessors expose it as the narrow error channel `IO[Unsupported,
 * _]`.
 */
final case class Unsupported(capability: Capability, backend: String):
  def message: String =
    s"Capability $capability is not supported by backend '$backend'"
