package samples.mock.support

import zio.schema.{DeriveSchema, Schema}

/**
 * Scenario state shared by the mock corpus. The interesting per-scenario values
 * — the active `MockSpace`, the `SutClient`, the last response — are kept in
 * `Stage` (they carry functions / aren't Schema-serialisable), so this state
 * only needs to exist to satisfy `ZIOSteps[R, S]`. All fields default so a
 * `Default[MockState]` derives from the `Schema`.
 */
final case class MockState(note: String = "")

object MockState:
  given Schema[MockState] = DeriveSchema.gen[MockState]
