package zio.bdd.mock

/**
 * A minimal, portable spec a [[MockSource.Dsl]] carries: canonical rules plus
 * an *advisory* port. The fluent builders that produce a `MockSpec` land in
 * #112 — here it is just the neutral payload the DSL source wraps.
 *
 * `port` is advisory only: [[Provisioning]] always strips it and auto-assigns a
 * fresh free port (see the fixed-port trap in #111). It is retained on the
 * normalized form purely for diagnostics.
 */
final case class MockSpec(rules: List[MockRule], port: Option[Int] = None)

/**
 * Where mock definitions come from. Every case normalizes through the single
 * [[Provisioning]] path to one [[NormalizedSource]] per space before reaching
 * an adapter, so adapters never re-implement loading, memoization or auto-port.
 *
 *   - `Dsl` : already-canonical rules built in-code.
 *   - `Json` : a raw backend wire document, inline.
 *   - `Resource` : a classpath resource holding a raw wire document.
 *   - `File` : a filesystem file holding a raw wire document.
 *   - `Dir` : a filesystem directory; each regular file becomes one space.
 *
 * The raw cases stay un-parsed here: each adapter parses its own wire format
 * (#113 Rift, #122 WireMock). This module owns only the neutral plumbing.
 */
enum MockSource:
  case Dsl(spec: MockSpec)
  case Json(raw: String)
  case Resource(path: String)
  case File(path: String)
  case Dir(path: String)
