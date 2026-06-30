package zio.bdd.mock

import zio.test.*

/**
 * The [[Headers]] value type (#162): multi-value per key, canonical lower-case
 * keys (so cross-adapter recorded-header assertions are well-defined), and
 * insertion-ordered values.
 */
object HeadersSpec extends ZIOSpecDefault:

  def spec = suite("Headers")(
    test("keys are canonicalised to lower-case for construction, lookup, and equality") {
      val h = Headers("Content-Type" -> "json")
      assertTrue(
        h.first("content-type").contains("json"),
        h.first("CONTENT-TYPE").contains("json"), // lookup is case-insensitive
        h.contains("Content-Type"),
        h.keys == Set("content-type"),
        h == Headers("content-type" -> "json") // mixed/lower construction compare equal
      )
    },
    test("multiple values per key are preserved in insertion order") {
      val h = Headers.multi("X-Multi" -> List("a", "b"))
      assertTrue(
        h.values("x-multi") == List("a", "b"),
        h.first("x-multi").contains("a")
      )
    },
    test("add appends under a key, building a multi-value header") {
      val h = Headers.empty.add("Set-Cookie", "a=1").add("set-cookie", "b=2")
      assertTrue(h.values("set-cookie") == List("a=1", "b=2"), h.keys == Set("set-cookie"))
    },
    test("a missing key yields no values; empty is empty; an empty value list adds no key") {
      assertTrue(
        Headers.empty.isEmpty,
        Headers.empty.values("nope") == Nil,
        Headers.empty.first("nope").isEmpty,
        Headers("a" -> "1").nonEmpty,
        Headers.multi("X" -> Nil) == Headers.empty, // a key with no values is absent
        !Headers.multi("X" -> Nil).contains("X")
      )
    },
    test("fromSingle lifts a single-valued map with canonical keys") {
      assertTrue(Headers.fromSingle(Map("A" -> "1", "B" -> "2")) == Headers("a" -> "1", "b" -> "2"))
    }
  )
