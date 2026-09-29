package zio.bdd.mock.rift

import java.time.Instant

import zio.bdd.mock as spi
import zio.test.*

import rift.json.Json
import rift.model.{Headers, Method, Port, RecordedRequest}

/**
 * Engine-free checks of the raw-document and readback translations: `"port": 0`
 * in a raw document is an absent port (#350), and a recorded string body reads
 * back as its raw text (#349).
 */
object RiftModelMappingSpec extends ZIOSpecDefault:

  private def doc(port: Option[String]): String =
    val portField = port.fold("")(p => s""""port": $p, """)
    s"""{$portField"protocol": "http", "stubs": [{"responses": [{"is": {"statusCode": 200}}]}]}"""

  private def recorded(body: Option[Json], bodyText: Option[String]): RecordedRequest =
    RecordedRequest(
      method = Method.POST,
      path = "/submit",
      query = Map.empty,
      headers = Headers.empty,
      body = body,
      bodyText = bodyText,
      timestamp = Instant.EPOCH,
      requestFrom = None,
      flowId = None,
      pathParams = Map.empty,
      raw = Json.obj()
    )

  def spec = suite("RiftModelMapping")(
    suite("fromRaw port handling (#350)")(
      test("port 0 is accepted as no port when the document port is honoured") {
        val result = RiftModelMapping.fromRaw(doc(Some("0")), honourDocPort = true)
        assertTrue(result.map(_.port) == Right(None))
      },
      test("port 0 is accepted as no port when the document port is stripped") {
        val result = RiftModelMapping.fromRaw(doc(Some("0")), honourDocPort = false)
        assertTrue(result.map(_.port) == Right(None))
      },
      test("a valid non-zero port is preserved when the document port is honoured") {
        val result = RiftModelMapping.fromRaw(doc(Some("4650")), honourDocPort = true)
        assertTrue(result.map(_.port.map(Port.value)) == Right(Some(4650)))
      },
      test("a valid non-zero port is stripped when the document port is not honoured") {
        val result = RiftModelMapping.fromRaw(doc(Some("4650")), honourDocPort = false)
        assertTrue(result.map(_.port) == Right(None))
      },
      test("an absent port stays absent") {
        val result = RiftModelMapping.fromRaw(doc(None), honourDocPort = true)
        assertTrue(result.map(_.port) == Right(None))
      },
      test("an out-of-range port is still refused as an invalid definition") {
        val result = RiftModelMapping.fromRaw(doc(Some("70000")), honourDocPort = false)
        assertTrue(result match
          case Left(spi.MockError.InvalidDefinition(reason)) => reason.contains("port")
          case _                                             => false
        )
      }
    ),
    suite("toRecorded body readback (#349)")(
      test("a JSON string body reads back as its raw text") {
        val r = RiftModelMapping.toRecorded(recorded(Some(Json.Str("hello=world")), None))
        assertTrue(r.body == Some("hello=world"))
      },
      test("an object body reads back as rendered JSON") {
        val json = Json.obj("a" -> Json.fromInt(1))
        val r    = RiftModelMapping.toRecorded(recorded(Some(json), None))
        assertTrue(r.body == Some(json.render))
      },
      test("bodyText takes precedence over body") {
        val r = RiftModelMapping.toRecorded(recorded(Some(Json.Str("parsed")), Some("raw text")))
        assertTrue(r.body == Some("raw text"))
      },
      test("no body reads back as None") {
        val r = RiftModelMapping.toRecorded(recorded(None, None))
        assertTrue(r.body.isEmpty)
      }
    )
  )
