package zio.bdd.mock.wiremock

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import java.security.KeyStore
import scala.jdk.CollectionConverters.*

/**
 * The HTTPS WireMock server's key stores hold a private key, so the temp files
 * WireMock reads them from must not outlive server start (#343).
 */
object WireMockTlsStoreSpec extends ZIOSpecDefault:

  private val pass = "zio-bdd".toCharArray

  private val emptyStore: Task[KeyStore] =
    ZIO.attempt { val ks = KeyStore.getInstance("PKCS12"); ks.load(null, null); ks }

  private val tempDir: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("zb-343-")))(d =>
      ZIO.attemptBlocking(Files.deleteIfExists(d)).ignore
    )

  private def entries(dir: Path): Task[List[Path]] =
    ZIO.attemptBlocking {
      val s = Files.list(dir)
      try s.iterator().asScala.toList
      finally s.close()
    }

  def spec = suite("WireMock TLS temp stores (#343)")(
    test("the store file exists inside its scope and is deleted when the scope closes") {
      ZIO.scoped {
        for
          dir    <- tempDir
          ks     <- emptyStore
          inside <- ZIO.scoped(WireMockControl.tempStore(ks, pass, Some(dir)).flatMap(p => entries(dir).map(p -> _)))
          after  <- entries(dir)
        yield assertTrue(inside._2 == List(inside._1), after.isEmpty)
      }
    },
    test("a store that fails to write leaves no file behind") {
      ZIO.scoped {
        for
          dir   <- tempDir
          ks    <- ZIO.attempt(KeyStore.getInstance("PKCS12")) // never loaded: store() throws
          exit  <- ZIO.scoped(WireMockControl.tempStore(ks, pass, Some(dir))).exit
          after <- entries(dir)
        yield assertTrue(exit.isFailure, after.isEmpty)
      }
    }
  )
