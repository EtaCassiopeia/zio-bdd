package zio.bdd.mock

import zio.*
import zio.test.*

import java.net.{ServerSocket, URI}
import java.nio.file.{Files, Path}

object ProvisioningSpec extends ZIOSpecDefault:

  // A canonical rule used wherever a DSL source needs content. Its shape is
  // irrelevant to this issue — provisioning never inspects the rules, only
  // routes them through normalisation and auto-port assignment.
  private val pingRule =
    MockRule(
      `match` = RequestMatch(method = Some(Method.Get), path = PathMatch.Exact("/ping")),
      respond = ResponseDef(status = 200, body = Body.Text("pong"))
    )

  /**
   * A `serve` callback that records the (space, normalized source) it was asked
   * to stand up, without binding anything.
   */
  private def recording(log: Ref[List[(MockSpace, NormalizedSource)]]): (NormalizedSource, MockSpace) => UIO[Unit] =
    (src, space) => log.update(_ :+ (space, src))

  private def portOf(space: MockSpace): Int = URI(space.baseUri).getPort

  private def isInvalidDefinition(res: Either[MockError, Any]): Boolean =
    res.left.exists(_.isInstanceOf[MockError.InvalidDefinition])

  def spec = suite("Provisioning")(
    test("provision resolves a classpath Resource to one space carrying its raw text") {
      for
        prov   <- Provisioning.make
        log    <- Ref.make(List.empty[(MockSpace, NormalizedSource)])
        spaces <- prov.provision(MockSource.Resource("mocks/ping.json"))(recording(log))
        seen   <- log.get
      yield
        val (space, src) = seen.head
        assertTrue(
          spaces.size == 1,
          seen.size == 1,
          src.name == "ping.json",
          src.payload match
            case SourcePayload.Raw(t) => t.contains("\"/ping\"")
            case _                    => false,
          portOf(space) > 0
        )
    },
    test("provision of a Dir yields one space per file, named by file") {
      ZIO.scoped {
        for
          dir    <- tempDir
          _      <- writeFile(dir.resolve("a.json"), """{"a":1}""")
          _      <- writeFile(dir.resolve("b.json"), """{"b":2}""")
          prov   <- Provisioning.make
          log    <- Ref.make(List.empty[(MockSpace, NormalizedSource)])
          spaces <- prov.provision(MockSource.Dir(dir.toString))(recording(log))
          seen   <- log.get
        yield assertTrue(
          spaces.size == 2,
          seen.map(_._2.name).sorted == List("a.json", "b.json"),
          // every resolved space got its own auto-assigned port
          spaces.map(portOf).distinct.size == 2
        )
      }
    },
    test("provisioning a fixed-port source twice gives two independently bindable spaces (fixed-port trap)") {
      // The source authors a single fixed port; provisioning it twice must NOT
      // try to bind that port twice. Each provision auto-assigns a distinct free
      // port, so both `serve` calls can bind a real socket simultaneously.
      val authored        = 18080
      val fixedPortSource = MockSource.Dsl(MockSpec(List(pingRule), port = Some(authored)))

      def bindServe(sockets: Ref[List[ServerSocket]]): (NormalizedSource, MockSpace) => IO[MockError, Unit] =
        (_, space) =>
          ZIO
            .attempt(ServerSocket(portOf(space)))
            .mapError(e => MockError.CommunicationError(e.getMessage))
            .flatMap(s => sockets.update(_ :+ s))

      ZIO.scoped {
        for
          prov    <- Provisioning.make
          sockets <- Ref.make(List.empty[ServerSocket])
          _       <- ZIO.addFinalizer(sockets.get.flatMap(ss => ZIO.foreachDiscard(ss)(s => ZIO.succeed(s.close()))))
          first   <- prov.provision(fixedPortSource)(bindServe(sockets))
          second  <- prov.provision(fixedPortSource)(bindServe(sockets))
          bound   <- sockets.get
        yield
          val p1 = portOf(first.head)
          val p2 = portOf(second.head)
          assertTrue(
            first.size == 1,
            second.size == 1,
            p1 != p2, // authored port was stripped + auto-assigned, twice
            p1 != authored,
            p2 != authored,
            bound.size == 2, // both sockets bound at once -> no collision
            bound.forall(_.isBound)
          )
      }
    },
    test("authored port is advisory: stripped from baseUri, auto-assigned port used instead") {
      val authored = 19090
      for
        prov   <- Provisioning.make
        log    <- Ref.make(List.empty[(MockSpace, NormalizedSource)])
        spaces <- prov.provision(MockSource.Dsl(MockSpec(List(pingRule), port = Some(authored))))(recording(log))
      yield
        val space = spaces.head
        assertTrue(
          portOf(space) != authored,
          portOf(space) > 0,
          space.baseUri.startsWith("http://localhost:"),
          space.inject(HttpRequest(Method.Get, "http://x/")) == HttpRequest(Method.Get, "http://x/")
        )
    },
    test("normalize memoizes: a source deleted after first normalize still resolves from cache") {
      ZIO.scoped {
        for
          dir   <- tempDir
          file   = dir.resolve("once.json")
          _     <- writeFile(file, """{"once":true}""")
          prov  <- Provisioning.make
          first <- prov.normalize(MockSource.File(file.toString))
          _     <- ZIO.attempt(Files.delete(file)).orDie
          again <- prov.normalize(MockSource.File(file.toString))
        yield assertTrue(
          first == again,
          first.head.payload == SourcePayload.Raw("""{"once":true}""")
        )
      }
    },
    test("a Dsl source normalizes to canonical rules with its advisory port retained") {
      for
        prov <- Provisioning.make
        norm <- prov.normalize(MockSource.Dsl(MockSpec(List(pingRule), port = Some(1234))))
      yield assertTrue(
        norm.size == 1,
        norm.head.authoredPort.contains(1234),
        norm.head.payload == SourcePayload.Rules(List(pingRule))
      )
    },
    test("a Json source normalizes to a raw payload named \"json\" with no authored port") {
      for
        prov <- Provisioning.make
        norm <- prov.normalize(MockSource.Json("""{"raw":true}"""))
      yield assertTrue(
        norm.size == 1,
        norm.head.name == "json",
        norm.head.authoredPort.isEmpty,
        norm.head.payload == SourcePayload.Raw("""{"raw":true}""")
      )
    },
    test("a File source normalizes to the file's raw contents, named by file") {
      ZIO.scoped {
        for
          dir  <- tempDir
          file  = dir.resolve("greeting.json")
          _    <- writeFile(file, """{"hello":"world"}""")
          prov <- Provisioning.make
          norm <- prov.normalize(MockSource.File(file.toString))
        yield assertTrue(
          norm.size == 1,
          norm.head.name == "greeting.json",
          norm.head.payload == SourcePayload.Raw("""{"hello":"world"}""")
        )
      }
    },
    test("a missing Resource fails with InvalidDefinition") {
      for
        prov <- Provisioning.make
        res  <- prov.normalize(MockSource.Resource("mocks/does-not-exist.json")).either
      yield assertTrue(isInvalidDefinition(res))
    },
    test("a missing File fails with InvalidDefinition") {
      for
        prov <- Provisioning.make
        res  <- prov.normalize(MockSource.File("/no/such/path/missing.json")).either
      yield assertTrue(isInvalidDefinition(res))
    },
    test("a Dir pointing at a non-directory fails with InvalidDefinition") {
      ZIO.scoped {
        for
          dir  <- tempDir
          file  = dir.resolve("not-a-dir.json")
          _    <- writeFile(file, "{}")
          prov <- Provisioning.make
          res  <- prov.normalize(MockSource.Dir(file.toString)).either
        yield assertTrue(isInvalidDefinition(res))
      }
    }
  )

  // --- filesystem helpers (scoped temp dir, auto-deleted) -------------------

  private val tempDir: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("provisioning-spec"))) { dir =>
      ZIO.attempt {
        import scala.jdk.CollectionConverters.*
        val walk = Files.walk(dir)
        try walk.iterator().asScala.toList.sortBy(_.toString).reverse.foreach(Files.deleteIfExists(_))
        finally walk.close()
      }.orDie
    }

  private def writeFile(path: Path, content: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt(Files.writeString(path, content)).unit
