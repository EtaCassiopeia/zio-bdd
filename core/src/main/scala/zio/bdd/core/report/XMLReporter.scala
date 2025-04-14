package zio.bdd.core.report

import zio.ZIO
import zio.bdd.core.{FeatureResult, LogCollector}

import java.time.Instant

case class XMLReporter(filePath: String) extends Reporter {
  override def report(results: List[FeatureResult]): ZIO[LogCollector, Throwable, Unit] =
    for {
      logCollector <- ZIO.service[LogCollector]
      testSuites <- ZIO.foreach(results) { feature =>
                      val testCases = feature.scenarioResults.map { scenarioResult =>
                        for {
                          scenarioLogs <- logCollector.getScenarioLogs(scenarioResult.scenario.id.toString)
                        } yield JUnitXMLFormatter.TestCase(
                          name = scenarioResult.scenario.name,
                          succeeded = scenarioResult.isPassed,
                          logs = scenarioLogs,
                          timestamp = Instant.now(),
                          duration = 0
                        )
                      }
                      ZIO.collectAll(testCases).map { cases =>
                        JUnitXMLFormatter.TestSuite(
                          name = feature.feature.name,
                          cases = cases,
                          timestamp = Instant.now()
                        )
                      }
                    }
      _ <- ZIO.foreach(testSuites) { suite =>
             JUnitXMLFormatter.writeToFile(suite, filePath, JUnitXMLFormatter.Format.JUnit5)
           }
    } yield ()
}
