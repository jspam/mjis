package mjis

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}
import CompilerTestMatchers.passIntegrationTest

class IntegrationTest extends FlatSpec with Matchers with BeforeAndAfter {

  val TestDir = "mj-test/run/"
  val testFiles = new java.io.File(TestDir).listFiles.filter(_.getName.endsWith(".mj")).sortBy(_.getName.toLowerCase)

  for (file <- testFiles) {
    file.getName should "produce correct output with the FIRM backend" in {
      file.getPath should passIntegrationTest(useFirmBackend = true)
    }

    it should "produce correct output with our backend" in {
      file.getPath should passIntegrationTest(useFirmBackend = false)
    }
  }
}
