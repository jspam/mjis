package mjis

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}
import CompilerTestMatchers.passIntegrationTest

class IntegrationTest extends FlatSpec with Matchers with BeforeAndAfter {

  // Change this to true to see which new files our backend can handle.
  // You can append the green tests to KnownGoodFiles.
  val KnownGoodFilesCheckMode = false

  val TestDir = "mj-test/run/"

  // Files our backend can already handle
  val KnownGoodFiles = Set[String]("LocalVarDeclScope.mj", "fortytwo.mj", "Void.mj",
    "Simon_039.mj", "Simon_092.mj", "Simon_068.mj", "Simon_052.mj", "Simon_008.mj", "Simon_055.mj",
    "Simon_004.mj", "Simon_024.mj", "Simon_001.mj", "Simon_011.mj", "Simon_014.mj", "Simon_002.mj",
    "Simon_021.mj", "Simon_007.mj", "Simon_010.mj", "Simon_005.mj", "Simon_012.mj", "Simon_054.mj",
    "Simon_061.mj", "Simon_053.mj", "Simon_025.mj", "Simon_003.mj", "Simon_062.mj", "Simon_006.mj",
    "Simon_009.mj", "Simon_022.mj", "Simon_013.mj", "Simon_044.mj", "ArrayParam.mj", "LinkedListInsertion.mj",
    "NestedArrays.mj", "Simon_016.mj", "Simon_017.mj", "Simon_026.mj", "Simon_028.mj", "Simon_030.mj",
    "Simon_031.mj", "Simon_032.mj", "Simon_033.mj", "Simon_035.mj", "Simon_037.mj", "Simon_038.mj",
    "Simon_040.mj", "Simon_041.mj", "Simon_045.mj", "Simon_047.mj", "Simon_049.mj", "Simon_056.mj",
    "Simon_064.mj", "Simon_065.mj", "Simon_069.mj", "Simon_070.mj", "Simon_077.mj", "Simon_081.mj",
    "Simon_091.mj", "Simon_093.mj", "Simon_094.mj", "Simon_097.mj", "Simon_101.mj", "Simon_102.mj",
    "Simon_113.mj", "Simon_114.mj", "Simon_115.mj", "Simon_126.mj", "Simon_127.mj", "Simon_128.mj",
    "Simon_129.mj", "fortyfive.mj", "fortysomething.mj", "Simon_131.mj", "Simon_132.mj", "Simon_134.mj",
    "Simon_136.mj", "Simon_142.mj")

  val testFiles = new java.io.File(TestDir).listFiles.filter(_.getName.endsWith(".mj"))

  if (KnownGoodFilesCheckMode) {
    for (file <- testFiles.filter(file => !KnownGoodFiles.contains(file.getName))) {
      file.getName should "produce correct output with our backend" in {
        file.getPath should passIntegrationTest(useFirmBackend = false)
      }
    }
  } else {
    for (file <- testFiles) {
      file.getName should "produce correct output with the FIRM backend" in {
        file.getPath should passIntegrationTest(useFirmBackend = true)
      }

      if (KnownGoodFiles.contains(file.getName)) {
        it should "produce correct output with our backend" in {
          file.getPath should passIntegrationTest(useFirmBackend = false)
        }
      }
    }
  }
}
