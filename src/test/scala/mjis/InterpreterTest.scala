package mjis

import mjis.CompilerTestMatchers._
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class InterpreterTest  extends FlatSpec with Matchers with BeforeAndAfter {

  val TestDir = "mj-test/run/"
  val testFiles = new java.io.File(TestDir).listFiles.filter(_.getName.endsWith(".mj")).sortBy(_.getName.toLowerCase)

  val slowTests = List("fannkuch.mj", "fixedpoint.mj", "mandelbrot.mj", "mandelbrot-mutable.mj", "MMul.mj", "Pi.mj", "RecursiveArraySum.mj", "Sorter.mj")
  val failingTests = List("Simon_048.mj")
  val ignoredTests = slowTests ++ failingTests

  for (file <- testFiles if !ignoredTests.contains(file.getName)) {
    file.getName should "produce correct output with the interpreter" in {
      file.getPath should passInterpreterTest
    }
  }
}
