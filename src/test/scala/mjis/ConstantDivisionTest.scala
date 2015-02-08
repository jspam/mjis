package mjis

import java.nio.file.{Files, Paths}
import mjis.CompilerTestMatchers._
import org.scalatest._
import scala.collection.JavaConversions._

class ConstantDivisionTest extends FlatSpec with Matchers {

  for (divisor <- (1 to 16).flatMap(i => Seq(i, -i)) ++ Seq(1 << 16, 1 << 31, (1 << 31) - 1)) {
    it should s"generate code for division by $divisor" in {
      Files.write(Paths.get("divByConst-test.check"), Array[Byte]())
      Files.write(Paths.get("divByConst-test.mj"),
        Files.readAllLines(Paths.get("assets/divByConst-test.mj")).map(_.replaceAll("\\$\\$", divisor.toString)))
      "divByConst-test.mj" should passIntegrationTest(Seq("--no-inline"))
    }
  }

}
